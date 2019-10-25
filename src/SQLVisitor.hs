module SQLVisitor (sqlVisit) where
  import Language.GraphQL.Draft.Syntax
  import Data.Text (unpack)
  import Lib
  import SymbolTable

  -- Converts the GQL AST to a SQL String
  sqlVisit :: SchemaDocument -> SymbolTable TypeDefinition -> [Char]
  sqlVisit ast symbolTable = visitSchemaDocument ast symbolTable

  -- Visits the Schema Document and converts it to SQL
  visitSchemaDocument :: SchemaDocument -> SymbolTable TypeDefinition -> [Char]
  visitSchemaDocument (SchemaDocument typeDefs) table =
    visitAll (visitTypeDefinition) typeDefs table ++
    visitAll (createForeignKey) typeDefs table ++ "\n"

  -- Visits the Type Definition and converts it to SQL
  visitTypeDefinition :: TypeDefinition -> SymbolTable TypeDefinition ->  [Char]
  visitTypeDefinition typeDef table = case typeDef of
    TypeDefinitionScalar scalar -> skip
    TypeDefinitionObject object -> visitObjectTypeDefinition object table
    TypeDefinitionInterface interface -> skip
    TypeDefinitionUnion union -> skip
    TypeDefinitionEnum enum -> skip
    TypeDefinitionInputObject inputObject -> skip

  -- Visits the Object Type Definition to generate SQL tables.
  visitObjectTypeDefinition :: ObjectTypeDefinition -> SymbolTable TypeDefinition -> [Char]
  visitObjectTypeDefinition (ObjectTypeDefinition _ name _ _ fields) table
    | isComposite name = skip
    | (fields == []) = skip
    | otherwise =
      "\nCREATE TABLE " ++ (toS name) ++ "(" ++
      visitAll (visitFieldDefinition) fields table ++ primaryKeys ++
      "\n);\n"
    where
      primaryKeys = case (getPrimaryKey (head fields) table) of
        Nothing -> skip
        Just pk -> "\n\tPRIMARY KEY (" ++ pk ++ ")"

  -- Visits each field in a type declaration and generates it as a SQL column.
  visitFieldDefinition :: FieldDefinition -> SymbolTable TypeDefinition -> [Char]
  visitFieldDefinition (FieldDefinition _ name args (TypeNamed nullability (NamedType typeName)) _) table
    | (isComposite typeName) = (visitType fieldType table) ++ (visitAll (visitArgument) args table)
    | otherwise = "\n\t" ++ (toS name) ++ " " ++ (visitType fieldType table) ++ (visitAll (visitArgument) args table) ++ ","
    where
      fieldType = (TypeNamed nullability (NamedType typeName))
      strName = (toS typeName)
  visitFieldDefinition _ _ = skip

  -- Converts a GQL type to a SQL type
  visitType :: GType -> SymbolTable TypeDefinition -> [Char]
  visitType (TypeNamed nullable (NamedType name)) table = case (toS name) of
    "Int" -> "INTEGER" ++ visitNullability nullable table
    "Float" -> "REAL" ++ visitNullability nullable table
    "String" -> "VARCHAR" ++ visitNullability nullable table
    "Boolean" -> "BOOLEAN" ++ visitNullability nullable table
    -- This has performance implications.
    -- Would be better with INTEGER, but some GQL may use this for UUIDs :(
    "ID" -> "VARCHAR" ++ visitNullability nullable table
    -- TODO: Visit GQL User-Defined Type w/ Symbol Table
    otherwise -> if (isComposite name)
      then visitCompositeKey (TypeNamed nullable (NamedType name)) table
      else getRelationColumns (TypeNamed nullable (NamedType name)) table
  -- TODO: Visit Lists of Primitives / User-Defined Types
  visitType (TypeList (Nullability isNullable) (ListType innerType)) table = skip

  -- TODO: Composite Keys
  visitCompositeKey :: GType -> SymbolTable TypeDefinition -> [Char]
  visitCompositeKey (TypeNamed nullable (NamedType name)) table = case (findElement table (toS name)) of
    Nothing -> skip
    (Just typeDef) -> case (typeDef) of
      TypeDefinitionObject (ObjectTypeDefinition _ name _ _ fields) -> visitAll (visitFieldDefinition) fields table
      otherwise -> skip

  -- Converts a GQL argument defintion to a SQL column property
  visitArgument :: InputValueDefinition -> SymbolTable TypeDefinition -> [Char]
  visitArgument (InputValueDefinition _ name argType defaultValue) table = case (toS name) of
    "default" -> visitDefaultArgument defaultValue table
    -- If there is any other column properties we want to specify, do so here
    otherwise -> skip

  -- Visits the default argument to set a SQL column property if applicable.
  visitDefaultArgument :: Maybe DefaultValue -> SymbolTable TypeDefinition -> [Char]
  visitDefaultArgument Nothing table = ""
  visitDefaultArgument (Just value) table = case value of
    VCInt num -> sqlDefault ++ (show num)
    VCFloat num -> sqlDefault ++ (show num)
    VCString (StringValue str) -> sqlDefault ++ "'" ++ (unpack str) ++ "'"
    VCBoolean bool -> sqlDefault ++ (toLowerCase (show bool))
    -- Skip enums, lists, and objects for now.
    otherwise -> skip

  -- Translates a participation constraint from GQL to SQL
  visitNullability :: Nullability -> SymbolTable TypeDefinition -> [Char]
  visitNullability (Nullability isNullable) table = if (isNullable) then "" else " NOT NULL"

  -- Applies the given visitor to all elements in the list and combines their results
  visitAll :: (e -> SymbolTable TypeDefinition -> [Char]) -> [e] -> SymbolTable TypeDefinition -> [Char]
  visitAll visit lst table = foldr (\ e acc -> (visit e table) ++ acc) "" lst

  -- Gets the Primary Key from a Field Definition
  getPrimaryKey :: FieldDefinition -> SymbolTable TypeDefinition -> Maybe [Char]
  getPrimaryKey (FieldDefinition _ name _ (TypeNamed _  (NamedType typeName)) _) table
    | (isPrimitive (toS typeName)) = Just (toS name)
    | otherwise = case (findElement table (toS typeName)) of
        Nothing -> Nothing
        (Just typeDef) -> Just (getFieldNames typeDef)
  getPrimaryKey _ _ = Nothing

  -- Creates a Foreign Key for the Type Defintion
  createForeignKey :: TypeDefinition -> SymbolTable TypeDefinition -> [Char]
  createForeignKey (TypeDefinitionObject (ObjectTypeDefinition _ name _ _ fields)) table =
    join $ map (\ (fieldName, fieldType, fieldPK) ->
      "\nALTER TABLE " ++ (toS name) ++
      " ADD FOREIGN KEY (" ++ fieldName ++ ") REFERENCES " ++
      fieldType ++ "(" ++ fieldPK ++ ");"
    ) foreignFields
    where
      foreignFields = foldr
        (\ field rest -> case () of
          _ | (isPrimitive $ fType field) -> rest
            | (isComposite $ (toName (fType field))) -> rest
            | otherwise -> case (primaryKeys field) of
                Nothing -> rest
                Just keys -> ((fName field, fType field, keys):rest)
        )
        []
        fields
      primaryKeys fieldDef = getPrimaryKey fieldDef table
      fType (FieldDefinition _ _ _ fieldType _) = (typeToS fieldType)
      fName (FieldDefinition _ fName _ _ _) = (toS fName)
  createForeignKey _ _ = skip

  -- Retrieves the primary keys of the relationship
  getRelationColumns :: GType -> SymbolTable TypeDefinition -> [Char]
  getRelationColumns (TypeNamed nullability (NamedType tName)) table = case (findElement table (toS tName)) of
    Nothing -> skip
    Just (TypeDefinitionObject (ObjectTypeDefinition _ name _ _ ((FieldDefinition _ fName _ fType _):t))) -> case (fType) of
      (TypeNamed _ theType) -> visitType (TypeNamed (Nullability True) theType) table
      (TypeList _ theType) -> skip -- TODO: Many to X relationships.
    Just _ -> skip
  getRelationColumns (TypeList nullability listType) table = skip

  -- Gets the names of all fields declared in a type declaration comma separated.
  getFieldNames :: TypeDefinition -> [Char]
  getFieldNames (TypeDefinitionObject (ObjectTypeDefinition _ name _ _ fields))
    | (isComposite name) = foldr (\ (FieldDefinition _ fName _ _ _) acc -> (toS fName) ++ (if (acc == []) then "" else ", ") ++ acc) [] fields
    | (isPrimitive (toS typeName)) = (toS firstName)
    | otherwise = skip
    where
      (FieldDefinition _ firstName _ firstType _) = (head fields)
      typeName = case (firstType) of
        (TypeNamed _ (NamedType tName)) -> tName
        (TypeList _ _) -> (toName skip)
  getFieldNames _ = skip

  -- Returns true if the named type is primitive, false otherwise
  isPrimitive :: [Char] -> Bool
  isPrimitive name = anyEq name ["Int", "Float", "String", "Boolean", "ID"]

  -- Returns true if the name represents a composite key.
  isComposite :: Name -> Bool
  isComposite name = endsWith (toS name) "_ID"

  -- Default visitor which turns any input into the empty string (i.e. it "skips" visiting itself).
  skip :: [Char]
  skip = ""

  -- Prints the default value argument in the schema.
  sqlDefault :: [Char]
  sqlDefault = " DEFAULT "

  -- Converts a Type to a String
  typeToS :: GType -> [Char]
  typeToS (TypeNamed nullability (NamedType name)) = toS name
  typeToS (TypeList nullability (ListType innerType)) = "[" ++ (typeToS innerType) ++ "]"
