module SQLVisitor (sqlVisit) where
  import Language.GraphQL.Draft.Syntax
  import Data.Text (unpack)
  import Lib
  import SymbolTable

  type VisitTable = SymbolTable TypeDefinition
  type Visitor = VisitTable -> [Char]
  type MaybeVisitor = VisitTable -> Maybe [Char]
  type StateVisitor = VisitTable -> VisitTable -> ([Char], VisitTable)

  -- Converts the GQL AST to a SQL String
  sqlVisit :: SchemaDocument -> Visitor
  sqlVisit ast symbolTable = visitSchemaDocument ast symbolTable

  -- Visits the Schema Document and converts it to SQL
  visitSchemaDocument :: SchemaDocument -> Visitor
  visitSchemaDocument (SchemaDocument typeDefs) table =
    visitAll (visitTypeDefinition) typeDefs table ++
    manyToManyTables ++
    visitAll (createForeignKey) typeDefs table
    where
      (manyToManyTables, manyToManyTypes) = visitAllStateful (createManyToMany) typeDefs table Empty

  -- Visits the Type Definition and converts it to SQL
  visitTypeDefinition :: TypeDefinition -> Visitor
  visitTypeDefinition typeDef table = case typeDef of
    TypeDefinitionScalar scalar -> skip
    TypeDefinitionObject object -> visitObjectTypeDefinition object table
    TypeDefinitionInterface interface -> skip
    TypeDefinitionUnion union -> skip
    TypeDefinitionEnum enum -> skip
    TypeDefinitionInputObject inputObject -> skip

  -- Visits the Object Type Definition to generate SQL tables.
  visitObjectTypeDefinition :: ObjectTypeDefinition -> Visitor
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
  visitFieldDefinition :: FieldDefinition -> Visitor
  visitFieldDefinition (FieldDefinition _ name args (TypeNamed nullability (NamedType typeName)) _) table
    | (isComposite typeName) = (visitType fieldType table) ++ (visitAll (visitArgument) args table)
    | otherwise = printName ++ visitedTypes ++ comma
    where
      fieldType = (TypeNamed nullability (NamedType typeName))
      visitedTypes = (visitType fieldType table) ++ (visitAll (visitArgument) args table)
      printName = if (contains visitedTypes '\n') then "" else "\n\t" ++ (toS name) ++ " "
      comma = if (endsWith visitedTypes ",") then "" else ","
  visitFieldDefinition _ _ = skip

  -- Converts a GQL type to a SQL type
  visitType :: GType -> Visitor
  visitType (TypeNamed nullable (NamedType name)) table = case (toS name) of
    "Int" -> "INTEGER" ++ visitNullability nullable table
    "Float" -> "REAL" ++ visitNullability nullable table
    "String" -> "VARCHAR" ++ visitNullability nullable table
    "Boolean" -> "BOOLEAN" ++ visitNullability nullable table
    -- This has performance implications.
    -- Would be better with INTEGER, but some GQL may use this for UUIDs :(
    "ID" -> "VARCHAR" ++ visitNullability nullable table
    otherwise -> if (isComposite name)
      then visitCompositeKey (TypeNamed nullable (NamedType name)) table
      else getRelationColumns (TypeNamed nullable (NamedType name)) table
  visitType (TypeList (Nullability isNullable) (ListType innerType)) table = skip

  -- Creates a composite key based on the table association of _ID.
  visitCompositeKey :: GType -> Visitor
  visitCompositeKey (TypeNamed nullable (NamedType name)) table = case (findElement table (toS name)) of
    Nothing -> skip
    (Just typeDef) -> case (typeDef) of
      TypeDefinitionObject (ObjectTypeDefinition _ name _ _ fields) -> visitAll (visitFieldDefinition) fields table
      otherwise -> skip

  -- Converts a GQL argument defintion to a SQL column property
  visitArgument :: InputValueDefinition -> Visitor
  visitArgument (InputValueDefinition _ name argType defaultValue) table = case (toS name) of
    "default" -> visitDefaultArgument defaultValue table
    -- If there is any other column properties we want to specify, do so here
    otherwise -> skip

  -- Visits the default argument to set a SQL column property if applicable.
  visitDefaultArgument :: Maybe DefaultValue -> Visitor
  visitDefaultArgument Nothing table = ""
  visitDefaultArgument (Just value) table = case value of
    VCInt num -> sqlDefault ++ (show num)
    VCFloat num -> sqlDefault ++ (show num)
    VCString (StringValue str) -> sqlDefault ++ "'" ++ (unpack str) ++ "'"
    VCBoolean bool -> sqlDefault ++ (toLowerCase (show bool))
    -- Skip enums, lists, and objects for now.
    otherwise -> skip

  -- Translates a participation constraint from GQL to SQL
  visitNullability :: Nullability -> Visitor
  visitNullability (Nullability isNullable) table = if (isNullable) then "" else " NOT NULL"

  -- Applies the given visitor to all elements in the list and combines their results
  visitAll :: (e -> Visitor) -> [e] -> Visitor
  visitAll visit lst table = foldr (\ e acc -> (visit e table) ++ acc) "" lst

  -- Applies the given stateful visitor to all elements in the list and combines their results
  -- Easily my favourite function in this entire module.
  visitAllStateful :: (e -> StateVisitor) -> [e] -> StateVisitor
  visitAllStateful visitWithState lst table seen =
    foldl
    (\ (prevResult, prevState) e ->
      let (newResult, newState) = visitWithState e table prevState
      in (prevResult ++ newResult, newState))
    (skip, seen)
    lst

  -- Gets the Primary Key from a Field Definition
  getPrimaryKey :: FieldDefinition -> MaybeVisitor
  getPrimaryKey (FieldDefinition _ name _ (TypeNamed _  (NamedType typeName)) _) table
    | (isPrimitive (toS typeName)) = Just (toS name)
    | otherwise = case (findElement table (toS typeName)) of
        Nothing -> Nothing
        (Just typeDef) -> Just (getFieldNames typeDef table)
  getPrimaryKey _ _ = Nothing

  -- Creates a Foreign Key for the Type Defintion
  createForeignKey :: TypeDefinition -> Visitor
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

  -- Creates all many to many tables inferred by the type definition.
  -- Easily my least favourite function in the entire module (written in a Denny's after my bedtime)
  createManyToMany :: TypeDefinition -> StateVisitor
  createManyToMany (TypeDefinitionObject (ObjectTypeDefinition desc name i d fields)) table seen = result
    where
      me = (TypeDefinitionObject (ObjectTypeDefinition desc name i d fields))
      -- Checks if a given field has a list type for the many to many relation.
      checkField (FieldDefinition _ fName _ fType _) = case (fType) of
        (TypeNamed _ namedType) -> Nothing
        (TypeList _ listType) -> Just listType
      -- Returns true if we have already seen this type definition.
      alreadySeen = case (findElement seen (toS name)) of
        Nothing -> isComposite name
        (Just _) -> True
      -- Returns true if the other type as a many relation to myself
      containsManyMe (TypeDefinitionObject (ObjectTypeDefinition _ otherName _ _ otherFields)) =
        any
          ( \ (FieldDefinition _ fName _ fType _) -> case (fType) of
                (TypeList _ (ListType (TypeNamed _ (NamedType innerName)))) -> innerName == name
                otherwise -> False )
          otherFields
      containsManyMe _ = False
      -- Creates the tables for the many to many relation
      createTables other =
        let
          (TypeDefinitionObject (ObjectTypeDefinition _ otherName _ _ otherFields)) = other
          combinedName = (toS name) ++ "_and_" ++ (toS otherName)
          primaryKeys (FieldDefinition _ fieldName _ fieldType _) =
            if (isPrimitive (typeToS fieldType))
            then [((toS fieldName), (primitiveToS $ typeToS fieldType), (toS name))]
            else case (findElement table (typeToS fieldType)) of
              Nothing -> []
              (Just (TypeDefinitionObject (ObjectTypeDefinition _ tableName _ _ compFields))) ->
                (foldr
                  (\ (FieldDefinition _ fName _ fType _) acc ->
                    ((toS fName), (primitiveToS $ typeToS fType), (toS otherName)):acc )
                  []
                  compFields)
              otherwise -> []
          primaryKeysCombined = (primaryKeys (head fields)) ++ (primaryKeys (head otherFields))
          combineForeignKeys [] = []
          combineForeignKeys (h:[]) = [h]
          combineForeignKeys ((k1Name, k1Type, k1Table) : ((k2Name, k2Type, k2Table) : t)) =
            if (k1Table == k2Table)
              then (k1Name ++ ", " ++ k2Name, k1Type, k1Table):(combineForeignKeys t)
              else (k1Name, k1Type, k1Table):(combineForeignKeys ((k2Name, k2Type, k2Table):t))
        in "\nCREATE TABLE " ++ combinedName ++ "(\n" ++
           (foldr (\ (kName, kType, _) acc -> "\t" ++ kName ++ " " ++ kType ++ " NOT NULL,\n" ++ acc) "" primaryKeysCombined) ++
           "\tPRIMARY KEY (" ++ (foldr (\ (kName, _, _) acc -> kName ++ (if (acc == []) then "" else ", ") ++ acc) ""  primaryKeysCombined) ++ "),\n" ++
           (foldr
            (\ (kNames, _, kTable) acc ->
              "\tFOREIGN KEY (" ++ kNames ++ ") REFERENCES " ++ kTable ++ "(" ++ kNames ++ ")"
              ++ (if (acc == []) then "" else ",") ++ "\n" ++ acc )
            ""
            (combineForeignKeys primaryKeysCombined)) ++
           ");\n"
      -- Adds the other type to the table of already visited types.
      markAsSeen otherType alreadySeen =
        let (TypeDefinitionObject (ObjectTypeDefinition _ otName _ _ _)) = otherType
        in addElement alreadySeen (toS otName) otherType
      -- Checks each field of this type for a many-to-many relation,
      -- creating the many-to-many tables along the way.
      addSelf (value, state) = (value, addElement state (toS name) me)
      result = if alreadySeen then (skip, seen) else
        addSelf $ (foldr
          (\ field (prevResult, prevSeen) -> let continue = (prevResult, prevSeen) in
            case (checkField field) of
              Nothing -> continue
              (Just (ListType inner)) -> case (inner) of
                (TypeList _ _) -> continue
                (TypeNamed _ (NamedType tName)) -> case (findElement table (toS tName)) of
                  Nothing -> continue
                  (Just otherType) -> if (containsManyMe otherType)
                    then ((createTables otherType) ++ prevResult, markAsSeen otherType prevSeen)
                    else continue
            )
          ("", seen)
          fields)
  createManyToMany _ _ seen = (skip, seen)

  -- Retrieves the primary keys of the relationship
  getRelationColumns :: GType -> Visitor
  getRelationColumns (TypeNamed nullability (NamedType tName)) table = case (findElement table (toS tName)) of
    Nothing -> skip
    Just (TypeDefinitionObject (ObjectTypeDefinition _ name _ _ ((FieldDefinition _ fName _ fType _):t))) -> case (fType) of
      (TypeNamed _ theType) -> visitType (TypeNamed (Nullability True) theType) table
      (TypeList _ theType) -> skip
    Just _ -> skip
  getRelationColumns (TypeList nullability listType) table = skip

  -- Gets the names of all fields declared in a type declaration comma separated.
  getFieldNames :: TypeDefinition -> Visitor
  getFieldNames (TypeDefinitionObject (ObjectTypeDefinition _ name _ _ fields)) table
    | (isComposite name) = foldr (\ (FieldDefinition _ fName _ _ _) acc -> (toS fName) ++ (if (acc == []) then "" else ", ") ++ acc) [] fields
    | (isPrimitive (toS typeName)) = (toS firstName)
    | otherwise = case (findElement table (toS typeName)) of
      Nothing -> skip
      (Just typeDef) -> getFieldNames typeDef table
    where
      (FieldDefinition _ firstName _ firstType _) = (head fields)
      typeName = case (firstType) of
        (TypeNamed _ (NamedType tName)) -> tName
        (TypeList _ _) -> (toName skip)
  getFieldNames _ _ = skip

  -- Converts a GQL primitive to a SQL primitive
  primitiveToS :: [Char] -> [Char]
  primitiveToS primitive = case primitive of
    "Int" -> "INTEGER"
    "Float" -> "REAL"
    "String" -> "VARCHAR"
    "Boolean" -> "BOOLEAN"
    "ID" -> "VARCHAR"
    otherwise -> skip

  -- Returns true if the named type is primitive, false otherwise
  isPrimitive :: [Char] -> Bool
  isPrimitive name = anyEq name ["Int", "Float", "String", "Boolean", "ID"]

  -- Returns true if the name represents a composite key.
  isComposite :: Name -> Bool
  isComposite name = endsWith (toS name) "_ID"

  -- Returns true if the name is a user-defined name, false otherwise.
  -- Excludes types representing composite keys.
  isUserDefined :: Name -> VisitTable -> Bool
  isUserDefined name table =
    not (isPrimitive (toS name)) &&
    not (isComposite name) &&
    exists (findElement table (toS name))

  -- Default visitor which turns any input into the empty string (i.e. it "skips" visiting itself).
  skip :: [Char]
  skip = ""

  -- Prints the default value argument in the schema.
  sqlDefault :: [Char]
  sqlDefault = " DEFAULT "

  -- Converts a Type to a Name
  -- If the type is a list, get the inner name.
  typeToName :: GType -> Name
  typeToName (TypeNamed _ (NamedType name)) = name
  typeToName (TypeList nullability (ListType innerType)) = typeToName innerType

  -- Converts a Type to a String
  typeToS :: GType -> [Char]
  typeToS (TypeNamed nullability (NamedType name)) = toS name
  typeToS (TypeList nullability (ListType innerType)) = "[" ++ (typeToS innerType) ++ "]"
