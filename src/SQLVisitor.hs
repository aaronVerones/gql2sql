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
  visitSchemaDocument (SchemaDocument typeDefs) table = visitAll (visitTypeDefinition) typeDefs table

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
  visitObjectTypeDefinition (ObjectTypeDefinition _ name _ _ fields) table =
    if (endsWith (toS name) "_ID") then skip else
    "\nCREATE TABLE " ++ (toS name) ++ "(" ++
    visitAll (visitFieldDefinition) fields table ++ (createPrimaryKey (head fields) table) ++
    "\n);\n"

  -- Visits each field in a type declaration and generates it as a SQL column.
  visitFieldDefinition :: FieldDefinition -> SymbolTable TypeDefinition -> [Char]
  visitFieldDefinition (FieldDefinition _ name args fieldType _) table =
    "\n\t" ++ (toS name) ++ " " ++ (visitType fieldType table) ++ (visitAll (visitArgument) args table) ++ ","

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
    otherwise -> ""
  -- TODO: Visit Lists of Primitives / User-Defined Types
  visitType (TypeList (Nullability isNullable) (ListType innerType)) table = ""

  -- Converts a GQL argument defintion to a SQL column property
  visitArgument :: InputValueDefinition -> SymbolTable TypeDefinition -> [Char]
  visitArgument (InputValueDefinition _ name argType defaultValue) table = case (toS name) of
    "default" -> visitDefaultArgument defaultValue table
    -- If there is any other column properties we want to specify, do so here
    otherwise -> ""

  -- Visits the default argument to set a SQL column property if applicable.
  visitDefaultArgument :: Maybe DefaultValue -> SymbolTable TypeDefinition -> [Char]
  visitDefaultArgument Nothing table = ""
  visitDefaultArgument (Just value) table = case value of
    VCInt num -> " DEFAULT " ++ (show num)
    VCFloat num -> " DEFAULT " ++ (show num)
    VCString (StringValue str) -> " DEFAULT '" ++ (unpack str) ++ "'"
    VCBoolean bool -> " DEFAULT " ++ (toLowerCase (show bool))
    -- Skip enums, lists, and objects for now.
    otherwise -> ""

  -- Translates a participation constraint from GQL to SQL
  visitNullability :: Nullability -> SymbolTable TypeDefinition -> [Char]
  visitNullability (Nullability isNullable) table = if (isNullable) then "" else " NOT NULL"

  -- Creates a Primary Key from the Field Definition
  createPrimaryKey :: FieldDefinition -> SymbolTable TypeDefinition -> [Char]
  createPrimaryKey (FieldDefinition _ name _ _ _) table = "\n\tPRIMARY KEY (" ++ (toS name) ++ ")"

  -- Applies the given visitor to all elements in the list and combines their results
  visitAll :: (e -> SymbolTable TypeDefinition -> [Char]) -> [e] -> SymbolTable TypeDefinition -> [Char]
  visitAll visit lst table = foldr (\ e acc -> (visit e table) ++ acc) "" lst

  -- Default visitor which turns any input into the empty string (i.e. it "skips" visiting itself).
  skip :: [Char]
  skip = ""
