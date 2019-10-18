module SQLVisitor (sqlVisit) where
  import Language.GraphQL.Draft.Syntax
  import Data.Text (unpack)

  -- Converts the GQL AST to a SQL String
  sqlVisit :: SchemaDocument -> [Char]
  sqlVisit ast = visitSchemaDocument ast

  -- Visits the Schema Document and converts it to SQL
  visitSchemaDocument :: SchemaDocument -> [Char]
  visitSchemaDocument (SchemaDocument typeDefs) = visitAll visitTypeDefinition typeDefs

  -- Visits the Type Definition and converts it to SQL
  visitTypeDefinition :: TypeDefinition -> [Char]
  visitTypeDefinition typeDef = case typeDef of
    -- TODO: Add Scalar to SymbolTable.
    TypeDefinitionScalar scalar -> skip scalar
    TypeDefinitionObject object -> visitObjectTypeDefinition object
    -- SQL gen only cares about actual types, so interfaces are skipped.
    TypeDefinitionInterface interface -> skip interface
    TypeDefinitionUnion union -> skip union
    TypeDefinitionEnum enum -> skip enum
    -- Inputs only affect mutations, so inputs are skipped.
    TypeDefinitionInputObject inputObject -> skip inputObject

  -- Visits the Object Type Definition to generate SQL tables.
  visitObjectTypeDefinition :: ObjectTypeDefinition -> [Char]
  visitObjectTypeDefinition (ObjectTypeDefinition _ name _ _ fields) =
    "CREATE TABLE " ++ (toS name) ++ "(" ++
    foldr (\ e acc -> (visitFieldDefinition e) ++ (if (acc == []) then "" else ",") ++ acc) "" fields ++
    "\n);"

  -- Visits each field in a type declaration and generates it as a SQL column.
  visitFieldDefinition :: FieldDefinition -> [Char]
  visitFieldDefinition (FieldDefinition _ name args fieldType _) =
    -- TODO: Visit Args
    "\n\t" ++ (toS name) ++ " " ++ (visitType fieldType)

  -- Applies the given visitor to all elements in the list and combines their results
  visitAll :: (a -> [Char]) -> [a] -> [Char]
  visitAll visit lst = foldr (\ e acc -> (visit e) ++ acc) "" lst

  -- Default visitor which turns any input into the empty string (i.e. it "skips" visiting itself).
  skip :: a -> [Char]
  skip any = ""

  -- Converts names to Strings
  toS :: Name -> [Char]
  toS (Name text) = unpack text
