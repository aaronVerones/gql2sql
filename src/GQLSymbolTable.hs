module GQLSymbolTable (constructTable) where
  import Language.GraphQL.Draft.Syntax
  import SymbolTable
  import Lib

  -- Creates a GraphQL Symbol Table based on the GQL AST.
  constructTable :: SchemaDocument -> SymbolTable TypeDefinition
  constructTable ast = visitSchemaDocument ast Empty

  -- Visits the Schema Document Node to construct the GraphQL Symbol Table.
  visitSchemaDocument :: SchemaDocument -> SymbolTable TypeDefinition -> SymbolTable TypeDefinition
  visitSchemaDocument (SchemaDocument typeDefs) symbolTable = visitAll (visitTypeDefinition) typeDefs symbolTable

  -- Visits the Type Declaration Node and adds it to the symbol table.
  visitTypeDefinition :: TypeDefinition -> SymbolTable TypeDefinition -> SymbolTable TypeDefinition
  visitTypeDefinition typeDef symbolTable = case typeDef of
    TypeDefinitionScalar (ScalarTypeDefinition desc name dirs) -> addElement symbolTable (toS name) typeDef
    TypeDefinitionObject (ObjectTypeDefinition desc name is dirs fields) -> addElement symbolTable (toS name) typeDef
    TypeDefinitionInterface interface -> skip interface symbolTable
    TypeDefinitionUnion (UnionTypeDefinition desc name dirs mems) -> addElement symbolTable (toS name) typeDef
    TypeDefinitionEnum (EnumTypeDefinition desc name dirs valDefs) -> addElement symbolTable (toS name) typeDef
    TypeDefinitionInputObject inputObject -> skip inputObject symbolTable

  -- Applies the given visitor to all elements in the list and combines their results
  visitAll :: (e -> SymbolTable TypeDefinition -> SymbolTable TypeDefinition) -> [e] -> SymbolTable TypeDefinition -> SymbolTable TypeDefinition
  visitAll visit lst table = foldr (\ e acc -> visit e acc) table lst

  -- Default visitor which turns any input into an empty symbol table (i.e. it "skips" visiting itself).
  skip :: a -> SymbolTable TypeDefinition -> SymbolTable TypeDefinition
  skip any table = table
