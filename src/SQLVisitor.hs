module SQLVisitor (sqlVisit) where
  import Language.GraphQL.Draft.Syntax

  -- Converts the GQL AST to a SQL String
  sqlVisit :: SchemaDocument -> [Char]
  sqlVisit ast = visit ast

  -- Recursively visits nodes of the gql ast to convert to sql.
  visit :: SchemaDocument -> [Char]
  visit _ = ""
