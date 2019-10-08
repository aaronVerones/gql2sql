module Main where

import Lib
import Language.GraphQL.Draft.Parser
import Data.String
import qualified Language.GraphQL.Draft.Syntax as AST
import Data.Text

main :: IO ()
main = do
  schemaFile <- readFile "schema.graphql"
  let schema = parseSchemaDoc (fromString schemaFile)
  print schema
  let sql = ast2sql schema
  writeFile "schema.sql" sql
  let dot = ast2dot schema
  writeFile "schema.gv" dot
  

-- TODO
ast2sql :: Either Text AST.SchemaDocument -> String
ast2sql schema = "CREATE DATABASE helloWorld;"


-- TODO
ast2dot :: Either Text AST.SchemaDocument -> String
ast2dot schema = "graph graphname {\
  \a -- b -- c;\
  \b -- d;\
\}"