module Main where

import Lib
import Language.GraphQL.Draft.Parser
import Data.String

main :: IO ()
main = do
  schemaFile <- readFile "schema.graphql"
  let schema = parseSchemaDoc (fromString schemaFile)
  print schema
