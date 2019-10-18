module Main where

import System.Environment
import Lib
import Language.GraphQL.Draft.Parser
import Data.String
import qualified Language.GraphQL.Draft.Syntax as AST
import Data.Text

import SQLVisitor

-- Reads a file path from the command line and outputs sql/dot files.
main :: IO ()
main = do
  args <- getArgs
  let inputFile = Prelude.head args
  schemaFile <- readFile inputFile
  let schema = parseSchemaDoc (fromString schemaFile)
  either
    (print) -- TODO: Error Handling.
    (codegen inputFile)
    schema

-- Generates the sql and dot output files from the gql ast.
codegen :: [Char] -> AST.SchemaDocument -> IO()
codegen filePath ast = do
  writeFile (getOutputPath filePath "sql") (ast2sql ast)
  writeFile (getOutputPath filePath "gv") (ast2dot ast)

-- TODO
ast2sql :: AST.SchemaDocument -> [Char]
ast2sql schema = "\
  \CREATE DATABASE IF NOT EXISTS TestDB;\n\
  \USE TestDB;\n\n\
\" ++ (sqlVisit schema)


-- TODO
ast2dot :: AST.SchemaDocument -> [Char]
ast2dot schema = "graph graphname {\
  \a -- b -- c;\
  \b -- d;\
\}"
