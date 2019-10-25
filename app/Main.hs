module Main where

import System.Environment
import System.Exit
import Lib
import Language.GraphQL.Draft.Parser
import Data.String
import qualified Language.GraphQL.Draft.Syntax as AST
import Data.Text

import SQLVisitor
import GQLSymbolTable
import SymbolTable (SymbolTable)

-- Reads a file path from the command line and outputs sql/dot files.
main :: IO ()
main = do
  args <- getArgs
  let inputFile = if (args == []) then "" else Prelude.head args
  if (inputFile == "")
    then print (pack "ERROR: Missing file argument") >> exitWith (ExitFailure 1)
    else do
      schemaFile <- readFile inputFile
      let schema = parseSchemaDoc (fromString schemaFile)
      either
        (\ arg -> print (pack ("ERROR: Failed to parse graphql document: " ++ (unpack arg))))
        (codegen inputFile)
        schema

-- Generates the sql output files from the gql ast.
codegen :: [Char] -> AST.SchemaDocument -> IO()
codegen filePath ast = do
  let symbolTable = constructTable ast
  writeFile (getOutputPath filePath "sql") (ast2sql ast symbolTable)

-- Converts a GraphQL Graph to a SQL Graph
ast2sql :: AST.SchemaDocument -> SymbolTable AST.TypeDefinition -> [Char]
ast2sql schema symbolTable = "\
  \CREATE DATABASE IF NOT EXISTS TestDB;\n\
  \USE TestDB;\n\
\" ++ (sqlVisit schema symbolTable)
