module Lib where
    import Language.GraphQL.Draft.Syntax
    import Data.Char (toLower)
    import Data.Text (pack, unpack)

    -- Gets the name of the file from the given file path.
    fileNameFromPath :: [Char] -> [Char]
    fileNameFromPath path = head $ reverse $ splitBySlash path
        where splitBySlash = foldr (\ e (h:t) -> if (e == '/') then ([]:(h:t)) else ((e:h):t)) [[]]

    -- Removes the extension from the given file.
    removeFileExtension :: [Char] -> [Char]
    removeFileExtension file = foldr (\ e acc -> if (e == '.') then [] else (e:acc)) "" file

    -- Gets the output path to the input file name, adding the given extension.
    getOutputPath :: [Char] -> [Char] -> [Char]
    getOutputPath inputPath extension = "output/" ++ (removeFileExtension $ fileNameFromPath inputPath) ++ ('.':extension)

    -- Returns true if the input is equal to any in the list.
    anyEq :: Eq e => e -> [e] -> Bool
    anyEq e [] = False
    anyEq e (h:t) = (h == e) || anyEq e t

    -- Converts a name to a string
    toS :: Name -> [Char]
    toS (Name text) = unpack text

    -- Converts a string to its lowercase form
    toLowerCase :: [Char] -> [Char]
    toLowerCase str = map toLower str

    -- True if the string starts with the given input
    startsWith :: [Char] -> [Char] -> Bool
    startsWith "" "" = True
    startsWith str "" = True
    startsWith "" postfix = False
    startsWith (h1:t1) (h2:t2) = (h1 == h2) && (startsWith t1 t2)

    -- True if the string ends with the given input
    endsWith :: [Char] -> [Char] -> Bool
    endsWith str postfix = startsWith (reverse str) (reverse postfix)
