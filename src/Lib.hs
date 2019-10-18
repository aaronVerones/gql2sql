module Lib
    ( getOutputPath
    ) where

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
