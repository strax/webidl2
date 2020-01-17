module Main where

import Prelude hiding (readFile)
import Text.Megaparsec
import Data.Text.IO (readFile)
import Language.WebIDL.AST
import Language.WebIDL.Parser (pFragment)
import Text.Show.Pretty (pPrint)

main :: IO ()
main = do
  content <- readFile "test/invalid/namespace-readwrite.webidl"
  case (parse pFragment "test/invalid/namespace-readwrite.webidl" content) of
    Right ast -> pPrint ast
    Left err -> putStr $ errorBundlePretty err

