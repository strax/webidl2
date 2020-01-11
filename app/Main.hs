module Main where

import Prelude hiding (readFile)
import Text.Megaparsec
import Data.Text.IO (readFile)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Text
import Language.WebIDL.AST
import Language.WebIDL.Printer
import Language.WebIDL.Parser (pFragment)

main :: IO ()
main = do
  content <- readFile "test/baselines/Example39.webidl"
  case (parse pFragment "test/baselines/Example39.webidl" content) of
    Right ast -> putDoc (pretty ast)
    Left err -> putStr $ errorBundlePretty err

