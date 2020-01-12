module Main where

import Prelude hiding (readFile)
import Text.Megaparsec
import Data.Text.IO (readFile)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Text
import Language.WebIDL.AST
import Language.WebIDL.Parser (pFragment)
import Text.Show.Pretty (pPrint)

main :: IO ()
main = do
  content <- readFile "test/baselines/DOM.webidl"
  case (parse pFragment "test/baselines/DOM.webidl" content) of
    Right ast -> pPrint ast
    Left err -> putStr $ errorBundlePretty err

