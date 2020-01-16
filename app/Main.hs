module Main where

import Prelude hiding (readFile)
import Text.Megaparsec
import Data.Text.IO (readFile)
import Data.Text.Prettyprint.Doc.Render.Text
import Language.WebIDL.AST
import Language.WebIDL.Parser (pFragment)
import Control.Monad.IO.Class
import Text.Pretty.Simple (pPrint)

parseFixture :: IO (Fragment SourcePos)
parseFixture = do
  content <- readFile "test/baselines/allowany.webidl"
  case (parse pFragment "test/baselines/allowany.webidl" content) of
    Right ast -> pure ast
    Left err -> do
      putStr $ errorBundlePretty err
      fail "Cannot parse fixture"

main :: IO ()
main = do
  ast <- parseFixture
  pPrint ast