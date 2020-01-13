import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Language.WebIDL.Parser (pFragment)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

baselines :: [String]
baselines =
  [ "Lexing",
    "Simple",
    "Example10",
    "Example39",
    "Example12",
    "Example8",
    "ExtendedAttributes",
    "DOM",
    "HTMLDOM",
    "Regression1",
    "Regression2",
    "PartialInterfaceMixin",
    "Types"
  ]

baselinePath :: String -> String
baselinePath name = "test/baselines/" <> name <> ".webidl"

main :: IO ()
main = hspec $ do
  describe "Integration tests" $ do
    forM_ baselines $ \name -> it (baselinePath name) $ do
      raw <- T.pack <$> readFile (baselinePath name)
      parse pFragment "" `shouldSucceedOn` raw
