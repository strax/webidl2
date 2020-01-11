import Test.Hspec
import Test.Hspec.Megaparsec
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Text.Megaparsec (parse)
import Language.WebIDL.Parser (pFragment)

baselines :: [String]
baselines = ["Lexing", "Simple", "Example10", "Example39", "Example12", "Example8", "ExtendedAttributes", "DOM"]

baselinePath :: String -> String
baselinePath name = "test/baselines/" <> name <> ".webidl"

main :: IO ()
main = hspec $ do
    describe "Integration tests" $ do
        forM_ baselines $ \name -> it (baselinePath name) $ do
            raw <- T.pack <$> readFile (baselinePath name)
            parse pFragment "" `shouldSucceedOn` raw
