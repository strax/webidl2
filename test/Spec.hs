import           Control.Monad
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Language.WebIDL.Parser         ( pFragment )
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec                ( parse )

baselines :: [String]
baselines =
  [ "Lexing"
  , "Simple"
  , "Example10"
  , "Example39"
  , "Example12"
  , "Example8"
  , "ExtendedAttributes"
  , "DOM"
  , "HTMLDOM"
  , "Regression1"
  , "Regression2"
  , "PartialInterfaceMixin"
  , "Types" ]

-- Baselines from https://github.com/w3c/webidl2.js/tree/gh-pages/test/syntax/idl
conformanceTests :: [String]
conformanceTests =
  [ "allowany"
  , "argument-constructor"
  , "argument-extattrs"
  , "async-iterable"
  , "async-name"
  , "callback"
  , "constants"
  , "constructor"
  , "default"
  , "dictionary-inherits"
  , "dictionary"
  , "documentation-dos"
  , "documentation"
  , "enum"
  , "equivalent-decl"
  , "escaped-name"
  , "escaped-type"
  , "extended-attributes"
  , "generic"
  , "getter-setter"
  , "identifier-hyphen"
  , "identifier-qualified-names"
  , "includes-name"
  , "indexed-properties"
  , "inherits-getter"
  , "interface-inherits"
  , "iterable"
  , "maplike"
  , "mixin"
  , "namedconstructor"
  , "namespace"
  , "nointerfaceobject"
  , "nullable"
  , "nullableobjects"
  , "obsolete-keywords"
  , "operation-optional-arg"
  , "overloading"
  , "overridebuiltins"
  , "partial-interface"
  , "primitives"
  , "promise-void"
  , "prototyperoot"
  , "putforwards"
  , "record"
  , "reflector-interface"
  , "reg-operations"
  , "replaceable"
  , "sequence"
  , "setlike"
  , "static"
  , "stringifier-attribute"
  , "stringifier"
  , "treatasnull"
  , "treatasundefined"
  , "typedef-union"
  , "typedef"
  , "typesuffixes"
  , "uniontype"
  , "variadic-operations" ]

baselinePath :: String -> String
baselinePath name = "test/baselines/" <> name <> ".webidl"

main :: IO ()
main = hspec $ do
  describe "Integration tests" $ do
    forM_ baselines $ \name -> it name $ parseFixture name

  describe "Conformance tests" $ do
    forM_ conformanceTests $ \name -> it name $ parseFixture name


parseFixture :: String -> IO ()
parseFixture name = do
  raw <- T.pack <$> readFile (baselinePath name)
  parse pFragment (baselinePath name) `shouldSucceedOn` raw