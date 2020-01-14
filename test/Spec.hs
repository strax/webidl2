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

invalidTests = 
  [ "any-keyword"
  , "argument-dict-default"
  , "argument-dict-nullable"
  , "argument-dict-optional"
  , "array"
  , "async-iterable-readonly"
  , "async-iterable-single"
  , "async-maplike"
  , "callback-attribute"
  , "callback-noassign"
  , "callback-noparen"
  , "callback-noreturn"
  , "callback-semicolon"
  , "caller"
  , "const-nullable"
  , "const-null"
  , "constructible-global"
  , "constructor-escaped"
  , "constructor"
  , "dict-field-unterminated"
  , "dict-no-default"
  , "dict-required-default"
  , "duplicate-escaped"
  , "duplicate"
  , "enum-bodyless"
  , "enum-empty"
  , "enum"
  , "enum-wo-comma"
  , "exception"
  , "exposed"
  , "extattr-double-field"
  , "extattr-double"
  , "extattr-empty-ids"
  , "float"
  , "frozenarray-empty"
  , "id-underscored-number"
  , "implements_and_includes_ws"
  , "implements"
  , "inheritance-typeless"
  , "inherit-readonly"
  , "int32array-keyword"
  , "iterable-empty"
  , "iterable-notype"
  , "iterator"
  , "legacyiterable"
  , "maplike-1type"
  , "module"
  , "namespace-readwrite"
  , "nonempty-sequence"
  , "nonnullableany"
  , "nonnullableobjects"
  , "no-semicolon-callback"
  , "no-semicolon-operation"
  , "no-semicolon"
  , "nullable-union-dictionary"
  , "operation-nameless"
  , "operation-too-special"
  , "overloads"
  , "promise-empty"
  , "promise-nullable"
  , "promise-with-extended-attribute"
  , "raises"
  , "readonly-iterable"
  , "record-key"
  , "record-key-with-extended-attribute"
  , "record-single"
  , "recursive-type"
  , "scopedname"
  , "sequenceAsAttribute"
  , "sequence-empty"
  , "setlike-2types"
  , "setter-creator"
  , "spaced-negative-infinity"
  , "spaced-variadic"
  , "special-omittable"
  , "stray-slash"
  , "stringconstants"
  , "tostring-escaped"
  , "tostring"
  , "typedef-nested"
  , "union-any"
  , "union-dangling-or"
  , "union-one"
  , "union-promise"
  , "union-zero"
  , "unknown-generic" ]

baselinePath :: String -> String
baselinePath name = "test/baselines/" <> name <> ".webidl"

invalidFixturePath :: String -> String
invalidFixturePath name = "test/invalid/" <> name <> ".webidl"

main :: IO ()
main = hspec $ do
  describe "Integration tests" $ do
    forM_ baselines $ \name -> it name $ parseFixture name

  describe "Conformance tests" $ do
    forM_ conformanceTests $ \name -> it name $ parseFixture name

  describe "Invalid" $ do
    forM_ invalidTests $ \name -> it name $ do
      raw <- T.pack <$> readFile (invalidFixturePath name)
      parse pFragment (invalidFixturePath name) `shouldFailOn` raw


parseFixture :: String -> IO ()
parseFixture name = do
  raw <- T.pack <$> readFile (baselinePath name)
  parse pFragment (baselinePath name) `shouldSucceedOn` raw