module Language.WebIDL.Parser.Lex where

import Prelude hiding (and, or)
import qualified Text.Megaparsec.Char.Lexer    as L
import Language.WebIDL.Parser.Types
import Text.Megaparsec
import Text.Megaparsec.Char (space1, string, alphaNumChar)
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Char (isAsciiUpper, isAsciiLower, isDigit)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T

-- | Consumes whitespace, line comments and block comments
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

argumentNameKeywords :: [Text]
argumentNameKeywords = [
    "async",
    "attribute",
    "callback",
    "const",
    "constructor",
    "deleter",
    "dictionary",
    "enum",
    "getter",
    "includes",
    "inherit",
    "interface",
    "interable",
    "maplike",
    "mixin",
    "namespace",
    "partial",
    "readonly",
    "required",
    "setlike",
    "setter",
    "static",
    "stringifier",
    "typedef",
    "unrestricted" ]

attributeNameKeywords :: [Text]
attributeNameKeywords = ["async", "required"]

operationNameKeywords :: [Text]
operationNameKeywords = ["includes"]

primitiveTypes :: [Text]
primitiveTypes = [
    "unsigned",
    "short",
    "long",
    "float",
    "double",
    "boolean",
    "byte",
    "octet" ]

stringTypes :: [Text]
stringTypes = [
    "ByteString",
    "DOMString",
    "USVString" ]

bufferRelatedTypes :: [Text]
bufferRelatedTypes = [
    "ArrayBuffer",
    "DataView",
    "Int8Array",
    "Int16Array",
    "Int32Array",
    "Uint8Array",
    "Uint16Array",
    "Uint32Array",
    "Uint8ClampedArray",
    "Float32Array",
    "Float64Array" ]

types :: [Text]
types = primitiveTypes <> stringTypes <> bufferRelatedTypes <> ["any"]

reserved :: [Text]
reserved = argumentNameKeywords <> types

-- | Builds a lexeme parser that consumes whitespace after it.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sym :: Text -> Parser Text
sym = L.symbol sc

-- Returns the tokens the inner parser matched
scan :: Parser a -> Parser Text
scan p = fst <$> match p

and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
and f g x = f x && g x

or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
or f g x = f x || g x

isAsciiLetter :: Char -> Bool
isAsciiLetter = isAsciiLower `or` isAsciiUpper

isHyphen :: Char -> Bool
isHyphen x = x == '_' || x == '-'

one :: (Char -> Bool) -> Parser Char
one = satisfy

opt :: (Char -> Bool) -> Parser (Maybe Char)
opt = optional . one

parens :: Parser a -> Parser a
parens = between (sym "(") (sym ")")

braces :: Parser a -> Parser a
braces = between (sym "{") (sym "}")

brackets :: Parser a -> Parser a
brackets = between (sym "[") (sym "]")

carets :: Parser a -> Parser a
carets = between (sym "<") (sym ">")

semi :: Parser Text
semi = sym ";"

comma :: Parser Text
comma = sym ","

colon :: Parser Text
colon = sym ":"

dot :: Parser Text
dot = sym "."

eq :: Parser Text
eq = sym "="

hyphen :: Parser Text
hyphen = sym "-"

-- Parses a terminal symbol
term :: Text -> Parser Text
term tag = lexeme $ string tag <* notFollowedBy (one isAsciiLetter)

decimal :: Integral a => Parser a
decimal = L.decimal

hexadecimal :: Integral a => Parser a
hexadecimal = L.hexadecimal

scientific :: Parser Scientific
scientific = L.scientific

takeWhileP' :: (Char -> Bool) -> Parser Text
takeWhileP' = takeWhileP Nothing

checkNonReserved :: Text -> Parser Text
checkNonReserved name | name `elem` reserved = unexpected $ Tokens $ NonEmpty.fromList $ T.unpack name
checkNonReserved name | otherwise = pure name

-- Parses the @identifier@ terminal symbol
identifier :: Parser Text
identifier = pat >>= checkNonReserved <?> "identifier"
    where pat = lexeme $ scan $ opt isHyphen *> one isAsciiLetter *> takeWhileP' (isAsciiLetter `or` isHyphen `or` isDigit)

argumentName :: Parser Text
argumentName = choice (try . term <$> argumentNameKeywords) <|> identifier

attributeName :: Parser Text
attributeName = choice (try . term <$> attributeNameKeywords) <|> identifier

operationName :: Parser Text
operationName = choice (try . term <$> operationNameKeywords) <|> identifier

signed :: Num a => Parser a -> Parser a
signed = L.signed (pure ())