module Language.WebIDL.Parser.Lex where

import qualified Text.Megaparsec.Char.Lexer    as L
import Language.WebIDL.Parser.Types
import Text.Megaparsec
import Text.Megaparsec.Char (space1, string, alphaNumChar)
import Data.Text (Text)
import Data.Scientific (Scientific)

ws :: Parser ()
ws = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

sym :: Text -> Parser Text
sym = L.symbol ws

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

eq :: Parser Text
eq = sym "="

keyword :: Text -> Parser Text
keyword tag = lexeme $ string tag <* notFollowedBy alphaNumChar

decimal :: Integral a => Parser a
decimal = L.decimal

hexadecimal :: Integral a => Parser a
hexadecimal = L.hexadecimal

scientific :: Parser Scientific
scientific = L.scientific