module Language.WebIDL.Parser where

import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Language.WebIDL.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Scientific

type Parser = Parsec Void Text

ws :: Parser ()
ws = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: Text -> Parser Text
symbol = L.symbol ws

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

semi :: Parser Text
semi = symbol ";"

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

pIdent :: Parser Ident
pIdent = label "identifier" $ lexeme $ Ident . T.pack <$> some (alphaNumChar <|> char '_')

pInterface :: Parser Defn
pInterface = do
  _ <- try (symbol "interface")
  ident' <- pIdent
  members <- braces (many pMember)
  _ <- semi
  pure $ Interface ident' Nothing (members)

  where
    pMember :: Parser InterfaceMember
    pMember = (IAttribute <$> pAttribute)  <|> (IConstant <$> pConstant) <|> (IOperation <$> pOperation)

pConstant :: Parser Constant
pConstant = do
    _ <- symbol "const"
    ty <- pNullableType
    ident <- pIdent
    _ <- symbol "="
    value <- pConstValue
    _ <- semi
    pure $ Constant ty ident value

integerType :: Parser Type
integerType =
  choice
    [ pure ULongLongT <* symbol "unsigned long long",
      pure LongLongT <* symbol "long long",
      pure ULongT <* symbol "unsigned long",
      pure UShortT <* symbol "unsigned short",
      pure ShortT <* symbol "short",
      pure OctetT <* symbol "octet",
      pure ByteT <* symbol "byte"
    ]

numericType :: Parser Type
numericType =
  integerType
    <|> choice
      [ pure UnrestrictedFloatT <* symbol "unrestricted float",
        pure UnrestrictedDoubleT <* symbol "unrestricted double",
        pure FloatT <* symbol "float",
        pure DoubleT <* symbol "double"
      ]

primitiveType :: Parser Type
primitiveType =
  numericType
    <|> pure BooleanT <* symbol "boolean"

stringType :: Parser Type
stringType =
  choice
    [ pure DOMStringT <* symbol "DOMString",
      pure USVStringT <* symbol "USVString",
      pure ByteStringT <* symbol "ByteString"
    ]

pVoidType :: Parser Type
pVoidType = pure VoidT <* symbol "void"

pInterfaceType :: Parser Type
pInterfaceType = InterfaceType <$> pIdent

pType :: Parser Type
pType = primitiveType <|> stringType <|> pInterfaceType

pNullableType :: Parser Type
pNullableType = try ((NullableT <$> pType) <* symbol "?") <|> pType

modifier :: Parser a -> Parser Bool
modifier p = option False (p *> pure True)

pAttribute :: Parser Attribute
pAttribute = do
  readonly <- modifier (symbol "readonly")
  _ <- symbol "attribute"
  ty <- pNullableType
  ident <- pIdent
  _ <- semi
  pure $ Attribute ty ident readonly

pOperation :: Parser Operation
pOperation = do
    ty <- pReturnType
    ident <- pIdent
    args <- parens $ sepBy pArgument comma
    _ <- semi
    pure $ RegularOperation ty ident args
    where pReturnType = pVoidType <|> pNullableType

pConstValue :: Parser ConstValue
pConstValue = (BooleanLiteral <$> pBooleanLiteral) <|> (ScientificLiteral <$> pScientificLiteral) <|> (IntegerLiteral <$> (pHexLiteral <|> pIntegerLiteral))

pBooleanLiteral :: Parser Bool
pBooleanLiteral = (pure True <* symbol "true") <|> (pure False <* symbol "false")

pIntegerLiteral :: Parser Int
pIntegerLiteral = L.decimal

pScientificLiteral :: Parser Scientific
pScientificLiteral = L.scientific

pHexLiteral :: Parser Int
pHexLiteral = char '0' *> char' 'x' *> L.hexadecimal

pOptionalArgument :: Parser Argument
pOptionalArgument = do
    _ <- symbol "optional"
    ty <- pNullableType
    ident <- pIdent
    defaultValue <- optional (symbol "=" *> pDefaultValue)
    pure $ OptionalArgument ty ident defaultValue
    where pDefaultValue = pConstValue

pVariadicArgument :: Parser Argument
pVariadicArgument = do
    ty <- pType
    _ <- symbol "..."
    ident <- pIdent
    pure $ VariadicArgument ty ident

pRegularArgument :: Parser Argument
pRegularArgument = pure RegularArgument <*> pNullableType <*> pIdent

pArgument :: Parser Argument
pArgument = try pOptionalArgument <|> try pVariadicArgument <|> pRegularArgument


pFragment :: Parser Fragment
pFragment = ws *> (Fragment <$> some pInterface)
