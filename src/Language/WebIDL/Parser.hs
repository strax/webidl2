module Language.WebIDL.Parser where

import Data.Functor (void)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Language.WebIDL.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)
import Data.Kind (Type)

type Parser = Parsec Void Text
type HParser (k :: Type -> Type) = Parser (k ())

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

eq :: Parser Text
eq = symbol "="

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword)

pIdent :: Parser Ident
pIdent = label "identifier" $ lexeme $ Ident . T.pack <$> some (alphaNumChar <|> char '_')

pExtendedAttributeList :: HParser ExtendedAttributeList
pExtendedAttributeList = ExtendedAttributeList <$> (option [] $ brackets (sepBy1 pExtendedAttribute comma))

pExtendedAttributeNoArgs :: HParser ExtendedAttribute
pExtendedAttributeNoArgs = ExtendedAttributeNoArgs <$> pIdent

pExtendedAttributeIdent :: HParser ExtendedAttribute
pExtendedAttributeIdent = pure ExtendedAttributeIdent <*> pIdent <* eq <*> pIdent

pExtendedAttributeArgList :: HParser ExtendedAttribute
pExtendedAttributeArgList = pure ExtendedAttributeArgList <*> pIdent <*> pArgumentList

pExtendedAttributeIdentList :: HParser ExtendedAttribute
pExtendedAttributeIdentList = pure ExtendedAttributeIdentList <*> pIdent <* eq <*> parens (sepBy1 pIdent comma)

pExtendedAttributeNamedArgList :: HParser ExtendedAttribute
pExtendedAttributeNamedArgList = pure ExtendedAttributeNamedArgList <*> pIdent <* eq <*> pIdent <*> pArgumentList

pArgList :: Parser (Ident, [Argument ()])
pArgList = pure (,) <*> pIdent <*> parens (sepBy pArgument comma)

pExtendedAttribute :: HParser ExtendedAttribute
pExtendedAttribute = choice [try pExtendedAttributeNamedArgList, try pExtendedAttributeArgList, try pExtendedAttributeIdent, try pExtendedAttributeIdentList, pExtendedAttributeNoArgs]

pInterface :: HParser InterfaceDefinition
pInterface = dbg "pInterface" $ do
  attributes <- pExtendedAttributeList
  _ <- try (pKeyword "interface")
  name <- pIdent
  members <- braces (many pMember)
  _ <- semi
  pure $ InterfaceDefinition {ann = (), attributes, name, members, parent = Nothing}
  where
    pMember :: HParser InterfaceMember
    pMember =
        choice [ ICtor <$> pConstructor,
          IAttr <$> pAttribute,
          IConst <$> pConstant,
          IOp <$> pOperation
        ]

pConstructor :: HParser Constructor
pConstructor = pure (Constructor ()) <*> (pKeyword "constructor" *> pArgumentList) <* semi

pNamespace :: Parser (NamespaceDefinition ())
pNamespace = dbg "pNamespace" $ do
  attributes <- pExtendedAttributeList
  _ <- try (pKeyword "namespace")
  name <- pIdent
  members <- braces (many pMember)
  _ <- semi
  pure $ NamespaceDefinition { ann = (), attributes, name, members }
  where
    pMember :: HParser NamespaceMember
    pMember = (NOp <$> pOperation) <|> (NAttr <$> pAttribute)

pConstant :: HParser Constant
pConstant = do
  _ <- symbol "const"
  ty <- pNullableType
  ident <- pIdent
  _ <- symbol "="
  value <- pConstValue
  _ <- semi
  pure $ Constant () ty ident value

integerType :: Parser TypeName
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

numericType :: Parser TypeName
numericType =
  integerType
    <|> choice
      [ pure UnrestrictedFloatT <* symbol "unrestricted float",
        pure UnrestrictedDoubleT <* symbol "unrestricted double",
        pure FloatT <* symbol "float",
        pure DoubleT <* symbol "double"
      ]

primitiveType :: Parser TypeName
primitiveType =
  numericType
    <|> pure BooleanT <* symbol "boolean"

stringType :: Parser TypeName
stringType =
  choice
    [ pure DOMStringT <* symbol "DOMString",
      pure USVStringT <* symbol "USVString",
      pure ByteStringT <* symbol "ByteString"
    ]

pVoidType :: Parser TypeName
pVoidType = pure VoidT <* symbol "void"

pInterfaceType :: Parser TypeName
pInterfaceType = InterfaceType <$> pIdent

pType :: Parser TypeName
pType = primitiveType <|> stringType <|> pSequenceType <|> pInterfaceType

pNullableType :: Parser TypeName
pNullableType = try ((NullableT <$> pType) <* symbol "?") <|> pType

pGenericType1 :: Text -> Parser TypeName
pGenericType1 tn = lexeme $ string tn *> between (char '<') (char '>') pType

pSequenceType :: Parser TypeName
pSequenceType = pGenericType1 "sequence"

modifier :: Parser a -> Parser Bool
modifier p = option False (p *> pure True)

pAttribute :: HParser Attribute
pAttribute = do
  attributes <- pExtendedAttributeList
  readonly <- modifier (pKeyword "readonly")
  _ <- pKeyword "attribute"
  ty <- pNullableType
  ident <- pIdent
  _ <- semi
  pure $ Attribute { attributes, ann = (), type' = ty, name = ident, readonly }

pOperation :: HParser Operation
pOperation = do
  ty <- pReturnType
  ident <- pIdent
  argumentList <- pArgumentList
  _ <- semi
  pure $ RegularOperation { ann = (), type' = ty, name = ident, arguments = argumentList }
  where
    pReturnType = pVoidType <|> pNullableType

pConstValue :: Parser ConstValue
pConstValue =
  (ConstBoolean <$> pBooleanLiteral)
    <|> (ConstNumeric <$> pHexLiteral)
    <|> (ConstNumeric <$> pScientificLiteral)

pBooleanLiteral :: Parser Bool
pBooleanLiteral = (pure True <* symbol "true") <|> (pure False <* symbol "false")

pIntegerLiteral :: Parser Int
pIntegerLiteral = L.decimal

pScientificLiteral :: Parser Scientific
pScientificLiteral = L.scientific

pStringLiteral :: Parser Text
pStringLiteral = char '"' *> takeWhileP Nothing (/= '"') <* char '"'

pHexLiteral :: Num a => Parser a
pHexLiteral = try (char '0' *> char' 'x') *> (fromInteger <$> (L.hexadecimal :: Parser Integer))

pOptionalArgument :: HParser Argument
pOptionalArgument = do
  _ <- symbol "optional"
  ty <- pNullableType
  ident <- pIdent
  defaultValue <- optional (symbol "=" *> pDefaultValue)
  pure $ OptionalArgument { ann = (), type' = ty, name = ident, defaultValue }
  where
    pDefaultValue = (DefaultConst <$> pConstValue) <|> (pure DefaultDict <* braces ws) <|> (pure DefaultSeq <* brackets ws)

pVariadicArgument :: HParser Argument
pVariadicArgument = do
  ty <- pType
  _ <- symbol "..."
  ident <- pIdent
  pure $ VariadicArgument { ann = (), type' = ty, name = ident }

pRegularArgument :: HParser Argument
pRegularArgument = pure (RegularArgument ()) <*> pNullableType <*> pIdent

pArgumentList :: HParser ArgumentList
pArgumentList = parens $ ArgumentList <$> sepBy pArgument comma

pArgument :: HParser Argument
pArgument = try pOptionalArgument <|> try pVariadicArgument <|> pRegularArgument

pDefn :: HParser Definition
pDefn = choice [try (Interface <$> pInterface), Namespace <$> pNamespace]

pFragment :: HParser Fragment
pFragment = ws *> (Fragment () <$> some pDefn)
