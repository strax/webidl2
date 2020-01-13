module Language.WebIDL.Parser where

import Data.Functor (void)
import Data.Kind (Type)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Language.WebIDL.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)

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
pExtendedAttribute =
  choice
    [ try pExtendedAttributeNamedArgList,
      try pExtendedAttributeArgList,
      try pExtendedAttributeIdent,
      try pExtendedAttributeIdentList,
      pExtendedAttributeNoArgs
    ]

pInterface :: HParser InterfaceDefinition
pInterface = stmt $ do
  attributes <- hidden pExtendedAttributeList
  _ <- try $ pKeyword "interface"
  name <- pIdent
  parent <- optional $ colon *> pIdent
  members <- braces (many pMember)
  pure $ InterfaceDefinition {ann = (), attributes, name, members, parent}
  where
    pMember :: HParser InterfaceMember
    pMember =
      choice
        [InterfaceIterableDeclaration <$> try pIterableDeclaration,
          InterfaceStringifiter <$> try pStringifier,
          InterfaceGetter <$> try pGetter,
          InterfaceConstructor <$> try pConstructor,
          InterfaceAttribute <$> pAttribute,
          InterfaceConstant <$> pConstant,
          InterfaceOperation <$> pOperation
        ]

pIterableDeclaration :: HParser IterableDeclaration
pIterableDeclaration = stmt $ do
  attrs <- hidden pExtendedAttributeList
  _ <- pKeyword "iterable"
  between (symbol "<") (symbol ">") (try (pPair attrs) <|> pValue attrs)
  where
    pPair attrs = pure (PairIteratorDeclaration () attrs) <*> pTypeWithExtendedAttributes <* comma <*> pTypeWithExtendedAttributes
    pValue attrs = pure (ValueIteratorDeclaration () attrs) <*> pTypeWithExtendedAttributes


pPartialInterface :: HParser PartialInterfaceDefinition
pPartialInterface = do
  attributes <- hidden pExtendedAttributeList
  _ <- try $ pKeyword "partial" *> pKeyword "interface"
  name <- pIdent
  members <- braces (many pMember)
  _ <- semi
  pure $ PartialInterfaceDefinition {ann = (), attributes, name, members}
  where
    pMember :: HParser PartialInterfaceMember
    pMember =
      choice
        [ PartialInterfaceAttribute <$> try pAttribute,
          PartialInterfaceConstant <$> pConstant,
          PartialInterfaceOperation <$> pOperation
        ]

stmt :: Parser a -> Parser a
stmt p = p <* semi

kw :: Text -> Parser Text
kw = pKeyword

pCallbackInterface :: HParser CallbackInterfaceDefinition
pCallbackInterface = stmt $ do
  attributes <- hidden pExtendedAttributeList
  _ <- try $ pKeyword "callback" *> pKeyword "interface"
  name <- pIdent
  members <- braces (many pMember)
  pure $ CallbackInterfaceDefinition {ann = (), attributes, name, members}
  where
    pMember :: HParser CallbackInterfaceMember
    pMember = CallbackInterfaceConstant <$> pConstant <|> CallbackInterfaceOperation <$> pOperation

pPartialMixin :: HParser MixinDefinition
pPartialMixin = stmt $ do
  _ <- try $ pKeyword "partial" *> pKeyword "interface" *> pKeyword "mixin"
  name <- pIdent
  members <- braces $ many pMember
  pure $ MixinDefinition {ann = (), name, members}
  where
    pMember :: HParser MixinMember
    pMember =
      choice
        [ MAttr <$> try pAttribute,
          MConst <$> pConstant,
          MOp <$> pOperation
        ]

pMixin :: HParser MixinDefinition
pMixin = stmt $ do
  _ <- try $ kw "interface" *> kw "mixin"
  name <- pIdent
  members <- braces $ many pMember
  pure $ MixinDefinition {ann = (), name, members}
  where
    pMember :: HParser MixinMember
    pMember =
      choice
        [ MAttr <$> try pAttribute,
          MConst <$> pConstant,
          MOp <$> pOperation
        ]

pConstructor :: HParser Constructor
pConstructor = stmt $ do
  attributes <- pExtendedAttributeList
  _ <- kw "constructor"
  arguments <- pArgumentList
  pure $ Constructor { ann = (), attributes, arguments }

pNamespace :: Parser (NamespaceDefinition ())
pNamespace = do
  attributes <- hidden pExtendedAttributeList
  _ <- try (pKeyword "namespace")
  name <- pIdent
  members <- braces (many pMember)
  _ <- semi
  pure $ NamespaceDefinition {ann = (), attributes, name, members}
  where
    pMember :: HParser NamespaceMember
    pMember = (NOp <$> pOperation) <|> (NAttr <$> pAttribute)

pEnum :: HParser EnumDefinition
pEnum = do
  _ <- try (pKeyword "enum")
  name <- pIdent
  values <- braces (sepBy1 pStringLiteral comma)
  _ <- semi
  pure $ EnumDefinition {ann = (), name, values}

pTypedef :: HParser TypedefDefinition
pTypedef = do
  _ <- try (pKeyword "typedef")
  type' <- pType
  name <- pIdent
  _ <- semi
  pure $ TypedefDefinition {ann = (), name, type'}

pDictionary :: HParser DictionaryDefinition
pDictionary = do
  _ <- try (pKeyword "dictionary")
  name <- pIdent
  parent <- optional $ colon *> pIdent
  members <- braces (many pMember)
  _ <- semi
  pure $ DictionaryDefinition {ann = (), name, parent, members}
  where
    pMember :: HParser DictionaryMember
    pMember = pRequiredMember <|> pOptionalMember
    pRequiredMember = do
      _ <- pKeyword "required"
      type' <- pType
      name <- pIdent
      _ <- semi
      pure $ RequiredDictionaryMember {ann = (), type', name}
    pOptionalMember = do
      type' <- pType
      name <- pIdent
      defaultValue <- optional (symbol "=" *> pDefaultValue)
      _ <- semi
      pure $ DictionaryMember {ann = (), type', name, defaultValue}

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

pAnyType :: Parser TypeName
pAnyType = pure AnyT <* symbol "any"

pVoidType :: Parser TypeName
pVoidType = pure VoidT <* symbol "void"

pInterfaceType :: Parser TypeName
pInterfaceType = InterfaceType <$> pIdent

pType :: Parser TypeName
pType = pUnionType <|> primitiveType <|> stringType <|> pSequenceType <|> pIterableType <|> pInterfaceType <|> pAnyType

pNullableType :: Parser TypeName
pNullableType = try ((NullableT <$> pType) <* symbol "?") <|> pType

pTypeWithExtendedAttributes :: HParser TypeWithExtendedAttributes
pTypeWithExtendedAttributes = pTypeWithExtendedAttributes' pNullableType

pTypeWithExtendedAttributes' :: Parser TypeName -> HParser TypeWithExtendedAttributes
pTypeWithExtendedAttributes' pInner = do
  attributes <- hidden pExtendedAttributeList
  inner <- pInner
  pure $ TypeWithExtendedAttributes { ann = (), attributes, inner }

pGenericType1 :: Text -> Parser TypeName
pGenericType1 tn = lexeme $ string tn *> between (char '<') (char '>') pType

pSequenceType :: Parser TypeName
pSequenceType = pGenericType1 "sequence"

pIterableType = pGenericType1 "iterable"

pUnionType :: Parser TypeName
pUnionType = (UnionT . Set.fromList) <$> parens (sepBy1 pType (pKeyword "or"))

pGetter :: HParser Getter
pGetter = stmt $ do
  attributes <- pExtendedAttributeList
  _ <- kw "getter"
  type' <- pTypeWithExtendedAttributes
  name <- optional pIdent
  arguments <- pArgumentList
  pure $ Getter { ann = (), attributes, type', name, arguments }

pStringifier :: HParser Stringifier
pStringifier = stmt $ do
  attrs <- pExtendedAttributeList
  _ <- kw "stringifier"
  (pStringifierAttribute attrs <|> pStringifierOperation attrs <|> pShorthand attrs)
    where
      pStringifierAttribute as = pure (StringifierAttribute () as) <*> (kw "attribute" *> pTypeWithExtendedAttributes) <*> pIdent
      pStringifierOperation as = do
        returnType <- pTypeWithExtendedAttributes
        arguments <- pArgumentList
        pure $ StringifierOperation () as returnType arguments
      pShorthand as = pure $ StringifierOperation () as (TypeWithExtendedAttributes () mempty DOMStringT) mempty

modifier :: Parser a -> Parser Bool
modifier p = option False (p *> pure True)

pAttribute :: HParser Attribute
pAttribute = stmt $ try $ do
  attributes <- hidden pExtendedAttributeList
  readonly <- modifier (pKeyword "readonly")
  _ <- pKeyword "attribute"
  ty <- pTypeWithExtendedAttributes
  ident <- pIdent
  pure $ Attribute {attributes, ann = (), type' = ty, name = ident, readonly}

pOperation :: HParser Operation
pOperation = stmt $ do
  attributes <- hidden pExtendedAttributeList
  ty <- pReturnType
  ident <- pIdent
  argumentList <- pArgumentList
  pure $ Operation {ann = (), attributes, type' = ty, name = ident, arguments = argumentList}
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
pStringLiteral = lexeme $ char '"' *> takeWhileP Nothing (/= '"') <* char '"'

pHexLiteral :: Num a => Parser a
pHexLiteral = try (char '0' *> char' 'x') *> (fromInteger <$> (L.hexadecimal :: Parser Integer))

pDefaultValue :: Parser DefaultValue
pDefaultValue =
  (pure DefaultNull <* symbol "null")
    <|> (DefaultConst <$> pConstValue)
    <|> (pure DefaultDict <* braces ws)
    <|> (pure DefaultSeq <* brackets ws)
    <|> (DefaultString <$> pStringLiteral)

pOptionalArgument :: HParser Argument
pOptionalArgument = do
  _ <- symbol "optional"
  ty <- pTypeWithExtendedAttributes
  ident <- pIdent
  defaultValue <- optional (symbol "=" *> pDefaultValue)
  pure $ OptionalArgument {ann = (), type' = ty, name = ident, defaultValue}

pVariadicArgument :: HParser Argument
pVariadicArgument = do
  ty <- pTypeWithExtendedAttributes
  _ <- symbol "..."
  ident <- pIdent
  pure $ VariadicArgument {ann = (), type' = ty, name = ident}

pRegularArgument :: HParser Argument
pRegularArgument = pure (RegularArgument ()) <*> pTypeWithExtendedAttributes <*> pIdent

pArgumentList :: HParser ArgumentList
pArgumentList = parens $ ArgumentList <$> sepBy pArgument comma

pArgument :: HParser Argument
pArgument = try pOptionalArgument <|> try pVariadicArgument <|> pRegularArgument

pIncludesStatement :: HParser IncludesStatementDefinition
pIncludesStatement = stmt $ do
  interface <- pIdent
  _ <- pKeyword "includes"
  mixin <- pIdent
  pure $ IncludesStatementDefinition {ann = (), interface, mixin}

pCallbackDefinition :: HParser CallbackDefinition
pCallbackDefinition = stmt $ do
  attributes <- pExtendedAttributeList
  _ <- pKeyword "callback"
  name <- pIdent
  _ <- eq
  returnType <- pType
  arguments <- pArgumentList
  pure $ CallbackDefinition { ann = (), attributes, name, returnType, arguments }

pDefn :: HParser Definition
pDefn =
  choice
    [ Dictionary <$> pDictionary,
      Typedef <$> pTypedef,
      Enum <$> pEnum,
      CallbackInterface <$> try pCallbackInterface,
      Callback <$> try pCallbackDefinition,
      PartialMixin <$> pPartialMixin,
      Mixin <$> pMixin,
      PartialInterface <$> try pPartialInterface,
      Interface <$> try pInterface,
      Namespace <$> pNamespace,
      IncludesStatement <$> pIncludesStatement
    ]

pFragment :: HParser Fragment
pFragment = ws *> (Fragment () <$> some pDefn) <* hidden eof
