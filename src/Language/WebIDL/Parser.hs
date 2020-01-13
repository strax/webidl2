module Language.WebIDL.Parser where

import Data.Functor (void, ($>))
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

type HParser (k :: Type -> Type) = Parser (k SourcePos)

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

semi :: Parser Text
semi = sym ";"

comma :: Parser Text
comma = sym ","

colon :: Parser Text
colon = sym ":" 

eq :: Parser Text
eq = sym "="

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme $ string keyword <* notFollowedBy alphaNumChar

pIdent :: Parser Ident
pIdent = label "identifier" $ lexeme $ Ident . T.pack <$> some (alphaNumChar <|> char '_')

pExtendedAttributeList :: HParser ExtendedAttributeList
pExtendedAttributeList = ExtendedAttributeList <$> (option [] $ brackets (sepBy1 pExtendedAttribute comma))

pExtendedAttributeNoArgs :: HParser ExtendedAttribute
pExtendedAttributeNoArgs = ExtendedAttributeNoArgs <$> pIdent

pExtendedAttributeIdent :: HParser ExtendedAttribute
pExtendedAttributeIdent = ExtendedAttributeIdent <$> pIdent <* eq <*> pIdent

pExtendedAttributeArgList :: HParser ExtendedAttribute
pExtendedAttributeArgList = ExtendedAttributeArgList <$> pIdent <*> pArgumentList

pExtendedAttributeIdentList :: HParser ExtendedAttribute
pExtendedAttributeIdentList = ExtendedAttributeIdentList <$> pIdent <* eq <*> parens (sepBy1 pIdent comma)

pExtendedAttributeNamedArgList :: HParser ExtendedAttribute
pExtendedAttributeNamedArgList = ExtendedAttributeNamedArgList <$> pIdent <* eq <*> pIdent <*> pArgumentList

-- pArgList :: Parser (Ident, [Argument ()])
-- pArgList = pure (,) <*> pIdent <*> parens (sepBy pArgument comma)

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
  pos <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _ <- try $ pKeyword "interface"
  name <- pIdent
  parent <- optional $ colon *> pIdent
  members <- braces (many pInterfaceMember) 
  pure $ InterfaceDefinition {ann = pos, attributes, name, members, parent}


pInterfaceMember :: HParser InterfaceMember
pInterfaceMember = do
        choice
          [ InterfaceIterableDeclaration <$> try pIterableDeclaration,
            InterfaceStringifiter <$> try pStringifier,
            InterfaceGetter <$> try pGetter,
            InterfaceConstructor <$> try pConstructor,
            InterfaceAttribute <$> pAttribute,
            InterfaceConstant <$> pConstant,
            InterfaceOperation <$> pOperation
          ]

pIterableDeclaration :: HParser IterableDeclaration
pIterableDeclaration = stmt $ do
  pos <- getSourcePos
  attrs <- hidden pExtendedAttributeList
  _ <- pKeyword "iterable"
  let pPair = (PairIteratorDeclaration pos attrs) <$> pTypeWithExtendedAttributes <* comma <*> pTypeWithExtendedAttributes
      pValue = (ValueIteratorDeclaration pos attrs) <$> pTypeWithExtendedAttributes
    in between (sym "<") (sym ">") (try pPair <|> pValue)


pPartialInterface :: HParser PartialInterfaceDefinition
pPartialInterface = do
  pos <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _ <- try $ pKeyword "partial" *> pKeyword "interface"
  name <- pIdent
  members <- braces (many pMember)
  _ <- semi
  pure $ PartialInterfaceDefinition {ann = pos, attributes, name, members}
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
  pos <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _ <- try $ pKeyword "callback" *> pKeyword "interface"
  name <- pIdent
  members <- braces (many pMember)
  pure $ CallbackInterfaceDefinition {ann = pos, attributes, name, members}
  where
    pMember :: HParser CallbackInterfaceMember
    pMember = CallbackInterfaceConstant <$> pConstant <|> CallbackInterfaceOperation <$> pOperation

pPartialMixin :: HParser MixinDefinition
pPartialMixin = stmt $ do
  pos <- getSourcePos
  _ <- try $ pKeyword "partial" *> pKeyword "interface" *> pKeyword "mixin"
  name <- pIdent
  members <- braces $ many pMember
  pure $ MixinDefinition {ann = pos, name, members}
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
  pos <- getSourcePos
  _ <- try $ kw "interface" *> kw "mixin"
  name <- pIdent
  members <- braces $ many pMixinMember
  pure $ MixinDefinition {ann = pos, name, members}

pMixinMember :: HParser MixinMember
pMixinMember = MAttr <$> try pAttribute <|> MConst <$> pConstant <|> MOp <$> pOperation

pConstructor :: HParser Constructor
pConstructor = stmt $ do
  pos <- getSourcePos
  attributes <- pExtendedAttributeList
  _ <- kw "constructor"
  arguments <- pArgumentList
  pure $ Constructor { ann = pos, attributes, arguments }

pNamespace :: HParser NamespaceDefinition
pNamespace = stmt $ do
  pos <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _ <- try (pKeyword "namespace")
  name <- pIdent
  members <- braces (many pMember)
  pure $ NamespaceDefinition {ann = pos, attributes, name, members}
  where
    pMember :: HParser NamespaceMember
    pMember = (NOp <$> pOperation) <|> (NAttr <$> pAttribute)

pEnum :: HParser EnumDefinition
pEnum = stmt $ do
  pos <- getSourcePos
  _ <- try (pKeyword "enum")
  name <- pIdent
  values <- braces (sepBy1 pStringLiteral comma)
  pure $ EnumDefinition {ann = pos, name, values}

pTypedef :: HParser TypedefDefinition
pTypedef = do
  pos <- getSourcePos
  _ <- try (pKeyword "typedef")
  type' <- pType
  name <- pIdent
  _ <- semi
  pure $ TypedefDefinition {ann = pos, name, type'}

pDictionary :: HParser DictionaryDefinition
pDictionary = stmt $ do
  pos <- getSourcePos
  _ <- try (pKeyword "dictionary")
  name <- pIdent
  parent <- optional $ colon *> pIdent
  members <- braces (many pMember)
  pure $ DictionaryDefinition {ann = pos, name, parent, members}
  where
    pMember :: HParser DictionaryMember
    pMember = pRequiredMember <|> pOptionalMember
    pRequiredMember = stmt $ do
      pos <- getSourcePos
      _ <- pKeyword "required"
      type' <- pType
      name <- pIdent
      pure $ RequiredDictionaryMember {ann = pos, type', name}
    pOptionalMember = stmt $ do
      pos <- getSourcePos
      type' <- pType
      name <- pIdent
      defaultValue <- optional (sym "=" *> pDefaultValue)
      pure $ DictionaryMember {ann = pos, type', name, defaultValue}

pConstant :: HParser Constant
pConstant = stmt $ do
  pos <- getSourcePos
  _ <- sym "const"
  ty <- pNullableType
  ident <- pIdent
  _ <- sym "="
  value <- pConstValue
  pure $ Constant pos ty ident value

integerType :: Parser TypeName
integerType =
        ULongLongT  <$ sym "unsigned long long" 
    <|> LongLongT   <$ sym "long long"
    <|> ULongT      <$ sym "unsigned long"
    <|> UShortT     <$ sym "unsigned short"
    <|> ShortT      <$ sym "short"
    <|> OctetT      <$ sym "octet"
    <|> ByteT       <$ sym "byte"

numericType :: Parser TypeName
numericType =
  integerType
    <|> UnrestrictedFloatT <$ sym "unrestricted float"
    <|> UnrestrictedDoubleT <$ sym "unrestricted double"
    <|> FloatT <$ sym "float"
    <|> DoubleT <$ sym "double"

primitiveType :: Parser TypeName
primitiveType =
  numericType
    <|> pure BooleanT <* sym "boolean"

stringType :: Parser TypeName
stringType =
      DOMStringT  <$ kw "DOMString"
  <|> USVStringT  <$ kw "USVString"
  <|> ByteStringT <$ kw "ByteString"

pAnyType :: Parser TypeName
pAnyType = pure AnyT <* sym "any"

pVoidType :: Parser TypeName
pVoidType = pure VoidT <* sym "void"

pInterfaceType :: Parser TypeName
pInterfaceType = InterfaceType <$> pIdent

pType :: Parser TypeName
pType = pUnionType <|> primitiveType <|> try stringType <|> pSequenceType <|> pIterableType <|> pInterfaceType <|> pAnyType

pNullableType :: Parser TypeName
pNullableType = try ((NullableT <$> pType) <* sym "?") <|> pType

pTypeWithExtendedAttributes :: HParser TypeWithExtendedAttributes
pTypeWithExtendedAttributes = pTypeWithExtendedAttributes' pNullableType

pTypeWithExtendedAttributes' :: Parser TypeName -> HParser TypeWithExtendedAttributes
pTypeWithExtendedAttributes' pInner = do
  pos <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  inner <- pInner
  pure $ TypeWithExtendedAttributes { ann = pos, attributes, inner }

pGenericType1 :: Text -> Parser TypeName
pGenericType1 tn = lexeme $ string tn *> between (char '<') (char '>') pType

pSequenceType :: Parser TypeName
pSequenceType = pGenericType1 "sequence"

pIterableType = pGenericType1 "iterable"

pUnionType :: Parser TypeName
pUnionType = (UnionT . Set.fromList) <$> parens (sepBy1 pType (pKeyword "or"))

pGetter :: HParser Getter
pGetter = stmt $ do
  pos <- getSourcePos
  attributes <- pExtendedAttributeList
  _ <- kw "getter"
  type' <- pTypeWithExtendedAttributes
  name <- optional pIdent
  arguments <- pArgumentList
  pure $ Getter { ann = pos, attributes, type', name, arguments }

pStringifier :: HParser Stringifier
pStringifier = stmt $ do
  pos <- getSourcePos
  attrs <- pExtendedAttributeList
  _ <- kw "stringifier"
  let pStringifierAttribute = StringifierAttribute pos attrs <$> (kw "attribute" *> pTypeWithExtendedAttributes) <*> pIdent
      pStringifierOperation = do
        returnType <- pTypeWithExtendedAttributes
        arguments <- pArgumentList
        pure $ StringifierOperation pos attrs returnType arguments
      pShorthand = pure $ StringifierOperation pos attrs (TypeWithExtendedAttributes pos mempty DOMStringT) mempty
    in (pStringifierAttribute <|> pStringifierOperation <|> pShorthand)

modifier :: Parser a -> Parser Bool
modifier p = option False (p *> pure True)

pAttribute :: HParser Attribute
pAttribute = dbg "pAttribute" $ stmt $ try $ do
  pos <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  readonly <- modifier $ kw "readonly"
  _ <- kw "attribute"
  ty <- pTypeWithExtendedAttributes
  ident <- pIdent
  pure $ Attribute {attributes, ann = pos, type' = ty, name = ident, readonly}

pOperation :: HParser Operation
pOperation = stmt $ do
  pos <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  ty <- pReturnType
  ident <- pIdent
  argumentList <- pArgumentList
  pure $ Operation {ann = pos, attributes, type' = ty, name = ident, arguments = argumentList}
  where
    pReturnType = pVoidType <|> pNullableType

pConstValue :: Parser ConstValue
pConstValue =
  (ConstBoolean <$> pBooleanLiteral)
    <|> (ConstNumeric <$> pHexLiteral)
    <|> (ConstNumeric <$> pScientificLiteral)

pBooleanLiteral :: Parser Bool
pBooleanLiteral = (pure True <* sym "true") <|> (pure False <* sym "false")

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
        DefaultNull <$ sym "null"
    <|> DefaultConst <$> pConstValue
    <|> DefaultDict <$ braces ws
    <|> DefaultSeq <$ brackets ws
    <|> DefaultString <$> pStringLiteral

pOptionalArgument :: HParser Argument
pOptionalArgument = do
  pos <- getSourcePos
  _ <- sym "optional"
  ty <- pTypeWithExtendedAttributes
  ident <- pIdent
  defaultValue <- optional (sym "=" *> pDefaultValue)
  pure $ OptionalArgument {ann = pos, type' = ty, name = ident, defaultValue}

pVariadicArgument :: HParser Argument
pVariadicArgument = do
  pos <- getSourcePos
  ty <- pTypeWithExtendedAttributes
  _ <- sym "..."
  ident <- pIdent
  pure $ VariadicArgument {ann = pos, type' = ty, name = ident}

pRegularArgument :: HParser Argument
pRegularArgument = RegularArgument <$> getSourcePos <*> pTypeWithExtendedAttributes <*> pIdent

pArgumentList :: HParser ArgumentList
pArgumentList = parens $ ArgumentList <$> sepBy pArgument comma

pArgument :: HParser Argument
pArgument = try pOptionalArgument <|> try pVariadicArgument <|> pRegularArgument

pIncludesStatement :: HParser IncludesStatementDefinition
pIncludesStatement = stmt $ do
  pos <- getSourcePos
  interface <- pIdent
  _ <- pKeyword "includes"
  mixin <- pIdent
  pure $ IncludesStatementDefinition {ann = pos, interface, mixin}

pCallbackDefinition :: HParser CallbackDefinition
pCallbackDefinition = stmt $ do
  pos <- getSourcePos
  attributes <- pExtendedAttributeList
  _ <- pKeyword "callback"
  name <- pIdent
  _ <- eq
  returnType <- pType
  arguments <- pArgumentList
  pure $ CallbackDefinition { ann = pos, attributes, name, returnType, arguments }

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
pFragment = ws *> (Fragment <$> getSourcePos <*> some pDefn) <* hidden eof
