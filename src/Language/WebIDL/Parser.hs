module Language.WebIDL.Parser where

import Language.WebIDL.Parser.Types
import qualified Language.WebIDL.Parser.Lex as L

import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Data.Kind                      ( Type )
import           Data.Scientific                ( Scientific )
import qualified Data.Scientific               as Scientific
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Language.WebIDL.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Debug          ( dbg )
import           Data.Char                      ( isLetter, isDigit )
import           Numeric.IEEE

pIdent :: Parser Ident
pIdent = label "identifier" $ L.lexeme $ Ident <$> p
  where
    p :: Parser Text
    p = fst <$> match (optional (char '_' <|> char '-') *> asciiLetter *> many (asciiLetter <|> digit <|> char '_' <|> char '-'))
    asciiLetter = satisfy isLetter
    digit = satisfy isDigit

pExtendedAttributeList :: HParser ExtendedAttributeList
pExtendedAttributeList =
  ExtendedAttributeList
    <$> (option [] $ L.brackets (sepBy1 pExtendedAttribute L.comma))

pExtendedAttributeNoArgs :: HParser ExtendedAttribute
pExtendedAttributeNoArgs = ExtendedAttributeNoArgs <$> pIdent

pExtendedAttributeIdent :: HParser ExtendedAttribute
pExtendedAttributeIdent = ExtendedAttributeIdent <$> pIdent <* L.eq <*> pIdent

pExtendedAttributeArgList :: HParser ExtendedAttribute
pExtendedAttributeArgList =
  ExtendedAttributeArgList <$> pIdent <*> pArgumentList

pExtendedAttributeIdentList :: HParser ExtendedAttribute
pExtendedAttributeIdentList =
  ExtendedAttributeIdentList <$> pIdent <* L.eq <*> L.parens (sepBy1 pIdent L.comma)

pExtendedAttributeNamedArgList :: HParser ExtendedAttribute
pExtendedAttributeNamedArgList = do
  ExtendedAttributeNamedArgList <$> pIdent <* L.eq <*> pIdent <*> pArgumentList

-- pArgList :: Parser (Ident, [Argument ()])
-- pArgList = pure (,) <*> pIdent <*> parens (sepBy pArgument comma)

pExtendedAttribute :: HParser ExtendedAttribute
pExtendedAttribute = choice
  [ try pExtendedAttributeNamedArgList
  , try pExtendedAttributeArgList
  , try pExtendedAttributeIdent
  , try pExtendedAttributeIdentList
  , pExtendedAttributeNoArgs
  ]

pInterface :: HParser InterfaceDefinition
pInterface = stmt $ do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _          <- try $ L.keyword "interface"
  name       <- pIdent
  parent     <- optional $ L.colon *> pIdent
  members    <- L.braces $ many pInterfaceMember
  pure $ InterfaceDefinition { ann = pos, attributes, name, members, parent }

pInterfaceMember :: HParser InterfaceMember
pInterfaceMember = do
  choice
    [ InterfaceSetlikeDeclaration <$> pSetlikeDeclaration
    , InterfaceAsyncIterableDeclaration <$> try pAsyncIterableDeclaration 
    , InterfaceIterableDeclaration <$> try pIterableDeclaration
    , InterfaceStringifier <$> try pStringifier
    , InterfaceGetter <$> try pGetter
    , InterfaceConstructor <$> try pConstructor
    , InterfaceAttribute <$> pAttribute
    , InterfaceConstant <$> pConstant
    , InterfaceOperation <$> pOperation
    ]

pSetlikeDeclaration :: HParser SetlikeDeclaration
pSetlikeDeclaration = stmt $ do
  pos <- getSourcePos
  variant <- try $ toConstructor <$> modifier (L.keyword "readonly") <* L.keyword "setlike"
  tn <- L.carets pTypeWithExtendedAttributes
  pure $ variant pos tn
  where
    toConstructor True = ReadOnlySetlikeDeclaration
    toConstructor False = ReadWriteSetlikeDeclaration


pAsyncIterableDeclaration :: HParser AsyncIterableDeclaration
pAsyncIterableDeclaration = stmt $ do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _          <- L.keyword "async" *> L.keyword "iterable"
  L.carets $ do
    key      <- pTypeWithExtendedAttributes
    _        <- L.comma
    value    <- pTypeWithExtendedAttributes
    pure $ AsyncIterableDeclaration { ann = pos, attributes, key, value }

pIterableDeclaration :: HParser IterableDeclaration
pIterableDeclaration = stmt $ do
  pos   <- getSourcePos
  attrs <- hidden pExtendedAttributeList
  _     <- L.keyword "iterable"
  let pPair =
        PairIteratorDeclaration pos attrs
          <$> pTypeWithExtendedAttributes
          <*  L.comma
          <*> pTypeWithExtendedAttributes
  let pValue =
        ValueIteratorDeclaration pos attrs <$> pTypeWithExtendedAttributes
  L.carets $ try pPair <|> pValue

pPartialInterface :: HParser PartialInterfaceDefinition
pPartialInterface = stmt $ do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _          <- try $ L.keyword "partial" *> L.keyword "interface"
  name       <- pIdent
  members    <- L.braces (many pMember)
  pure $ PartialInterfaceDefinition { ann = pos, attributes, name, members }
 where
  pMember :: HParser PartialInterfaceMember
  pMember =
    (PartialInterfaceAttribute <$> try pAttribute)
      <|> (PartialInterfaceConstant <$> pConstant)
      <|> (PartialInterfaceOperation <$> pOperation)

stmt :: Parser a -> Parser a
stmt p = p <* L.semi

pCallbackInterface :: HParser CallbackInterfaceDefinition
pCallbackInterface = stmt $ do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _          <- try $ L.keyword "callback" *> L.keyword "interface"
  name       <- pIdent
  members    <- L.braces (many pMember)
  pure $ CallbackInterfaceDefinition { ann = pos, attributes, name, members }
 where
  pMember :: HParser CallbackInterfaceMember
  pMember =
    (CallbackInterfaceConstant <$> pConstant)
      <|> (CallbackInterfaceOperation <$> pOperation)

pPartialMixin :: HParser MixinDefinition
pPartialMixin = stmt $ do
  pos     <- getSourcePos
  _       <- try $ L.keyword "partial" *> L.keyword "interface" *> L.keyword "mixin"
  name    <- pIdent
  members <- L.braces $ many pMixinMember
  pure $ MixinDefinition { ann = pos, name, members }

pMixin :: HParser MixinDefinition
pMixin = stmt $ do
  pos     <- getSourcePos
  _       <- try $ L.keyword "interface" *> L.keyword "mixin"
  name    <- pIdent
  members <- L.braces $ many pMixinMember
  pure $ MixinDefinition { ann = pos, name, members }

pMixinMember :: HParser MixinMember
pMixinMember =
  MixinAttribute <$> try pAttribute <|> MixinConstant <$> pConstant <|> MixinOperation <$> pOperation

pConstructor :: HParser Constructor
pConstructor = stmt $ do
  pos        <- getSourcePos
  attributes <- pExtendedAttributeList
  _          <- L.keyword "constructor"
  arguments  <- pArgumentList
  pure $ Constructor { ann = pos, attributes, arguments }

pNamespace :: HParser NamespaceDefinition
pNamespace = stmt $ do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _          <- try $ L.keyword "namespace"
  name       <- pIdent
  members    <- L.braces $ many pMember
  pure $ NamespaceDefinition { ann = pos, attributes, name, members }
 where
  pMember :: HParser NamespaceMember
  pMember = (NamespaceOperation <$> pOperation) <|> (NamespaceAttribute <$> pAttribute)

pEnum :: HParser EnumDefinition
pEnum = stmt $ do
  pos    <- getSourcePos
  _      <- try $ L.keyword "enum"
  name   <- pIdent
  values <- L.braces $ sepEndBy1 pStringLiteral L.comma
  pure $ EnumDefinition { ann = pos, name, values }

pTypedef :: HParser TypedefDefinition
pTypedef = stmt $ do
  pos   <- getSourcePos
  _     <- try $ L.keyword "typedef"
  type' <- pTypeWithExtendedAttributes
  name  <- pIdent
  pure $ TypedefDefinition { ann = pos, name, type' }

pDictionary :: HParser DictionaryDefinition
pDictionary = stmt $ do
  pos     <- getSourcePos
  _       <- try $ L.keyword "dictionary"
  name    <- pIdent
  parent  <- optional $ L.colon *> pIdent
  members <- L.braces $ many pMember
  pure $ DictionaryDefinition { ann = pos, name, parent, members }
 where
  pMember :: HParser DictionaryMember
  pMember         = pRequiredMember <|> pOptionalMember
  pRequiredMember = stmt $ do
    pos   <- getSourcePos
    _     <- L.keyword "required"
    type' <- pType
    name  <- pIdent
    pure $ RequiredDictionaryMember { ann = pos, type', name }
  pOptionalMember = stmt $ do
    pos          <- getSourcePos
    type'        <- pType
    name         <- pIdent
    defaultValue <- optional (L.sym "=" *> pDefaultValue)
    pure $ DictionaryMember { ann = pos, type', name, defaultValue }

pConstant :: HParser Constant
pConstant = stmt $ do
  pos   <- getSourcePos
  _     <- L.sym "const"
  ty    <- pNullableType
  ident <- pIdent
  _     <- L.sym "="
  value <- pConstValue
  pure $ Constant pos ty ident value

prim :: Text -> (forall a. a -> TypeName a) -> HParser TypeName
prim tn f = f <$> getSourcePos <* L.sym tn

integerType :: HParser TypeName
integerType =
  prim "unsigned long long" ULongLongT
    <|> prim "long long" LongLongT
    <|> prim "unsigned long" ULongT
    <|> prim "unsigned short" UShortT
    <|> prim "short" ShortT
    <|> prim "octet" OctetT
    <|> prim "byte" ByteT

numericType :: HParser TypeName
numericType =
  integerType
    <|> prim "unrestricted float" UnrestrictedFloatT
    <|> prim "unrestricted double" UnrestrictedDoubleT
    <|> prim "float" FloatT
    <|> prim "double" DoubleT

primitiveType :: HParser TypeName
primitiveType = numericType <|> prim "boolean" BooleanT

stringType :: HParser TypeName
stringType =
  prim "DOMString" DOMStringT
    <|> prim "USVString" USVStringT
    <|> prim "ByteString" ByteStringT

pAnyType :: HParser TypeName
pAnyType = prim "any" AnyT

pVoidType :: HParser TypeName
pVoidType = prim "void" VoidT

pInterfaceType :: HParser TypeName
pInterfaceType = InterfaceType <$> getSourcePos <*> pIdent

pPromiseType :: HParser TypeName
pPromiseType = pGenericType1 "Promise" PromiseT

pSequenceType :: HParser TypeName
pSequenceType = pGenericType1' "sequence" SequenceT

pType :: HParser TypeName
pType =
  pUnionType
    <|> primitiveType
    <|> try stringType
    <|> pPromiseType
    <|> pSequenceType
    <|> pInterfaceType
    <|> pAnyType

pNullableType :: HParser TypeName
pNullableType = try ((NullableT <$> pType) <* L.sym "?") <|> pType

pTypeWithExtendedAttributes :: HParser TypeWithExtendedAttributes
pTypeWithExtendedAttributes = pTypeWithExtendedAttributes' pNullableType

pTypeWithExtendedAttributes'
  :: HParser TypeName -> HParser TypeWithExtendedAttributes
pTypeWithExtendedAttributes' pInner = do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  inner      <- pInner
  pure $ TypeWithExtendedAttributes { ann = pos, attributes, inner }

pGenericType1 :: Text -> (forall a. TypeName a -> TypeName a) -> HParser TypeName
pGenericType1 tn f = f <$> (L.sym tn *> L.carets pNullableType)

pGenericType1' :: Text -> (forall a. TypeWithExtendedAttributes a -> TypeName a) -> HParser TypeName
pGenericType1' tn f = f <$> (L.sym tn *> L.carets pTypeWithExtendedAttributes)

pUnionType :: HParser TypeName
pUnionType = UnionT <$> L.parens (sepBy1 pTypeWithExtendedAttributes (L.keyword "or"))

pGetter :: HParser Getter
pGetter = stmt $ do
  pos        <- getSourcePos
  attributes <- pExtendedAttributeList
  _          <- L.keyword "getter"
  type'      <- pTypeWithExtendedAttributes
  name       <- optional pIdent
  arguments  <- pArgumentList
  pure $ Getter { ann = pos, attributes, type', name, arguments }

pStringifier :: HParser Stringifier
pStringifier = stmt $ do
  pos   <- getSourcePos
  attrs <- pExtendedAttributeList
  _     <- L.keyword "stringifier"
  let pStringifierAttribute =
        StringifierAttribute pos attrs
          <$> (L.keyword "attribute" *> pTypeWithExtendedAttributes)
          <*> pIdent
  let pStringifierOperation = do
        returnType <- pTypeWithExtendedAttributes
        arguments  <- pArgumentList
        pure $ StringifierOperation pos attrs returnType arguments
  let pShorthand = pure $ StringifierOperation
        pos
        attrs
        (TypeWithExtendedAttributes pos mempty (DOMStringT pos))
        mempty
  pStringifierAttribute <|> pStringifierOperation <|> pShorthand

modifier :: Parser a -> Parser Bool
modifier p = option False (p *> pure True)

pAttribute :: HParser Attribute
pAttribute = stmt $ try $ do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  readonly   <- modifier $ L.keyword "readonly"
  _          <- L.keyword "attribute"
  ty         <- pTypeWithExtendedAttributes
  ident      <- pIdent
  pure $ Attribute { attributes, ann = pos, type' = ty, name = ident, readonly }

pOperation :: HParser Operation
pOperation = stmt $ do
  pos          <- getSourcePos
  attributes   <- hidden pExtendedAttributeList
  ty           <- pReturnType
  ident        <- pIdent
  argumentList <- pArgumentList
  pure $ Operation { ann        = pos
                   , attributes
                   , type'      = ty
                   , name       = ident
                   , arguments  = argumentList
                   }
  where pReturnType = pVoidType <|> pNullableType

pConstValue :: Parser ConstValue
pConstValue =
  (ConstBoolean <$> pBooleanLiteral)
    <|> (ConstInt <$> pHexLiteral)
    <|> (ConstInt <$> pIntegerLiteral)
    <|> (ConstFloat <$> pFloatLiteral)

pBooleanLiteral :: Parser Bool
pBooleanLiteral = (pure True <* L.sym "true") <|> (pure False <* L.sym "false")

pIntegerLiteral :: Parser Int
pIntegerLiteral = try $ L.decimal <* notFollowedBy L.dot

pInfinity :: Parser Float
pInfinity = L.signed $ infinity <$ L.keyword "Infinity"

pNaN :: Parser Float
pNaN = nan <$ L.keyword "NaN"

pFloatLiteral :: Parser Float
pFloatLiteral = try pNaN <|> try pInfinity <|> Scientific.toRealFloat <$> L.signed L.scientific

pStringLiteral :: Parser Text
pStringLiteral = L.lexeme $ char '"' *> takeWhileP Nothing (/= '"') <* char '"'

pHexLiteral :: Num a => Parser a
pHexLiteral =
  try (char '0' *> char' 'x')
    *> (fromInteger <$> (L.hexadecimal :: Parser Integer))

pDefaultValue :: Parser DefaultValue
pDefaultValue =
  (DefaultNull <$ L.sym "null")
    <|> (DefaultConst <$> pConstValue)
    <|> (DefaultDict <$ L.braces L.ws)
    <|> (DefaultSeq <$ L.brackets L.ws)
    <|> (DefaultString <$> pStringLiteral)

pOptionalArgument :: HParser Argument
pOptionalArgument = do
  pos          <- getSourcePos
  attributes   <- hidden pExtendedAttributeList
  _            <- L.sym "optional"
  ty           <- pTypeWithExtendedAttributes
  ident        <- pIdent
  defaultValue <- optional (L.sym "=" *> pDefaultValue)
  pure $ OptionalArgument { ann = pos, attributes, type' = ty, name = ident, defaultValue }

pVariadicArgument :: HParser Argument
pVariadicArgument = do
  pos   <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  ty    <- pTypeWithExtendedAttributes
  _     <- L.sym "..."
  ident <- pIdent
  pure $ VariadicArgument { ann = pos, attributes, type' = ty, name = ident }

pRegularArgument :: HParser Argument
pRegularArgument =
  (flip RegularArgument) <$> hidden pExtendedAttributeList <*> getSourcePos <*> pTypeWithExtendedAttributes <*> pIdent

pArgumentList :: HParser ArgumentList
pArgumentList = L.parens $ ArgumentList <$> sepBy pArgument L.comma

pArgument :: HParser Argument
pArgument =
  try pOptionalArgument <|> try pVariadicArgument <|> pRegularArgument

pIncludesStatement :: HParser IncludesStatementDefinition
pIncludesStatement = stmt $ do
  pos       <- getSourcePos
  interface <- pIdent
  _         <- L.keyword "includes"
  mixin     <- pIdent
  pure $ IncludesStatementDefinition { ann = pos, interface, mixin }

pCallbackDefinition :: HParser CallbackDefinition
pCallbackDefinition = stmt $ do
  pos        <- getSourcePos
  attributes <- pExtendedAttributeList
  _          <- L.keyword "callback"
  name       <- pIdent
  _          <- L.eq
  returnType <- pType
  arguments  <- pArgumentList
  pure $ CallbackDefinition { ann        = pos
                            , attributes
                            , name
                            , returnType
                            , arguments
                            }

pDefn :: HParser Definition
pDefn = choice
  [ Dictionary <$> pDictionary
  , Typedef <$> pTypedef
  , Enum <$> pEnum
  , CallbackInterface <$> try pCallbackInterface
  , Callback <$> try pCallbackDefinition
  , PartialMixin <$> pPartialMixin
  , Mixin <$> pMixin
  , PartialInterface <$> try pPartialInterface
  , Interface <$> try pInterface
  , Namespace <$> pNamespace
  , IncludesStatement <$> pIncludesStatement
  ]

pFragment :: HParser Fragment
pFragment = L.ws *> (Fragment <$> getSourcePos <*> some pDefn) <* hidden eof
