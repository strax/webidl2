module Language.WebIDL.Parser where

import           Language.WebIDL.Parser.Types
import qualified Language.WebIDL.Parser.Lex    as L

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
import           Control.Monad                  ( guard )
import           Language.WebIDL.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Debug          ( dbg )
import           Data.Char                      ( isLetter
                                                , isDigit
                                                )
import           Numeric.IEEE

-- Parses the @Definitions@ nonterminal symbol
pFragment :: HParser Fragment
pFragment = L.sc *> (Fragment <$> getSourcePos <*> some pDefn) <* hidden eof

-- Parses the @Definition@ nonterminal symbol
pDefn :: HParser Definition
pDefn = choice
  [ Dictionary <$> pDictionary
  , PartialDictionary <$> pPartialDictionary
  , Typedef <$> pTypedef
  , Enum <$> pEnum
  , CallbackInterface <$> try pCallbackInterface
  , Callback <$> try pCallbackDefinition
  , PartialMixin <$> pPartialMixin
  , Mixin <$> pMixin
  , PartialInterface <$> try pPartialInterface
  , Interface <$> try pInterface
  , PartialNamespace <$> (L.term "partial" *> pNamespace)
  , Namespace <$> pNamespace
  , IncludesStatement <$> pIncludesStatement
  ]

pIdent :: Parser Ident
pIdent = Ident <$> L.identifier

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
  ExtendedAttributeIdentList <$> pIdent <* L.eq <*> L.parens
    (sepBy1 pIdent L.comma)

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
  _          <- try $ L.term "interface"
  name       <- pIdent
  parent     <- optional $ L.colon *> pIdent
  members    <- L.braces $ many pInterfaceMember
  pure $ InterfaceDefinition { ann = pos, attributes, name, members, parent }

pInterfaceMember :: HParser InterfaceMember
pInterfaceMember = do
  choice
    [ L.term "static" *> pStaticMember
    , InterfaceSetlikeDeclaration <$> pSetlikeDeclaration
    , InterfaceMaplikeDeclaration <$> pMaplikeDeclaration
    , InterfaceAsyncIterableDeclaration <$> try pAsyncIterableDeclaration
    , InterfaceIterableDeclaration <$> try pIterableDeclaration
    , InterfaceStringifier <$> try pStringifier
    , InterfaceGetter <$> try pGetter
    , InterfaceSetter <$> try pSetter
    , InterfaceDeleter <$> try pDeleter
    , InterfaceConstructor <$> try pConstructor
    , InterfaceAttribute <$> pAttribute
    , InterfaceConstant <$> pConstant
    , InterfaceOperation <$> pOperation
    ]
 where
  pStaticMember =
    InterfaceStaticAttribute
      <$> pAttribute
      <|> InterfaceStaticOperation
      <$> pOperation

pSetlikeDeclaration :: HParser SetlikeDeclaration
pSetlikeDeclaration = stmt $ do
  pos     <- getSourcePos
  variant <- try $ toConstructor <$> modifier (L.term "readonly") <* L.term
    "setlike"
  tn <- L.carets pAnnotatedType
  pure $ variant pos tn
 where
  toConstructor True  = ReadOnlySetlikeDeclaration
  toConstructor False = ReadWriteSetlikeDeclaration

pMaplikeDeclaration :: HParser MaplikeDeclaration
pMaplikeDeclaration = stmt $ do
  pos     <- getSourcePos
  variant <- try $ toConstructor <$> modifier (L.term "readonly") <* L.term
    "maplike"
  L.carets $ do
    keyType   <- pAnnotatedType
    _         <- L.comma
    valueType <- pAnnotatedType
    pure $ variant pos keyType valueType
 where
  toConstructor True  = ReadOnlyMaplikeDeclaration
  toConstructor False = ReadWriteMaplikeDeclaration


pAsyncIterableDeclaration :: HParser AsyncIterableDeclaration
pAsyncIterableDeclaration = stmt $ do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _          <- L.term "async" *> L.term "iterable"
  L.carets $ do
    key   <- pAnnotatedType
    _     <- L.comma
    value <- pAnnotatedType
    pure $ AsyncIterableDeclaration { ann = pos, attributes, key, value }

pIterableDeclaration :: HParser IterableDeclaration
pIterableDeclaration = stmt $ do
  pos   <- getSourcePos
  attrs <- hidden pExtendedAttributeList
  _     <- L.term "iterable"
  let pPair =
        PairIteratorDeclaration pos attrs
          <$> pAnnotatedType
          <*  L.comma
          <*> pAnnotatedType
  let pValue =
        ValueIteratorDeclaration pos attrs <$> pAnnotatedType
  L.carets $ try pPair <|> pValue

pPartialInterface :: HParser PartialInterfaceDefinition
pPartialInterface = stmt $ do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _          <- try $ L.term "partial" *> L.term "interface"
  name       <- pIdent
  members    <- L.braces (many pPartialInterfaceMember)
  pure $ PartialInterfaceDefinition { ann = pos, attributes, name, members }


pPartialInterfaceMember :: HParser PartialInterfaceMember
pPartialInterfaceMember = do
  choice
    [ L.term "static" *> pStaticMember
    , PartialInterfaceSetlikeDeclaration <$> pSetlikeDeclaration
    , PartialInterfaceMaplikeDeclaration <$> pMaplikeDeclaration
    , PartialInterfaceAsyncIterableDeclaration <$> try pAsyncIterableDeclaration
    , PartialInterfaceIterableDeclaration <$> try pIterableDeclaration
    , PartialInterfaceStringifier <$> try pStringifier
    , PartialInterfaceGetter <$> try pGetter
    , PartialInterfaceSetter <$> try pSetter
    , PartialInterfaceDeleter <$> try pDeleter
    , PartialInterfaceAttribute <$> pAttribute
    , PartialInterfaceConstant <$> pConstant
    , PartialInterfaceOperation <$> pOperation
    ]
 where
  pStaticMember =
    PartialInterfaceStaticAttribute
      <$> pAttribute
      <|> PartialInterfaceStaticOperation
      <$> pOperation


stmt :: Parser a -> Parser a
stmt p = p <* L.semi

pCallbackInterface :: HParser CallbackInterfaceDefinition
pCallbackInterface = stmt $ do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _          <- try $ L.term "callback" *> L.term "interface"
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
  _       <- try $ L.term "partial" *> L.term "interface" *> L.term "mixin"
  name    <- pIdent
  members <- L.braces $ many pMixinMember
  pure $ MixinDefinition { ann = pos, name, members }

pMixin :: HParser MixinDefinition
pMixin = stmt $ do
  pos     <- getSourcePos
  _       <- try $ L.term "interface" *> L.term "mixin"
  name    <- pIdent
  members <- L.braces $ many pMixinMember
  pure $ MixinDefinition { ann = pos, name, members }

pMixinMember :: HParser MixinMember
pMixinMember =
  (MixinAttribute <$> try pAttribute)
    <|> (MixinConstant <$> pConstant)
    <|> (MixinOperation <$> pOperation)

pConstructor :: HParser Constructor
pConstructor = stmt $ do
  pos        <- getSourcePos
  attributes <- pExtendedAttributeList
  _          <- L.term "constructor"
  arguments  <- pArgumentList
  pure $ Constructor { ann = pos, attributes, arguments }

pNamespace :: HParser NamespaceDefinition
pNamespace = stmt $ do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  _          <- try $ L.term "namespace"
  name       <- pIdent
  members    <- L.braces $ many pNamespaceMember
  pure $ NamespaceDefinition { ann = pos, attributes, name, members }

pNamespaceMember :: HParser NamespaceMember
pNamespaceMember =
  (NamespaceAttribute <$> pAttribute) <|> (NamespaceOperation <$> pOperation)

pEnum :: HParser EnumDefinition
pEnum = stmt $ do
  pos    <- getSourcePos
  _      <- try $ L.term "enum"
  name   <- pIdent
  values <- L.braces $ sepEndBy1 pStringLiteral L.comma
  pure $ EnumDefinition { ann = pos, name, values }

pTypedef :: HParser TypedefDefinition
pTypedef = stmt $ do
  pos   <- getSourcePos
  _     <- try $ L.term "typedef"
  type' <- pAnnotatedType
  name  <- pIdent
  pure $ TypedefDefinition { ann = pos, name, type' }

pDictionary :: HParser DictionaryDefinition
pDictionary = stmt $ do
  pos     <- getSourcePos
  _       <- try $ L.term "dictionary"
  name    <- pIdent
  parent  <- optional $ L.colon *> pIdent
  members <- L.braces $ many pDictionaryMember
  pure $ DictionaryDefinition { ann = pos, name, parent, members }

pPartialDictionary :: HParser PartialDictionaryDefinition
pPartialDictionary = stmt $ do
  pos     <- getSourcePos
  _       <- try $ L.term "partial" *> L.term "dictionary"
  name    <- pIdent
  members <- L.braces $ many pDictionaryMember
  pure $ PartialDictionaryDefinition { ann = pos, name, members }


pDictionaryMember :: HParser DictionaryMember
pDictionaryMember = pRequiredMember <|> pOptionalMember
 where
  pRequiredMember = stmt $ do
    pos   <- getSourcePos
    _     <- L.term "required"
    type' <- pNullableType
    name  <- pIdent
    pure $ RequiredDictionaryMember { ann = pos, type', name }
  pOptionalMember = stmt $ do
    pos          <- getSourcePos
    type'        <- pNullableType
    name         <- pIdent
    defaultValue <- optional (L.sym "=" *> pDefaultValue)
    pure $ DictionaryMember { ann = pos, type', name, defaultValue }

pConstant :: HParser Constant
pConstant = stmt $ do
  pos   <- getSourcePos
  _     <- L.sym "const"
  ty    <- pType
  ident <- pIdent
  _     <- L.sym "="
  value <- pConstValue
  pure $ Constant pos ty ident value

prim :: Text -> (forall a . a -> TypeName a) -> HParser TypeName
prim tn f = f <$> getSourcePos <* L.term tn

integerType :: HParser TypeName
integerType =
  prim "unsigned long long" ULongLongT
    <|> prim "long long"      LongLongT
    <|> prim "long"           LongT
    <|> prim "unsigned long"  ULongT
    <|> prim "unsigned short" UShortT
    <|> prim "short"          ShortT
    <|> prim "octet"          OctetT
    <|> prim "byte"           ByteT

numericType :: HParser TypeName
numericType =
  integerType
    <|> prim "unrestricted float"  UnrestrictedFloatT
    <|> prim "unrestricted double" UnrestrictedDoubleT
    <|> prim "float"               FloatT
    <|> prim "double"              DoubleT

primitiveType :: HParser TypeName
primitiveType = numericType <|> prim "boolean" BooleanT

stringType :: HParser TypeName
stringType =
  prim "DOMString" DOMStringT
    <|> prim "USVString"  USVStringT
    <|> prim "ByteString" ByteStringT

pAnyType :: HParser TypeName
pAnyType = prim "any" AnyT <* notFollowedBy (L.term "?")

pVoidType :: HParser TypeName
pVoidType = prim "void" VoidT

pObjectType :: HParser TypeName
pObjectType = prim "object" ObjectT

pInterfaceType :: HParser TypeName
pInterfaceType = InterfaceType <$> getSourcePos <*> pIdent

nonNullable :: HParser TypeName -> HParser TypeName
nonNullable p = p <* notFollowedBy (L.term "?")

pPromiseType :: HParser TypeName
pPromiseType = nonNullable $ pGenericType1 "Promise" PromiseT

pSequenceType :: HParser TypeName
pSequenceType = pGenericType1' "sequence" SequenceT

pType :: HParser TypeName
pType =
  pUnionType
    <|> primitiveType
    <|> try stringType
    <|> pPromiseType
    <|> pRecordType
    <|> pSequenceType
    <|> pObjectType
    <|> pAnyType
    <|> pInterfaceType

pNullableType :: HParser TypeName
pNullableType = try ((NullableT <$> pType) <* L.sym "?") <|> pType

pAnnotatedType :: HParser TypeName
pAnnotatedType = pAnnotatedType' pNullableType

pAnnotatedType'
  :: HParser TypeName -> HParser TypeName
pAnnotatedType' pInner = do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  inner      <- pInner
  pure $ AnnotatedType pos attributes inner

pGenericType1
  :: Text -> (forall a . TypeName a -> TypeName a) -> HParser TypeName
pGenericType1 tn f = f <$> (L.sym tn *> L.carets pNullableType)

pGenericType1'
  :: Text
  -> (forall a . TypeName a -> TypeName a)
  -> HParser TypeName
pGenericType1' tn f = f <$> (L.sym tn *> L.carets pAnnotatedType)

sepBy2 :: Parser a -> Parser sep -> Parser [a]
sepBy2 p sep = (:) <$> p <* sep <*> sepBy1 p sep

nullable :: HParser TypeName -> HParser TypeName
nullable p = do
  inner  <- p
  isNull <- modifier (L.term "?")
  pure $ if isNull then NullableT inner else inner

pSymbolType :: HParser TypeName
pSymbolType = prim "symbol" SymbolT

pFrozenArrayType :: HParser TypeName
pFrozenArrayType = pGenericType1' "FrozenArray" FrozenArrayT

pBufferRelatedType :: HParser TypeName
pBufferRelatedType =
  prim "ArrayBuffer" ArrayBufferT
    <|> prim "DataView"          DataViewT
    <|> prim "Int8Array"         Int8ArrayT
    <|> prim "Int16Array"        Int16ArrayT
    <|> prim "Int32Array"        Int32ArrayT
    <|> prim "Uint8Array"        Uint8ArrayT
    <|> prim "Uint16Array"       Uint16ArrayT
    <|> prim "Uint32Array"       Uint32ArrayT
    <|> prim "Uint8ClampedArray" Uint8ClampedArrayT
    <|> prim "Float32Array"      Float32ArrayT
    <|> prim "Float64Array"      Float64ArrayT

pDistinguishableType :: HParser TypeName
pDistinguishableType =
  nullable
    $   primitiveType
    <|> stringType
    <|> pInterfaceType
    <|> pSequenceType
    <|> pObjectType
    <|> pSymbolType
    <|> pBufferRelatedType
    <|> pFrozenArrayType
    <|> pRecordType

pUnionType :: HParser TypeName
pUnionType = UnionT <$> L.parens (sepBy2 ((pAnnotatedType' pDistinguishableType) <|> nullable pUnionType) (L.term "or")) 

pRecordType :: HParser TypeName
pRecordType = do
  _ <- L.term "record"
  L.carets $ do
    keyType   <- stringType
    _         <- L.comma
    valueType <- pAnnotatedType
    pure $ RecordT keyType valueType


pGetter :: HParser Getter
pGetter = stmt $ do
  pos        <- getSourcePos
  attributes <- pExtendedAttributeList
  _          <- L.term "getter"
  type'      <- pAnnotatedType
  name       <- optional pIdent
  arguments  <- pArgumentList
  pure $ Getter { ann = pos, attributes, type', name, arguments }

pSetter :: HParser Setter
pSetter = stmt $ do
  pos        <- getSourcePos
  attributes <- pExtendedAttributeList
  _          <- L.term "setter"
  type'      <- pAnnotatedType
  name       <- optional pIdent
  arguments  <- pArgumentList
  pure $ Setter { ann = pos, attributes, type', name, arguments }

pDeleter :: HParser Deleter
pDeleter = stmt $ do
  pos        <- getSourcePos
  attributes <- pExtendedAttributeList
  _          <- L.term "deleter"
  type'      <- pAnnotatedType
  name       <- optional pIdent
  arguments  <- pArgumentList
  pure $ Deleter { ann = pos, attributes, type', name, arguments }

pStringifier :: HParser Stringifier
pStringifier = stmt $ do
  pos   <- getSourcePos
  attrs <- pExtendedAttributeList
  _     <- L.term "stringifier"
  let pStringifierAttribute =
        StringifierAttribute pos attrs
          <$> (L.term "attribute" *> pAnnotatedType)
          <*> pIdent
  let pStringifierOperation = do
        returnType <- pAnnotatedType
        arguments  <- pArgumentList
        pure $ StringifierOperation pos attrs returnType arguments
  let pShorthand = pure $ StringifierOperation
        pos
        attrs
        (AnnotatedType pos mempty (DOMStringT pos))
        mempty
  pStringifierAttribute <|> pStringifierOperation <|> pShorthand

modifier :: Parser a -> Parser Bool
modifier p = option False (p *> pure True)

pAttribute :: HParser Attribute
pAttribute = stmt $ try $ do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  inherited  <- modifier $ L.term "inherit"
  -- An attribute can be marked readonly only if it is not inherited
  readonly   <- if inherited then pure False else modifier $ L.term "readonly"
  _          <- L.term "attribute"
  ty         <- pAnnotatedType
  ident      <- pAttributeName
  pure $ Attribute { attributes
                   , inherited
                   , ann        = pos
                   , type'      = ty
                   , name       = ident
                   , readonly
                   }

pAttributeName :: Parser Ident
pAttributeName = Ident <$> L.attributeName

pOperation :: HParser Operation
pOperation = stmt $ do
  pos          <- getSourcePos
  attributes   <- hidden pExtendedAttributeList
  ty           <- pReturnType
  ident        <- pOperationName
  argumentList <- pArgumentList
  pure $ Operation { ann        = pos
                   , attributes
                   , type'      = ty
                   , name       = ident
                   , arguments  = argumentList
                   }
  where pReturnType = pVoidType <|> pNullableType

pOperationName :: Parser Ident
pOperationName = Ident <$> L.operationName

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
pInfinity = L.signed $ infinity <$ L.term "Infinity"

pNaN :: Parser Float
pNaN = nan <$ L.term "NaN"

pFloatLiteral :: Parser Float
pFloatLiteral =
  try pNaN
    <|> try pInfinity
    <|> Scientific.toRealFloat
    <$> L.signed L.scientific

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
    <|> (DefaultDict <$ L.braces L.sc)
    <|> (DefaultSeq <$ L.brackets L.sc)
    <|> (DefaultString <$> pStringLiteral)

pOptionalArgument :: HParser Argument
pOptionalArgument = do
  pos          <- getSourcePos
  attributes   <- hidden pExtendedAttributeList
  _            <- L.sym "optional"
  ty           <- pAnnotatedType
  ident        <- pIdent
  defaultValue <- optional (L.sym "=" *> pDefaultValue)
  pure $ OptionalArgument { ann          = pos
                          , attributes
                          , typeName'    = ty
                          , name         = ident
                          , defaultValue
                          }

pVariadicArgument :: HParser Argument
pVariadicArgument = do
  pos        <- getSourcePos
  attributes <- hidden pExtendedAttributeList
  ty         <- pAnnotatedType
  _          <- L.sym "..."
  ident      <- pArgumentName
  pure
    $ VariadicArgument { ann = pos, attributes, typeName' = ty, name = ident }

pRegularArgument :: HParser Argument
pRegularArgument =
  (flip RegularArgument)
    <$> hidden pExtendedAttributeList
    <*> getSourcePos
    <*> pNullableType
    <*> pArgumentName

pArgumentList :: HParser ArgumentList
pArgumentList = L.parens $ ArgumentList <$> sepBy pArgument L.comma

pArgument :: HParser Argument
pArgument =
  try pOptionalArgument <|> try pVariadicArgument <|> pRegularArgument

pArgumentName :: Parser Ident
pArgumentName = Ident <$> L.argumentName

pIncludesStatement :: HParser IncludesStatementDefinition
pIncludesStatement = stmt $ do
  pos       <- getSourcePos
  interface <- pIdent
  _         <- L.term "includes"
  mixin     <- pIdent
  pure $ IncludesStatementDefinition { ann = pos, interface, mixin }

pCallbackDefinition :: HParser CallbackDefinition
pCallbackDefinition = stmt $ do
  pos        <- getSourcePos
  attributes <- pExtendedAttributeList
  _          <- L.term "callback"
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
