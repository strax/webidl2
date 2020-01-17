{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.WebIDL.AST where

import           Data.Data
import           Data.Scientific
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data Fragment a = Fragment a [Definition a]
  deriving (Show, Eq, Generic, Functor, Typeable)

data Definition a
  = Typedef (TypedefDefinition a)
  | Interface (InterfaceDefinition a)
  | Mixin (MixinDefinition a)
  | PartialMixin (MixinDefinition a)
  | PartialInterface (PartialInterfaceDefinition a)
  | CallbackInterface (CallbackInterfaceDefinition a)
  | Namespace (NamespaceDefinition a)
  | PartialNamespace (NamespaceDefinition a)
  | Enum (EnumDefinition a)
  | Dictionary (DictionaryDefinition a)
  | PartialDictionary (PartialDictionaryDefinition a)
  | IncludesStatement (IncludesStatementDefinition a)
  | Callback (CallbackDefinition a)
  deriving (Show, Eq, Generic, Functor, Typeable)

data CallbackDefinition a = CallbackDefinition
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , name       :: Ident
  , returnType :: TypeName a
  , arguments  :: ArgumentList a
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data TypedefDefinition a = TypedefDefinition
  { ann   :: a
  , name  :: Ident
  , type' :: TypeName a
  }
  deriving (Show, Eq, Generic, Functor)

data InterfaceDefinition a = InterfaceDefinition
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , name       :: Ident
  , parent     :: Maybe Ident
  , members    :: [InterfaceMember a]
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data MixinDefinition a = MixinDefinition
  { ann     :: a
  , name    :: Ident
  , members :: [MixinMember a]
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data PartialInterfaceDefinition a = PartialInterfaceDefinition
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , name       :: Ident
  , members    :: [PartialInterfaceMember a]
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data IncludesStatementDefinition a = IncludesStatementDefinition
  { ann       :: a
  , interface :: Ident
  , mixin     :: Ident
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data CallbackInterfaceDefinition a = CallbackInterfaceDefinition
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , name       :: Ident
  , members    :: [CallbackInterfaceMember a]
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data MixinMember a
  = MixinAttribute (Attribute a)
  | MixinConstant (Constant a)
  | MixinOperation (Operation a)
  deriving (Show, Eq, Generic, Functor, Typeable)

data PartialInterfaceMember a
  = PartialInterfaceAttribute (Attribute a)
  | PartialInterfaceConstant (Constant a)
  | PartialInterfaceOperation (Operation a)
  | PartialInterfaceStaticAttribute (Attribute a)
  | PartialInterfaceStaticOperation (Operation a)
  | PartialInterfaceGetter (Getter a)
  | PartialInterfaceSetter (Setter a)
  | PartialInterfaceDeleter (Deleter a)
  | PartialInterfaceStringifier (Stringifier a)
  | PartialInterfaceIterableDeclaration (IterableDeclaration a)
  | PartialInterfaceAsyncIterableDeclaration (AsyncIterableDeclaration a)
  | PartialInterfaceSetlikeDeclaration (SetlikeDeclaration a)
  | PartialInterfaceMaplikeDeclaration (MaplikeDeclaration a)
  deriving (Show, Eq, Generic, Functor, Typeable)

data CallbackInterfaceMember a = CallbackInterfaceConstant (Constant a) | CallbackInterfaceOperation (Operation a)
  deriving (Show, Eq, Generic, Functor, Typeable)

data InterfaceMember a
  = InterfaceAttribute (Attribute a)
  | InterfaceConstant (Constant a)
  | InterfaceOperation (Operation a)
  | InterfaceConstructor (Constructor a)
  | InterfaceStaticAttribute (Attribute a)
  | InterfaceStaticOperation (Operation a)
  | InterfaceGetter (Getter a)
  | InterfaceSetter (Setter a)
  | InterfaceDeleter (Deleter a)
  | InterfaceStringifier (Stringifier a)
  | InterfaceIterableDeclaration (IterableDeclaration a)
  | InterfaceAsyncIterableDeclaration (AsyncIterableDeclaration a)
  | InterfaceSetlikeDeclaration (SetlikeDeclaration a)
  | InterfaceMaplikeDeclaration (MaplikeDeclaration a)
  deriving (Show, Eq, Generic, Functor, Typeable)

data NamespaceDefinition a = NamespaceDefinition
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , name       :: Ident
  , members    :: [NamespaceMember a]
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data NamespaceMember a
  = NamespaceAttribute (Attribute a)
  | NamespaceOperation (Operation a)
  deriving (Show, Eq, Generic, Functor, Typeable)

data EnumDefinition a = EnumDefinition
  { ann    :: a
  , name   :: Ident
  , values :: [Text]
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data DictionaryDefinition a = DictionaryDefinition
  { ann     :: a
  , name    :: Ident
  , parent  :: Maybe Ident
  , members :: [DictionaryMember a]
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data PartialDictionaryDefinition a = PartialDictionaryDefinition
  { ann     :: a
  , name    :: Ident
  , members :: [DictionaryMember a]
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data DictionaryMember a
  = RequiredDictionaryMember {ann :: a, type' :: TypeName a, name :: Ident}
  | DictionaryMember {ann :: a, type' :: TypeName a, name :: Ident, defaultValue :: Maybe DefaultValue}
  deriving (Show, Eq, Generic, Functor, Typeable)

data IterableDeclaration a
  = ValueIteratorDeclaration { ann :: a, attributes :: ExtendedAttributeList a, value :: TypeName a }
  | PairIteratorDeclaration { ann :: a, attributes :: ExtendedAttributeList a, key :: TypeName a, value :: TypeName a }
  deriving (Show, Eq, Generic, Functor, Typeable)

data AsyncIterableDeclaration a
  = AsyncIterableDeclaration { ann :: a, attributes :: ExtendedAttributeList a, key :: TypeName a, value :: TypeName a }
  deriving (Show, Eq, Generic, Functor, Typeable)

data Constant a = Constant
  { ann   :: a
  , type' :: TypeName a
  , name  :: Ident
  , value :: ConstValue
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

type ExtendedAttributeList a = [ExtendedAttribute a]
type ArgumentList a = [Argument a]

data Constructor a = Constructor
  { attributes :: ExtendedAttributeList a
  , ann        :: a
  , arguments  :: ArgumentList a
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data ExtendedAttribute a
  = ExtendedAttributeNoArgs Ident
  | ExtendedAttributeArgList Ident (ArgumentList a)
  | ExtendedAttributeNamedArgList Ident Ident (ArgumentList a)
  | ExtendedAttributeIdent Ident Ident
  | ExtendedAttributeIdentList Ident [Ident]
  deriving (Show, Eq, Generic, Functor, Typeable)

data Operation a = Operation
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , type'      :: TypeName a
  , name       :: Ident
  , arguments  :: ArgumentList a
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data Getter a = Getter
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , type'      :: TypeName a
  , name       :: Maybe Ident
  , arguments  :: ArgumentList a
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data Setter a = Setter
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , type'      :: TypeName a
  , name       :: Maybe Ident
  , arguments  :: ArgumentList a
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data Deleter a = Deleter
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , type'      :: TypeName a
  , name       :: Maybe Ident
  , arguments  :: ArgumentList a
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data Stringifier a
  = StringifierOperation { ann :: a, attributes :: ExtendedAttributeList a, type' :: TypeName a, arguments :: ArgumentList a }
  | StringifierAttribute { ann :: a, attributes :: ExtendedAttributeList a, type' :: TypeName a, name :: Ident }
  deriving (Show, Eq, Generic, Functor, Typeable)

data SetlikeDeclaration a
  = ReadWriteSetlikeDeclaration { ann :: a, type' :: TypeName a }
  | ReadOnlySetlikeDeclaration { ann :: a, type' :: TypeName a }
  deriving (Show, Eq, Generic, Functor, Typeable)

data MaplikeDeclaration a
  = ReadWriteMaplikeDeclaration { ann :: a, keyType :: TypeName a, valueType :: TypeName a }
  | ReadOnlyMaplikeDeclaration { ann :: a, keyType :: TypeName a, valueType :: TypeName a }
  deriving (Show, Eq, Generic, Functor, Typeable)

data Argument a
  = RegularArgument { ann :: a, attributes :: ExtendedAttributeList a, typeName :: TypeName a, name :: Ident }
  | OptionalArgument {ann :: a, attributes :: ExtendedAttributeList a, typeName' :: TypeName a, name :: Ident, defaultValue :: (Maybe DefaultValue)}
  | VariadicArgument {ann :: a, attributes :: ExtendedAttributeList a, typeName' :: TypeName a, name :: Ident}
  deriving (Show, Eq, Generic, Functor, Typeable)

data DefaultValue = DefaultConst ConstValue | DefaultString Text | DefaultDict | DefaultSeq | DefaultNull deriving (Show, Eq)

data ConstValue
  = ConstBoolean Bool
  | ConstInt Int
  | ConstFloat Float
  deriving (Show, Eq, Typeable, Generic)

data Attribute a = Attribute
  { ann        :: a
  , inherited  :: Bool
  , attributes :: ExtendedAttributeList a
  , type'      :: TypeName a
  , name       :: Ident
  , readonly   :: Bool
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

newtype Ident = Ident Text deriving (Show, Eq, Ord, Generic, Typeable)

unIdent :: Ident -> Text
unIdent (Ident ident) = ident

data TypeName a
  = AnyT a
  | VoidT a
  | BooleanT a
  | ByteT a
  | OctetT a
  | ShortT a
  | UShortT a
  | LongT a
  | ULongT a
  | LongLongT a
  | ULongLongT a
  | FloatT a
  | UnrestrictedFloatT a
  | DoubleT a
  | UnrestrictedDoubleT a
  | DOMStringT a
  | ByteStringT a
  | USVStringT a
  | ObjectT a
  | SymbolT a
  | InterfaceType a Ident
  | NullableT (TypeName a)
  | SequenceT (TypeName a)
  | RecordT (TypeName a) (TypeName a)
  | PromiseT (TypeName a)
  | FrozenArrayT (TypeName a)
  | ArrayBufferT a
  | DataViewT a
  | Int8ArrayT a
  | Int16ArrayT a
  | Int32ArrayT a
  | Uint8ArrayT a
  | Uint16ArrayT a
  | Uint32ArrayT a
  | Uint8ClampedArrayT a
  | Float32ArrayT a
  | Float64ArrayT a
  | UnionT [TypeName a]
  | AnnotatedType a (ExtendedAttributeList a) (TypeName a)
  deriving (Show, Eq, Functor, Generic, Typeable)
