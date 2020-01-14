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
  deriving (Show, Eq)

data Definition a
  = Typedef (TypedefDefinition a)
  | Interface (InterfaceDefinition a)
  | Mixin (MixinDefinition a)
  | PartialMixin (MixinDefinition a)
  | PartialInterface (PartialInterfaceDefinition a)
  | CallbackInterface (CallbackInterfaceDefinition a)
  | Namespace (NamespaceDefinition a)
  | Enum (EnumDefinition a)
  | Dictionary (DictionaryDefinition a)
  | IncludesStatement (IncludesStatementDefinition a)
  | Callback (CallbackDefinition a)
  deriving (Show, Eq, Generic, Functor, Typeable)

data CallbackDefinition a = CallbackDefinition
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , name       :: Ident
  , returnType :: TypeName
  , arguments  :: ArgumentList a
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data TypedefDefinition a = TypedefDefinition
  { ann   :: a
  , name  :: Ident
  , type' :: TypeName
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
  | PartialInterfaceConstant  (Constant a)
  | PartialInterfaceOperation (Operation a)
  deriving (Show, Eq, Generic, Functor, Typeable)

data CallbackInterfaceMember a = CallbackInterfaceConstant (Constant a) | CallbackInterfaceOperation (Operation a)
  deriving (Show, Eq, Generic, Functor, Typeable)

data InterfaceMember a
  = InterfaceAttribute (Attribute a)
  | InterfaceConstant (Constant a)
  | InterfaceOperation (Operation a)
  | InterfaceConstructor (Constructor a)
  | InterfaceGetter (Getter a)
  | InterfaceSetter (Setter a)
  | InterfaceDeleter (Deleter a)
  | InterfaceStringifier (Stringifier a)
  | InterfaceIterableDeclaration (IterableDeclaration a)
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

data DictionaryMember a
  = RequiredDictionaryMember {ann :: a, type' :: TypeName, name :: Ident}
  | DictionaryMember {ann :: a, type' :: TypeName, name :: Ident, defaultValue :: Maybe DefaultValue}
  deriving (Show, Eq, Generic, Functor, Typeable)

data IterableDeclaration a
  = ValueIteratorDeclaration { ann :: a, attributes :: ExtendedAttributeList a, value :: TypeWithExtendedAttributes a }
  | PairIteratorDeclaration { ann :: a, attributes :: ExtendedAttributeList a, key :: TypeWithExtendedAttributes a, value :: TypeWithExtendedAttributes a }
  deriving (Show, Eq, Generic, Functor, Typeable)

data Constant a = Constant
  { ann   :: a
  , type' :: TypeName
  , name  :: Ident
  , value :: ConstValue
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

newtype ExtendedAttributeList a = ExtendedAttributeList [ExtendedAttribute a]
  deriving (Show, Eq, Functor, Typeable)

deriving newtype instance Semigroup (ExtendedAttributeList a)
deriving newtype instance Monoid (ExtendedAttributeList a)

newtype ArgumentList a = ArgumentList [Argument a]
  deriving (Show, Eq, Generic, Functor, Typeable)


deriving newtype instance Semigroup (ArgumentList a)
deriving newtype instance Monoid (ArgumentList a)

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
  , type'      :: TypeName
  , name       :: Ident
  , arguments  :: ArgumentList a
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data Getter a = Getter
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , type'      :: TypeWithExtendedAttributes a
  , name       :: Maybe Ident
  , arguments  :: ArgumentList a
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data Setter a = Setter
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , type'      :: TypeWithExtendedAttributes a
  , name       :: Maybe Ident
  , arguments  :: ArgumentList a
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data Deleter a = Deleter
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , type'      :: TypeWithExtendedAttributes a
  , arguments  :: ArgumentList a
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data Stringifier a
  = StringifierOperation { ann :: a, attributes :: ExtendedAttributeList a, type' :: TypeWithExtendedAttributes a, arguments :: ArgumentList a }
  | StringifierAttribute { ann :: a, attributes :: ExtendedAttributeList a, type' :: TypeWithExtendedAttributes a, name :: Ident }
  deriving (Show, Eq, Generic, Functor, Typeable)

data Argument a
  = RegularArgument { ann :: a, attributes :: ExtendedAttributeList a, type' :: TypeWithExtendedAttributes a, name :: Ident }
  | OptionalArgument {ann :: a, attributes :: ExtendedAttributeList a, type' :: TypeWithExtendedAttributes a, name :: Ident, defaultValue :: (Maybe DefaultValue)}
  | VariadicArgument {ann :: a, attributes :: ExtendedAttributeList a, type' :: TypeWithExtendedAttributes a, name :: Ident}
  deriving (Show, Eq, Generic, Functor, Typeable)

data DefaultValue = DefaultConst ConstValue | DefaultString Text | DefaultDict | DefaultSeq | DefaultNull deriving (Show, Eq)

data ConstValue
  = ConstBoolean Bool
  | ConstNumeric Scientific
  deriving (Show, Eq, Typeable, Generic)

data Attribute a = Attribute
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , type'      :: TypeWithExtendedAttributes a
  , name       :: Ident
  , readonly   :: Bool
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

newtype Ident = Ident Text deriving (Show, Eq, Ord, Generic, Typeable)

data TypeWithExtendedAttributes a = TypeWithExtendedAttributes
  { ann        :: a
  , attributes :: ExtendedAttributeList a
  , inner      :: TypeName
  }
  deriving (Show, Eq, Generic, Functor, Typeable)

data TypeName
  = AnyT
  | VoidT
  | BooleanT
  | ByteT
  | OctetT
  | ShortT
  | UShortT
  | LongT
  | ULongT
  | LongLongT
  | ULongLongT
  | FloatT
  | UnrestrictedFloatT
  | DoubleT
  | UnrestrictedDoubleT
  | DOMStringT
  | ByteStringT
  | USVStringT
  | ObjectT
  | SymbolT
  | InterfaceType Ident
  | NullableT TypeName
  | SequenceT TypeName
  | RecordT TypeName TypeName
  | PromiseT TypeName
  | UnionT (Set TypeName)
  deriving (Show, Eq, Ord, Generic, Typeable)
