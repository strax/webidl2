{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable #-}

module Language.WebIDL.AST where

import Data.Kind (Type)
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Prelude.Extras (Show1(..), show1, Eq1(..))
import Data.Typeable
import Data.Data

data Fragment a = Fragment a [Definition a] deriving (Show, Eq)

data Definition a
  = Typedef (TypedefDefinition a)
  | Interface (InterfaceDefinition a)
  | Namespace (NamespaceDefinition a)
  deriving (Show, Eq, Generic, Functor)

class (Functor f) => IsInterfaceMember (f :: Type -> Type)

data TypedefDefinition a
  = TypedefDefinition
      { ann :: a,
        attributes :: ExtendedAttributeList a,
        name :: Ident,
        type' :: TypeName
      }
  deriving (Show, Eq, Generic, Functor)

data InterfaceDefinition a
  = InterfaceDefinition
      { ann :: a,
        attributes :: ExtendedAttributeList a,
        name :: Ident,
        parent :: Maybe Ident,
        members :: [InterfaceMember a]
      }
  deriving (Show, Eq, Generic, Functor, Typeable)

data InterfaceMember a = IAttr (Attribute a) | IConst (Constant a) | IOp (Operation a) | ICtor (Constructor a)
  deriving (Show, Eq, Generic, Functor, Typeable)

data NamespaceDefinition a
  = NamespaceDefinition
      { ann :: a,
        attributes :: ExtendedAttributeList a,
        name :: Ident,
        members :: [NamespaceMember a]
      }
  deriving (Show, Eq, Generic, Functor, Typeable)

data NamespaceMember a
  = NAttr (Attribute a)
  | NOp (Operation a)
  deriving (Show, Eq, Generic, Functor, Typeable)

data Constant a
  = Constant
      { ann :: a,
        type' :: TypeName,
        name :: Ident,
        value :: ConstValue
      }
  deriving (Show, Eq, Generic, Functor, Typeable)

newtype ExtendedAttributeList a = ExtendedAttributeList [ExtendedAttribute a]
  deriving (Show, Eq, Semigroup, Monoid, Generic, Functor, Typeable)

newtype ArgumentList a = ArgumentList [Argument a]
  deriving (Show, Eq, Semigroup, Monoid, Generic, Functor, Typeable)

data Constructor a = Constructor { ann :: a, arguments :: ArgumentList a }
  deriving (Show, Eq, Generic, Functor, Typeable)

data ExtendedAttribute a
  = ExtendedAttributeNoArgs Ident
  | ExtendedAttributeArgList Ident (ArgumentList a)
  | ExtendedAttributeNamedArgList Ident Ident (ArgumentList a)
  | ExtendedAttributeIdent Ident Ident
  | ExtendedAttributeIdentList Ident [Ident]
  deriving (Show, Eq, Generic, Functor, Typeable)

data Operation a
  = RegularOperation {ann :: a, type' :: TypeName, name :: Ident, arguments :: ArgumentList a}
  | GetterOperation {ann :: a, type' :: TypeName, name :: Ident}
  | SetterOperation {ann :: a, type' :: TypeName, name :: Ident}
  | DeleterOperation {ann :: a, type' :: TypeName, name :: Ident}
  | StringifierOperation {ann :: a, type' :: TypeName, name :: Ident}
  deriving (Show, Eq, Generic, Functor, Typeable)

data Argument a
  = RegularArgument {ann :: a, type' :: TypeName, name :: Ident}
  | OptionalArgument {ann :: a, type' :: TypeName, name :: Ident, defaultValue :: (Maybe DefaultValue)}
  | VariadicArgument {ann :: a, type' :: TypeName, name :: Ident}
  deriving (Show, Eq, Generic, Functor, Typeable)

data DefaultValue = DefaultConst ConstValue | DefaultDict | DefaultSeq deriving (Show, Eq)

data ConstValue
  = ConstBoolean Bool
  | ConstNumeric Scientific
  deriving (Show, Eq, Typeable, Generic)

data Attribute a
  = Attribute
      { ann :: a,
        attributes :: ExtendedAttributeList a,
        type' :: TypeName,
        name :: Ident,
        readonly :: Bool
      }
  deriving (Show, Eq, Generic, Functor, Typeable)

newtype Ident = Ident Text deriving (Show, Eq, Generic, Typeable)

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
  deriving (Show, Eq, Generic, Typeable)
