module Language.WebIDL.AST where

import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype Fragment = Fragment [Ann Defn] deriving (Show, Eq)

data Defn
  = Typedef Type Ident
  | Interface Ident (Maybe Ident) [InterfaceMember]
  | Namespace Ident [NamespaceMember]
  deriving (Show, Eq, Generic)

data InterfaceMember
  = IAttribute (Ann Attribute)
  | IOperation (Ann Operation)
  | IConstructor Constructor
  | IConstant (Ann Constant)
  deriving (Show, Eq)

data NamespaceMember = NSAttribute (Ann Attribute) | NSOperation (Ann Operation) deriving (Show, Eq, Generic)

data Constant = Constant Type Ident ConstValue deriving (Show, Eq, Generic)

newtype ExtendedAttributeList = ExtendedAttributeList [ExtendedAttribute] deriving (Show, Eq, Semigroup, Monoid, Generic)

newtype ArgumentList = ArgumentList [Argument] deriving (Show, Eq, Semigroup, Monoid, Generic)

data Ann a = Ann ExtendedAttributeList a deriving (Show, Eq, Generic)

data Constructor = Constructor ArgumentList deriving (Show, Eq, Generic)

data ExtendedAttribute
  = ExtendedAttributeNoArgs Ident
  | ExtendedAttributeArgList Ident ArgumentList
  | ExtendedAttributeNamedArgList Ident Ident ArgumentList
  | ExtendedAttributeIdent Ident Ident
  | ExtendedAttributeIdentList Ident [Ident]
  deriving (Show, Eq, Generic)

data Operation
  = RegularOperation Type Ident ArgumentList
  | GetterOperation Type Ident
  | SetterOperation Type Ident
  | DeleterOperation Type Ident
  | StringifierOperation Type Ident
  deriving (Show, Eq, Generic)

data Argument
  = RegularArgument Type Ident
  | OptionalArgument Type Ident (Maybe ConstValue)
  | VariadicArgument Type Ident
  deriving (Show, Eq, Generic)

data ConstValue
  = BooleanLiteral Bool
  | IntegerLiteral Int
  | ScientificLiteral Scientific
  | StringLiteral Text
  deriving (Show, Eq, Generic)

data Attribute
  = Attribute
      { type' :: Type,
        ident :: Ident,
        readonly :: Bool
      }
  deriving (Show, Eq, Generic)

newtype Ident = Ident Text deriving (Show, Eq, Generic)

data Type
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
  | NullableT Type
  deriving (Show, Eq)
