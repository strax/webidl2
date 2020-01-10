module Language.WebIDL.AST where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific

newtype Fragment = Fragment [Defn] deriving (Show, Eq)

data Defn where
  Typedef :: Type -> Ident -> Defn
  Interface :: Ident -> (Maybe Ident) -> [InterfaceMember] -> Defn

deriving instance Show Defn

deriving instance Eq Defn

data InterfaceMember = IAttribute Attribute | IOperation Operation | IConstant Constant deriving (Show, Eq)

data Constant = Constant Type Ident ConstValue deriving (Show, Eq)

data Operation
  = RegularOperation Type Ident [Argument]
  | GetterOperation Type Ident
  | SetterOperation Type Ident
  | DeleterOperation Type Ident
  | StringifierOperation Type Ident
  deriving (Show, Eq)

data Argument = RegularArgument Type Ident | OptionalArgument Type Ident (Maybe ConstValue) | VariadicArgument Type Ident deriving (Show, Eq)

data ConstValue = BooleanLiteral Bool | IntegerLiteral Int | ScientificLiteral Scientific deriving (Show, Eq)

data Attribute
  = Attribute
      { type' :: Type,
        ident :: Ident,
        readonly :: Bool
      }
  deriving (Show, Eq)

newtype Ident = Ident Text deriving (Show, Eq)

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
