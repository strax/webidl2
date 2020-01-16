{-# LANGUAGE PolyKinds #-}

module Language.WebIDL.Printer where

import qualified Language.WebIDL.AST as AST
import Text.Megaparsec (parse, SourcePos)
import Language.WebIDL.Parser (pFragment)
import Generics.SOP
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Text.Prettyprint.Doc

data SExpr = Named Text [SExpr] | List [SExpr] | Value Text
    deriving (Show)

instance Pretty SExpr where
    pretty (Named name members) = parens $ pretty name <+> nest 2 (vsep (map pretty members))
    pretty (List members) = parens $ sep (map pretty members)
    pretty (Value v) = pretty v

class Inspect a where
    inspect :: a -> SExpr

    default inspect :: (Generic a, HasDatatypeInfo a, All2 Inspect (Code a)) => a -> SExpr
    inspect = ginspect

instance Inspect a => Inspect (I a) where
    inspect = inspect . unI

instance Inspect String where
    inspect = Value . T.pack . show

instance Inspect Text where
    inspect = Value

instance Inspect SourcePos where
    inspect _ = Value "<SourcePos>"

instance Inspect Bool where
    inspect = Value . T.pack . show

instance Inspect Int where
    inspect n = Value (T.pack (show n))

instance Inspect Float where
    inspect n = Value (T.pack (show n))

instance Inspect () where
    inspect _ = Value (T.pack (show ()))

instance Inspect a => Inspect (Maybe a) where
    inspect (Just a) = inspect a
    inspect Nothing = List []

instance Inspect a => Inspect [a] where
    inspect as = List (map inspect as)


instance Inspect AST.Ident where
    inspect (AST.Ident a) = Value a
instance Inspect a => Inspect (AST.Definition a) where
instance Inspect a => Inspect (AST.TypedefDefinition a) where
instance Inspect a => Inspect (AST.InterfaceDefinition a) where
instance Inspect a => Inspect (AST.DictionaryDefinition a) where
instance Inspect a => Inspect (AST.EnumDefinition a) where
instance Inspect a => Inspect (AST.PartialInterfaceDefinition a) where
instance Inspect a => Inspect (AST.ExtendedAttributeList a) where
instance Inspect a => Inspect (AST.MixinDefinition a) where
instance Inspect a => Inspect (AST.NamespaceDefinition a) where
instance Inspect a => Inspect (AST.IncludesStatementDefinition a) where
instance Inspect a => Inspect (AST.CallbackDefinition a) where
instance Inspect a => Inspect (AST.MixinMember a) where
instance Inspect a => Inspect (AST.InterfaceMember a) where
instance Inspect a => Inspect (AST.NamespaceMember a) where
instance Inspect a => Inspect (AST.DictionaryMember a) where
instance Inspect a => Inspect (AST.PartialInterfaceMember a) where
instance Inspect a => Inspect (AST.CallbackInterfaceDefinition a) where
instance Inspect a => Inspect (AST.CallbackInterfaceMember a) where
instance Inspect a => Inspect (AST.Getter a) where
instance Inspect a => Inspect (AST.Setter a) where
instance Inspect a => Inspect (AST.Deleter a) where
instance Inspect a => Inspect (AST.Stringifier a) where
instance Inspect a => Inspect (AST.IterableDeclaration a) where
instance Inspect a => Inspect (AST.AsyncIterableDeclaration a) where
instance Inspect a => Inspect (AST.SetlikeDeclaration a) where
instance Inspect a => Inspect (AST.MaplikeDeclaration a) where
instance Inspect a => Inspect (AST.Constructor a) where
instance Inspect a => Inspect (AST.Operation a) where
instance Inspect a => Inspect (AST.Attribute a) where
instance Inspect AST.DefaultValue where
instance Inspect a => Inspect (AST.Constant a) where
instance Inspect AST.ConstValue where
instance Inspect a => Inspect (AST.ExtendedAttribute a) where
instance Inspect a => Inspect (AST.Argument a) where
instance Inspect a => Inspect (AST.TypeName a) where
instance Inspect a => Inspect (AST.ArgumentList a) where
instance Inspect a => Inspect (AST.TypeWithExtendedAttributes a) where
instance Inspect a => Inspect (AST.Fragment a) where

ginspect :: forall a. (Generic a, HasDatatypeInfo a, All2 Inspect (Code a))
      => a -> SExpr
ginspect a =
  gshow' (constructorInfo (datatypeInfo (Proxy :: Proxy a))) (from a)

gshow' :: (All2 Inspect xss, SListI xss) => NP ConstructorInfo xss -> SOP I xss -> SExpr
gshow' cs (SOP sop) = hcollapse $ hcliftA2 (allp) goConstructor cs sop

goConstructor :: All Inspect xs => ConstructorInfo xs -> NP I xs -> K SExpr xs
goConstructor (Constructor n) args =
    K $ Named (T.pack n) args'
  where
    args' :: [SExpr]
    args' = hcollapse $ hcliftA p (K . inspect . unI) args
goConstructor (Record n ns) args =
    -- K $ "(" <> n <> "\n" <> intercalate "\n" (map indent args') <> ")"
    K $ Named (T.pack n) args'
  where
    args' :: [SExpr]
    args' = hcollapse $ hcliftA2 p goField ns args
goConstructor (Infix n _ _) (arg1 :* arg2 :* Nil) =
    K $ Named (T.pack n) [inspect arg1, inspect arg2]

goField :: Inspect a => FieldInfo a -> I a -> K SExpr a
goField (FieldInfo field) (I a) = K $ Named (T.pack field) [inspect a]

p :: Proxy Inspect
p = Proxy

allp :: Proxy (All Inspect)
allp = Proxy