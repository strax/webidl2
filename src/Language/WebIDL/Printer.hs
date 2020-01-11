{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.WebIDL.Printer where

import Data.Text.Prettyprint.Doc
import Data.List (intersperse)
import Language.WebIDL.AST

instance Pretty Fragment where
  pretty (Fragment defns) = vsep $ pretty <$> defns

instance Pretty Type where
  pretty AnyT = "any"
  pretty VoidT = "void"
  pretty BooleanT = "boolean"
  pretty ByteT = "byte"
  pretty OctetT = "octet"
  pretty ShortT = "short"
  pretty UShortT = "unsigned short"
  pretty ULongT = "unsigned long"
  pretty LongT = "long"
  pretty LongLongT = "long long"
  pretty SymbolT = "symbol"
  pretty ULongLongT = "unsigned long long"
  pretty FloatT = "float"
  pretty UnrestrictedFloatT = "unrestricted float"
  pretty DoubleT = "double"
  pretty UnrestrictedDoubleT = "unrestricted double"
  pretty ObjectT = "object"
  pretty (NullableT ty) = pretty ty <> "?"
  pretty DOMStringT = "DOMString"
  pretty USVStringT = "USVString"
  pretty ByteStringT = "ByteString"
  pretty (InterfaceType ident) = pretty ident

joined :: [Doc a] -> Doc a
joined = mconcat . intersperse (comma <> space)

instance Pretty Ident where
  pretty (Ident name) = pretty name

instance Pretty Defn where
  pretty (Typedef ty ident) = "typedef" <+> pretty ty <+> pretty ident <> semi
  pretty (Interface ident base members) = "interface" <+> pretty ident <+> pAncestor <> braces pMembers <> semi
    where
      pAncestor = case base of
        Just base' -> colon <+> pretty base' <> space
        Nothing -> mempty
      pMembers = line' <> (vsep (indent 2 . pretty <$> members)) <> line'
  pretty (Namespace ident members) = "namespace" <+> pretty ident <+> braces pMembers <> semi
    where
      pMembers = line' <> (vsep (indent 2 . pretty <$> members)) <> line'

instance Pretty a => Pretty (Ann a) where
  pretty (Ann attrs a) = vsep [pretty attrs, pretty a]

instance Pretty ExtendedAttributeList where
  pretty (ExtendedAttributeList []) = mempty
  pretty (ExtendedAttributeList attrs) = brackets $ joined $ map pretty attrs

instance Pretty ExtendedAttribute where
  pretty (ExtendedAttributeNoArgs ident) = pretty ident
  pretty (ExtendedAttributeIdentList attr idents) = pretty attr <> equals <> parens (joined (map pretty idents))
  pretty (ExtendedAttributeNamedArgList name ident args) = pretty name <> equals <> pretty ident <> parens (pretty args)
  pretty (ExtendedAttributeArgList ident args) = pretty ident <> parens (pretty args)
  pretty (ExtendedAttributeIdent name ident) = pretty name <> equals <> pretty ident

instance Pretty ArgumentList where
  pretty (ArgumentList args) = parens $ mconcat $ intersperse (comma <> space) $ map pretty args

instance Pretty InterfaceMember where
  pretty (IAttribute attr) = pretty attr
  pretty (IOperation op) = pretty op

instance Pretty NamespaceMember where
  pretty (NSAttribute attr) = pretty attr
  pretty (NSOperation op) = pretty op

instance Pretty Operation where
  pretty (RegularOperation ty ident args) = group $ pretty ty <+> pretty ident <> pretty args <> semi

instance Pretty Argument where
  pretty (RegularArgument ty ident) = pretty ty <+> pretty ident
  pretty (OptionalArgument ty ident defaultValue) = "optional" <+> pretty ty <+> pretty ident <> prettyDefaultValue defaultValue
    where
      prettyDefaultValue (Just defaultValue) = space <> equals <> space <> pretty defaultValue
      prettyDefaultValue Nothing = mempty

instance Pretty ConstValue where
  pretty (BooleanLiteral True) = "true"
  pretty (BooleanLiteral False) = "false"
  pretty (ScientificLiteral n) = viaShow n
  pretty (IntegerLiteral n) = viaShow n

instance Pretty Attribute where
  pretty Attribute{..} = "attribute" <+> pretty type' <+> pretty ident <> semi
