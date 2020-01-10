{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.WebIDL.Printer where

import Data.Text.Prettyprint.Doc
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
  pretty ULongLongT = "unsigned long long"
  pretty _ = "not implemented"

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

instance Pretty InterfaceMember where
  pretty (IAttribute attr) = pretty attr

instance Pretty Attribute where
  pretty Attribute{..} = "attribute" <+> pretty type' <+> pretty ident <> semi
