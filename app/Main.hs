module Main where

import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Text
import Language.WebIDL.AST
import Language.WebIDL.Printer

doc :: Fragment
doc = Fragment
  [ Typedef ULongT (Ident "size"),
    Interface (Ident "Document") Nothing [
      IAttribute (Attribute { type' = ULongT, ident = Ident "size", readonly = False })
    ]
  ]

main :: IO ()
main = do
  putDoc (pretty doc)
