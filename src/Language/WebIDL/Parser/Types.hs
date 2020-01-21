module Language.WebIDL.Parser.Types
    ( Parser
    , HParser
    )
where

import           Text.Megaparsec
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Data.Kind                      ( Type )

type Parser = Parsec Void Text

type HParser (k :: Type -> Type) = Parser (k SourcePos)
