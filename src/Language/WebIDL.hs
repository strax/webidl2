{-# LANGUAGE DeriveDataTypeable #-}

module Language.WebIDL
    ( parse
    , module X
    , SourcePos(..)
    , ParseError
    )
where

import           Language.WebIDL.AST           as X
import           Language.WebIDL.Parser         ( pFragment )
import           Text.Megaparsec                ( runParser
                                                , errorBundlePretty
                                                )
import qualified Text.Megaparsec
import           Text.Megaparsec.Pos            ( SourcePos(..) )
import           Data.Text                      ( Text )
import           Data.Void
import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )
import           Control.DeepSeq                ( NFData )

newtype ParseError = ParseError (Text.Megaparsec.ParseErrorBundle Text Void) deriving (Eq, Generic, Data, NFData)

instance Show ParseError where
    show (ParseError err) = errorBundlePretty err

parse :: String -> Text -> Either ParseError (Fragment SourcePos)
parse filename input = case (runParser pFragment filename input) of
    Left  err      -> Left (ParseError err)
    Right fragment -> Right fragment
