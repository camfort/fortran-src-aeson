{-# LANGUAGE OverloadedStrings #-}

module Language.Fortran.Extra.JSON.Position where

import Language.Fortran.Util.Position

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Void ( Void )
import Data.Text ( Text )
import Data.Text qualified as Text
import Data.Functor ( void )

import Data.Aeson

type Parser = Parsec Void Text

pPosition :: Parser Position
pPosition = do
    posLine   <- L.decimal
    void $ char ':'
    posColumn <- L.decimal
    return initPosition { posLine = posLine, posColumn = posColumn }

-- TODO better error reporting
instance ToJSON   Position where toJSON = String . tshow
instance FromJSON Position where
    parseJSON  = withText "position" $ \t ->
        case parseMaybe pPosition t of
          Nothing  -> fail "failed to parse position"
          Just pos -> pure pos

pSrcSpan :: Parser SrcSpan
pSrcSpan = do
    void $ char '('
    posFrom <- pPosition
    void $ string ")-("
    void $ char ')'
    posTo   <- pPosition
    return $ SrcSpan posFrom posTo

-- TODO better error reporting
instance ToJSON   SrcSpan where toJSON = String . tshow
instance FromJSON SrcSpan where
    parseJSON  = withText "SrcSpan" $ \t ->
        case parseMaybe pSrcSpan t of
          Nothing -> fail "failed to parse SrcSpan"
          Just ss -> pure ss

--------------------------------------------------------------------------------

tshow :: Show a => a -> Text
tshow = Text.pack . show
