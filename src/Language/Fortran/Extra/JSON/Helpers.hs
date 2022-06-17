{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for defining Aeson instances for fortran-src types.

module Language.Fortran.Extra.JSON.Helpers where

import Data.Aeson hiding ( Value )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Language.Fortran.Util.Position ( SrcSpan )
import Data.Text ( Text )

import GHC.Generics ( Generic, Rep )

-- | Base Aeson generic deriver config for product types.
jcProd :: (String -> String) -> Aeson.Options
jcProd f = Aeson.defaultOptions
  { Aeson.rejectUnknownFields = True
  , Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . f
  }

jcProdDrop :: String -> Aeson.Options
jcProdDrop x = jcProd (drop (length x))

-- | Base Aeson generic deriver config for sum types.
jcSum :: (String -> String) -> String -> String -> Aeson.Options
jcSum f tag contents = Aeson.defaultOptions
  { Aeson.rejectUnknownFields = True
  , Aeson.constructorTagModifier = Aeson.camelTo2 '_' . f
  , Aeson.sumEncoding = Aeson.TaggedObject
    { Aeson.tagFieldName = tag
    , Aeson.contentsFieldName = contents
    }
  }

jcSumDrop :: String -> Aeson.Options
jcSumDrop x = jcSum (drop (length x)) "tag" "contents"

-- | Base Aeson generic deriver config for enum types (no fields in any cons).
jcEnum :: (String -> String) -> Aeson.Options
jcEnum f = Aeson.defaultOptions
  { Aeson.rejectUnknownFields = True
  , Aeson.constructorTagModifier = Aeson.camelTo2 '_' . f
  }

jcEnumDrop :: String -> Aeson.Options
jcEnumDrop = jcEnum . drop . length

-- | Base Aeson config for non-record product types (1 cons, no named fields).
jc :: Aeson.Options
jc = jcProd id

-- | Shortcut for common function 'genericToJSON'
gtj :: (Generic a, Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)) => Aeson.Options -> a -> Aeson.Value
gtj = genericToJSON

-- | Shortcut for common function 'genericToEncoding'
gte :: (Generic a, Aeson.GToJSON' Aeson.Encoding Aeson.Zero (Rep a)) => Aeson.Options -> a -> Aeson.Encoding
gte = genericToEncoding

-- TODO weird context due to dependency awkwardness
-- safe, clean version: doesn't merge keys
toJSONAnno
    :: (ToJSON a, ToJSON SrcSpan)
    => Text -> a -> SrcSpan -> [Aeson.Pair] -> Aeson.Value
toJSONAnno t a ss m = object
  [ "anno"     .= a
  , "span"     .= ss
  , "tag"      .= t
  , "contents" .= object m ]

-- TODO weird context due to dependency awkwardness
-- concise version: merge keys
toJSONAnno'
    :: (ToJSON a, ToJSON SrcSpan)
    => Text -> a -> SrcSpan -> [Aeson.Pair] -> Aeson.Value
toJSONAnno' t a ss m = object $
  [ "anno"     .= a
  , "span"     .= ss
  , "tag"      .= t ] <> m

tja :: (ToJSON a, ToJSON SrcSpan)
    => Text -> a -> SrcSpan -> [Aeson.Pair] -> Aeson.Value
tja = toJSONAnno'
