{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Fortran.Extra.JSON where

import Language.Fortran.Extra.JSON.Position()

import Language.Fortran.Version
import Language.Fortran.AST.Literal
import Language.Fortran.AST.Literal.Boz qualified as Boz
import Language.Fortran.AST.Literal.Boz
import Language.Fortran.AST.Literal.Real qualified as Real
import Language.Fortran.AST.Literal.Real
import Language.Fortran.AST.Literal.Complex
import Language.Fortran.AST
import Data.Aeson hiding ( Value )
import Data.Aeson qualified as Aeson

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
jcEnumDrop x = jcEnum (drop (length x))

-- | Base Aeson config for non-record product types (1 cons, no named fields).
jc :: Aeson.Options
jc = jcProd id

-- shortcut
gtj :: (Generic a, Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)) => Aeson.Options -> a -> Aeson.Value
gtj = genericToJSON

instance (ToJSON (t a), ToJSON a) => ToJSON (AList t a) where toJSON = gtj $ jc
instance (ToJSON a, ToJSON (t1 a), ToJSON (t2 a)) => ToJSON (ATuple t1 t2 a)

instance ToJSON a => ToJSON (KindParam a) where toJSON = gtj $ jcSumDrop "KindParam"

-- TODO override to reparse/print?
instance ToJSON Boz.Conforming where toJSON = gtj $ jcEnum id
instance ToJSON BozPrefix where toJSON = gtj $ jcEnumDrop "BozPrefix"
instance ToJSON Boz where toJSON = gtj $ jcProd $ drop $ length "boz"

-- TODO override to reparse/print?
instance ToJSON Real.ExponentLetter where toJSON = gtj $ jcEnumDrop "ExpLetter"
instance ToJSON Real.Exponent where toJSON = gtj $ jcProdDrop "exponent"
instance ToJSON RealLit where toJSON = gtj $ jcProdDrop "realLit"

-- TODO override to reparse/print?
instance ToJSON a => ToJSON (ComplexPart a) where toJSON = gtj $ jcSumDrop "ComplexPart"
instance ToJSON a => ToJSON (ComplexLit a) where toJSON = gtj $ jcProdDrop "complexLit"

instance ToJSON a => ToJSON (Value a) where toJSON = gtj $ jcSumDrop "Val"

instance ToJSON UnaryOp where toJSON = gtj $ jcSumDrop mempty
instance ToJSON BinaryOp where toJSON = gtj $ jcSumDrop mempty

instance ToJSON a => ToJSON (Selector a)
instance ToJSON a => ToJSON (TypeSpec a)

instance ToJSON a => ToJSON (DimensionDeclarator a)
instance ToJSON a => ToJSON (DeclaratorType a)
instance ToJSON a => ToJSON (Declarator a)

instance ToJSON a => ToJSON (Suffix a)
instance ToJSON a => ToJSON (Attribute a)

instance ToJSON a => ToJSON (StructureItem a)
instance ToJSON a => ToJSON (UnionMap a)

-- standalone
instance ToJSON BaseType where toJSON = gtj $ jcSumDrop "Type"
instance ToJSON Intent where toJSON = gtj $ jcEnumDrop mempty
deriving via String instance ToJSON a => ToJSON (Comment a)
instance ToJSON ModuleNature where toJSON = gtj $ jcEnumDrop "Mod"
instance ToJSON Only where toJSON = gtj $ jcEnumDrop mempty

-- standalone apart from a, SrcSpan
instance ToJSON a => ToJSON (Prefix a) where toJSON = gtj $ jcSumDrop "Pfx"

instance ToJSON a => ToJSON (DataGroup a)
instance ToJSON a => ToJSON (Namelist a)
instance ToJSON a => ToJSON (CommonGroup a)
instance ToJSON a => ToJSON (FormatItem a)

instance ToJSON a => ToJSON (ImpElement a)
instance ToJSON a => ToJSON (ImpList a)

-- random
instance ToJSON a => ToJSON (ControlPair a)
instance ToJSON a => ToJSON (FlushSpec a)
instance ToJSON a => ToJSON (AllocOpt a)
instance ToJSON a => ToJSON (Use a)
instance ToJSON a => ToJSON (ProcInterface a)
instance ToJSON a => ToJSON (ProcDecl a)

instance ToJSON a => ToJSON (Statement a)
instance ToJSON a => ToJSON (DoSpecification a)

instance ToJSON a => ToJSON (Expression a)
instance ToJSON a => ToJSON (Index a)
instance ToJSON a => ToJSON (Argument a)
instance ToJSON a => ToJSON (ArgumentExpression a)

instance ToJSON a => ToJSON (ForallHeader a)

instance ToJSON a => ToJSON (ProgramUnit a)

instance ToJSON a => ToJSON (Block a)

instance ToJSON FortranVersion where toJSON = gtj $ jcEnumDrop mempty
instance ToJSON MetaInfo where toJSON = gtj $ jcSumDrop "mi"

instance ToJSON a => ToJSON (ProgramFile a) where toJSON = gtj $ jcProdDrop "programFile"
