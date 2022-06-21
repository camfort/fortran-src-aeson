-- | Aeson instances for definitions used for representing Fortran literals.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Fortran.Extra.JSON.Literals() where

import Language.Fortran.Extra.JSON.Helpers
import Language.Fortran.Extra.JSON.Supporting()
import Data.Aeson
import Language.Fortran.AST.Literal
import Language.Fortran.AST.Literal.Boz qualified as Boz
import Language.Fortran.AST.Literal.Boz
import Language.Fortran.AST.Literal.Real qualified as Real
import Language.Fortran.AST.Literal.Real
import Language.Fortran.AST.Literal.Complex

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
