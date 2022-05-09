{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Fortran.Extra.JSON.Example where

import Language.Fortran.Extra.JSON
import Language.Fortran.Util.Position
import Language.Fortran.AST.Literal
import Language.Fortran.AST
import Language.Fortran.AST.Literal.Boz

import Data.Aeson
import GHC.Generics ( Generic )

exPos :: Position
exPos = initPosition

exSs :: SrcSpan
exSs = initSrcSpan

exKp :: KindParam A0
exKp = KindParamInt () exSs "kindparamvar"

exBt :: BaseType
exBt = TypeInteger

data T = T0 | T1 String | T2 { t1 :: String } deriving (Generic, Show, Eq)
instance ToJSON T where toJSON = genericToJSON $ jcSumDrop ""

boz :: Boz
boz = Boz BozPrefixB "" Conforming

data M1 = M1L M2 | M1R deriving (Generic, Show, Eq)
data M2 = M2L M1 | M2R deriving (Generic, Show, Eq)
instance ToJSON M1 where toJSON = genericToJSON $ jcSumDrop mempty
instance ToJSON M2 where toJSON = genericToJSON $ jcSumDrop mempty
