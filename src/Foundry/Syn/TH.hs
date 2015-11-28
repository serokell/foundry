module Foundry.Syn.TH where

import qualified Language.Haskell.TH as TH
import qualified Control.Lens.Internal.FieldTH as TH
import qualified Control.Lens.Internal.PrismTH as TH
import qualified Control.Lens.TH as TH

makeLensesDataInst :: TH.Name -> TH.Name -> TH.DecsQ
makeLensesDataInst familyName instTypeName = do
  ds <- TH.reifyInstances familyName [TH.ConT instTypeName]
  fmap concat $ traverse (TH.makeFieldOpticsForDec TH.lensRules) ds

makePrismsDataInst :: TH.Name -> TH.Name -> TH.DecsQ
makePrismsDataInst familyName instTypeName = do
  ds <- TH.reifyInstances familyName [TH.ConT instTypeName]
  fmap concat $ traverse (TH.makeDecPrisms True) ds
