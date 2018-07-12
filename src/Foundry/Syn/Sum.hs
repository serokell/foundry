module Foundry.Syn.Sum where

import Data.Kind (Type)
import Control.Lens
import Data.Foldable

import Source.Syntax
import Foundry.Syn.Common

data SynAdd s1 s2 = SynAugend s1 | SynAddend s2
  deriving (Eq, Ord, Show)

makePrisms ''SynAdd

instance (SynSelfSelected s1, SynSelfSelected s2)
      => SynSelfSelected (SynAdd s1 s2) where
  synSelfSelected = \case
    SynAugend s -> synSelfSelected s
    SynAddend s -> synSelfSelected s

instance (UndoEq s1, UndoEq s2)
      => UndoEq (SynAdd s1 s2) where
  undoEq (SynAugend s1) (SynAugend s2) = undoEq s1 s2
  undoEq (SynAddend s1) (SynAddend s2) = undoEq s1 s2
  undoEq  _              _             = False

instance (SyntaxLayout la lctx s1, SyntaxLayout la lctx s2)
      => SyntaxLayout la lctx (SynAdd s1 s2) where
  layout = \case
    SynAugend s -> layout s
    SynAddend s -> layout s

instance (SyntaxReact rp la s1, SyntaxReact rp la s2)
      => SyntaxReact rp la (SynAdd s1 s2) where
  react = asum [reactRedirect _SynAugend, reactRedirect _SynAddend]
  subreact = asum [subreactRedirect _SynAugend, subreactRedirect _SynAddend]

type family SynSum (s :: [Type]) :: Type where
  SynSum '[s] = s
  SynSum (s ': ss) = SynAdd s (SynSum ss)
