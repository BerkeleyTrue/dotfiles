module Berks.Layouts.Accordion
  ( accordion
  , horizAccordion
  ) where

import XMonad.Layout as L (Mirror(..))
import XMonad.Layout.Accordion as Acc (Accordion(..))
import qualified XMonad.Layout.LayoutModifier as LM
import qualified XMonad.Layout.Renamed as RN

accordion :: LM.ModifiedLayout RN.Rename Accordion a
accordion = RN.renamed [RN.Replace "Acc"] Accordion

horizAccordion :: LM.ModifiedLayout RN.Rename (Mirror Accordion) a
horizAccordion = RN.renamed [RN.Replace "HorizAcc"] $ Mirror Accordion
