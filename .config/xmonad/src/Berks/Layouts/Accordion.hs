module Berks.Layouts.Accordion
  ( accordion,
    horizAccordion,
  )
where

import XMonad.Layout
import XMonad.Layout.Accordion
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Renamed

accordion :: ModifiedLayout Rename Accordion a
accordion = renamed [Replace "Acc"] Accordion

horizAccordion :: ModifiedLayout Rename (Mirror Accordion) a
horizAccordion = renamed [Replace "HorizAcc"] $ Mirror Accordion
