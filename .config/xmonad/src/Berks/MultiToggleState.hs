module Berks.MultiToggleState
  ( toggleRefIO,
    createToggleStateLogHook,
    ToggleData (..),
  )
where

import Data.IORef
  ( IORef,
    newIORef,
    writeIORef,
  )
import XMonad
  ( WindowSpace,
    X,
    io,
    withWindowSet,
  )
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
  ( StdTransformers
      ( FULL,
        NOBORDERS
      ),
  )
import XMonad.StackSet
  ( Screen (workspace),
    StackSet (current),
  )

data ToggleData = Ts
  { isFull :: Bool,
    isNoBorders :: Bool
  }
  deriving (Show)

toggleRefIO :: IO (IORef ToggleData)
toggleRefIO = newIORef Ts {isFull = False, isNoBorders = False}

isFullWindow :: WindowSpace -> X (Maybe Bool)
isFullWindow = isToggleActive FULL

isNoBordersWindow :: WindowSpace -> X (Maybe Bool)
isNoBordersWindow = isToggleActive NOBORDERS

-- | Store current toggle state in an IORef
createToggleStateLogHook :: IORef ToggleData -> X ()
createToggleStateLogHook toggleRef =
  withWindowSet
    ( \stackSet -> do
        let ws = workspace $ current stackSet
        isNoBorders' <- isNoBordersWindow ws
        isFull' <- isFullWindow ws
        let toggleData' =
              Ts
                { isFull = isFull' == Just True,
                  isNoBorders = isNoBorders' == Just True
                }
        -- io $ putStrLn $ "current toggle state: " ++ show toggleData'
        io $ writeIORef toggleRef toggleData'
    )
