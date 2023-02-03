module Berks.HideLazyPointer
  ( lazyPointerIO,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Exit
import System.Posix.Types (Fd (..))

-- A version of maskEvent that does not block in foreign calls.
maskEvent' :: Display -> EventMask -> XEventPtr -> IO ()
maskEvent' d m p = do
  pend <- pending d
  if pend /= 0
    then maskEvent d m p
    else do
      threadWaitRead (Fd fd)
      maskEvent' d m p
  where
    fd = connectionNumber d

-- creates an invisible cursor
nullCursor :: Display -> Window -> IO Cursor
nullCursor d w = do
  let c = Color 0 0 0 0 0
  p <- createPixmap d w 1 1 1
  cursor <- createPixmapCursor d p p c c 0 0
  freePixmap d p
  return cursor

waitASecond :: Int -> IO ()
waitASecond i = threadDelay (i * 1000000)

-- hides and grabs the pointer till the user moves it
hidePointer :: Display -> Window -> IO ()
hidePointer d w = do
  let em = buttonPressMask .|. pointerMotionMask
  cursor <- nullCursor d w
  ps <-
    grabPointer
      d
      w
      False
      em
      grabModeAsync
      grabModeAsync
      w
      cursor
      currentTime
  when (ps /= grabSuccess) $ do
    waitASecond 1
    hidePointer d w
  allocaXEvent $ \e -> do
    maskEvent d em e
    ungrabPointer d currentTime
    waitForMotion d w

-- when the pointer is not moved a timer starts: after ten seconds, if
-- no motion interrupts the timer, the pointer is grabbed and made
-- invisible.
waitForMotion :: Display -> Window -> IO ()
waitForMotion d w = do
  mt <- myThreadId
  t <- forkIO (timer mt)
  mask $ \restore -> go restore t
  where
    -- interrupt the waiting for motion (and thus hide the pointer)
    timer t = do
      waitASecond 10
      throwTo t (ErrorCall "done")
    -- wait for the next motion, and restart the timer (?)
    stopAndWait t = do
      allocaXEvent $ maskEvent' d pointerMotionMask
      -- this seems to just suspend the timer...
      throwTo t ExitSuccess
      waitForMotion d w
    -- wait for a timer interrupt to hide the pointer
    go restore t = do
      catch
        (restore $ stopAndWait t)
        (const $ hidePointer d w :: ErrorCall -> IO ())

lazyPointerIO :: IO ()
lazyPointerIO = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw <- rootWindow dpy dflt
  waitForMotion dpy rootw
