module Berks.XMobar.Conf
  ( getXMobarRC
  , getDefaultXMobarRC
  ) where

-- -----------------------------------------------------------------------------
-- it checks whether there exists a file called '$XDG_CONF_HOME/xmobar/.xmobarrc'
-- and, if not, creates one using the default configuration.
-- -----------------------------------------------------------------------------
import System.Directory
import System.FilePath.Posix
import System.Posix.Files (getFileStatus, modificationTime)
import System.Posix.Types (EpochTime)

import Control.Monad (when)
import Data.List (intercalate)

-- | Locations of files on the syste
xmobarRC :: IO FilePath -- Something like "/home/laumann/.xmobarrc
xmobarRC = do
  home <- getHomeDirectory
  return $ home </> ".xmobarrc"

xmobarhs :: IO FilePath -- Something like "/home/laumann/.xmonad/lib/XMobar.hs"
xmobarhs = do
  home <- getHomeDirectory
  return $ foldl (</>) home [".xmonad", "lib", "XMobar.hs"]

-- | File manipulation and inspection (specifically whether one file
-- is newer than another)
-- Is true if f1 is newer than f2
isFileNewer :: FilePath -> FilePath -> IO Bool
isFileNewer f1 f2 = do
  mod1 <- getModTime f1
  mod2 <- getModTime f2
  return $ mod1 > mod2

getModTime :: FilePath -> IO EpochTime
getModTime f = do
  s <- getFileStatus f
  return $ modificationTime s

-- | Return a string representation of an xmobar configuration that can be written to a file.
printConfig conf = putStr $ show conf

writeConfig :: FilePath -> Config -> IO ()
writeConfig f conf = writeFile f $ show conf

getXMobarRC :: Config -> IO FilePath
getXMobarRC conf = do
  f <- xmobarRC
  exists <- doesFileExist f
  if exists
    then do
      xmobarHS <- xmobarhs
      new <- isFileNewer xmobarHS f
      when new $ writeConfig f conf
    else do
      writeConfig f conf
  return f

-- | XMobar configuration definition in Haskell.
-- This section deals with the representation of XMobar
-- configurations, there's a plethora of ways to represent these I
-- guess. The defaultXMobarRC provided is my configuration.
newtype Config =
  Config [Option]

data Option
  = Opt String String
  | OptEnum String String
  | OptList String [String]

instance Show Config where
  show (Config options) = concat ["Config { ", fmt options, "\n       }\n"]
    where
      fmt options = intercalate "\n       , " $ map show options

instance Show Option where
  show (Opt key value) = concat [key, " = ", show value]
  show (OptEnum key value) = concat [key, " = ", value]
  show (OptList key values) = concat [key, " = ", fmt (length key + 12) values]
    where
      fmt indent values = concat ["[ ", intercalate sep values, end]
        where
          space = replicate indent ' '
          sep = concat ["\n", space, ", "]
          end = concat ["\n", space, "]"]

defaultXMobarRC :: Config
defaultXMobarRC =
  Config
    [ Opt "font" "-*-Fixed-Bold-R-Normal-*-12-*-*-*-*-*-*-*"
    , Opt "bgColor" "black"
    , Opt "fgColor" "grey"
    , OptEnum "position" "Top L 90"
    , OptEnum "lowerOnStart" $ show True
    , OptList
        "commands"
        [ "Run Swap [] 10"
        , "Run Memory [\"-t\", \"Mem: <usedratio>%\" 10]"
        , "Run Date \"%a %b %_d %l:%M\" \"date\" 10"
        , "Run Battery [] 10"
        , "Run Wireless \"wlan0\" [] 10"
        , "Run StdinReader"
        ]
    , Opt "sepChar" "%"
    , Opt "alignSep" "}{"
    , Opt
        "template"
        "%StdinReader% }{ %wlan0wi% | %cpu% | %memory% * %swap% <fc=#ee9a00>%date%</fc> | %battery%"
    ]

getDefaultXMobarRC = getXMobarRC defaultXMobarRC
