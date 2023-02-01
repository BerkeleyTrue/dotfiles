-- Sourced from: https://codeberg.org/xmobar/xmobar/src/branch/master/src/Xmobar/Plugins/Monitors/MultiCoreTemp.hs
module Berks.Information.MultiCoreTemp
  ( getMultiCoreTemps,
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (filterM)
import Data.Char (isDigit)
import Data.Functor
import Data.List (isPrefixOf)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )

-- | Given a prefix, suffix, and path string, return true if the path string
-- format is prefix ++ numeric ++ suffix.
numberedPathMatcher :: String -> String -> String -> Bool
numberedPathMatcher prefix suffix path =
  prefix `isPrefixOf` path && not (null digits) && afterDigits == suffix
  where
    afterPrefix = drop (length prefix) path
    digits = takeWhile isDigit afterPrefix
    afterDigits = dropWhile isDigit afterPrefix

-- | Returns all paths in dir matching the predicate.
getMatchingPathsInDir :: FilePath -> (String -> Bool) -> IO [FilePath]
getMatchingPathsInDir dir f = do
  exists <- doesDirectoryExist dir
  if exists
    then do
      files <- filter f <$> listDirectory dir
      return $ fmap (\file -> dir ++ "/" ++ file) files
    else return []

-- | Returns the first coretemp.N path found.
coretempPath :: IO (Maybe String)
coretempPath = do
  ps <- getMatchingPathsInDir "/sys/bus/platform/devices" coretempMatcher
  xs <- filterM doesDirectoryExist ps
  return (if null xs then Nothing else Just $ head xs ++ "/")
  where
    coretempMatcher = numberedPathMatcher "coretemp." ""

hwmonPaths :: IO [String]
hwmonPaths = do
  p <- coretempPath
  let (sc, path) = case p of
        Just s -> (False, s)
        Nothing -> (True, "/sys/class/")
  cps <- getMatchingPathsInDir (path ++ "hwmon") hwmonMatcher
  ecps <- filterM doesDirectoryExist cps
  return $ if sc || null ecps then ecps else [head ecps]
  where
    hwmonMatcher = numberedPathMatcher "hwmon" ""

-- | Transform a path to Label to a path to core-temperature.
labelToCore :: FilePath -> FilePath
labelToCore = (++ "input") . reverse . drop 5 . reverse

-- | Checks Labels, if they refer to a core and returns Strings of core-
-- temperatures.
corePaths :: IO [String]
corePaths = do
  paths <- hwmonPaths
  cpaths <- concat <$> traverse (`getMatchingPathsInDir` corePathMatcher) paths
  filePaths <- filterM doesFileExist cpaths
  crePaths <- filterM isLabelFromCore filePaths
  return $ map labelToCore crePaths
  where
    corePathMatcher = numberedPathMatcher "temp" "_label"

-- | Checks if Label refers to a core.
isLabelFromCore :: FilePath -> IO Bool
isLabelFromCore p = do
  a <- readFile p
  return $ take 4 a `elem` ["Core", "Tdie", "Tctl"]

-- | Reads core-temperatures as data from the system.
coreDatas :: IO [Double]
coreDatas = do
  fps <- corePaths
  traverse readSingleFile fps
  where
    readSingleFile :: FilePath -> IO Double
    readSingleFile s = do
      a <- readFile s
      return $ parseContent a
      where
        parseContent :: String -> Double
        parseContent = read . head . lines

-- | Returns the average of all core-temperatures
getMultiCoreTemps :: IO Double
getMultiCoreTemps =
  coreDatas <&> (/ 1000) . liftA2 (/) sum (fromIntegral . length)
