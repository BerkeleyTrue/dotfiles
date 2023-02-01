module Berks.Information.FSMonitor
  ( showFSInfo,
  )
where

import System.Process

showFSInfo :: [String] -> IO String
showFSInfo fsList =
  head -- take only the first item
    . take 1 -- take the first line
    . map (last . take 3 . reverse . words) -- get the GB of free space
    . drop 1 -- drop the headers
    . lines -- split the output into lines
    <$> readProcess "df" ("-kPH" : fsList) ""
