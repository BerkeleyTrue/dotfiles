module Berks.Trayer
  ( startTrayer,
  )
where

import XMonad hiding (kill)

trayer :: String
trayer = "trayer"

-- | Kill a process by name using the bash command 'killall'
-- and suppress the output of the command.
kill :: MonadIO m => String -> m ()
kill prog = spawn $ "killall " ++ prog ++ " &> /dev/null"

-- Process the list passed into startTrayer,
-- pairing, then concating the key with "--" and then returning a flat list
processArgs :: [String] -> [String]
processArgs [] = []
processArgs [_] =
  error "Expected an even number of arguments but found a odd number"
processArgs (x : y : xs) = ("--" ++ x) : y : processArgs xs

spawnTrayer :: MonadIO m => [String] -> m ()
spawnTrayer args =
  spawn $
    unwords $
      ["sleep", "1", "&&", trayer]
        ++ (processArgs args ++ ["-l", "&"])

-- | Start trayer with the given arguments
-- Arguments are past in as a flat list of key value,
-- e.g. ["width", "10", "height", "10"]
-- The key is prefixed with "--", then passed to trayer through safeSpawn
-- This function will also kill any existing trayer processes
-- and wait 2 second before starting trayer
startTrayer :: MonadIO m => [String] -> m ()
startTrayer args = do
  -- _ <- spawn $ "echo '\n trayer args: " ++ unwords (processArgs args) ++ " '"
  kill "trayer" >> spawnTrayer args
