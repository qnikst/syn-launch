module Execute
  ( SynCmd(..)
  , exec
  , fromList 
  )
  where

import System.Posix.Process

-- | Command wrapper
data SynCmd = SynCmd FilePath [String] deriving (Eq, Show)

fromList :: [String] -> SynCmd
fromList (x:xs) = SynCmd x xs

{-# INLINE exec #-}
-- | execute wrapper
exec :: SynCmd -> IO ()
exec (SynCmd cmd args) = executeFile cmd True args Nothing

