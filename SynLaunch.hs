{-# LANGUAGE Arrows #-}

module Main where


import Options.Applicative
import Options.Applicative.Arrows

import Execute
import Timed

data SynLaunch = SynLaunch
        { synCmd :: SynCmd
        , synAt  :: Maybe String
        }
        deriving (Show)

synLaunch :: Parser SynLaunch 
synLaunch = runA $ proc () -> do
  syn  <- asA (optional $ strOption (short 'a' 
                                    <> long "at"
                                    <> metavar "TIME")) -< ()
  args <- asA (arguments str idm) -< ()
  returnA -< SynLaunch (fromList args) syn


main :: IO ()
main = execParser opts >>= \syn -> do
    case synAt syn of
      Nothing -> print syn
      Just  a -> timedLaunch a (synCmd syn)
  where 
    opts = info (helper <*> synLaunch)
              ( fullDesc
              <> progDesc "synchronized program launch"
              <> header "synchronized program launch"
              )

