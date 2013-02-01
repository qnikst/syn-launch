{-# LANGUAGE Arrows #-}
-- | Module: Main
--   Author: Alexander Vershilov <alexander.vershilov@gmail.com>
--   License: LGPL-3
--   Copyright: ARCCN
--
module Main where

import Control.Applicative
import Control.Monad (unless, void)

import Options.Applicative
import Options.Applicative.Arrows

import Execute
import Timed
import UdpLaunch 

-- | Options
data SynLaunch = SynLaunch
        { synCmd :: [String]
        , synType :: Maybe SynType
        }
        deriving (Show)

data SynType = SynUdp Int
             | SynAt  String
             | SynEnter
             deriving (Show)


-- | Option parser
synLaunch :: Parser SynLaunch 
synLaunch = runA $ proc () -> do
  sat <- asA (optional $ strOption (short 'a' 
             <> long "at"
             <> metavar "TIME")) -< ()
  udp  <- asA (optional $ option (short 'u' 
                                 <> long "udp"
                                 <> metavar "PORT")) -< ()
  args <- asA (arguments str idm) -< ()
  returnA -< SynLaunch args $   (SynAt <$> sat) 
                            <|> (SynUdp <$> udp)
                            <|> (Just SynEnter)


main :: IO ()
main = execParser opts >>= \syn -> do
    case synType syn of
      Nothing -> error "you must specify synchronization type"
      Just (SynAt s)  -> waitTime s
      Just (SynUdp p) -> waitUdp p
      Just (SynEnter) -> void getLine 
    unless ([] == synCmd syn) (exec . fromList $! (synCmd syn))
  where 
    opts = info (helper <*> synLaunch)
              ( fullDesc
              <> progDesc "synchronized program launch"
              <> header "synchronized program launch"
              )

