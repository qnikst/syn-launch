-- | Module: Timed
--   License: BSD-3
--   Author:  Alexander Vershilov
--   portability: unportable
--
-- Provide a simple timed launch like 'at' utility
module Timed
  ( timedLaunch )
  where

import Control.Exception
import Data.Time
import Data.Function
import System.Locale
import System.Time.Monotonic

-- | launch command at a given time
-- TODO: check days boundary
timedLaunch :: String -> IO ()
timedLaunch t = parseT supportedFormats t >>= \t' -> currentUTC >>= delay . (on (-) utctDayTime t')
  where currentUTC = getZonedTime >>= \z -> return . zonedTimeToUTC $! z{zonedTimeZone=utc}

parse' :: String -> String -> IO (Either SomeException UTCTime)
parse'  f s = try . evaluate $ (readTime defaultTimeLocale f s ::UTCTime)
  
parseT :: [String] -> String -> IO UTCTime
parseT [] _ = error "cannot parse time"
parseT (f:fs) s = parse' f s >>= either (const $ parseT fs s) return

supportedFormats :: [String]
supportedFormats = [ "%R" -- "%H:%M"
                   , "%T" -- "%H:%M:%S"
                   , "%H%M%S"
                   ] 

