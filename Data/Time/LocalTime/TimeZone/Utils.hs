module Data.Time.LocalTime.TimeZone.Utils (findTimeZone) where

import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Monad ((>=>), ap)

timeZoneRoot = "/usr/share/zoneinfo"

-- | Find a time zone by name
findOlson :: String -> IO (Maybe OlsonData)
findOlson name = 
        do let p = timeZoneRoot </> name
           e <- doesFileExist p
           if e then getOlsonFromFile p >>= (return . Just)
                else return Nothing

findTimeZone :: String -> IO (Maybe TimeZone)
findTimeZone name =
        do olson <- findOlson name
           return $ case olson of
                Just o -> return tzsTimeZone `ap` olsonToTimeZoneSeries o
                Nothing -> Nothing

