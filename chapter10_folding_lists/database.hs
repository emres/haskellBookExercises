module Database where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [ DbDate (UTCTime
                        (fromGregorian 1911 5 1)
                        (secondsToDiffTime 34123))
              , DbNumber 9001
              , DbString "Hello, world!"
              , DbDate (UTCTime
                        (fromGregorian 1921 5 1)
                        (secondsToDiffTime 34123))
              ]

-- Write a function that filters for DbDate values
-- and returns a list of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr conditionalCons []

conditionalCons :: DatabaseItem -> [UTCTime] -> [UTCTime]
conditionalCons (DbDate t) ts = t:ts
conditionalCons _          ts =   ts

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = [ x | DbNumber x <- xs ]

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / fromIntegral (length $ filterDbNumber db)

-- see the following for alternative solutions:
-- http://stackoverflow.com/questions/38805482/how-to-get-only-a-particular-type-of-elements-from-a-list-in-haskell

-- isDbDate :: DatabaseItem -> Bool
-- isDbDate (DbDate _) = True
-- isDbDate _ = False

-- getDbDate1 :: DatabaseItem -> UTCTime
-- getDbDate1 (DbDate utcTime) = utcTime

-- getDbDate2 :: DatabaseItem -> Maybe UTCTime
-- getDbDate2 (DbDate utcTime) = Just utcTime
-- getDbDate2 _ = Nothing

-- -- filterDbDate2 :: [DatabaseItem] -> [UTCTime]
-- -- filterDbDate2 database = foldr ((:) . getDbDate2) [] database

-- --getDbDate3 :: DatabaseItem -> [UTCTime]
-- getDbDate3 item =
--   case item of
--       DbDate utcTime -> [utcTime]
--       _ -> []

-- --filterDbDate3 :: [DatabaseItem] -> [UTCTime]


-- --catMaybes :: [Maybe a] -> [a]

-- filterDbDate9 :: [DatabaseItem] -> [UTCTime]
-- filterDbDate9 db = foldl filterDbDate'' [] db
--    where filterDbDate'' :: [UTCTime] -> DatabaseItem -> [UTCTime]
--          filterDbDate'' rest (DbDate utcTime) = (rest ++ [utcTime])
--          filterDbDate'' rest _                = rest

