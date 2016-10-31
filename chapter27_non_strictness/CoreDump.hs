module CoreDump where

-- Try loading this with
--  :set -ddump-simpl
-- and then
--  :set -dsuppress-all

discriminatory :: Bool -> Int
discriminatory b =
  case b of
    False -> 0
    True -> 1
