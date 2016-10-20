module JSONTypes (
  JValue(..),
  mkJPair,
  mkJObj
  ) where

import Data.Map hiding ( map )

type JMap = Data.Map.Map String JValue
data JValue = JString String
            | JNumber Integer
            | JObject JMap
            | JArray [JValue]
            | JBool Bool
            | JNull 
    deriving (Show)

mkJPair :: String -> JValue -> JValue
mkJPair k v = JObject (Data.Map.singleton k v)

mkJObj :: [JValue] -> JValue
mkJObj j_vals = 
    let
        list_of_maps = map (\(JObject pair) -> pair) j_vals
        combined_map = Data.Map.unions list_of_maps
    in
        JObject combined_map

yin_yang :: Parser String
yin_yang =
  do  xs <- string "yin" <|> string "yang"
      return xs


-- json_parser :: Parser _hole1
-- json_parser = do
--         whiteSpace
--         j_top <- ( json_array_parser <|> json_obj_parser)
--         return j_top
