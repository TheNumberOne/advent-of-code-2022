module Main (main) where

import Data.List (intersect)
import Data.Set (elemAt, intersection)
import Data.Text qualified as T (length)
import GHC.List (foldl1')
import Main.Utf8 qualified as Utf8

takeGroups :: Int -> [a] -> [[a]]
takeGroups _ [] = []
takeGroups n l =
  let (left, right) = splitAt n l
   in left : takeGroups n right

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    fileContents :: Text <- decodeUtf8 <$> readFileBS "inputs/input-3.txt"
    let rucksacks = lines fileContents
    let sumOfPriorities = sum $ priority . sharedItem <$> rucksacks
    putTextLn $ show sumOfPriorities

    -- part 2
    let groups = takeGroups 3 $ toString <$> rucksacks
    let badges = mapM (!!? 0) (foldl1' intersect <$> groups) ?: error "invalid input"
    let sumOfBadgePriorities = sum $ priority <$> badges
    putTextLn $ show sumOfBadgePriorities

priority :: Char -> Int
priority c
  | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
  | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
  | otherwise = error "unexpected char"

sharedItem :: Text -> Char
sharedItem line =
  let (left, right) = splitAt (T.length line `div` 2) (toString line)
      leftSet :: Set Char = fromList left
      rightSet :: Set Char = fromList right
      common = intersection leftSet rightSet
   in elemAt 0 common
