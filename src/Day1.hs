module Main (main) where

import Data.Foldable (maximum)
import Main.Utf8 qualified as Utf8

split :: Eq a => a -> [a] -> [[a]]
split separator = split' [] []
  where
    split' resultReversed [] (h : t)
      | h == separator = split' resultReversed [] t
    split' resultReversed beforeReversed (h : t)
      | h == separator = split' (reverse beforeReversed : resultReversed) [] t
      | otherwise = split' resultReversed (h : beforeReversed) t
    split' resultReversed [] [] = reverse resultReversed
    split' resultReversed beforeReversed [] = reverse (reverse beforeReversed : resultReversed)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    fileContents :: Text <- decodeUtf8 <$> readFileBS "inputs/input-1.txt"
    let inventoriesText = split "" $ lines fileContents
    let inventories = mapM (mapM $ readEither @Int . toString) inventoriesText
    inventories' <- case inventories of
      Left e -> fail $ toString e
      Right i -> pure i
    let calorieTotals = map sum inventories'
    let mostCalories = maximum calorieTotals
    putTextLn $ show mostCalories
    let top3 = take 3 $ sortBy (flip compare) calorieTotals
    putTextLn $ show $ sum top3
