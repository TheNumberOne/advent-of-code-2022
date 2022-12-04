module Main (main) where

import Control.Exception (throw)
import Main.Utf8 qualified as Utf8
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

data Assignment = Range Int Int
type AssignmentPair = (Assignment, Assignment)

type Parser = Parsec Void Text

parseAssignment :: Parser Assignment
parseAssignment = do
  lower <- L.decimal
  void $ char '-'
  Range lower <$> L.decimal

parseAssignmentPair :: Parser AssignmentPair
parseAssignmentPair = do
  a1 <- parseAssignment
  void $ char ','
  a2 <- parseAssignment
  return (a1, a2)

isPairContainedBy :: Assignment -> Assignment -> Bool
isPairContainedBy (Range l1 h1) (Range l2 h2) = l1 <= l2 && h2 <= h1

pairOverlapAtAll :: AssignmentPair -> Bool
pairOverlapAtAll (Range l1 h1, Range l2 h2) = l1 <= h2 && h1 >= l2

arePairsFullyContained :: AssignmentPair -> Bool
arePairsFullyContained (as, as') = isPairContainedBy as as' || isPairContainedBy as' as

countFullyContainedPairs :: [AssignmentPair] -> Int
countFullyContainedPairs = length . filter arePairsFullyContained

countOverlappingPairs :: [AssignmentPair] -> Int
countOverlappingPairs = length . filter pairOverlapAtAll

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    fileContents :: Text <- decodeUtf8 <$> readFileBS "inputs/input-4.txt"
    let parsedLines = case mapM (runParser parseAssignmentPair "") (lines fileContents) of
          Left e -> throw e
          Right l -> l
    let solution1 = countFullyContainedPairs parsedLines
    putTextLn $ show solution1

    -- part 2
    let solution2 = countOverlappingPairs parsedLines
    putTextLn $ show solution2
