module Main (main) where

import Main.Utf8 qualified as Utf8

data Move = Rock | Paper | Scissors
data Result = Lose | Tie | Win

scoreRound :: (Move, Move) -> Int
scoreRound (Rock, Rock) = 1 + 3
scoreRound (Rock, Paper) = 2 + 6
scoreRound (Rock, Scissors) = 3 + 0
scoreRound (Paper, Rock) = 1 + 0
scoreRound (Paper, Paper) = 2 + 3
scoreRound (Paper, Scissors) = 3 + 6
scoreRound (Scissors, Rock) = 1 + 6
scoreRound (Scissors, Paper) = 2 + 0
scoreRound (Scissors, Scissors) = 3 + 3

parseMoves :: [Text] -> (Move, Move)
parseMoves [m1, m2] = (parseMove m1, parseMove m2)
parseMoves _ = error "invalid line"

parseMove :: Text -> Move
parseMove "A" = Rock
parseMove "B" = Paper
parseMove "C" = Scissors
parseMove "X" = Rock
parseMove "Y" = Paper
parseMove "Z" = Scissors
parseMove _ = error "invalid move"

parseMoves' :: [Text] -> (Move, Result)
parseMoves' [m, g] = (parseMove m, parseGoal g)
parseMoves' _ = error "invalid line"

parseGoal :: Text -> Result
parseGoal "X" = Lose
parseGoal "Y" = Tie
parseGoal "Z" = Win
parseGoal _ = error "Invalid goal"

scoreRound' :: (Move, Result) -> Int
scoreRound' (enemyMove, result) =
  let move = moveFor enemyMove result
   in scoreMove move + scoreResult result

moveFor :: Move -> Result -> Move
moveFor enemyMove Tie = enemyMove
moveFor Rock Lose = Scissors
moveFor Rock Win = Paper
moveFor Paper Lose = Rock
moveFor Paper Win = Scissors
moveFor Scissors Lose = Paper
moveFor Scissors Win = Rock

scoreResult :: Result -> Int
scoreResult Lose = 0
scoreResult Tie = 3
scoreResult Win = 6

scoreMove :: Move -> Int
scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissors = 3

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    fileContents :: Text <- decodeUtf8 <$> readFileBS "inputs/input-2.txt"
    let puzzleInput = words <$> lines fileContents
    let totalScore = sum $ scoreRound . parseMoves <$> puzzleInput
    putTextLn $ show totalScore

    -- part 2
    let totalScore' = sum $ scoreRound' . parseMoves' <$> puzzleInput
    putTextLn $ show totalScore'
