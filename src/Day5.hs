module Main (main) where

import Control.Exception (throw)
import Data.Array.MArray (readArray, writeArray)
import Data.Array.ST (newListArray, runSTArray)
import GHC.Arr (elems)
import Main.Utf8 qualified as Utf8
import Text.Megaparsec (Parsec, anySingle, runParser, try)
import Text.Megaparsec.Char (char, digitChar, eol, hspace, hspace1, spaceChar, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void Text

type Crate = Char
type Crates = [[Crate]]

data Move = Move
  { count :: Int
  , from :: Int
  , to :: Int
  }
  deriving (Show)

data Input = Input
  { crates :: Crates
  , moves :: [Move]
  }
  deriving (Show)

parseCrate :: Parser Crate
parseCrate = do
  void $ char '['
  name <- anySingle
  void $ char ']'
  return name

parseCrateLine :: Parser [Maybe Crate]
parseCrateLine = some $ do
  crate <- dbg "crate" $ (Nothing <$ string "   ") <|> (Just <$> parseCrate)
  void . optional $ char ' '
  return crate

parseMove :: Parser Move
parseMove = do
  void $ string "move "
  count <- L.decimal
  void $ string " from "
  from <- L.decimal
  void $ string " to "
  to <- L.decimal
  void eol
  return Move {count, from, to}

fixCrates :: [[Maybe Crate]] -> [[Crate]]
fixCrates crates = catMaybes <$> transpose crates

parseIgnoredLines :: Parser ()
parseIgnoredLines = do
  void . dbg "ignored column numbers" . many $ hspace1 <|> void digitChar
  void eol
  void eol

parseInput :: Parser Input
parseInput = dbg "input" $ do
  crates <- dbg "stacks" . some . dbg "row" . try $ do
    line <- dbg "row crates" parseCrateLine
    void eol
    return line
  parseIgnoredLines
  moves <- dbg "moves" . some $ parseMove
  return Input {crates = fixCrates crates, moves}

simulate :: Input -> [[Crate]]
simulate input =
  let Input {crates, moves} = input
   in elems $ runSTArray $ do
        stacks <- newListArray (1, length crates) crates
        forM_ moves $ \Move {count, from, to} -> replicateM_ count $ do
          (top : rest) <- readArray stacks from
          writeArray stacks from rest
          toStack <- readArray stacks to
          writeArray stacks to (top : toStack)
        return stacks

simulate' :: Input -> [[Crate]]
simulate' input =
  let Input {crates, moves} = input
   in elems $ runSTArray $ do
        stacks <- newListArray (1, length crates) crates
        forM_ moves $ \Move {count, from, to} -> do
          fromStack <- readArray stacks from
          let (top, rest) = splitAt count fromStack
          writeArray stacks from rest
          toStack <- readArray stacks to
          writeArray stacks to (top ++ toStack)
        return stacks

topOfStacks :: [[Crate]] -> [Crate]
topOfStacks = map topOfStack
  where
    topOfStack [] = '0'
    topOfStack (h : _) = h

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    fileContents :: Text <- decodeUtf8 <$> readFileBS "inputs/input-5.txt"
    let input = case runParser parseInput "" fileContents of
          Left e -> throw e
          Right l -> l

    let solution1 = topOfStacks $ simulate input
    putTextLn $ show solution1

    let solution2 = topOfStacks $ simulate' input
    putTextLn $ show solution2

-- part 2
-- let solution2 = countOverlappingPairs parsedLines
-- putTextLn $ show solution2
