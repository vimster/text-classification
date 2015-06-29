module Main where

import           Control.Applicative
import           Data.Array
import           Data.List               (maximumBy)
import           Data.List.Extras.Argmax
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Ord
import           NLP.Tokenize.String     (tokenize)
import           System.Directory        (getCurrentDirectory,
                                          getDirectoryContents)
import           System.FilePath
import           System.IO               ()


------------------------------------------------------------------------
--  References
------------------------------------------------------------------------

------------------------------------------------------------------------
--  types
------------------------------------------------------------------------
type Word = String
type Pr = Double
type Document = [Word]
type Frequencies = M.Map (Word, Word) Integer
-- type TagTransitionPr = M.Map (Tag, Tag) Pr
-- type WordLikelihoodPr = M.Map (Word, Tag) Pr


------------------------------------------------------------------------
--  Bayes Model
------------------------------------------------------------------------
data Bayes = Bayes [Word] deriving(Show)

------------------------------------------------------------------------
--  IO
------------------------------------------------------------------------

isRegularFile :: FilePath -> Bool
isRegularFile f = f /= "." && f /= ".." && takeExtension f == ".xml"

-- | read dir
readDir :: String -> IO [FilePath]
readDir path = do
  directory <- getCurrentDirectory
  filter isRegularFile <$> getDirectoryContents (directory ++ "/" ++ path)

readFile :: String -> [Word]
readFile source = tokenize source


------------------------------------------------------------------------
--  Train
--  Calculating parameters of Naive Bayes Model
------------------------------------------------------------------------

train :: [Document] -> Bayes
train taggedSentences = undefined

findPr :: (Fractional v, Ord k) => k -> M.Map k v -> v
findPr = M.findWithDefault 0.00001


------------------------------------------------------------------------
--  Evaluation
------------------------------------------------------------------------

precision :: [Document] -> Bayes -> Pr
precision testDocuments bayes = undefined -- truePositiveCount / fromIntegral (length result)
  -- where
  --   sentences = map (map fst) testSentences
  --   expectedTags = filter (/= "BOS") $ concatMap (map snd) testSentences
  --   bestTagSequences = concatMap (viterbi hmm) sentences
  --   result = zipWith (==) expectedTags bestTagSequences
  --   truePositiveCount = fromIntegral $ length $ filter (==True) result

readStopwords :: IO [Word]
readStopwords = lines <$> readFile "stop-words-list.txt"

------------------------------------------------------------------------
--  main
------------------------------------------------------------------------
main :: IO ()
main = do
  let trainingPath = "corpus/training"
  let testPath = "corpus/test"
  files <- readDir corpusPath
  filePaths <- map (corpusPath++) files
  contents <- mapM readFile filePaths
  putStrLn "Calculating..."
  let modelDocuments = concatMap parseXml model
      testModelDocuments = concatMap parseXml test
      bayesModel = train modelDocuments
  putStr "Precision: "
  -- print $ precision testModelSentences hiddenMarkovModel


