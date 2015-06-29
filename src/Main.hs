module Main where

import           Control.Applicative
import qualified Data.Map            as M
import           NLP.Tokenize.String (tokenize)
import           System.Directory    (getCurrentDirectory, getDirectoryContents)

-- import           Data.List.Extras.Argmax
-- import           Data.Ord


------------------------------------------------------------------------
--  References
------------------------------------------------------------------------

------------------------------------------------------------------------
--  types
------------------------------------------------------------------------
type Word = String
type Category = String
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
isRegularFile f = f /= "." && f /= ".."

-- | read dir
readDir :: String -> IO [FilePath]
readDir path = do
  directory <- getCurrentDirectory
  filter isRegularFile <$> getDirectoryContents (directory ++ "/" ++ path)

tokenizeDocument :: String -> [Word]
tokenizeDocument = tokenize

readCategoryDir :: FilePath -> IO [Document]
readCategoryDir path = do
  files <- readDir path
  documents <- mapM (readFile . fullPath) files
  return $ map tokenizeDocument documents
  where fullPath p = path ++ "/" ++ p


readModel :: FilePath -> IO (M.Map Category [Document])
readModel path = undefined --do
  -- categories <- readDir path
  -- return $ M.fromList $ map (\ category -> (category, readCategoryDir (path ++ "/" ++ category))) categories
  -- contents <- mapM readFile filePaths

------------------------------------------------------------------------
--  Train
--  Calculating parameters of Naive Bayes Model
------------------------------------------------------------------------

train :: M.Map Category [Document] -> Bayes
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
readStopwords = lines <$> readFile "stop-word-list.txt"

------------------------------------------------------------------------
--  main
------------------------------------------------------------------------
main :: IO ()
main = do
  let trainingPath = "corpus/training"
  let testPath = "corpus/test"
  putStrLn "Calculating..."
  trainingDocuments <- readModel trainingPath
  testModelDocuments <- readModel testPath
  let bayesModel = train trainingDocuments
  putStr "Precision: "
  -- print $ precision testModelSentences hiddenMarkovModel


