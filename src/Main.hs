module Main where

import           Control.Applicative
import           Data.List           (nub)
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
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
type Vocabulary = [Word]
type CategoryDocuments = M.Map Category [Document]
type CategoryPr = M.Map Category Double
type Frequencies = M.Map (Word, Word) Integer
-- type TagTransitionPr = M.Map (Tag, Tag) Pr
-- type WordLikelihoodPr = M.Map (Word, Tag) Pr

threshold :: Double
threshold = 0.8

------------------------------------------------------------------------
--  Bayes Model
------------------------------------------------------------------------
data Bayes = Bayes Vocabulary CategoryPr (M.Map Category Int) deriving(Show)

------------------------------------------------------------------------
--  IO
------------------------------------------------------------------------

readFileStrict :: FilePath -> IO String
readFileStrict = fmap T.unpack . TIO.readFile

isRegularFile :: FilePath -> Bool
isRegularFile f = f /= "." && f /= ".."

-- | read dir
readDir :: String -> IO [FilePath]
readDir path = do
  directory <- getCurrentDirectory
  filter isRegularFile <$> getDirectoryContents (directory ++ "/" ++ path)

tokenizeDocument :: String -> [Word]
tokenizeDocument = tokenize

readStopwords :: IO [Word]
readStopwords = lines <$> readFile "stop-word-list.txt"

readCategoryDir :: FilePath -> IO [Document]
readCategoryDir path = do
  files <- readDir path
  documents <- mapM (readFileStrict . fullPath) files
  return $ map tokenizeDocument documents
  where fullPath p = path ++ "/" ++ p

readModel :: FilePath -> IO CategoryDocuments
readModel path = do
  categories <- readDir path
  M.fromList <$> zip categories <$> mapM (\ c -> readCategoryDir (path ++ "/" ++ c)) categories

------------------------------------------------------------------------
--  Train
--  Calculating parameters of Naive Bayes Model
------------------------------------------------------------------------

train :: CategoryDocuments -> Bayes
train categoryDocuments = Bayes vocabulary categoryPr categoryWords
  where documents = allDocuments categoryDocuments
        vocabulary = unique $ concat documents
        totalDocs = fromIntegral $ length documents
        categoryPr = M.map (\ docs -> fromIntegral (length docs) / totalDocs) categoryDocuments
        categoryWords = M.map documentLength categoryDocuments

findPr :: (Fractional v, Ord k) => k -> M.Map k v -> v
findPr = M.findWithDefault 0.00001

count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate

countWord :: Word -> [Word] -> Int
countWord word = count (==word)

unique :: Eq a => [a] -> [a]
unique = nub

allDocuments :: CategoryDocuments -> [Document]
allDocuments = M.fold (flip (++)) []

documentLength :: [Document] -> Int
documentLength = length . concat

categories :: Bayes -> [Category]
categories (Bayes _ categoryPr _) = M.keys categoryPr


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


