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

threshold :: Double
threshold = 0.8

------------------------------------------------------------------------
--  Bayes Model
------------------------------------------------------------------------
data Bayes = Bayes Vocabulary CategoryPr (M.Map Category Int) (M.Map (Word, Category) Int) deriving(Show)

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
train categoryDocuments = Bayes vocabulary categoryPr categoryWords wordCounts
  where documents = allDocuments categoryDocuments
        vocabulary = unique $ concat documents
        totalDocs = fromIntegral $ length documents
        categoryPr = M.map (\ docs -> fromIntegral (length docs) / totalDocs) categoryDocuments
        categoryWords = M.map documentLength categoryDocuments

        emptyMap :: M.Map (Word, Category) Int
        emptyMap = M.empty

        wordCounts = M.foldWithKey (\k a b -> M.union b $ wordCategoryCounts k a) emptyMap categoryDocuments

findCount :: (Num v, Ord k) => k -> M.Map k v -> v
findCount = M.findWithDefault 1

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

categoriesOfBayes :: Bayes -> [Category]
categoriesOfBayes (Bayes _ categoryPr _ _) = M.keys categoryPr

wordCategoryCounts :: Category -> [Document] -> M.Map (Word, Category) Int
wordCategoryCounts category documents = foldr (flip (M.insertWith (+)) 1) M.empty joined
  where joined = map (\word -> (word, category)) $ concat documents


------------------------------------------------------------------------
--  Evaluation
------------------------------------------------------------------------

precision :: CategoryDocuments -> Bayes -> Pr
precision testDocuments bayes = fromIntegral truePositiveCount / fromIntegral testDocumentCount
  where
    testDocumentCount = M.fold ((+) . length) 0 testDocuments
    categories = categoriesOfBayes bayes
    truePositiveCount :: Int
    truePositiveCount = 12

calculateScore :: Category -> Document -> Bayes -> Pr
calculateScore category document bayes@(Bayes vocabulary categoryPr _ _) =
  likelihoodProbability * priorProbability
  where
    likelihoodProbability = product $ map (probability bayes category) vocabulary
    priorProbability = M.findWithDefault 0 category categoryPr

probability :: Bayes -> Category -> Word -> Pr
probability (Bayes vocabulary _ categoryWords wordCounts) category word =
  (wordCount + 1) / (documentSize + vocabularySize)
  where
    wordCount = fromIntegral $ M.findWithDefault 0 (word, category) wordCounts
    documentSize = fromIntegral $ M.findWithDefault 0 category categoryWords
    vocabularySize = fromIntegral $ length vocabulary


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
  print bayesModel
  putStr "Precision: "
  -- print $ precision testModelSentences hiddenMarkovModel


