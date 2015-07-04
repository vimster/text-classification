module Main where

import           Control.Applicative
import           Data.Char           (toLower)
import           Data.List           (nub)
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
-- import           Debug.Trace
import           NLP.Tokenize.String (tokenize)
import           System.Directory    (getCurrentDirectory, getDirectoryContents)

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
threshold = 1e-200

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

tokenizeDocument :: [Word] -> String -> [Word]
tokenizeDocument stopwords text = filter (not . flip elem stopwords) $ tokenize lowText
  where lowText = map toLower text

readStopwords :: IO [Word]
readStopwords = lines <$> readFile "stop-word-list.txt"

readCategoryDir :: FilePath -> IO [Document]
readCategoryDir path = do
  files <- readDir path
  documents <- mapM (readFileStrict . fullPath) files
  stopwords <- readStopwords
  return $ map (tokenizeDocument stopwords) documents
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
--  Score calculation
------------------------------------------------------------------------

classifyDocument :: Document -> Bayes -> [Category]
classifyDocument testDocument bayes =
  filter (\category -> threshold <= calculateScore category testDocument bayes) categories
  where
    categories = categoriesOfBayes bayes

calculateScore :: Category -> Document -> Bayes -> Pr
calculateScore category document bayes@(Bayes _ categoryPr _ _) =
  likelihoodProbability * priorProbability
  where
    likelihoodProbability = product $ map (probability bayes category) document
    priorProbability =  M.findWithDefault 0 category categoryPr

probability :: Bayes -> Category -> Word -> Pr
probability (Bayes vocabulary _ categoryWords wordCounts) category word =
  (wordCount + 1) / (documentSize + vocabularySize)
  where
    wordCount = fromIntegral $ M.findWithDefault 0 (word, category) wordCounts
    documentSize = fromIntegral $ M.findWithDefault 0 category categoryWords
    vocabularySize = fromIntegral $ length vocabulary


------------------------------------------------------------------------
--  Evaluation
------------------------------------------------------------------------

precision :: CategoryDocuments -> Bayes -> Pr
precision testDocuments bayes = fromIntegral truePositiveCount / fromIntegral testDocumentCount
  where
    testDocumentCount = M.fold ((+) . length) 0 testDocuments
    truePositives :: M.Map Category Int
    truePositives = M.mapWithKey (\category docs -> length $ filter (containsCategory category) docs) testDocuments
    containsCategory category document = elem category $ classifyDocument document bayes
    truePositiveCount = M.fold (+) 0 truePositives

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
  print $ precision testModelDocuments bayesModel


