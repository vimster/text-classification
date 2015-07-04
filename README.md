# Text Classification
Script for classifying text documents using the Naive Bayes classification.

## Usage
Copy the xml corpus files into the root folder named corpus (corpus/[training|test]/[C01..C24]/<document>).
Haskell and its package and building system cabal are required.
The Haskell Plattform (https://www.haskell.org/platform/) bundles all requirements.

Run in the root directory:
$ cabal update
$ cabal install
$ cabal run

## Results on Ohsumed corpus (category C02 tested)
Precision = ~ 20.49%
