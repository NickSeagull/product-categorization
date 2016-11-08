{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, GADTs,
             OverloadedStrings, PatternSynonyms, QuasiQuotes,
             ScopedTypeVariables, TemplateHaskell, TypeOperators,
             ViewPatterns #-}

module Main where

{- Product sorting
===============

The problem is the following, a small store needs to know how much products they
have to order for this year.

We try to deduce this from their sales from the last year. The problem here is
that the product categories are in the name. An example would be:

*Hello Doggy hair accessory*

One should be able to infer that this product is an accesory, incrementing the
number of accessories to order by one.
-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative
import qualified Control.Foldl as L
import qualified Data.Foldable as F
import Data.Char
import Data.Proxy (Proxy(..))
import Data.List
import Lens.Family
import Frames
import Frames.CSV (readTableOpt, rowGen, RowGen(..))
import Pipes hiding (Proxy)
import qualified Pipes.Prelude as P

tableTypes "Sale" "resources/order_line_products_from_april_to_august.csv"

loadSales :: IO (Frame Sale)
loadSales = inCoreAoS
          $ readTableOpt
            saleParser
            "resources/order_line_products_from_april_to_august.csv"

fileAsList :: String -> IO [Text]
fileAsList filename = T.words <$> T.readFile filename

namesFromFrame :: Frame Sale -> [Text]
namesFromFrame f = F.toList $ view nameTemplate <$> f

preformatName :: Text -> [Text]
preformatName = T.words
              . T.filter (\x -> isAlpha x || isSeparator x)
              . T.toLower

vocabulary :: [[Text]] -> [Text]
vocabulary = nub . concat

occurrencesOf :: Text -> [Text] -> Int
occurrencesOf word txt = length $ filter (== word) txt

countedVocabulary :: [[Text]] -> [(Text, Int)]
countedVocabulary s = map (\x -> (x, occurrencesOf x $ concat s)) $ vocabulary s

main :: IO ()
main = do
  salesNames <- namesFromFrame <$> loadSales
  stops        <- fileAsList "resources/spanish-stopwords.txt"
  spanishWords <- fileAsList "resources/spanish-words.txt"
  let formattedNames = map ( filter (\x -> T.length x > 2)
                           . filter (`elem` spanishWords)
                           . (\\ stops)
                           . preformatName )
                           salesNames
  mapM_ print . sort $ countedVocabulary formattedNames
