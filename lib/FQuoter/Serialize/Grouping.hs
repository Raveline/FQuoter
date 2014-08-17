module FQuoter.Serialize.Grouping 
( groupSql )
where

import Database.HDBC
import Data.List
import Data.Function
import FQuoter.Serialize.SerializedTypes

{- The purpose of this function is to handle case where we have to use
several lines in a SQL query to return similar information, typically
when we cannot use group_concat.
For instance, authors, metadata values of sources, and so on, cannot be
practically returned with group concat, which mean we have to use several
lines to parse them properly. -}
groupSql :: DBType -> [[SqlValue]] -> [[SqlOutput]]
groupSql DBQuote = map (groupQuotes . transpose) . groupById
groupSql _ = toSqlOutputs

groupById :: [[SqlValue]] -> [[[SqlValue]]]
groupById = groupBy ((==) `on` head)

toSqlOutputs :: [[SqlValue]] -> [[SqlOutput]]
toSqlOutputs = map (map Single)

groupQuotes :: [[SqlValue]] -> [SqlOutput]
groupQuotes values = (regroup metadata):(regroup tags):(regroup authors):quote'
    where
        (quote, others) = splitAt 5 values
        (metadata, others') = splitAt 2 others
        (tags, authors) = splitAt 1 others'
        regroup = Grouped . map toSqlOutput . nub . transpose
        toSqlOutput [x] = Single x
        toSqlOutput xs = Grouped $ map Single xs
        quote' = map (toSqlOutput . nub) quote
