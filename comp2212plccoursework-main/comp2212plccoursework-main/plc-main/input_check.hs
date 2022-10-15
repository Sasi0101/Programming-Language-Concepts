{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use camelCase" #-}
-- module Eval where

-- import Grammar
-- import Token
-- import Condition
import Data.List
import Data.Char
-- import ReadFile (all_together)
import System.Environment
import Control.Exception
import System.IO
import Control.Monad

-- [x] Check number of elements (3) in supposedly 3tuple
-- [x] Check if there's a '.' at the end of the line (don't forget to trim before)
-- [x] Opened '<' has a closed '>' and vice versa
-- <http://google/subj> <http://google/pred> <http://google/obj> .

main = do
    file_content <- readFile "input.ttl"
    let a =  map (filter (not . null)) $ map (map (filter (/= '.'))) $ map (split ' ') $ filter (not . null) $ map trim $ map (filter (/= '>')) $ split '<' file_content
    -- let a =  filter (/= [""]) $ map (split '>') $ split '<' file_content
    mapM_ print a

-- False = Not good
all_check file_content
    | (check_empty_chevrons file_content) == True = False
    | (check_size_of_3tuple file_content) == False = False
    | (check_dot_end_line file_content) == False = False
    | (check_closed_chevron file_content) == False = False
    | otherwise = True

check_closed_chevron str = (count_occ str '<') == (count_occ str '>')

check_dot_end_line :: String -> Bool
check_dot_end_line file_content = (concat $ take (length $ splitted file_content) $ repeat ".") == (map last $ splitted file_content)
    where splitted xs = map trim $ filter (not . null) $ split '\n' xs

check_size_of_3tuple file_content = 3 /= (length $ filter (/= ".")$ filter (not . null) $ init $ split ' ' $ intercalate " " $ filter (not . null) $ map (filter (/= '>')) $ split '<' file_content)

check_empty_chevrons file_content
    | isInfixOf "<>" $ filter (/= ' ') file_content = True
    | isInfixOf ">>" $ filter (/= ' ') file_content = True
    | isInfixOf "<<" $ filter (/= ' ') file_content = True
    | otherwise = False

trim :: String -> String
trim = unwords . words

split :: Char -> String -> [String]
split c xs = case break (==c) xs of
    (ls, "") -> [ls]
    (ls, x:rs) -> ls : split c rs

count_occ str c = length $ filter (== c) str

