{-
-- EPITECH PROJECT, 2024
-- $PROJECT_NAME
-- File description:
-- $DESCRIPTION
-}

module Lib
    (
        Conf(..),
        Rows(..),
        defaultConf,
        defaultRows,
        getOpts,
        getResults,
    ) where

import RuleConverter
import GHC.Base()
import System.Exit(exitSuccess)
import Data.Maybe(fromMaybe)
import Text.Read(readMaybe)

data Conf = Conf {rule :: [Int], start :: Int, line :: Maybe Int, window :: Int, move :: Maybe Int}
  deriving (Show)

data Rows = Rows {current :: [Int], next :: [Int]}
    deriving(Show)

defaultConf :: Conf
defaultConf = Conf {rule = [], start = 0, line = Nothing,
                    window = 80, move = Nothing}

defaultRows :: Conf -> Rows
defaultRows conf = Rows {current = modifyElemInList [0, 0..] 1
                (window conf `div` 2 + start conf + fromMaybe 0
                (line conf) + fromMaybe 0 (move conf)), next = [0, 0..]}

modifyElemInList :: [Int] -> Int -> Int -> [Int]
modifyElemInList [] _ _ = []
modifyElemInList (_:end) e 0 = e:end
modifyElemInList (a:end) e index = a:modifyElemInList end e (index - 1)

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = Just conf
getOpts conf ("--rule":value:end) = case readMaybe value of
    Just val | val < 0 -> Nothing
             | val > 255 -> Nothing
             | otherwise -> getOpts conf { rule = toWolframBinary val } end
    Nothing -> Nothing
getOpts conf ("--start":value:end) = case readMaybe value of
    Just val | val < 0 -> Nothing
             | otherwise -> getOpts conf { start = val } end
    Nothing -> Nothing
getOpts conf ("--lines":value:end) = case readMaybe value of
    Just val | val < 0 -> Nothing
             | otherwise -> getOpts conf { line = Just val } end
    Nothing -> Nothing
getOpts conf ("--window":value:end) = case readMaybe value of
    Just val | val < 0 -> Nothing
             | otherwise -> getOpts conf { window = val } end
    Nothing -> Nothing
getOpts conf ("--move":value:end) = case readMaybe value of
    Just val -> getOpts conf { move = Just val } end
    Nothing -> Nothing
getOpts _ _ = Nothing

advance :: [Int] -> Int -> [Int]
advance [] _ = error "Empty list out of bonds"
advance list 0 = list
advance (_:end) n = advance end (n - 1)

applyRule :: [Int] -> Conf -> Int
applyRule [1, 1, 1] conf = head (rule conf)
applyRule [1, 1, 0] conf = rule conf !! 1
applyRule [1, 0, 1] conf = rule conf !! 2
applyRule [1, 0, 0] conf = rule conf !! 3
applyRule [0, 1, 1] conf = rule conf !! 4
applyRule [0, 1, 0] conf = rule conf !! 5
applyRule [0, 0, 1] conf = rule conf !! 6
applyRule [0, 0, 0] conf = rule conf !! 7
applyRule _ _ = 0

getNextRow :: [Int] -> Conf -> Rows -> Rows
getNextRow curr conf rows = rows {next = 0:zipWith3
        (\a b c -> applyRule [a, b, c] conf) curr
        (advance curr 1) (advance curr 2)}

getResults :: Conf -> Rows -> Int -> Int -> IO ()
getResults conf rows ogStart ogLine =
    let newNext = next (getNextRow (current rows) conf rows)
    in if start conf > 0 
        then getResults conf { start = start conf - 1 }
            (rows {current = newNext, next = [0, 0..]}) ogStart ogLine
        else do
            displayResults conf rows ogStart ogLine
            let newLine = case line conf of
                    Just l -> Just (l - 1)
                    Nothing -> Nothing
            getResults conf {line = newLine}
                (rows {current = newNext, next = [0, 0..]}) ogStart ogLine

modifyRowEven :: Conf -> Rows -> Int -> Int -> [Char]
modifyRowEven conf rows ogStart ogLine =
    map (\x -> if x == 0 then ' ' else '*') (take (window conf)
    (advance (current rows) (ogStart + ogLine)))

modifyRowOdd :: Conf -> Rows -> Int -> Int -> [Char]
modifyRowOdd conf rows ogStart ogLine =
    map (\x -> if x == 0 then ' ' else '*') (take
    (window conf - 1) (advance (current rows) (ogStart + ogLine)))

displayResults :: Conf -> Rows -> Int -> Int -> IO ()
displayResults conf rows ogStart ogLine =
    let curr = if even (window conf)
                    then modifyRowEven conf rows ogStart ogLine
                    else modifyRowOdd conf rows ogStart ogLine
    in case line conf of
        Nothing -> putStrLn curr
        Just lineNum | lineNum > 0 -> putStrLn curr
                     | otherwise -> exitSuccess
