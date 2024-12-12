{--
-- EPITECH PROJECT, 2024
-- B-FUN-400-BDX-4-1-wolfram-alea.chlodnik
-- File description:
-- Main
--}

module Main (main) where

import Lib
import RuleConverter()
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Data.Maybe (fromMaybe)

usageMessage :: IO ()
usageMessage = putStrLn "USAGE:\n\t./wolfram --rule RULE [--start START] [--lines LINES] \
\[--window WINDOW] [--move MOVE]\n\n\tRULE:\tCan only be 30, 90 or 110.\n\tSTART:\tThe \
\generation number at which to start the display.\n\tLINES:\tThe number of lines to display.\
\\n\tWINDOW:\tThe number of cells to display per line (line width).\n\tMOVE:\tA translation \
\to apply on the window. If negative, the window is translated to the left. If positive, \
\it’s translated to the right.\n"

main :: IO ()
main = getArgs >>= \args ->
    case getOpts defaultConf args of
        Nothing -> usageMessage >> print "Error: Input needed." >>
            exitWith (ExitFailure 84)
        Just conf -> case conf of
            (Lib.Conf {rule=[], start=_, line=_, window=_, move=_}) ->
                usageMessage >> print "Error: Invalid input." >>
                    exitWith (ExitFailure 84)
            _ -> getResults conf (defaultRows conf) (start conf)
                (fromMaybe 0 (line conf))
