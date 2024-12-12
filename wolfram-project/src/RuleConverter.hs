{--
-- EPITECH PROJECT, 2024
-- B-FUN-400-BDX-4-1-wolfram-alea.chlodnik
-- File description:
-- RuleConverter
--}

module RuleConverter
    (
        toBinary,
        padBits,
        toWolframBinary,
    ) where

import Data.List (unfoldr)

toBinary :: Int -> [Int]
toBinary n = reverse $ unfoldr
    (\x -> if x == 0 then Nothing else Just (mod x 2, div x 2)) n

padBits :: Int -> [Int] -> [Int]
padBits len bits = replicate (len - length bits) 0 ++ bits

toWolframBinary :: Int -> [Int]
toWolframBinary ruleNum = padBits 8 $ toBinary ruleNum
