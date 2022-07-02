module NanoUI.Internal where

safeInit :: [a] -> [a]
safeInit [] = []
safeInit l = init l

safeTail :: [a] -> [a]
safeTail [] = []
safeTail l = tail l

