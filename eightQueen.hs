-------- 8 queens problem--------------------
module Main where

import System.IO
import System.Directory
import Data.Char
import Data.List



main :: IO ()
main = do
    handle <- openFile "eightqueen.txt" ReadWriteMode
    (temppath , temphandle ) <- openTempFile "." "temp"
    content <- hGetLine handle
    let wordscontent = words content
    let newsol = abc wordscontent queens
    hClose handle
    hClose temphandle
    let newsola = show newsol
    if ( newsola == show  ( map (\x-> read x :: Int) wordscontent) )
      then
        do
          putStrLn "no solution found........."
          putStrLn "no change in file..........."
      else
        do
        putStrLn (newsola)
        putStrLn "result saved in file"
        writeFile temppath (newsola)
        removeFile "eightqueen.txt"
        renameFile temppath "eightqueen.txt"

pos :: [Int] -> Int -> Int
pos [] a = 0
pos (x:xs) a
  | x==a      = 0
  | otherwise = 1 + pos xs a


type Position = [Int]
extend :: Position -> [Position]
extend [] = [[c] | c <- [0..7]]
extend p  = [ p ++ [c] | c <- [0..7], not (c `elem` (ls p)) ]


ls p = concat $ map (\x -> [x]  ++ [(length p) + x - (pos p x)] ++ [x + (pos p x) - (length p)] ) p

extendall :: [Position] -> [Position]
extendall [] = [[c] | c <- [0..7]]
extendall  l = concat (map (\p -> extend p ) l)

allsolution = iterate extendall [[]]

queens = allsolution !! 8

abc :: [String] -> [Position] -> [Int]
abc [] _ = []
abc x [[]] = map (\a -> (read a :: Int)) x
abc (x:xs) ((z:zs):ys)
    | comp (x:xs) (z:zs)      = [ z ] ++ abc (xs) ((zs):ys)
    | otherwise              = abc (x:xs) (ys)
abc x _ =  map (\a -> (read a :: Int)) x

------------comp function compare that the solution is possible or not--------
comp :: [String] -> Position -> Bool
comp [] [] = True
comp (x:xs) (z:zs)
     | ( read x :: Int ) == 9 = True && comp xs zs
     | ( read x :: Int ) == z = True && comp xs zs
     | otherwise = False
