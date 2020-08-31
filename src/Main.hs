module Main where

import Control.Monad ((>=>), forever)
import System.Directory
import Data.List (sort)
import Control.Exception
import Data.List.Split (splitOn)
import System.IO

main :: IO ()
main = forever $ do
  hSetBuffering stdout NoBuffering
  currentDir <- getCurrentDirectory
  putStr $ currentDir ++ " $ "
  command <- getLine
  parseInput command
    `catch` handler

handler :: IOError -> IO ()
handler _ = putStrLn "unexpected error" >> main

parseInput :: String -> IO ()
parseInput input =
  case cmd of
    "ls"    -> mapM_ ls args
    "cat"   -> mapM_ cat args
    "mkdir" -> mapM_ createDirectory args
    "cd"    -> setCurrentDirectory $ head args
    "rm"    -> mapM_ removePathForcibly args
    "mv"    -> renamePath (head args) (args !! 1)
    "echo"  -> mapM_ echo args >> putStrLn ""
    "touch" -> mapM_ touch args
    _       -> putStrLn "Invalid command"
  where (cmd:args) = splitOn " " input

cat :: FilePath -> IO ()
cat = readFile >=> putStr

ls :: FilePath -> IO ()
ls = getDirectoryContents >=> pure . sort >=> mapM_ putStrLn

touch :: FilePath -> IO ()
touch = flip writeFile ""

echo :: String -> IO ()
echo = putStr . (++ " ")
