{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import Database.SQLite.Simple

testDB :: IO ()
testDB = do
      homePath <- getHomeDirectory 
      conn <- open $ homePath ++ "/test.db"
      putStrLn "Testing database"
      close conn


showTasks :: Bool -> IO ()
showTasks currentOnly = if currentOnly == True then
                           putStrLn "TODO: show current tasks"
                         else
                           putStrLn "TODO: show all tasks"

setTaskProgress :: Integer -> IO ()
setTaskProgress progress = putStrLn "TODO: set task progress"

addNewTask :: [String] -> IO ()
addNewTask task = putStrLn "TODO: add new task"

deleteTask :: Integer -> IO ()
deleteTask taskId = putStrLn "TODO: remove task from list"

dailyTasks args
  | args == []            = showTasks True
  | head args == "all"    = showTasks False
  | head args == "add"    = addNewTask $ tail args
  | head args == "update" = setTaskProgress 0
  | head args == "del"    = deleteTask 0 
  | head args == "testdb" = testDB 
  | otherwise             = putStrLn "TODO: add usage message"


main :: IO ()
main = do
  args <- getArgs
  dailyTasks args
