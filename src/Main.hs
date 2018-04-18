{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import Data.Text (pack)
import Database.SQLite.Simple

createTableQuery :: Connection -> IO ()
createTableQuery conn = execute_ conn $ Query $ pack ( 
                                          "CREATE TABLE IF NOT EXISTS tasks "
                                          ++ "(id INTEGER PRIMARY KEY AUTOINCREMENT, "
                                          ++ "task_name TEXT, progress  INTEGER, "
                                          ++ "updated TEXT, created TEXT)")

testDB :: IO ()
testDB = do
      homePath <- getHomeDirectory 
      conn <- open $ homePath ++ "/test.db"
      createTableQuery conn
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

usageMsg :: String
usageMsg = ( "daily 0.1.0.0\n"
          ++ "usage: daily [all|add|update|del] [OPTION USAGE]\n\n"
          ++ "daily add \"...\"                -- add a new task")

dailyTasks args
  | args == []            = showTasks True
  | head args == "all"    = showTasks False
  | head args == "add"    = addNewTask $ tail args
  | head args == "update" = setTaskProgress 0
  | head args == "del"    = deleteTask 0 
  | head args == "testdb" = testDB 
  | otherwise             = putStrLn usageMsg


main :: IO ()
main = do
  args <- getArgs
  dailyTasks args
