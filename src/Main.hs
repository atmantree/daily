{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import Data.Text (pack)
import Database.SQLite.Simple
-- import Database.SQLite.Simple.ToField

data TaskOptions = ShowCurrent | ShowAll | AddTask String
                   | UpdateProgress Integer Integer
                   | UpdateName Integer String
                   | DeleteTask Integer
                   deriving (Eq, Show)


usageMsg :: String
usageMsg =
  ( "daily 0.1.0.0\n"
  ++ "usage: daily [all|new|progress|name|del] [OPTION USAGE]\n\n"
  ++ "daily                               -- show current tasks\n"
  ++ "daily all                           -- show all tasks\n"
  ++ "daily new \"<task-name>\"             -- add a new task\n"
  ++ "daily progress <task-id> <progress> -- set task progress\n"
  ++ "daily name <task-id> \"<new-name>\"   -- set task new name\n"
  ++ "daily del <task-id>                 -- delete task.\n")


-- addNewTask :: [String] -> IO ()
-- addNewTask task = do
--               homePath <- getHomeDirectory
--               conn <- open $ homePath ++ "/daily.db"
--               createTable conn
--               execute conn "INSERT INTO tasks (task_name, progress, updated, created) values (?, 0, datetime('now','localtime'), datetime('now','localtime'));" task
--               close conn
--               putStrLn "Task added."

daily :: [String] -> IO ()
daily args
  | args == []              = tasks ShowCurrent
  | head args == "all"      = tasks ShowAll
  | head args == "add"      = tasks $ AddTask "Test"
  | head args == "progress" = tasks $ UpdateProgress 0 0
  | head args == "name"     = tasks $ UpdateName 0 "Test"
  | head args == "del"      = tasks $ DeleteTask 0
  | otherwise               = putStrLn usageMsg
  where
    tasks :: TaskOptions -> IO ()
    tasks opt = do
            homePath <- getHomeDirectory
            conn <- open $ homePath ++ "/daily.db"
            createTable conn
            case opt of
              ShowCurrent        -> showTasks conn False
              ShowAll            -> showTasks conn True
              AddTask _          -> putStrLn "TODO: add new task"
              UpdateProgress _ _ -> putStrLn "TODO: update task progress"
              UpdateName _ _     -> putStrLn "TODO: update task progress"
              DeleteTask _       -> putStrLn "TODO: delete task"
            close conn

    createTable :: Connection -> IO ()
    createTable conn = execute_ conn $ Query $ pack (
      "CREATE TABLE IF NOT EXISTS tasks (id INTEGER PRIMARY KEY AUTOINCREMENT, "
      ++ "task_name TEXT, progress  INTEGER, updated TEXT, created TEXT)")

    showTasks :: Connection -> Bool -> IO ()
    showTasks conn showAll = if showAll == True then
                               putStrLn "TODO: show all tasks"
                             else
                               putStrLn "TODO: show current tasks"


main :: IO ()
main = do
  args <- getArgs
  daily args
