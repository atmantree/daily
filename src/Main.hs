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


-- SQL statements
createTable :: String
createTable = "CREATE TABLE IF NOT EXISTS tasks (id INTEGER, description TEXT, done INTEGER, priority INT, created TEXT, updated TEXT);"

getTodayTasks :: String
getTodayTasks = "SELECT * FROM tasks WHERE date = ?;"

getAllTasks :: String
getAllTasks = "SELECT * FROM tasks;"

addTask :: String
addTask = "INSERT INTO task (description, done, created, updated) VALUES (?, 0, datetime('now', 'localtime'), datetime('now', 'localtime'));"

setTaskPriority :: String
setTaskPriority = "UPDATE task SET priority = ?, update = datetime('now', 'localtime') WHERE id = ?;"

setTaskProgress :: String
setTaskProgress = "UPDATE task SET done = ?, update = datetime('now', 'localtime') WHERE id = ?;"

renameTask :: String
renameTask = "UPDATE task SET description = ?, update = datetime('now', 'localtime') WHERE id = ?;"

delTask :: String
delTask = "DELETE task WHERE id = ?;"


-- Actions
addTable :: Connection -> IO ()
addTable conn = execute_ conn $ Query $ pack (createTable) 

showTasks :: Connection -> Bool -> IO ()
showTasks conn showAll = if showAll == True then
                           putStrLn "TODO: show all tasks"
                         else
                           putStrLn "TODO: show current tasks"



-- addNewTask :: [String] -> IO ()
-- addNewTask task = do
--               homePath <- getHomeDirectory
--               conn <- open $ homePath ++ "/daily.db"
--               createTable conn
--               execute conn "INSERT INTO tasks (task_name, progress, updated, created) values (?, 0, datetime('now','localtime'), datetime('now','localtime'));" task
--               close conn
--               putStrLn "Task added."


tasks :: TaskOptions -> IO ()
tasks opt = do
        homePath <- getHomeDirectory
        conn <- open $ homePath ++ "/daily.db"
        addTable conn
        case opt of
          ShowCurrent        -> showTasks conn False
          ShowAll            -> showTasks conn True
          AddTask _          -> putStrLn "TODO: add new task"
          UpdateProgress _ _ -> putStrLn "TODO: update task progress"
          UpdateName _ _     -> putStrLn "TODO: update task progress"
          DeleteTask _       -> putStrLn "TODO: delete task"
        close conn

daily :: [String] -> IO ()
daily args
  | args == []              = tasks ShowCurrent
  | head args == "all"      = tasks ShowAll
  | head args == "add"      = tasks $ AddTask "Test"
  | head args == "progress" = tasks $ UpdateProgress 0 0
  | head args == "name"     = tasks $ UpdateName 0 "Test"
  | head args == "del"      = tasks $ DeleteTask 0
  | otherwise               = putStrLn usageMsg


main :: IO ()
main = do
  args <- getArgs
  daily args
