{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import Data.Text (pack)
import Database.SQLite.Simple
-- import Database.SQLite.Simple.ToField

data TaskOptions = ShowCurrent | ShowAll | AddTask String
                   | UpdateProgress String String
                   | UpdateName String String
                   | DeleteTask String
                   deriving (Eq, Show)

usageMsg :: String
usageMsg =
  ( "daily 0.1.0.0\n"
  ++ "usage: daily [all|new|progress|rename|delete] [OPTION USAGE]\n\n"
  ++ "daily                               -- show current tasks\n"
  ++ "daily all                           -- show all tasks\n"
  ++ "daily new \"<task-name>\"             -- add a new task\n"
  ++ "daily progress <task-id> <progress> -- set task progress\n"
  ++ "daily rename <task-id> \"<new-name>\" -- set task new name\n"
  ++ "daily delete <task-id>              -- delete task.\n")


-- SQL statements
createTable :: String
createTable = "CREATE TABLE IF NOT EXISTS tasks (id INTEGER PRIMARY KEY, description TEXT, done INTEGER, priority INT, created TEXT, updated TEXT);"

getTasks :: String
getTasks = "SELECT * FROM tasks WHERE date = ?;"

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
                           -- query: getAllTasks
                           putStrLn "TODO: show all tasks"
                         else
                           -- query: getTasks
                           putStrLn "TODO: show current tasks"

addNewTask :: Connection -> String -> IO ()
addNewTask conn task = putStrLn $ "TODO: add new task \"" ++ task ++ "\"."

updateProgress :: Connection -> String -> String -> IO ()
updateProgress conn taskId progress = putStrLn $ "TODO: update progress of task #" ++ taskId ++ " to " ++ progress ++ "."

updateDescription :: Connection -> String -> String -> IO ()
updateDescription conn taskId description = putStrLn $ "TODO: rename task #" ++ taskId ++ " to \"" ++ description ++ "\"."

deleteTask :: Connection -> String -> IO ()
deleteTask conn taskId = putStrLn $ "TODO: delete task #" ++ taskId ++ "."

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
          AddTask d          -> addNewTask conn d
          UpdateProgress i p -> updateProgress conn i p
          UpdateName i n     -> updateDescription conn i n
          DeleteTask i       -> deleteTask conn i
        close conn

daily :: [String] -> IO ()
daily args
  | args == []              = tasks ShowCurrent
  | head args == "all"      = tasks ShowAll
  | head args == "new"      = tasks $ AddTask take2nd
  | head args == "progress" = tasks $ UpdateProgress take2nd take3rd
  | head args == "rename"   = tasks $ UpdateName take2nd take3rd
  | head args == "delete"   = tasks $ DeleteTask take2nd
  | otherwise               = putStrLn usageMsg
  where
    take2nd = args !! 1
    take3rd = args !! 2


main :: IO ()
main = do
  args <- getArgs
  daily args
