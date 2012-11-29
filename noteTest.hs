{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Database.HDBC.PostgreSQL
import Data.List (intercalate, elemIndex, findIndices)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as B
import qualified Database.PostgreSQL.LibPQ as PQ
import Data.Maybe (fromJust)

-- OS X /private/tmp/mysql.sock
connMysql = connectMySQL defaultMySQLConnectInfo { mysqlHost = "localhost", mysqlUser = "root", mysqlDatabase = "mackey_production", mysqlUnixSocket = "/private/tmp/mysql.sock" }
-- Ubuntu:
-- connMysql = connectMySQL defaultMySQLConnectInfo { mysqlHost = "localhost", mysqlUser = "root", mysqlDatabase = "mackey_development", mysqlUnixSocket = "/run/mysqld/mysqld.sock" }

connPg = "dbname=mackey"

{- checks if the note bodies are the same between two databases -}


main = do
  pg <- connectPostgreSQL connPg
  mysql <- connMysql
  pq <- PQ.connectdb (Char8.pack connPg)
  r <- (describeTable mysql "notes")
  mapM_ (putStrLn . show) r
  putStrLn "PostgreSQL:"
  rPg <- (describeTable pg "notes")
  mapM_ (putStrLn . show) rPg
  

  -- loop through mysql notes
  mysqlRows <- quickQuery' mysql "select id, title, body from notes" [] 
  forM_ mysqlRows $ \[id, title, body] -> do
      putStrLn $ intercalate "," $ map (take 50 . fromSql) [id, title]
      -- compare to postgres row
      [[id', title', body']] <- quickQuery' pg "select id, title, body from notes where id = ?" [id]
      putStrLn (if title == title' then "Titles match" else "TITLES DON'T MATCH")
      putStrLn (if body == body' then "Bodies  match" else "BODIES  DON'T MATCH")
      if (body /= SqlNull) then do
          let bodyLength = length $ (fromSql body :: String)
          putStrLn ("Body length: " ++ (show bodyLength))
      else putStrLn "Body null"
       



