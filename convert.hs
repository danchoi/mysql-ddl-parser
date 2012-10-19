-- try to convert mysql schema to postgrse
module Main where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Database.HDBC.PostgreSQL
import Data.List (intercalate)

-- OS X /private/tmp/mysql.sock
connMysql = connectMySQL defaultMySQLConnectInfo { mysqlHost = "localhost", mysqlUser = "root", mysqlDatabase = "mackey_production", mysqlUnixSocket = "/private/tmp/mysql.sock" }
-- Ubuntu:
-- connMysql = connectMySQL defaultMySQLConnectInfo { mysqlHost = "localhost", mysqlUser = "root", mysqlDatabase = "mackey_development", mysqlUnixSocket = "/run/mysqld/mysqld.sock" }


-- target
connPg = connectPostgreSQL "dbname=mackey"
   
main = do
  pg <- connPg
  mysql <- connMysql
  tables <- getTables mysql
  forM_ tables (\t -> do 
      putStrLn t
      cols <- map fst  `liftM` describeTable mysql t
      putStrLn $ show cols
      rows <- withRTSSignalsBlocked $ do
        quickQuery' mysql ("select * from " ++ t) []

      forM_ rows $ \row -> do 
        let placeholders = intercalate "," $ take (length cols) $ repeat "?"
            query = "insert into " ++ t ++ "(" ++ (intercalate "," cols) ++ ") values (" ++ placeholders ++ ")"
        run pg query row
        print "."
        commit pg
    )
  putStrLn $ "tables: " ++ (show tables)
  disconnect pg

