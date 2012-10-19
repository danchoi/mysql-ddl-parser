{-# LANGUAGE ScopedTypeVariables #-}
-- try to convert mysql schema to postgrse
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

-- target
connPg = connectPostgreSQL "dbname=mackey"
   
isSqlUnknown :: SqlTypeId -> Bool
isSqlUnknown (SqlUnknownT _) = True
isSqlUnknown _ = False

getByteString :: PQ.Connection -> SqlValue -> IO SqlValue
getByteString c (SqlByteString x) = do 
    x <- PQ.escapeByteaConn c x  
    return $ SqlByteString $ fromJust x
getByteString c SqlNull = do 
    return SqlNull
getByteString conn x = fail $ "error: " ++ (show x)


main = do
  pg <- connPg
  mysql <- connMysql
  pq <- PQ.connectdb (Char8.pack "dbname=mackey")
  tables <- getTables mysql
  forM_ tables (\t -> do 
      putStrLn t
      cols' <- map fst  `liftM` describeTable mysql t
      colmeta <- map snd  `liftM` describeTable mysql t
      let colmeta2 = show (map colType colmeta)
      -- putStrLn $ show $ elemIndex (SqlUnknownT "longblob") colmeta2 

      let cols = map (\x -> "\"" ++ x ++ "\"") cols' -- quote all table columns 
      putStrLn $ show cols'

      -- find any SqlUnknownT columns
      let toFix = findIndices (isSqlUnknown . colType) colmeta 
      putStrLn $ "Unknown types at " ++ (show toFix)

      rows <- withRTSSignalsBlocked $ do
          quickQuery' mysql ("select * from " ++ t) []

      forM_ rows $ \row -> do 
        -- putStrLn $ show row  -- DEBUGGER

        let placeholders = intercalate "," $ take (length cols) $ repeat "?"
            query = "insert into " ++ t ++ "(" ++ (intercalate ", " cols) ++ ") values (" ++ placeholders ++ ")"
            row' = zipWithM fixUnknown row colmeta
            fixUnknown :: SqlValue -> SqlColDesc -> IO SqlValue
            fixUnknown x meta = case (isSqlUnknown . colType) meta of
                                  True -> getByteString pq x
                                  False -> return x

        -- row is IO [SqlValue]
        row'' <- row'
        run pg query row''
        putChar '.'
        commit pg
    )
  putStrLn $ "tables: " ++ (show tables)
  disconnect pg

