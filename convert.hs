{-# LANGUAGE ScopedTypeVariables #-}
-- try to convert mysql schema to postgrse
module Main where

import Prelude hiding (catch)
import Control.Monad 
import Control.Exception (catch)
import Database.HDBC
import Database.HDBC.MySQL
import Database.HDBC.PostgreSQL
import Data.List (intercalate, elemIndex, findIndices)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Database.PostgreSQL.LibPQ as PQ
import Data.Maybe (fromJust)
import qualified Codec.Text.IConv as IConv
-- import Codec.Text.Detect 

-- OS X /private/tmp/mysql.sock
connMysql = connectMySQL defaultMySQLConnectInfo { mysqlHost = "localhost", mysqlUser = "root", mysqlDatabase = "mackey_production", mysqlUnixSocket = "/private/tmp/mysql.sock" }
-- Ubuntu:
-- connMysql = connectMySQL defaultMySQLConnectInfo { mysqlHost = "localhost", mysqlUser = "root", mysqlDatabase = "mackey_development", mysqlUnixSocket = "/run/mysqld/mysqld.sock" }

-- target

connPg = "dbname=mackey"
   
isSqlUnknown :: SqlTypeId -> Bool
isSqlUnknown (SqlUnknownT _) = True
-- isSqlUnknown (SqlBinaryT) = True
isSqlUnknown _ = False

getByteString :: PQ.Connection -> SqlValue -> IO SqlValue
getByteString c (SqlByteString x) = do 
    x <- PQ.escapeByteaConn c x  
    return $ SqlByteString $ fromJust x
getByteString c SqlNull = do 
    return SqlNull
getByteString conn x = fail $ "error getByteString: " ++ (show x)

-- translit x = toSql (IConv.convertFuzzy IConv.Transliterate "UTF-8" "UTF-8" (fromSql x)) 
translit x = toSql (IConv.convert "LATIN1" "UTF-8" (fromSql x)) 

main = do
  pg <- connectPostgreSQL connPg
  mysql <- connMysql
  pq <- PQ.connectdb (Char8.pack connPg)
  tables <- getTables mysql
  forM_ tables (\t -> do 
      putStrLn $ "\n" ++ t
      columnData <- describeTable mysql t
      mapM (putStrLn . show) columnData
      let colnames = map fst columnData
      putStrLn $ show colnames
      let colMetaData = map snd columnData
      let cols = map (\x -> "\"" ++ x ++ "\"") colnames -- quote all table columns 

      -- find any SqlUnknownT columns
      let xs = filter (isSqlUnknown . colType . snd) columnData
      when (not $ null xs) 
          (putStrLn $ "Unknown datatypes: " ++ (intercalate "\n" $ map show xs) ++ "\nAssuming ByteString.")

      rows <- withRTSSignalsBlocked $ do
          quickQuery' mysql ("select * from " ++ t) []

      forM_ rows $ \row -> do 
        -- putStrLn $ show row  -- DEBUGGER
        let placeholders = intercalate "," $ take (length cols) $ repeat "?"
            query = "insert into " ++ t ++ "(" ++ (intercalate ", " cols) ++ ") values (" ++ placeholders ++ ")"
            row' = zipWithM fixUnknown row colMetaData
            fixUnknown :: SqlValue -> SqlColDesc -> IO SqlValue
            fixUnknown x meta = case (colType meta, x) of
                                  (SqlUnknownT _, SqlByteString _) -> getByteString pq x
                                  (SqlUnknownT "longblob", SqlByteString _) -> getByteString pq x
                                  (SqlUnknownT "mediumtext", SqlByteString y) -> return $ translit x
                                  (SqlUnknownT "longtext", SqlByteString _) -> return $ translit x
                                  (SqlVarCharT, SqlByteString _) -> return $ translit x
                                  (SqlBinaryT, SqlByteString _) -> return $ translit x
                                  _ -> return x

        -- row' is IO [SqlValue]
        row'' <- row'
        -- putStrLn $ show row''

        run pg query row'' 

        putChar '.'
        commit pg
    )
  putStrLn $ "tables: " ++ (show tables)
  disconnect pg

