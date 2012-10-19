-- try to convert mysql schema to postgrse
module Main where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Database.HDBC.PostgreSQL
import Data.List (intercalate, elemIndex, findIndices)
import qualified Data.ByteString as B

-- OS X /private/tmp/mysql.sock
connMysql = connectMySQL defaultMySQLConnectInfo { mysqlHost = "localhost", mysqlUser = "root", mysqlDatabase = "mackey_production", mysqlUnixSocket = "/private/tmp/mysql.sock" }
-- Ubuntu:
-- connMysql = connectMySQL defaultMySQLConnectInfo { mysqlHost = "localhost", mysqlUser = "root", mysqlDatabase = "mackey_development", mysqlUnixSocket = "/run/mysqld/mysqld.sock" }


-- target
connPg = connectPostgreSQL "dbname=mackey"
   
isSqlUnknown :: SqlTypeId -> Bool
isSqlUnknown (SqlUnknownT _) = True
isSqlUnknown _ = False

getByteString :: SqlValue -> B.ByteString
getByteString (SqlByteString x) = x
getByteString _ = undefined


main = do
  pg <- connPg
  mysql <- connMysql
  tables <- getTables mysql
  let tables' = filter (\t -> not $ elem t ["archived_file_uploads", "file_uploads"] ) tables
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
            row' = zipWith fixUnknown row colmeta
            fixUnknown :: SqlValue -> SqlColDesc -> SqlValue
            fixUnknown x meta = case (isSqlUnknown . colType) meta of
                                  True -> (SqlByteString (getByteString x))
                                  False -> x
                
        run pg query row'
        putChar '.'
        commit pg
    )
  putStrLn $ "tables: " ++ (show tables)
  disconnect pg

