module Main where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (<*>))
import Text.ParserCombinators.Parsec.Combinator
import Control.Monad (liftM)
import Control.Applicative ((<*))
import Data.List (intercalate)


type Width = String
type Type = String
type ColName = String
data Datatype = Datatype Type (Maybe Width) deriving (Show)
data NullOpt = Null | NotNull deriving (Show)
data SerialOpt = NotSerial | Serial deriving (Show)
data DefaultOpt = NoDefault 
                | Default String 
                deriving (Show)

data CreateDefinition = ColumnDefinition ColName Datatype NullOpt SerialOpt DefaultOpt
                      | Index String String 
                      | PrimaryKey String 
                      | ForeignKeyConstraint String String String String String
                      | UniqueConstraint String String 
                      deriving (Show)

data Statement = CreateTable String [CreateDefinition] 
               | DropTable String  -- entire string can be copied verbatim
               deriving (Show)

------------------------------------------------------------------------

class Postgres a where
    translate :: a -> String

instance Postgres Statement where 
    translate (CreateTable x ys) = "create table " ++ x ++ "\n" ++ 
                                      intercalate "\n" (map translate ys)
    translate (DropTable x) =  "drop table " ++ x
    translate x =  "don't know how to translate: "  ++ (show x)


instance Postgres CreateDefinition where
    translate (ColumnDefinition c d n s df) = "  " ++ (show c) ++ " " ++ (show d)
    translate _ = "  create definition"

------------------------------------------------------------------------


comment :: GenParser Char st ()
comment = 
    (string "--" >> manyTill anyChar newline >> return ()) <|>
    (string "/*" >> manyTill anyChar (string "*/") >> return ())

notComment = manyTill anyChar (lookAhead (comment <|> eof))

stripComments :: GenParser Char st String
stripComments = do
  optional comment
  xs <- sepBy (spaces >> notComment <* spaces) (comment >> optional spaces >> optional (char ';'))
  optional comment
  return $ intercalate "" xs


------------------------------------------------------------------------
-- Real ddl parsing functions:

dropTable :: GenParser Char st Statement
dropTable = do 
    x <- string "DROP TABLE" <* spaces
    optional (string "IF EXISTS" <* spaces)
    t <- betweenTicks
    xs <- many (noneOf ";")
    return $ DropTable t

createTable :: GenParser Char st Statement
createTable = do 
    x <-  string "CREATE TABLE" <* spaces
    t <- betweenTicks 
    spaces >> char '('  >> spaces 
    ds <- definitions
    many (noneOf ";")
    return $ CreateTable t ds  

eol = char '\n'

betweenTicks :: GenParser Char st String
betweenTicks = char '`' >> many (noneOf "`") <* (char '`' >> spaces)

betweenParens :: GenParser Char st String
betweenParens = char '(' >> many (noneOf ")") <* (char ')' >> spaces)

betweenParensTicks :: GenParser Char st String
betweenParensTicks = char '(' >> betweenTicks <* (char ')' >> spaces)

definitions :: GenParser Char st [CreateDefinition]
definitions = many createDefinition <* char ')' 
  
createDefinition :: GenParser Char st CreateDefinition
createDefinition = do
    x <- primaryKey <|> index <|> foreignKeyConstraint <|> uniqueConstraint <|> columnDefinition 
    optional (char ',') >> optional eol >> spaces
    return x
     -- <|> check

datatype :: GenParser Char st Datatype 
datatype = do
    -- change these later to types
    t <- string "int" <|> string "varchar" <|> try (string "tinyint") <|> try (string "datetime") <|> try (string "longblob") <|> try (string "blob")  <|>
          try (string "text") <|> string "longtext" <|> string "decimal" <|> try (string "smallint") <|> try (string "bigint")
    width <- optionMaybe $ betweenParens
    spaces
    return $ Datatype t width

columnDefinition :: GenParser Char st CreateDefinition
columnDefinition = do 
    tbl <- betweenTicks
    d <- datatype
    optional (string "COLLATE" >> spaces >> (many (noneOf " "))) >> spaces
    n <- optionMaybe (string "NOT NULL") <* spaces
    let nopt = case n of
                  Nothing -> Null
                  _ -> NotNull
    s <- option NotSerial (try $ string "AUTO_INCREMENT" >> return Serial)
    df <- option NoDefault (Default `liftM` (string "DEFAULT " >> (many (noneOf " ,\n"))))
    return $ ColumnDefinition tbl d nopt s df

keyColumns = char '(' >> liftM (intercalate ",") (sepBy betweenTicks (char ',')) <* (char ')' >> spaces)

primaryKey :: GenParser Char st CreateDefinition
primaryKey = string "PRIMARY KEY " >> PrimaryKey `liftM` betweenParensTicks

index :: GenParser Char st CreateDefinition
index = string "KEY " >> Index `liftM` betweenTicks <*> keyColumns 

foreignKeyConstraint :: GenParser Char st CreateDefinition
foreignKeyConstraint = do 
    string "CONSTRAINT " 
    ident <- betweenTicks
    string "FOREIGN KEY "
    col <- keyColumns
    string "REFERENCES "
    tbl <- betweenTicks
    tblCol <- betweenParensTicks
    action <- manyTill anyChar (char ',' <|> eol) -- LEAK
    return $ ForeignKeyConstraint ident col tbl tblCol action

uniqueConstraint :: GenParser Char st CreateDefinition
uniqueConstraint = string "UNIQUE KEY " >> UniqueConstraint `liftM` betweenTicks <*> keyColumns

statement :: GenParser Char st Statement
statement = dropTable <|> createTable

ddlFile :: GenParser Char st [Statement]
ddlFile = endBy statement (char ';' >> spaces)

test s = do 
  case parse stripComments "" s of 
    Left err -> "Error stripping comments"
    -- Right s' -> s'
    Right s' -> case parse ddlFile "" s' of 
                  Left e -> "No match " ++ show e
                  Right res -> show res

prettyPrint :: [Statement] -> IO ()
prettyPrint xs = 
    mapM_ pprint xs
    where pprint (CreateTable x ys) = do
              putStrLn x
              mapM_ (putStrLn . showCreateDefinition) ys
          pprint _ = return ()

          showCreateDefinition :: CreateDefinition -> String
          showCreateDefinition x = "  " ++ (show x)

toPostgres :: [Statement] -> IO ()
toPostgres xs = mapM_ (putStrLn . translate) xs


main = do 
    s <- getContents
    case parse stripComments "" s of 
      Left err -> putStrLn "Error stripping comments"
      Right s' -> do
          writeFile "stripped.sql" s'
          putStrLn "Stripped comments out to stripped.sql"
          case parse ddlFile "" s' of 
                  Left e -> putStrLn $ "No match " ++ show e
                  Right xs -> do 
                      -- prettyPrint xs
                      toPostgres xs

    

{-
 -
 - :
 -
 -   simpleComment   = do{ string "<!--"
 -                         ; manyTill anyChar (try (string "-->"))
 -                                               }
 -
 -
main = do 
    res <- parseFromFile schemadump "mysql-schema.sql"
    case res of
      Left err -> print err
      Right xs -> print xs
-}
