module Main where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (<*>))
import Text.ParserCombinators.Parsec.Combinator
import Control.Monad (liftM)
import Control.Applicative ((<*))
import Data.List (intercalate)


data Default = Default String deriving (Show)

data CreateDefinition = ColumnDefinition String (String, Maybe String) (Maybe Default)
                      | Index String String 
                      | PrimaryKey String 
                      | ForeignKeyConstraint String String String String String
                      | UniqueConstraint String String 
                      deriving (Show)

data Statement = CreateTable String [CreateDefinition] 
               | DropTable String  -- entire string can be copied verbatim
               deriving (Show)

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

-- Now we assume a clean file

dropTable :: GenParser Char st Statement
dropTable = do 
    x <- string "DROP TABLE "
    optional (string "IF EXISTS ")
    t <- betweenTicks
    xs <- many (noneOf ";")
    semispaces
    return $ DropTable t

createTable :: GenParser Char st Statement
createTable = do 
    x <-  string "CREATE TABLE "
    t <- betweenTicks 
    spaces >> char '('  >> spaces 
    ds <- definitions
    many (noneOf ";")
    semispaces
    return $ CreateTable t ds  

eol = char '\n'

betweenTicks :: GenParser Char st String
betweenTicks = do 
    char '`' 
    x <- many (noneOf "`")
    char '`' 
    spaces
    return x

betweenParens :: GenParser Char st String
betweenParens = do 
    char '(' 
    x <- many (noneOf ")") 
    char ')' 
    spaces
    return x

betweenParensTicks :: GenParser Char st String
betweenParensTicks = do
    char '(' 
    x <- betweenTicks
    char ')'
    spaces
    return x

definitions :: GenParser Char st [CreateDefinition]
definitions = do
    xs <- many createDefinition
    char ')' 
    return xs
  
createDefinition :: GenParser Char st CreateDefinition
createDefinition = do
    x <- primaryKey <|> index <|> foreignKeyConstraint <|> uniqueConstraint <|> columnDefinition 
    optional (char ',') >> optional eol >> spaces
    return x
     -- <|> check

datatype :: GenParser Char st (String, Maybe String)
datatype = do
    -- change these later to types
    t <- string "int" <|> string "varchar" <|> try (string "tinyint") <|> try (string "datetime") <|> try (string "longblob") <|> try (string "blob")  <|>
          try (string "text") <|> string "longtext" <|> string "decimal" <|> try (string "smallint") <|> try (string "bigint")
    width <- optionMaybe $ betweenParens
    spaces
    return (t, width)

columnDefinition :: GenParser Char st CreateDefinition
columnDefinition = do 
    tbl <- betweenTicks
    d <- datatype
    optional (string "COLLATE " >> (many (noneOf " "))) >> spaces
    optional (try $ string "NOT NULL AUTO_INCREMENT")
    optional (string "NOT NULL") >>  spaces
    df <- optionMaybe $ Default `liftM` (string "DEFAULT " >> (many (noneOf " ,\n")))
    return $ ColumnDefinition tbl d df


keyColumns = do
  char '('
  xs <- sepBy betweenTicks (char ',')
  char ')'
  spaces
  return $ intercalate "," xs

primaryKey :: GenParser Char st CreateDefinition
primaryKey = do 
    string "PRIMARY KEY " 
    x <- betweenParensTicks
    return $ PrimaryKey x

index :: GenParser Char st CreateDefinition
index = do 
    string "KEY " 
    ident <- betweenTicks 
    col <- keyColumns
    return $ Index ident col

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
uniqueConstraint = do
    string "UNIQUE KEY "
    a <- betweenTicks
    b <- keyColumns
    return $ UniqueConstraint a b

statement :: GenParser Char st Statement
statement = dropTable <|> createTable

-- eats up ';' and any following spaces
semispaces :: GenParser Char st ()
semispaces = (char ';') >> spaces >> return ()

ddlFile :: GenParser Char st [Statement]
ddlFile = many statement 

test s = do 
  case parse stripComments "" s of 
    Left err -> "Error stripping comments"
    -- Right s' -> s'
    Right s' -> case parse ddlFile "" s' of 
                  Left e -> "No match " ++ show e
                  Right res -> show res

main = do 
    s <- getContents
    case parse stripComments "" s of 
      Left err -> putStrLn "Error stripping comments"
      Right s' -> do
          writeFile "stripped.sql" s'
          putStrLn "Stripped comments out to stripped.sql"
          case parse ddlFile "" s' of 
                  Left e -> putStrLn $ "No match " ++ show e
                  Right res -> putStrLn $ show res

    

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
