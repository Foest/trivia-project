import Database.HDBC
import Database.HDBC.Sqlite3
import System.Random
import Text.EditDistance
import Data.Char
import Data.List

main = do
  conn <- connectSqlite3 "clues.db"
  get_random_category conn
  disconnect conn
  return ()

--grabs a random category from db, prints the questions
get_random_category connection = do
  randomCategory <- (randomRIO(1,215828) :: IO Integer)
  stmt <- prepare connection ("SELECT category, value, clue, answer FROM clues JOIN documents ON clues.id = documents.id JOIN classifications ON clues.id = classifications.clueid JOIN categories ON catid = categories.id WHERE categories.id=" ++ show randomCategory)
  execute stmt []
  statementResults <- fetchAllRowsAL' stmt
  if length statementResults == 0
    then get_random_category connection
    else print_category statementResults
  return ()

--prints the db tables
print_db_tables connection = do
  db_tables <- getTables connection
  mapM_ putStrLn db_tables
  return ()

show_statement_results results = return ()
