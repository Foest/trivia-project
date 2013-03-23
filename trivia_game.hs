import Database.HDBC
import Database.HDBC.Sqlite3
import System.Random
import Text.EditDistance
import Data.Char
import Data.List

main = do
  conn <- connectSqlite3 "clues.db"
  init_category conn
  disconnect conn
  return ()

init_category connection = do
  show_db_tables connection
  randomCategory <- (randomRIO(1,215828) :: IO Integer)
  stmt <- prepare connection ("SELECT category, value, clue, answer FROM clues JOIN documents ON clues.id = documents.id JOIN classifications ON clues.id = classifications.clueid JOIN categories ON catid = categories.id WHERE categories.id=" ++ show randomCategory)
  execute stmt []
  statementResults <- fetchAllRowsAL' stmt
  show_statement_results statementResults
  return ()
