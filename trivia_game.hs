import Database.HDBC
import Database.HDBC.Sqlite3
import System.Random
import Text.EditDistance
import Data.Char
import Data.List

main = do
  conn <- connectSqlite3 "clues.db"
  disconnect conn
  return ()


--takes a connection to a database and creates a game
--init_game connection =

init_category connection = do
  randomCategory <- (randomRIO(1,215828) :: IO Integer)
  stmt <- prepare connection ("SELECT category, value, clue, answer FROM clues JOIN documents ON clues.id = documents.id JOIN classifications ON clues.id = classifications.clueid JOIN categories ON catid = categories.id WHERE categories.id=" ++ show randomCategory)
  return ()
