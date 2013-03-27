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

{-
IMPLEMENT:
init_category
init_game
-}

--will eventually return a category based off of arg. not sure if this is the
--best way to accomplish this
get_category :: Char -> IO Connection -> [[(String, SqlValue)]]
get_category arg connection = do
  case arg of
    'j' -> round_one_category connection
    'd' -> round_two_category connection
    'f' -> round_three_category connection
    _ -> [[]]

--functions to return categories for specific rounds
--one thing these should do is set the values for each question
round_one_category connection = [[]]
round_two_category connection = [[]]
round_three_category connection = [[]]

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

--given the results of a query, prints out the total number of questions
--returned, as well as the questions
print_category statementResults = do
  putStrLn . show . length $ statementResults
  mapM_ print_category_helper statementResults

print_category_helper statementResult = mapM_ (putStrLn . fromSql . snd) statementResult
