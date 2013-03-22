import Database.HDBC
import Database.HDBC.Sqlite3
import System.Random
import Text.EditDistance
import Data.Char
import Data.List

main = do
	rand <- (randomRIO(1,215851) :: IO Integer)
	conn <- connectSqlite3 "clues.db"
	stmt <- prepare conn ("SELECT clues.id, value, clue, answer, category FROM clues JOIN documents ON clues.id = documents.id JOIN classifications ON clues.id = classifications.clueid JOIN categories ON catid = categories.id WHERE clues.id=" ++ show rand)
	execute stmt []
	res <- fetchAllRowsAL' stmt
	displayQuestion res
	putStr "Enter your response(:q to quit)\n"
	response <- getLine
	putStr $ checkAnswer response (fromSql . snd $ head res !! 3)
	putStr "\n\n"
	if response == ":q" then return () else main

displayQuestion question = putStr $ "Category: " ++ category ++  "\nValue: " ++ value ++ "\nClue: " ++ clue ++ "\nAnswer: " ++ answer ++ "\n"
	where value = fromSql . snd $ head question !! 1
	      clue = fromSql . snd $ head question !! 2
	      answer = fromSql . snd $ head question !! 3
	      category = fromSql . snd $ head question !! 4

checkAnswer playerGuess correctAnswer
	| playerGuess == ":q" = ""
	| levDistance (prepInput playerGuess) (prepInput correctAnswer) < 3 = "Correct"
	| otherwise = "Wrong"
	where prepInput = map toLower . rmSpace
{-
Computes the Levenshtein Distance given two strings.
NOTE: This is an inefficient solution, use levenshteinDistance

levDistance string1 string2
	| length string1 == 0 = length string2
	| length string2 == 0 = length string1
	| otherwise = min (cost + levDistance tail1 tail2) $ min (1 + levDistance tail1 string2) (1 + levDistance string1 tail2)
	where cost = if head string1 == head string2 then 0 else 1
	      tail1 = tail string1
	      tail2 = tail string2
-}

levDistance = levenshteinDistance defaultEditCosts

rmSpace [] = []
rmSpace (x : xs) = if isSpace x then rmSpace xs else  x : rmSpace xs
