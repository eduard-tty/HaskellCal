module Main where
import System.Environment (getArgs)
import Calendar (calendarForYear)
 
main :: IO ()
main = do 
          args <- getArgs
          let year     = (read $ head args)::Int
              calendar = calendarForYear year
          print calendar
           
