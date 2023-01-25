import IntCode
import Data.Char(chr,ord)
emit x = if x < 128 then [chr x] else show x
script = [ "OR A T", "AND B T", "AND C T", "NOT T J", "AND D J",
           "NOT A T", "AND A T", -- T to false
           "OR E T", "OR H T", "AND T J",
           "RUN" ]
main = do
  m <- load <$> readFile "in/21.txt"
  let (_,output) = run m 0 0 $ ord <$> unlines script
  sequence_ $ putStr.emit <$> output
