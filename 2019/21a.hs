import IntCode
import Data.Char(chr,ord)
emit x = if x < 128 then [chr x] else show x
script = [ "OR A T", "AND B T", "AND C T", "NOT T J", "AND D J", "WALK" ]
main = do
  m <- load <$> readFile "in/21.txt"
  let (_,output) = run m 0 0 $ ord <$> unlines script
  sequence_ $ putStr.emit <$> output
