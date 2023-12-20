import Data.Char(isDigit)
import qualified Data.Map as M
data Part = Part {x,m,a,s::Int} deriving(Read,Show)
get 'x' = x; get 'm' = m; get 'a' = a; get 's' = s
chunked [] = []
chunked ("":rest) = chunked rest
chunked lns = let (chunk, rest) = break null lns in chunk : chunked rest
parseStep str
  | (xmas:'<':_) <- str = (\p -> [ to | get xmas p < n ])
  | (xmas:'>':_) <- str = (\p -> [ to | get xmas p > n ])
  | otherwise = (\_ -> [str])
  where
    (ns,(':':to)) = span isDigit $ drop 2 str
    n = read ns
parseRule ln = (name, (\p -> head $ concat $ (parseStep <$> steps) <*> [p]))
  where (name:steps) = words $ (\ch -> if ch=='{' || ch == ',' then ' ' else ch) <$> init ln
process ruleMap "A" part = [part]
process ruleMap "R" part = []
process ruleMap name part = process ruleMap (ruleMap M.! name $ part) part
partValue (Part x m a s) = x + m + a + s
main = do
  [ruleLns,partLns] <- (chunked.lines) <$> readFile "in/19.txt"
  let ruleMap = M.fromList $ map parseRule ruleLns
      parts = map (read . ("Part "++)) partLns
  print $ sum $ map partValue $ parts >>= process ruleMap "in"
