import Data.Char(isDigit)
eval s = uncurry evalMore $ opd s
opd ('(':rest) = eval rest
opd n = let (digs,rest) = span isDigit n in (read digs,rest)
evalMore x "" = (x,"")
evalMore x (')':rest) = (x,rest)
evalMore x ('+':rest) = let (y, rest') = opd rest in evalMore (x+y) rest'
evalMore x ('*':rest) = let (y, rest') = opd rest in evalMore (x*y) rest'
main = readFile "in/18.txt" >>= print . sum . map (fst . eval . filter (/=' ')) . lines
