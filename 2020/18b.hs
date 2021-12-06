import Data.Char(isDigit)
eval s = let (n,s') = evalS s in evalM n s'
evalM n ('*':s) = let (n',s') = evalS s in evalM (n*n') s'
evalM n s = (n,s)
evalS s = let (n,s') = opd s in evalS' n s'
evalS' n ('+':s) = let (n',s') = opd s in evalS' (n+n') s'
evalS' n s = (n,s)
opd ('(':rest) = let (n,')':rest') = eval rest in (n, rest')
opd n = let (digs,rest) = span isDigit n in (read digs,rest)
main = readFile "input18.txt" >>= print . sum . map (fst . eval . filter (/=' ')) . lines
