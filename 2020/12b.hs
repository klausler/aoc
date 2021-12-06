step :: ((Int,Int),(Int,Int)) -> String -> ((Int,Int),(Int,Int))
step ((e,n),(we,wn)) ('F':v) = let t = read v in ((e + t * we, n + t * wn),(we,wn))
step (s,(we,wn)) ('E':v) = (s,(we + read v, wn))
step (s,(we,wn)) ('N':v) = (s,(we, wn + read v))
step (s,(we,wn)) ('W':v) = (s,(we - read v, wn))
step (s,(we,wn)) ('S':v) = (s,(we, wn - read v))
step (s,(we,wn)) "L90" = (s,(-wn,we))
step (s,(we,wn)) "L180" = (s,(-we,-wn))
step (s,(we,wn)) "L270" = (s,(wn,-we))
step (s,(we,wn)) "R90" = (s,(wn,-we))
step (s,(we,wn)) "R180" = (s,(-we,-wn))
step (s,(we,wn)) "R270" = (s,(-wn,we))
main = do
  txt <- readFile "in/12.txt"
  let ((e,n),_) = foldl step ((0,0),(10,1)) $ lines txt
  print (e,n,abs(e)+abs(n))
