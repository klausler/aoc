import qualified Data.Map as M
toTuple [a,b,c,d,e,f,g] = (b,c,d,e,f,g)
parse = toTuple . (read <$>) . words . map (\c -> if c >= '0' && c <= '9' then c else ' ')
simulate n (oo,co,obso,obsc,go,gobs) = fst $ sim n (0,0,0,0) (1,0,0,0) $ repeat M.empty
  where moR = maximum [oo, co, obso, go]
        mcR = obsc
        mobsR = gobs
        sim 0 (_,_,_,geodes) _ m = (geodes,m)
        sim n (os,cs,obss,gs) (robots@(oR,cR,obsR,gR)) (mstk@(m:ms))
          | Just geodes <- M.lookup key m = (geodes,mstk)
          | otherwise = (g5, M.insert key g5 m : m5)
          where key = (robots,if oR >= moR then min moR os else os,if cR >= mcR then min mcR cs else cs,if obsR >= mobsR then min mobsR obss else obss,gs)
                (os',cs',obss',gs') = (os+oR, cs+cR, obss+obsR, gs+gR)
                (g1,m1) = sim (n-1) (os',cs',obss',gs') robots ms
                (g2,m2) | oR < moR, os >= oo = let (g',m') = sim (n-1) (os'-oo,cs',obss',gs') (oR+1,cR,obsR,gR) m1 in if g' > g1 then (g', m') else (g1, m')
                        | otherwise = (g1,m1)
                (g3,m3) | cR < mcR, os >= co = let (g',m') = sim (n-1) (os'-co,cs',obss',gs') (oR,cR+1,obsR,gR) m2 in if g' > g2 then (g', m') else (g2, m')
                        | otherwise = (g2,m2)
                (g4,m4) | obsR < mobsR, os >= obso, cs >= obsc = let (g',m') = sim (n-1) (os'-obso,cs'-obsc,obss',gs') (oR,cR,obsR+1,gR) m3 in if g' > g3 then (g', m') else (g3, m')
                        | otherwise = (g3,m3)
                (g5,m5) | os >= go, obss >= gobs = let (g',m') = sim (n-1) (os'-go,cs',obss'-gobs,gs') (oR,cR,obsR,gR+1) m4 in if g' > g4 then (g', m') else (g4, m')
                        | otherwise = (g4,m4)
main = do
  bps <- ((parse <$>) . lines) <$> readFile "in/19.txt"
  print $ sum [ q * g | (q,g) <- zip [1..] $ simulate 24 <$> bps ] -- part A
  print $ product $ simulate 32 <$> take 3 bps -- part B
