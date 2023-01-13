import IntCode
import qualified Data.Set as S
draw _ _ set ever [] = ([], (set,ever))
draw (r,c) (dr,dc) set ever (color:turn:rest) = ((fromEnum $ S.member rc' set') : out', result)
  where set' = if color == 0 then S.delete (r,c) set else S.insert (r,c) set
        (dr',dc') = if turn == 0 then (-dc,dr) else (dc,-dr)
        rc' = (r+dr',c+dc')
        (out',result) = draw rc' (dr',dc') set' (S.insert (r,c) ever) rest
main = do
  m <- load <$> readFile "in/11.txt"
  let (inputA, (_, ever)) = draw (0,0) (-1,0) S.empty S.empty $ snd $ run m 0 0 (0 : inputA)
      (inputB, (final, _)) = draw (0,0) (-1,0) S.empty S.empty $ snd $ run m 0 0 (1 : inputB)
      (rs,cs) = unzip $ S.toList final
      [minR,minC,maxR,maxC] = [minimum, maximum] <*> [rs,cs]
  print $ S.size ever -- part A
  sequence [ putStrLn [ if S.member (r,c) final then '#' else '.' | c <- [minC..maxC]] | r <- [minR..maxR]] -- part B
