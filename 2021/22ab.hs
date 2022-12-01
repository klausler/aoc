toCmd str = (onOff, pair $ read <$> wrap <$> (words $ f <$> rest))
  where
    (onOff,rest) = break (==' ') str
    f c = if c `elem` "-0123456789" then c else ' '
    wrap str = '(':str++")"
    pair [a,b,c,d,e,f] = ((a,b),(c,d),(e,f))
bite (a1,a2) (b1,b2)
  | b2 < a1 = ([(a1,a2)],[])
  | b1 > a2 = ([(a1,a2)],[])
  | b1 <= a1, b2 >= a2 = ([],[(a1,a2)])
  | b1 <= a1 = ([(b2+1,a2)],[(a1,b2)])
  | b2 >= a2 = ([(a1,b1-1)],[(b1,a2)])
  | b1 > a1, b2 < a2 = ([(a1,b1-1),(b2+1,a2)],[(b1,b2)])
remove (xs',ys',zs') (xs,ys,zs) =
    [ (sx,ys,zs) | sx <- safeX ] ++
    [ (bx,sy,zs) | bx <- badX, sy <- safeY ] ++
    [ (bx,by,sz) | bx <- badX, by <- badY, sz <- safeZ ]
  where
    (safeX,badX) = bite xs xs'
    (safeY,badY) = bite ys ys'
    (safeZ,_) = bite zs zs'
apply cubes ("off",cube) = cubes >>= remove cube
apply cubes ("on",cube) = cube : (cubes >>= remove cube)
size ((x1,x2),(y1,y2),(z1,z2)) = (x2-x1+1)*(y2-y1+1)*(z2-z1+1)
main = do
  lns <- lines <$> readFile "in/22.txt"
  let cmds = toCmd <$> lns
      part2 = foldl apply [] cmds
      part1 = foldl apply part2
        [("off",((-999999,-51),(-999999,999999),(-999999,999999)))
        ,("off",((51,999999),(-999999,999999),(-999999,999999)))
        ,("off",((-999999,999999),(-999999,-51),(-999999,999999)))
        ,("off",((-999999,999999),(51,999999),(-999999,999999)))
        ,("off",((-999999,999999),(-999999,999999),(-999999,-51)))
        ,("off",((-999999,999999),(-999999,999999),(51,999999)))]
  print $ sum $ size <$> part1
  print $ sum $ size <$> part2
