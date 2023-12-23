import Data.List(sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
((.*.) `on` f) x y = f x .*. f y
data Brick = Brick {xl,xh,yl,yh,zl,zh::Int} deriving(Eq,Ord,Show)
xyOverlap (Brick axl axh ayl ayh _ _) (Brick bxl bxh byl byh _ _)
  = axh >= bxl && bxh >= axl && ayh >= byl && byh >= ayl
supports (a@(Brick axl axh ayl ayh _ azh)) (b@(Brick bxl bxh byl byh bzl _))
  = xyOverlap a b && azh + 1 == bzl
dropAll [] revStack = reverse revStack
dropAll (((brick@(Brick bxl bxh byl byh bzl bzh))):bs) revStack =
    dropAll bs ((Brick bxl bxh byl byh (bzl+zadj) (bzh+zadj)):revStack)
  where zadj = maximum (0:beneath) + 1 - bzl
        beneath = map zh $ filter (xyOverlap brick) revStack
closure supportedBy supportees gone
  | gone' == gone = gone
  | otherwise = closure supportedBy supportees gone'
  where gone' = S.union gone $ S.fromList [ supportee | brick <- S.elems gone
                           , supportee <- supportees M.! brick
                           , (supportedBy M.! supportee) `S.isSubsetOf` gone ]
main = do
  lns <- lines <$> readFile "in/22.txt"
  let lns' = map (map (\ch -> if ch >= '0' && ch <= '9' then ch else ' ')) lns
      bricks0 = [ Brick (min x x') (max x x') (min y y') (max y y') (min z z') (max z z')
                | ln <- lns' , let [x,y,z,x',y',z'] = map read $ words ln ]
      dropped = dropAll (sortBy (compare `on` zl) bricks0) []
      supportedBy = M.fromListWith S.union
        [ (brick, S.fromList $ filter (`supports` brick) dropped)
        | brick <- dropped ]
      supportees = M.fromList [ (brick, filter (brick `supports`) dropped)
                              | brick <- dropped ]
      soleSupporters = S.fromList [ head $ S.elems supporters
                                  | (brick,supporters) <- M.assocs supportedBy
                                  , S.size supporters == 1 ]
  print $ length dropped - S.size soleSupporters -- part A
  print $ sum [ (S.size $ closure supportedBy supportees $ S.singleton sole) - 1
              | sole <- S.elems soleSupporters ] -- part B
