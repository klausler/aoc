module BitStream where
newtype Bits a = Bits { runBits :: [Int] -> (a, [Int]) }
getBits = Bits . splitAt
getInt n = do
  pulled <- getBits n
  pure $ sum $ zipWith (*) twoPows $ reverse pulled
twoPows = iterate (2*) 1
isEmpty = Bits (\bits -> (null bits, bits))
instance Functor Bits where
  fmap f p = Bits (\bits -> let (x, bits') = runBits p bits in (f x, bits'))
instance Applicative Bits where
  pure x = Bits (\bits -> (x, bits))
  p <*> q = Bits (\bits -> let (f, bits') = runBits p bits
                               (x, bits'') = runBits q bits' in (f x, bits''))
instance Monad Bits where
  p >>= q = Bits (\bits -> let (x, bits') = runBits p bits in (runBits $ q x) bits')
