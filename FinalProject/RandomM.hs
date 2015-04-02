module RandomM where
  import System.Random
  
  data RandomM a = R (StdGen -> (a, StdGen))
  
  evalRand :: RandomM a -> StdGen -> a
  evalRand (R f) g = fst $ f g
  
  randomM :: Random a => RandomM a
  randomM = R $ \g -> (random g)
  
  randomRangeM :: Random a => (a, a) -> RandomM a
  randomRangeM range = R $ \g -> (randomR range g)
  
  randomElem :: [a] -> RandomM a
  randomElem lst = do
       let x = length lst
       index <- randomRangeM (0, x-1)
       return (lst!!index)

  instance Monad RandomM where
     return x = R (\g -> (x, g))
     (R r1) >>= f = R $ \g -> let (av, g') = r1 g
                                  (R r2)   = f av
                                  (bv, g'')= r2 g'
                              in (bv, g'')
     (R r1) >> (R r2) = R $ \g -> let (av, g') = r1 g
                                      (bv, g'')= r2 g'
                                  in (bv, g'')
