module Timings where
import Traffic
import RandomM

randLight :: Intersection -> (Integer,Integer) -> RandomM SingleTiming
randLight inter range = do
        first  <- randomRangeM range
        second <- randomRangeM range
        return (inter, Light first second)

randTiming :: [Intersection] -> (Integer,Integer) -> RandomM Timing
randTiming inters range = sequence $ map (\i -> randLight i range) inters

randNTimings :: Int -> [Intersection] -> (Integer,Integer) -> RandomM [Timing]
randNTimings n inters range = sequence $ replicate n (randTiming inters range)
