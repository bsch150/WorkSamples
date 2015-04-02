--Thomas, Bryan, Garrett
--
module Traffic where
import Data.List
import Data.Fixed
import System.Random
import Data.Maybe
import RandomM

data Light = Light {ns::Integer,ew::Integer} deriving (Eq,Ord)

instance Show Light where
  show l = "    Light:\n        North/South = "++ show (ns l) ++ "\n        " ++ "East/West =" ++ show (ew l) ++ "\n"

data Street = Street {dest::String, time::Integer} deriving (Show, Eq, Ord)

data Direction = North | South | East | West deriving (Show, Eq)

data Intersection = Intersection {name::String,
                                  north::Maybe Street,
                                  south::Maybe Street,
                                  east::Maybe Street,
                                  west::Maybe Street} deriving (Eq,Ord)

instance Show Intersection where
  show inter = "name: " ++ show (name inter) ++
               "\n    north: " ++ show (north inter) ++
               "\n    south: " ++ show (south inter) ++
               "\n    east: " ++ show (east inter) ++
               "\n    west: " ++ show (west inter) ++"\n"

dI = Intersection "" Nothing Nothing Nothing Nothing

data Car = Car {inter::String, curTime::Integer,dir::Direction} deriving (Show)

type SingleTiming = (Intersection,Light)
type Timing = [SingleTiming]

makeCar :: String -> Direction -> Car
makeCar inter dir = Car inter 0 dir

lookupTiming :: String -> Timing -> Maybe SingleTiming
lookupTiming iName timings = find (\t -> name (fst t) == iName) timings

destIntersection :: Direction -> Intersection -> Timing -> Maybe String
destIntersection dir currInter t = do
    street <- streetOfDir dir currInter 
    let d = dest street
    (inter,light) <- lookupTiming d t
    return $ name inter 

streetOfDir :: Direction -> Intersection -> Maybe Street
streetOfDir North = north
streetOfDir South = south
streetOfDir East  = east
streetOfDir West  = west

timeToCross :: Intersection -> Direction -> Integer
timeToCross i dir = case streetOfDir dir i of
                    Nothing -> 0
                    Just x -> time x

period :: Light -> Integer
period l = ns l + ew l

isGreen :: Direction -> Light -> Integer -> Bool
isGreen dir light num
  |(time < (ns light))  = dir `elem` [North,South]
  |(time >= (ns light))  = dir `elem` [East,West]
    where time =  num `mod'` ((ns light) + (ew light))

timeToChange :: Light -> Integer -> Integer
timeToChange light num = let n =((ns light) - (num `mod` period light)) in
                          if n <= 0 then n + (ew light) else n

hasNext :: Direction -> Intersection -> Bool
hasNext dir inter = case streetOfDir dir inter of
                      Nothing -> False
                      Just x -> True

findWait :: Direction -> Integer -> Light -> Integer
findWait dir currTime light = if not $ isGreen dir light currTime 
                              then timeToChange light currTime + 10
                              else 0

timeToTravel :: Timing -> Car -> Integer
timeToTravel grid (Car iName time dir)
  |not (hasNext dir inter) = time + waitTime 
  |otherwise = let next = fromMaybe (error"given intersection name was not found in the timing") $ 
                                    destIntersection dir inter grid
                   car' = Car next (time + crossTime + waitTime) dir
               in timeToTravel grid car'
  where (inter,light) = fromMaybe (error"Timing not found") $ lookupTiming iName grid
        waitTime = findWait dir time light 
        crossTime = timeToCross inter dir 

findMaxTime :: [Car] -> Timing -> Integer
findMaxTime [] _ = 0
findMaxTime cars grid = maximum $ map (timeToTravel grid) cars

findBestTiming :: [Timing] -> [Car] -> Int -> (Integer,Timing)
findBestTiming timings cars n = minimum $ map (\t -> (findMaxTime cars t, t)) timings
