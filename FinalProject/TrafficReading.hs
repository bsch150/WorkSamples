module TrafficReading where
import Traffic
import Data.List (partition,elemIndex)
import Data.List.Split (splitOn)

parseFile :: String -> ([Intersection], [Car])
parseFile str = let strByLine = lines str 
                    splitLines = map (\s -> splitOn "," s) strByLine
                    (strCars,strInters) = partition (\x -> head x == "Car") splitLines
                in (map parseInter strInters, map parseCar strCars)

parseDirection :: String -> Direction
parseDirection dir  
   | dir == "north" || dir == "North" = North
   | dir == "south" || dir == "South" = South
   | dir == "west" || dir == "West" = West 
   | dir == "East" || dir == "east" = East
   | otherwise =  error("Not a good direction "++dir)

parseCar :: [String] -> Car
parseCar lst = let dir = parseDirection (lst!!2)
                   startInter = (lst!!1) 
               in makeCar startInter dir

parseInter :: [String] -> Intersection
parseInter lst = dI {name = (lst!!1),north = parseInterHelper lst "north" ,south = parseInterHelper lst "south", west = parseInterHelper lst "west", east = parseInterHelper lst "east"} 

parseInterHelper :: [String] -> String -> Maybe Street
parseInterHelper lst dir = do 
                ind <- dir `elemIndex` lst
                return (Street (lst!!(ind -1)) (read (lst!!(ind + 1))))

csvOfSingleTiming :: SingleTiming -> String
csvOfSingleTiming (inter,light) = (name inter) ++ "," ++ (show $ ns light) ++ "," ++ (show $ ew light)

prettyPrint :: Bool -> (Integer, Timing) -> String
prettyPrint showTime (duration, timing) =
    if showTime then (show (duration)) ++ "\n" else unlines $ map csvOfSingleTiming timing
