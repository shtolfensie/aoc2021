import AOC
import Data.Array
import Data.List.Split


main = print "Not working!"
-- main = interactl $ f . map (map read . tail . splitOn "")

data Octopus = Octopus { energyLvl :: Int
                       , flashed   :: Bool
                       } deriving (Show)

type Grid = Array (Int, Int) Octopus

mapArray :: (Octopus -> Octopus) -> Grid -> Grid
mapArray f g = m f (0,0) g
  where
    m :: (Octopus -> Octopus) -> (Int, Int) -> Grid -> Grid
    m fn (i1, i2) g
      | i1 <= fst bs && i2 <= snd bs = m fn (i1, succ i2) (g // [((i1, i2), fn (g ! (i1,i2)))])
      | i1 <= fst bs = m fn (succ i1, 0) g
      | otherwise = g
    bs = snd $ bounds g

findFlashed :: Grid -> [(Int, Int)]
findFlashed g = m (0,0) g
  where
    m :: (Int, Int) -> Grid -> [(Int, Int)]
    m (i1, i2) g
      | i1 <= fst bs && i2 <= snd bs && energyLvl co >= 9 = (i1,i2) : m (i1, succ i2) g
      | i1 <= fst bs && i2 <= snd bs = m (i1, succ i2) g
      | i1 <= fst bs = m (succ i1, 0) g
      | otherwise = []
      where
        co = g!(i1,i2)
    bs = snd $ bounds g

newOctopus :: Int -> Octopus
newOctopus l = Octopus {energyLvl=l, flashed=False}

incEnergy :: Octopus -> Octopus
incEnergy o = Octopus {energyLvl=energyLvl o + 1, flashed=flashed o}

-- f :: [[Int]] -> Grid
f l = step oa
  where
    oa = toArr cs rs $ map (map newOctopus) l
    cs = length $ head l
    rs = length l

-- step :: Grid -> Grid
step g = fd
  where
    -- p1 = array (bounds g) $ map (\(is,o) -> (is, incEnergy o)) $ assocs g
    p1 = mapArray incEnergy g
    fd = findFlashed p1

-- stepAdj :: [(Int, Int)] -> Grid -> Grid
-- stepAdj fd g = foldl () g fd


