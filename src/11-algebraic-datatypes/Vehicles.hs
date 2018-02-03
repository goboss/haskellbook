module Vehicles where

data Price = Price Integer deriving (Eq, Show)

type Width = Integer
type Height = Integer
type Length = Integer

data Size = Size Width Height Length
  deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
  deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size
  deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir (Size 50 100 70)

-- Exercise 2
-- Given the following, define the functions.

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _            = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- Exercise 3
-- Now we’re going to write a function to tell us the manufacturer
-- of a piece of data.
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

