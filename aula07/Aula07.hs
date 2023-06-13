{-# LANGUAGE StandaloneDeriving #-}
module Aula07 where

import Prelude hiding ( Maybe
                      , Nothing
                      , Just
                      )

deriving instance Show Direction
deriving instance Show Point
deriving instance Show Shape
deriving instance Show IntList
deriving instance Show IntTree
deriving instance Show a => Show (Maybe a)
deriving instance Show Cli1
deriving instance Show Client

data Direction = North | South
               | East  | West

directionName :: Direction -> String
directionName North = "N"
directionName South = "S"
directionName East  = "E"
directionName West  = "W"

move :: (Int, Int) -> Direction -> (Int, Int)
move (x,y) North = (x, y + 1)
move (x,y) South = (x, y - 1)
move (x,y) East  = (x + 1, y)
move (x,y) West  = (x - 1, y)

data Point = Point Float Float

norm :: Point -> Float
norm (Point x y) = sqrt (x * x + y * y)

data Shape
   = Rectangle Point Float Float
   | Circle Point Float
   | Triangle Point Point Point

perimeter :: Shape -> Float
perimeter (Rectangle _ w h) = 2 * w + 2 * h
perimeter (Circle _ r) = 2 * pi * r
perimeter (Triangle a b c) = dist (a,b) + dist (b,c) + dist (c,a)

area :: Shape -> Float
area (Rectangle _ w h) = w * h
area (Circle _ r) = pi * r ^^ 2
area (Triangle a b c) = dist (a,b) + dist (b,c) + dist (c,a)

dist :: (Point, Point) -> Float
dist ((Point x1 y1), (Point x2 y2))
  = sqrt (x11 * x11 + y11 * y11)
   where
     x11 = x1 - x2
     y11 = y1 - y2

data IntList = INil | ICons Int IntList

data IntTree = ILeaf | INode Int IntTree IntTree

elemIntTree :: Int -> IntTree -> Bool
elemIntTree _ ILeaf = False
elemIntTree x (INode y l r)
  | x < y           = elemIntTree x l
  | x > y           = elemIntTree x r
  | otherwise       = True

treeEx = INode 2  (INode 1 ILeaf ILeaf) 
                  (INode 3 ILeaf ILeaf)

treeEx2 = ILeaf

treeEx3 = INode 2 (INode 1 ILeaf ILeaf) 
                  (ILeaf)

treeEx4 = INode 2 (INode 1 ILeaf 
                                  (INode 4 ILeaf ILeaf)) 
                  (INode 3 ILeaf ILeaf)

elemLeafTree :: IntTree -> (Int, Int)
elemLeafTree tree = elemLeafTree' tree 0 0

elemLeafTree' :: IntTree -> Int -> Int -> (Int, Int)
elemLeafTree' ILeaf elems leafs = (elems, leafs)
elemLeafTree' (INode _ ILeaf ILeaf) elems leafs = (elems, leafs)
elemLeafTree' (INode _ l r) elems leafs = concat (elemLeafTree' l (elems+1) (leafs+1))  
                                                 (elemLeafTree' r (elems+1) (leafs+1))
  where
    concat (x, y) (z, w) = (x+z, w+y)

concatIntList :: IntList -> IntList -> IntList
concatIntList INil         ys = ys
concatIntList (ICons x xs) ys = ICons x (concatIntList xs ys)

intTreeToList :: IntTree -> IntList
intTreeToList ILeaf         = INil
intTreeToList (INode x l r) = ICons x (concatIntList l' r')
    where
      l' = intTreeToList l
      r' = intTreeToList r

data Maybe a
  = Just a | Nothing

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

position :: Eq a => a -> [a] -> Maybe Int
position x xs = pos x xs 0
   where
     pos _ [] _        = Nothing
     pos x (y : ys) ac
       | x == y        = Just ac
       | otherwise     = pos x ys (ac + 1)

ex1 :: (String, String, Bool)
ex1 = ("José", "Silva", False)

type Name      = String
type Surname   = String
type SendOffer = Bool

type Cli = (Name, Surname, SendOffer)

ex2 :: Cli
ex2 = ("José", "Silva", False)

data Cli1
 = Cust Name Surname SendOffer

ex3 :: Cli1
ex3 = Cust "José" "Silva" False

greet :: Cli1 -> String
greet (Cust n _ _) = "Welcome, " ++ n ++ "!"

data Client
  = Customer {
      name    :: Name
    , surname :: Surname
    , offers  :: SendOffer
    }

client1 :: Client
client1 = Customer {
  name = "Jose",
  surname = "Silva",
  offers = False
}

client2 :: Client
client2 = Customer {
  name = "Raimundo",
  surname = "Pereira",
  offers = True
}

client3 :: Client
client3 = Customer {
  name = "Gonzaguinha",
  surname = "",
  offers = True
}

sendOffer :: [Client] -> [Client]
sendOffer clients = filter (\c -> offers c == True) clients
