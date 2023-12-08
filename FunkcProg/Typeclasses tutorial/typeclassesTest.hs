module TypeClassesTest where
  type Model = String
  type Age = Int

  -- Ez egy adattípus ami laptop adatokat táról.
  data Laptop = Dell Model Age | Apple Model Age | Lenovo Model Age

  instance Eq Laptop where
  -- (==) deklarációja az Eq-ból:
  -- (==) :: a -> a -> Bool
    (==) (Dell model1 _) (Dell model2 _) = model1 == model2
    (==) (Apple model1 _) (Apple model2 _) = model1 == model2
    (==) (Lenovo model1 _) (Lenovo model2 _) = model1 == model2
    (==) _ _ = False

  data Prop = Furniture | Decoration
    deriving Show
  data NPC = QuestGiver | Friendly | Enemy
    deriving Show

  type Health = Integer
  type Level = Int
  type Player = (Health, Level)

  type Location = (Int,Int)
  data ObjectOnMap x = Object x Location
    deriving Show

  instance Eq x => Eq (ObjectOnMap x) where
    (==) (Object x location) (Object x2 location2) = x == x2 && location == location2

  leeroyJenkins :: ObjectOnMap Player
  leeroyJenkins = Object (100, 20) (0,0)
  faker :: ObjectOnMap Player
  faker = Object (100, 30) (0,0)

  elf :: NPC
  elf = Friendly

  legolas :: ObjectOnMap NPC
  legolas = Object elf (0,0)
  elrond :: ObjectOnMap NPC
  elrond = Object elf (1,0)

  class Moveable a where
    getLocation :: a -> Location
    moveTo :: a -> Location -> a

    swapLocation :: (Moveable b) => (a,b) -> (a,b)
    swapLocation (object1,object2) = (moveTo object1 location2, moveTo object2 location1) where
      location1 :: Location
      location1 = getLocation object1
      location2 :: Location
      location2 = getLocation object2

  instance Moveable (ObjectOnMap a) where
    getLocation (Object _ location) = location
    moveTo (Object x location) newlocation = Object x newlocation

  type Size = Int
  data Area = Indoor Size [Prop] Location | Outdoor Size [Prop] Location
    deriving Show

  instance Moveable Area where
    getLocation (Indoor _ _ location) = location
    getLocation (Outdoor _ _ location) = location

    moveTo (Indoor size props location) newlocation = Indoor size props newlocation
    moveTo (Outdoor size props location) newlocation = Outdoor size props newlocation

  house :: Area
  house = Indoor 70 [] (20,4)