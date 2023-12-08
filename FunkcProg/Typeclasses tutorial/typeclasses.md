## DISCLAIMER - Nem hiszem hogy a helyes kifejezéseket használom ebben a szövegben, a célja az hogy segítsen az említett dolgok használatát megérteni a gyakorlatban. Viszont nem vagyok Haskell mágus, hiába csináltam meg a beadandót, ezért csak az én pontatlan értlmezésemet tudom átadni a témáról.

# Adattípusok példák (Data types)

Hozzunk létre egy egyszerű adattípust

```hs
data Mood = Happy | Bored | Okay | Sad
```

Mit is jelent ez a sor?
Deklaráltunk egy adattípust, aminek a neve `Mood`.
A `Mood`-ot 4 különböző konstruktorral deklaráltuk. A konstruktorokat mindig kell használni.
A `Mood` most egy típus, mint az Int vagy a Char, és lehet függvények és konstansok deklarálásában is használni

```hs
moodDuringLecture :: Mood
moodDuringLecture = Bored

-- Milyen lesz a Mood azután hogy kapok ZHra valami jegyet?
moodAfterGrade :: Int -> Mood
moodAfterGrade jegy
| jegy < 2 = Sad
| jegy < 4 = Okay
| otherwise = Happy
```

Csak azokat az értékeket tudja felvenni amiket mi megadtunk neki. Ezek most a Happy, Bored, Okay és Sad.
Az ilyen jó felsorolásokra, pl napok a hétben. Egy Bool-hoz hasonló adat típust is lehet deklarálni (csak ezt a két értéket veheti fel).

```hs
data MyBool = True | False
```

!!!!! Itt a True és a False csak konstruktorok, semmi közük az a Bool típusú True és False-hoz.

Így mellékesen megjegyezném hogy egyszerű típusokat is tudunk deklarálni ami leginkább az átláthatóságban segít.
Pl egy Point típus.

```hs
type Point = (Int, Int)
origin :: Point -- Így nem kell minding (Int,Int)-et írni, elég a Point, de a kettő ugyan az.
origin = (0,0)

origin2 :: (Int, Int)
origin2 = (0,0)
```

GHCI szerint `origin == origin2`, pedig más típusként deklaráltuk őket (igazából nem).

# Típusosztályok (typeclasses)

Náhány típusosztály a `Show`, `Eq`, és `Ord`
Mi is ez a `Show`? És az `Eq`? Itt a definíciójuknak egy része:

```hs
class Show a where
show :: a -> String

class Eq a where
(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool
```

Egy típusosztályt egy magára értelmezett függvények gyüjteményeként lehet tekinteni. A `Show`-ba pl definiálva van egy show függvény. Ezt használja a GHCI a kiíráshoz. Az `Eq`-ba (Az EQuivalence szóból, ami egyenértékűséget jelent) meg a == és /= operátorok vannak definiálva. Ez azt jelenti, hogy bármi ami az Eq-nak egy Instance-je (nem tudom mi lehet ez magyarul), arra értelmezve van az (==) és (/=).
Saját típusosztályt is lehet definiálni! Majd csinálunk egyet később.

De hogy lehet őket használni?

# Deriving

```hs
-- Syntax:
data Mood = Happy | Bored | Okay | Sad
  deriving Show
```

Mit is csináltunk most? Megmondtuk, hogy a `Mood` típus származtasson a `Show` típusosztályból, vagyis hogy a `Show` típusosztály függvényei legyenek értelmezve a Mood adattípusra. Ez azt jelenti hogy a `Show` függvényei használhatóak a Mood típusokra is.
Olyan mintha írtunk volna egy

```hs
show :: Mood -> String
```

függvényt. Amikor deriving-ot használunk, akkor valami alapértelmezett működése lesz definiálva a deriving-olt típusosztály függvényeinek.
Ez lehet hogy jó, lehet hogy nem. A show pl csak kiírja a konstruktort `String`-ként, majd az utána található pareméterekre is meghívja a show fgv-t.

# Adattípus változóval

Tegyük fel hogy van egy játékunk, amiben vannak statikus prop-ok (random tárgyak, pl kövek, növényzet, fák stb..), meg vannak NPC-k meg vannak Player karakterek.
Hozzunk létre teszt adattípusokat. Most más paramétereket nem adok meg mert nem szükséges a példához.

```hs
data Prop = Furniture | Decoration
  deriving Show
data NPC = QuestGiver | Friendly | Enemy
  deriving Show
type Health = Integer
type Level = Int
type Player = (Health, Level) -- Csak azért írok (Health, Level)-t hogy könnyebb legyen olvasni. Egyébkény teljesen ugyan az mintha (Integer, Int)-et írtam volna)
```

Ezek mind nagyon különböző adattípus szerkezetet igényelnek, viszont akarjuk ezeket mind mozgatni, és tárolni azt hogy hol vannak.
Létre tudunk hozni egy adattípust ami "becsomagolja" nekünk a Prop, NPC és Player típusú változókat, és hozzájuk rendel egy koordinátát.

```hs
type Location = (Int,Int)
data ObjectOnMap x = Object x Location
```

Itt azt mondtuk hogy ez az `ObjectOnMap` adattípus fogjon egy bármilyen típusú valamit (az `x`-t), és csomagolja be egy Locationnel együtt az Object konstruktorral. Ez akkor kell ha több típus lehet az x helyén. Ha csak egy típusú dolog lehet ott, akkor ne használj semmilyen változót, csak írd be a várt típust, pl így:

```hs
data ObjectOnMap = Object VártTípus Location

```

# Instance

### Minimáli definíció

Egy típusosztály, ahogy fentebb néztük, végülis egy magára értelmezett függvények gyüjteményeként lehet gondolni. Ami jó az, hogy ezekben a gyüjteményekben sok függvény egy másik, a típusosztályban definiált függvénnyel van definiálva.
Pl nézzük az Eq-t: írjuk be `ghci`-be hogy `:i Eq`. Ezt kapjuk az elején:

```hs
type Eq :: * -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
```

és utána egy csomó Eq instance-t (ezekre kitérünk mindjárt).
Szóval Eq-ba van egy `(==)` és egy `(/=)` függvény. De van egy `{-# MINIMAL (==) | (/=) #-}` sor is. Ez az Eq típusosztálynak az **elégséges meghatározása (minimal complete definition)**. Ez mit jelent? Hát azt, hogy ahhoz hogy tudd használni az összes függvényt az Eq-n belül, a `(==)` VAGY (a `|` jel jelöli a VAGY-t) a `(/==)` függvényt kell definiálni csak, nem mindkettőt. Azért csak az egyiket kell mert `(==) = not (/=)` vagy `(/=) = not (==)`.

Nézzünk még egy példát: `:t Foldable`:

```hs
class Foldable t where
  Data.Foldable.fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  Data.Foldable.foldMap' :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  Data.Foldable.foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  Data.Foldable.toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}
```

Itt kicsit több függvény van. Ha valami új, saját típusra akarnánk a Foldable függvényeit implementálni, elég sokáig tartana, de szerencsére itt is van minimális definíció: `{-# MINIMAL foldMap | foldr #-}`. Elég a `foldMap` vagy a `foldr` függvényeket definiálni.

## Instance Definition

Oké, most tudjuk hogy konkrétan milyen függvényt vagy függvényeket kell definiálni, de nem tudjuk azt hogy hogyan. Amikor használtuk a `:i Eq` parancsot GHCI-ben, láttunk a `{-# MINIMAL ... #-}` rész alatt egy csomó `instance`-el kezdődő sort. Nézzünk meg egyet: `instance Eq Integer -- Defined in ‘GHC.Num.Integer’`. Ez azt jelenti hogy `Integer` típusra értelmezve vannak az Eq függvények, mert létezik egy Eq Integer `instance`.
Aki valamennyire használt valami Objektum Orientált nyelvet, lehet ismeri az `instance` szót (magyarul a "példány" szót használnám erre). Nagy vonalakban annyi a lényeg, hogy amikor létrehozunk vagy típusosztályt, mint az `Eq`-t (azt nem mi hoztuk létre de mindegy), az végülis csak egy sablon/minta/terv. Annak egy megvalósítása az instance. Szóval létrehozunk egy `Eq` megvalósítást az `Integer` típusra, amivel aztán bármilyen `Eq` függvényt használhatunk Integerekre.
(Objektum orientál nyelvekben az instancek konkrét változók szoktak lenni, és a minták/sablonok meg az objectek, és az objectek alapján hozunk létre új változókat. Haskellben nem, az instance egy új típusra definiál egy függvény gyüjteményt)

Hát ez egy kicsit nehezebb téma volt... inkább nézzünk egy példát:

```hs
type Model = String
type Age = Int

-- Ez egy adattípus ami laptop adatokat táról.
data Laptop = Dell Model Age | Apple Model Age | Lenovo Model Age
```

Tegyük fel hogy az Eq függvényeket akarjuk implementálni erre az adattípusra. Az Eq minimális definicío mi is? `:i Eq`, és ott már leolvashatjuk hogy `(==) | (/=)`. Ezt a sablont kell követni: `instance "Típusosztály" "Típus" where "és ide a fügvényeket"`. A célunk legyen az hogy két laptop egyforma ha a Márka és a Model egyforma, a kor nem számít.

```hs
instance Eq Laptop where
  -- (==) deklarációja az Eq-ból:
  -- (==) :: a -> a -> Bool
  (==) (Dell model1 _) (Dell model2 _) = model1 == model2
  (==) (Apple model1 _) (Apple model2 _) = model1 == model2
  (==) (Lenovo model1 _) (Lenovo model2 _) = model1 == model2
  (==) _ _ = False
```

Mostmár ha használjuk a `==` függvényt `Laptop` típusú változókra, működni fog. De a `(/=)` is használható, pedig nem foglalkoztunk vele!

Kicsit bonyolultabb példa is legyen, használjuk a fentebbi `ObjectOnMap` adattípust, és definiáljuk rá az Eq-t. A cél az hogy két `ObjectOnMap` akkor legyen egyforma, ha az `x` egyforma és a `Location` is.

```hs
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

instance Eq (ObjectOnMap x) where -- az instance definícióba kell az az x változó amit az ObjectOnMap adattípus definícióban is használtunk.
  (==) (Object x location) (Object x2 location2) = x == x2 && location == location2
```

Ha ezt a részt bemásoljátok egy .hs fájlba, egy hibát fogtok kapni. Ugyanis az x itt bármi lehet, viszont használni akarjuk a `==` függvényt ahhoz hogy összehasonlítsuk őket. Ha az `ObjectOnMap`-be beraktuktunk volna egy NPC típust, ami nem az `Eq`-nak egy instance-je, nem tudna mit tenni a Haskell. Akkor valahogy meg kell mondani hogy "oké, ez az Eq instance csak azokra az `Object x`-ekre vonatkozzon, ahol az `x` típusa Eq-nak egy instance". Kijavítva:

```hs
instance Eq x => Eq (ObjectOnMap x) where
    (==) (Object x location) (Object x2 location2) = x == x2 && location == location2
```

Ugyanúgy kell típus megszorítást alkalmazni mint függvényeknél! Még egyszer: Azt mondtuk hogy hozzunk létre eg `Eq` instance-t az (ObjectOnMap x) típusra, ahol x egy olyan típus ami Eq-nak az instance-je.

He létrehoznánk egy új `ObjectOnMap` változót és egy `NPC` típusú változót rakunk bele, a `==` függvény már nem lenne értelmezve rá.

```hs
leeroyJenkins :: ObjectOnMap Player
leeroyJenkins = Object (100, 20) (0,0)
faker :: ObjectOnMap Player
faker = Object (100, 30) (0,0)

-- leeroyJenkins == faker     - False

legolas :: ObjectOnMap NPC
legolas = Object Friendly (0,0)
elrond :: ObjectOnMap NPC
elrond = Object Friendly (1,0)

-- legolas == elrond          - Error
```

Milyen hibát kapunk ha a `legolas == elrond` sort futtatjuk GHCI-be?

```
  <interactive>:13:9: error:
    • No instance for (Eq NPC) arising from a use of ‘==’
```

Hát azt amit vártunk. `NPC`-re nincs értelmezve az `Eq`, viszont az `ObjectOnMap`-re igen, de csak ha olyan változót kap, amire értelmezve van az `Eq`.

_Megjegyzés: Az Ord típus egyik feltétele hogy Eq már értelmezve legyen. Ezért csak olyan Típusokra lehet Ord instance-t csinálni, aminek már van Eq instance-je_

# Új típusosztályok

Ez már az utolsó rész lesz, esküszöm. Tudunk típusosztály instanceket létrehozni. Most csak az hiányzik hogy saját típusosztályt hozzunk létre. Tekintsünk vissza egy kicsit. A típusosztályra tekinthetünk úgy mint egy függvény gyűjtemény. Hozzunk létre egy típusosztályt, aminek a célja az mozgat entitásokat, mint pl az `ObjectOnMap` típusú változókat.

Kezdjük egyszerűen. Egy függvént amivel egy valami arréb tudunk mozgatni, és egy függvény ami megadja egy valaminek a helyét:

```hs
class Moveable a where
  getLocation :: a -> Location
  moveTo :: a -> Location -> a
```

kész is! Viszont így még nem csinál semmit. Létre kell hozni instanceket. Alapértelmezett definíciót nem lehet létrehozni, hogy csak `deriving`-olni kelljen az új típusosztályunkat (vagy legalábbis nem tudok róla.)

```hs
instance Moveable (ObjectOnMap a) where
  getLocation (Object _ location) = location
  moveTo (Object x location) newlocation = Object x newlocation

-- getLocation legolas                    - (0,0)
-- getLocation elrond                     - (1,0)
-- getLocation ( moveTo legolas (2,1))    - (2,1)
```

Remek! Tudunk mozgatni ObjectOnMap-eket. De igazából semennyivel sem vagyunk előrrébb mintha sima függvényeket hoztunk volna létre, például így:

```hs
getLocation :: ObjectOnMap x -> Location
getLocation (Object _ location) = location
moveTo :: ObjectOnMap x -> Location -> Location
moveTo (Object x location) newlocation = Object x newlocation
```

De ez csak ObjectOnMap típusokra vonatkozik, ezért ha létrehozunk egy új típust amit akarunk mozgatni, akkor teljesen új függvényeket kéne hívni. Új nevekkel meg minden. Mutatok példát:

```hs
-- Ez az Area adattípus lehet a játékon belül pl egy ház, amiben vannak bútorok, lámpák, stb.
-- Ennek is van egy Location-je, és tudhatnám mozgatni a Moveable típusosztály fügvényei segítségével.
type Size = Int
data Area = Indoor Size [Prop] Location | Outdoor Size [Prop] Location
  deriving Show

-- Mivel a moveTo függvénynév már foglalt ezért mást kell használnom:
moveAreaTo :: Area -> Location -> Location
moveAreaTo (Indoor x y location) newlocation = Indoor x y newlocation
moveAreaTo (Outdoor x y location) newlocation = Outdoor x y newlocation
getAreaLocation :: ObjectOnMap x -> Location
getAreaLocation (Indoor _ location) = location
getAreaLocation (Outdoor _ location) = location
```

Hát nem tudom ti hogy gondoljátok, de kicsit macerának tűnik. Jobb lenne ha egyszerűen tudnánk a `moveTo` függvényt használni bármire amit mozgatni akarunk, és hagyni a Haskellt hogy alkalmazza a helyes implementációt a típus alapján.

Akkor menjünk vissza a típusosztályos módszerhez, és hozzunk létre egy `Moveable` instance-t az `Area` adattípusra.

```hs
instance Moveable (ObjectOnMap a) where
  getLocation (Object _ location) = location
  moveTo (Object x location) newlocation = Object x newlocation

instance Moveable Area where
  getLocation (Indoor _ _ location) = location
  getLocation (Outdoor _ _ location) = location

  moveTo (Indoor size props location) newlocation = Indoor size props newlocation
  moveTo (Outdoor size props location) newlocation = Outdoor size props newlocation

  -- és most lehet hívni ezeket arra amire szeretnénk

  leeroyJenkins :: ObjectOnMap Player
  leeroyJenkins = Object (100, 20) (0,0)
  house :: Area
  house = Indoor 70 [] (20,4)

  -- getLocation house          - (20,4)
  -- getLocation leeroyJenkins  - (0,0)
```

Remek! One function to rule them all.
De tudod mi még hasznosabb! Újabb függvényt definiálni, ami automatikusan működik minden instance-re! Visszamegyek a Moveable típusosztály definícióba és írok egy `swapLocation` függvényt, ami megcseréli két valaminek a helyét.

```hs
class Moveable a where
  getLocation :: a -> Location
  moveTo :: a -> Location -> a
  --eddig itt tartottunk

  swapLocation :: (Moveable b) => (a,b) -> (a,b)
  -- Itt annyi történik hogy megmondom hogy az első valamit átmozgatom a második pozíciójára, a másodikat meg az első pozíciójára.
  -- a b-nek le kellett szűkíteni a típusát hogy csak Moveable legyen, mert meghívjuk rá a getLocation és a moveTo függvényeket, amik csak Moveable-re működnek
  -- az a-t azért nem kell szűkíteni mert a 'class Moveable a where' sorban már megtettük.
  swapLocation (object1,object2) = (moveTo object1 location2, moveTo object2 location1) where
    location1 :: Location
    location1 = getLocation object1
    location2 :: Location
    location2 = getLocation object2
```

Ezt a `swapLocation` függvényt már rögtön a típusosztály definícióban megírtuk. És képzeljétek, rögtön működik az `Area` és az `ObjectOnMap` adattípúsokra is! Miért? Azért mert a `getLocation` és `moveTo` függvényekkel lett definiálva!
Tulajdonképpen most a `Moveable` típusosztálunak 3 függvénye van, de a minimális teljes definíció egy instance-hez csak a `moveTo` és a `getLocation`. Ez elképesztően hasznos amikor egy típusosztálynak van vagy 8 instance-je és 8 külön definíció helyett megírhatsz egyet ami működik mindenre.
