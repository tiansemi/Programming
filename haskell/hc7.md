# HC7 - Classes de types et types personnalisés

Solutions pour HC7T1 → HC7T10. Chaque tâche contient le code Haskell demandé et un `main` de test.

---

## HC7T1 - Tâche 1 : Implémenter une instance `Eq` pour `Color`

```haskell
-- HC7T1

data Color = Red | Green | Blue
    deriving (Show, Read, Enum, Bounded)

instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False

-- test
main :: IO ()
main = do
    print (Red == Green)   -- False
    print (Blue == Blue)   -- True
```

---

## HC7T2 - Tâche 2 : Implémenter `Ord` pour `Color` (Red < Green < Blue)

```haskell
-- HC7T2

instance Ord Color where
    compare Red   Red   = EQ
    compare Red   Green = LT
    compare Red   Blue  = LT
    compare Green Red   = GT
    compare Green Green = EQ
    compare Green Blue  = LT
    compare Blue  Red   = GT
    compare Blue  Green = GT
    compare Blue  Blue  = EQ

-- test
main :: IO ()
main = do
    print (Red < Green)   -- True
    print (Blue > Green)  -- True
```

---

## HC7T3 - Tâche 3 : Fonction avec contraintes multiples (`Eq` et `Ord`)

```haskell
-- HC7T3

compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

-- test
main :: IO ()
main = do
    print (compareValues 5 3)    -- 5
    print (compareValues 2 7)    -- 7
    print (compareValues 'a' 'b') -- 'b'
```

---

## HC7T4 - Tâche 4 : Type `Shape` avec `Show` et `Read`

```haskell
-- HC7T4

data Shape = Circle Double | Rectangle Double Double
    deriving (Show, Read)

-- test
main :: IO ()
main = do
    print (Circle 3.0)
    print (Rectangle 2.0 4.0)
```

---

## HC7T5 - Tâche 5 : Fonction utilisant la contrainte `Num`

```haskell
-- HC7T5

squareArea :: Num a => a -> a
squareArea side = side * side

main :: IO ()
main = do
    print (squareArea 3 :: Int)
    print (squareArea 2.5 :: Double)
```

---

## HC7T6 - Tâche 6 : Utiliser `Integral` et `Floating`

```haskell
-- HC7T6

-- prend un rayon de type Integral et retourne un Floating
circleCircumference :: (Integral a, Floating b) => a -> b
circleCircumference r = 2 * pi * (fromIntegral r)

main :: IO ()
main = do
    print (circleCircumference (3 :: Int) :: Double) -- ~18.8495
```

---

## HC7T7 - Tâche 7 : Utiliser `Bounded` et `Enum` pour `nextColor`

```haskell
-- HC7T7

nextColor :: Color -> Color
nextColor c
    | c == maxBound = minBound
    | otherwise      = succ c

-- test (minBound/maxBound fournis par deriving Bounded)
main :: IO ()
main = do
    print (nextColor Red)   -- Green
    print (nextColor Blue)  -- Red (wrap around)
```

---

## HC7T8 - Tâche 8 : Parser une valeur avec `Read`

```haskell
-- HC7T8

parseShape :: String -> Maybe Shape
parseShape s = case reads s of
    [(sh, "")] -> Just sh
    _            -> Nothing

-- tests
main :: IO ()
main = do
    print (parseShape "Circle 3.0")        -- Just (Circle 3.0)
    print (parseShape "Rectangle 2 5")    -- Just (Rectangle 2.0 5.0)
    print (parseShape "NotAShape")        -- Nothing
```

---

## HC7T9 - Tâche 9 : Définir une classe de type `Describable`

```haskell
-- HC7T9

class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True  = "Boolean: True"
    describe False = "Boolean: False"

instance Describable Shape where
    describe (Circle r)      = "Circle of radius " ++ show r
    describe (Rectangle w h) = "Rectangle " ++ show w ++ " x " ++ show h

-- test
main :: IO ()
main = do
    putStrLn (describe True)
    putStrLn (describe (Circle 2.5))
```

---

## HC7T10 - Tâche 10 : `describeAndCompare` avec contraintes multiples

```haskell
-- HC7T10

-- prend deux valeurs du même type a, qui sont Describable et Ord
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y = describe (if x >= y then x else y)

-- Pour tester on réutilise des types qui ont Ord et Describable, par ex. Shape
instance Eq Shape where
    (Circle r1)    == (Circle r2)    = r1 == r2
    (Rectangle w1 h1) == (Rectangle w2 h2) = w1 == w2 && h1 == h2
    _ == _ = False

instance Ord Shape where
    compare (Circle r1) (Circle r2) = compare r1 r2
    compare (Rectangle w1 h1) (Rectangle w2 h2) = compare (w1*h1) (w2*h2)
    compare (Circle r) (Rectangle w h) = compare (pi * r * r) (w*h)
    compare (Rectangle w h) (Circle r) = compare (w*h) (pi * r * r)

-- test
main :: IO ()
main = do
    putStrLn (describeAndCompare (Circle 2.0) (Circle 3.0))
    putStrLn (describeAndCompare (Rectangle 2 3) (Circle 2))
```

---

Fin du document HC7. Si tu veux des explications ligne par ligne pour une tâche précise, dis-moi laquelle.
