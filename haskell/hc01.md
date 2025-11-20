# HC1 - Solutions des tâches

Ce document contient les solutions Haskell demandées pour HC1T1 → HC1T8. Les extraits de code sont prêts à être collés dans un fichier `.hs` et testés.

---

## HC1T1 - Tâche 1 : Composition de fonctions

```haskell
-- HC1T1

double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

-- Composition : d'abord double, puis increment
doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

main :: IO ()
main = do
    print (double 5)
    print (increment 10)
    print (doubleThenIncrement 7)
```

---

## HC1T2 - Tâche 2 : Fonction pure circleArea

```haskell
-- HC1T2

circleArea :: Float -> Float
circleArea r = pi * r * r

main :: IO ()
main = do
    print (circleArea 3)
    print (circleArea 5.5)
```

---

## HC1T3 - Tâche 3 : Vérifier si un nombre est > 18

```haskell
-- HC1T3

greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

main :: IO ()
main = do
    print (greaterThan18 10)
    print (greaterThan18 20)
```

---

## HC1T4 - Tâche 4 : Traitement des données de joueurs

```haskell
-- HC1T4

import Data.List (sortBy)
import Data.Ord (comparing)

extractPlayers :: [(String, Int)] -> [String]
extractPlayers xs = map fst xs

sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (flip (comparing snd))

topThree :: [(String, Int)] -> [(String, Int)]
topThree xs = take 3 (sortByScore xs)

getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree

main :: IO ()
main = do
    let players = [("Alice", 50), ("Bob", 80), ("Charlie", 60), ("Dora", 90)]
    print (getTopThreePlayers players)
```

---

## HC1T5 - Tâche 5 : Paresse et liste infinie

```haskell
-- HC1T5

infiniteNumbers :: [Int]
infiniteNumbers = [0..]

firstN :: Int -> [Int]
firstN n = take n infiniteNumbers

main :: IO ()
main = do
    print (firstN 10)
```

---

## HC1T6 - Tâche 6 : Fonction avec signature

```haskell
-- HC1T6

addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

main :: IO ()
main = do
    print (addNumbers 4 6)
```

---

## HC1T7 - Tâche 7 : Fahrenheit → Celsius

```haskell
-- HC1T7

fToC :: Float -> Float
fToC f = (f - 32) * 5 / 9

main :: IO ()
main = do
    print (fToC 32)
    print (fToC 100)
```

---

## HC1T8 - Tâche 8 : Fonction d'ordre supérieur

```haskell
-- HC1T8

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

main :: IO ()
main = do
    print (applyTwice (+1) 5)
    print (applyTwice (*2) 3)
```

---

Fin du document.
