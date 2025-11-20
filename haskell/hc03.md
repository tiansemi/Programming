# HC3 - Solutions des tâches

Ce document contient les solutions Haskell pour HC3T1 → HC3T10. Chaque tâche inclut la fonction demandée et un `main` de test indépendant.

---

## HC3T1 - Tâche 1 : Vérifier si un nombre est positif, négatif ou nul

```haskell
-- HC3T1

checkNumber :: Int -> String
checkNumber x =
    if x > 0 then "Positif"
    else if x < 0 then "Négatif"
    else "Zéro"

main :: IO ()
main = do
    putStrLn (checkNumber 5)
    putStrLn (checkNumber (-3))
    putStrLn (checkNumber 0)
```

---

## HC3T2 - Tâche 2 : Déterminer la note à partir d’un score avec des gardes

```haskell
-- HC3T2

grade :: Int -> String
grade n
    | n >= 90 = "A"
    | n >= 80 = "B"
    | n >= 70 = "C"
    | n >= 60 = "D"
    | otherwise = "F"

main :: IO ()
main = do
    putStrLn (grade 95)
    putStrLn (grade 72)
    putStrLn (grade 50)
```

---

## HC3T3 - Tâche 3 : Convertir une couleur RGB en chaîne hexadécimale avec let

```haskell
-- HC3T3

import Numeric (showHex)

rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
    let pad h = if length h == 1 then '0' : h else h
        rh = pad (showHex r "")
        gh = pad (showHex g "")
        bh = pad (showHex b "")
    in rh ++ gh ++ bh

main :: IO ()
main = do
    putStrLn (rgbToHex (255, 0, 127)) -- "ff007f"
    putStrLn (rgbToHex (0, 255, 64))  -- "00ff40"
```

---

## HC3T4 - Tâche 4 : Calculer l’aire d’un triangle avec la formule de Héron

```haskell
-- HC3T4

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

main :: IO ()
main = do
    print (triangleArea 3 4 5)   -- 6.0
    print (triangleArea 7 8 9)   -- 26.832815
```

---

## HC3T5 - Tâche 5 : Déterminer le type d’un triangle avec des gardes

```haskell
-- HC3T5

triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Équilatéral"
    | a == b || b == c || a == c = "Isocèle"
    | otherwise = "Scalène"

main :: IO ()
main = do
    putStrLn (triangleType 3 3 3)
    putStrLn (triangleType 5 5 8)
    putStrLn (triangleType 6 7 8)
```

---

## HC3T6 - Tâche avancée 6 : Vérifier une année bissextile avec if-then-else

```haskell
-- HC3T6

isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0
        then True
    else if year `mod` 100 == 0
        then False
    else if year `mod` 4 == 0
        then True
    else False

main :: IO ()
main = do
    print (isLeapYear 2000) -- True
    print (isLeapYear 1900) -- False
    print (isLeapYear 2024) -- True
```

---

## HC3T7 - Tâche avancée 7 : Déterminer la saison en fonction du mois avec des gardes

```haskell
-- HC3T7

season :: Int -> String
season m
    | m == 12 || m == 1 || m == 2 = "Hiver"
    | m == 3  || m == 4 || m == 5 = "Printemps"
    | m == 6  || m == 7 || m == 8 = "Été"
    | m == 9  || m == 10 || m == 11 = "Automne"
    | otherwise = "Mois invalide"

main :: IO ()
main = do
    putStrLn (season 3)
    putStrLn (season 7)
    putStrLn (season 11)
```

---

## HC3T8 - Tâche avancée 8 : Calculer l’IMC et retourner la catégorie avec where

```haskell
-- HC3T8

bmiCategory :: Float -> Float -> String
bmiCategory poids taille
    | bmi < 18.5 = "Insuffisance pondérale"
    | bmi < 25   = "Normal"
    | bmi < 30   = "Surpoids"
    | otherwise  = "Obésité"
  where
    bmi = poids / (taille * taille)

main :: IO ()
main = do
    putStrLn (bmiCategory 70 1.75)
    putStrLn (bmiCategory 90 1.8)
```

---

## HC3T9 - Tâche avancée 9 : Trouver le maximum de trois nombres avec let

```haskell
-- HC3T9

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
    let maxAB = if a > b then a else b
        maxABC = if maxAB > c then maxAB else c
    in maxABC

main :: IO ()
main = do
    print (maxOfThree 10 20 15)
    print (maxOfThree 5 25 10)
```

---

## HC3T10 - Tâche avancée 10 : Vérifier si une chaîne est un palindrome (récursion, gardes)

```haskell
-- HC3T10

isPalindrome :: String -> Bool
isPalindrome str
    | length str <= 1 = True
    | head str == last str = isPalindrome (init (tail str))
    | otherwise = False

main :: IO ()
main = do
    print (isPalindrome "racecar") -- True
    print (isPalindrome "haskell") -- False
    print (isPalindrome "madam")   -- True
```

---

Fin du document HC3. Tous les extraits ont un `main` pour permettre des tests indépendants.
