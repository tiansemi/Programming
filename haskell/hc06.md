# HC6 – Exercices pratiques : Récursion et traitement de listes

Solutions complètes pour HC6T1 → HC6T10. Chaque exercice inclut un `main` pour tester.

---

## HC6T1 : Factorielle (récursif)

```haskell
-- HC6T1
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
    print (factorial 5) -- 120
    print (factorial 0) -- 1
```

---

## HC6T2 : Suite de Fibonacci (récursif)

```haskell
-- HC6T2
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = do
    print (fibonacci 6) -- 8
    print (fibonacci 10) -- 55
```

---

## HC6T3 : Somme avec foldr

```haskell
-- HC6T3
sumList :: Num a => [a] -> a
sumList = foldr (+) 0

main :: IO ()
main = do
    print (sumList [1,2,3,4,5]) -- 15
```

---

## HC6T4 : Produit avec foldl

```haskell
-- HC6T4
productList :: Num a => [a] -> a
productList = foldl (*) 1

main :: IO ()
main = do
    print (productList [1,2,3,4]) -- 24
```

---

## HC6T5 : Inverser une liste (récursif)

```haskell
-- HC6T5
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

main :: IO ()
main = do
    print (reverseList [1,2,3,4]) -- [4,3,2,1]
```

---

## HC6T6 : Existence d'un élément

```haskell
-- HC6T6
exists :: Eq a => a -> [a] -> Bool
exists _ [] = False
exists y (x:xs)
    | x == y    = True
    | otherwise = exists y xs

main :: IO ()
main = do
    print (exists 3 [1,2,3,4]) -- True
    print (exists 9 [1,2,3,4]) -- False
```

---

## HC6T7 : Taille d'une liste

```haskell
-- HC6T7
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

main :: IO ()
main = do
    print (listLength [1,2,3,4,5]) -- 5
```

---

## HC6T8 : Filtrer les nombres pairs

```haskell
-- HC6T8
filterEvens :: [Int] -> [Int]
filterEvens [] = []
filterEvens (x:xs)
    | even x    = x : filterEvens xs
    | otherwise = filterEvens xs

main :: IO ()
main = do
    print (filterEvens [1..10]) -- [2,4,6,8,10]
```

---

## HC6T9 : Implémentation de map

```haskell
-- HC6T9
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

main :: IO ()
main = do
    print (map' (*2) [1,2,3]) -- [2,4,6]
    print (map' show [10,20]) -- ["10","20"]
```

---

## HC6T10 : Récupérer les chiffres d'un nombre (récursif)

```haskell
-- HC6T10

getDigits :: Int -> [Int]
getDigits n
    | n < 10    = [n]
    | otherwise = getDigits (n `div` 10) ++ [n `mod` 10]

main :: IO ()
main = do
    print (getDigits 1234) -- [1,2,3,4]
    print (getDigits 907)  -- [9,0,7]
```

---

Fin du document HC6.
