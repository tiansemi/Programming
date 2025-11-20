# HC5 - Exercices pratiques (Chapitre 5)

Solutions pour HC5T1 → HC5T10. Chaque tâche inclut la fonction demandée et un `main` pour tester.

---

## HC5T1 : Utiliser applyTwice

Définir une fonction qui prend une fonction et un entier, puis applique la fonction trois fois à l’entier.

```haskell
-- HC5T1

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- applique trois fois en réutilisant applyTwice
applyThrice :: (a -> a) -> a -> a
applyThrice f x = f (applyTwice f x)

main :: IO ()
main = do
    print (applyThrice (+1) 5)   -- 8
    print (applyThrice (*2) 3)   -- 24
```

---

## HC5T2 : Filtrer les nombres impairs

Utiliser `filter` pour extraire tous les nombres impairs d’une liste de 1 à 30.

```haskell
-- HC5T2

oddNumbers1to30 :: [Int]
oddNumbers1to30 = filter odd [1..30]

main :: IO ()
main = do
    print oddNumbers1to30
```

---

## HC5T3 : Vérifier la présence de majuscules

Écrire une fonction utilisant `any` qui vérifie si une liste de mots contient au moins un mot commençant par une majuscule.

```haskell
-- HC5T3

startsWithUpper :: [String] -> Bool
startsWithUpper ws = any startsUpper ws
  where
    startsUpper w = case w of
        (c:_) -> c >= 'A' && c <= 'Z'
        _     -> False

main :: IO ()
main = do
    print (startsWithUpper ["hello", "world"])        -- False
    print (startsWithUpper ["hello", "World"])        -- True
    print (startsWithUpper ["Bonjour", "salut"])      -- True
```

---

## HC5T4 : Utiliser les fonctions lambda

Réécrire la fonction suivante en utilisant une fonction lambda :

```haskell
-- HC5T4

-- version nommée
biggerThan10 :: Int -> Bool
biggerThan10 x = x > 10

-- version lambda (identique)
biggerThan10Lambda :: Int -> Bool
biggerThan10Lambda = \x -> x > 10

main :: IO ()
main = do
    print (biggerThan10 12)
    print (biggerThan10Lambda 8)
```

---

## HC5T5 : Application partielle

Créer `multiplyByFive` qui utilise l’application partielle pour multiplier par 5.

```haskell
-- HC5T5

multiplyByFive :: Num a => a -> a
multiplyByFive = (*5)

main :: IO ()
main = do
    print (multiplyByFive 7)
    print (multiplyByFive 2.5)
```

---

## HC5T6 : Composition de fonctions

Créer une fonction qui prend une liste de nombres et retourne leurs carrés filtrés pour ne garder que les pairs (utiliser la composition `.`).

```haskell
-- HC5T6

evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (\x -> x * x)

main :: IO ()
main = do
    print (evenSquares [1..10]) -- [4,16,36,64,100]
```

---

## HC5T7 : L’opérateur $

Réécrire la fonction suivante en utilisant `$` :

```haskell
-- HC5T7

result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

main :: IO ()
main = do
    print result -- 88
```

---

## HC5T8 : Style point-free

Convertir `addFive x = x + 5` en style point-free.

```haskell
-- HC5T8

addFive :: Num a => a -> a
addFive = (+5)

main :: IO ()
main = do
    print (addFive 10)
```

---

## HC5T9 : Fonction d’ordre supérieur pour transformer une liste

Écrire `transformList` qui applique deux fois une fonction donnée à chaque élément d’une liste.

```haskell
-- HC5T9

applyTwice' :: (a -> a) -> a -> a
applyTwice' f x = f (f x)

transformList :: (a -> a) -> [a] -> [a]
transformList f = map (applyTwice' f)

main :: IO ()
main = do
    print (transformList (+1) [1,2,3]) -- [3,4,5]
    print (transformList (*2) [1,2,3]) -- [4,8,12]
```

---

## HC5T10 : Combiner filter, map et any

Créer une fonction qui vérifie si une valeur au carré dans une liste est supérieure à 50. On combine `filter`, `map` et `any`.

```haskell
-- HC5T10

-- La stratégie : filtrer les éléments dont le carré dépasse 50,
-- puis prendre leurs carrés et vérifier s'il en existe au moins un.

anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 = any (>50) . map (\x -> x * x) . filter (\x -> x * x > 50)

-- équivalent (plus direct) : any (\x -> x*x > 50) list

main :: IO ()
main = do
    print (anySquareGreaterThan50 [1..7])   -- False (7^2 = 49)
    print (anySquareGreaterThan50 [1..8])   -- True  (8^2 = 64)
    print (anySquareGreaterThan50 [10,2,3]) -- True
```

---

Fin du document HC5. Si tu veux, je peux générer des fichiers `.hs` séparés pour chaque tâche ou regrouper tous les `main` dans un seul exécutable interactif.
