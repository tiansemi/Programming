# HC2 - Solutions des tâches

Ce document contient les solutions pour HC2T1 → HC2T7. Chaque exercice comprend le code Haskell et un petit `main` pour tester.

---

## HC2T1 - Tâche 1 : Vérification des types dans GHCi

**Énoncé :** Ouvrez GHCi et vérifiez les types des expressions suivantes. Écrivez d'abord le type attendu.

**Expressions et types attendus (ce que `:t` de GHCi affichera généralement) :**

* `42`          → `42 :: Num a => a`  (littéral numérique polymorphe)
* `3.14`        → `3.14 :: Fractional a => a`  (littéral flottant polymorphe)
* `"Haskell"`  → `"Haskell" :: [Char]`  (alias `String`)
* `'Z'`         → `'Z' :: Char`
* `True && False`→ `True && False :: Bool`

> Remarque : selon le contexte, GHCi peut afficher un type plus précis (par ex. `Int`, `Double`) si il infère un type concret. Les littéraux numériques sont polymorphes (classe `Num` ou `Fractional`).

**Exemple de vérification en GHCi :**

```
Prelude> :t 42
42 :: Num p => p
Prelude> :t 3.14
3.14 :: Fractional p => p
Prelude> :t "Haskell"
"Haskell" :: [Char]
Prelude> :t 'Z'
'Z' :: Char
Prelude> :t True && False
True && False :: Bool
```

---

## HC2T2 - Tâche 2 : Signatures de fonctions

**Énoncé :** Écrire les signatures et implémentations des fonctions `add`, `isEven`, `concatStrings`.

```haskell
-- HC2T2

add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

main :: IO ()
main = do
    print (add 4 5)                   -- 9
    print (isEven 10)                 -- True
    putStrLn (concatStrings "Hi, " "Haskell") -- "Hi, Haskell"
```

---

## HC2T3 - Tâche 3 : Variables immuables

**Énoncé :** Définir des valeurs immuables `myAge`, `piValue`, `greeting`, `isHaskellFun`.

```haskell
-- HC2T3

myAge :: Int
myAge = 30

piValue :: Double
piValue = 3.141592653589793

greeting :: String
greeting = "Bonjour"

isHaskellFun :: Bool
isHaskellFun = True

-- tentative de modification (impossible en Haskell, provoque une erreur si décommentée)
-- myAge = 31  -- erreur : multiple definitions of `myAge`

main :: IO ()
main = do
    print myAge
    print piValue
    putStrLn greeting
    print isHaskellFun
```

> Remarque : les valeurs top-level en Haskell sont immuables. On ne peut pas les réassigner.

---

## HC2T4 - Tâche 4 : Notation préfixe et infixe

**Énoncé :** Convertir les expressions entre notation préfixe et infixe.

```haskell
-- HC2T4

-- Notation préfixe pour les expressions données :
-- 5 + 3    ==> (+) 5 3
-- 10 * 4   ==> (*) 10 4
-- True && False ==> (&&) True False

-- Notation infixe pour les fonctions données :
-- (+) 7 2  ==> 7 + 2
-- (*) 6 5  ==> 6 * 5
-- (&&) True False ==> True && False

main :: IO ()
main = do
    print ((+) 5 3)
    print ((*) 10 4)
    print ((&&) True False)
    print (7 + 2)
    print (6 * 5)
    print (True && False)
```

---

## HC2T5 - Tâche 5 : Définir et utiliser des fonctions

**Énoncé :** `circleArea` (Float -> Float) et `maxOfThree` (Int -> Int -> Int -> Int).

```haskell
-- HC2T5

circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

main :: IO ()
main = do
    print (circleArea 3.0)    -- approximativement 28.274334
    print (maxOfThree 10 20 15) -- 20
    print (maxOfThree 5 25 10)  -- 25
```

---

## HC2T6 - Tâche 6 : Comprendre Int vs Integer

**Énoncé et définitions :**

```haskell
-- HC2T6

smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127

main :: IO ()
main = do
    print (smallNumber :: Int)
    print (bigNumber :: Integer)
    -- Attention : (2^64) :: Int peut déborder selon l'implémentation et produire un résultat incorrect
    -- Exemple à essayer dans GHCi : 2^64 :: Int
    -- Sur beaucoup d'implémentations, Int est borné (habituellement 64 bits) et l'opération peut provoquer un overflow (wrap-around).
```

> Remarque : `Int` est un entier borné (taille fixe, typiquement 64 bits sur les systèmes modernes) tandis que `Integer` est un entier de précision arbitraire.

---

## HC2T7 - Tâche 7 : Expressions booléennes

**Énoncé :** Écrire des expressions booléennes qui évaluent aux valeurs demandées.

```haskell
-- HC2T7

-- True en utilisant &&
exprTrue :: Bool
exprTrue = True && True

-- False en utilisant ||
exprFalse :: Bool
exprFalse = False || False

-- True en utilisant not
exprNot :: Bool
exprNot = not False

-- Une comparaison qui retourne False
exprComp :: Bool
exprComp = 3 > 5  -- False

main :: IO ()
main = do
    print exprTrue
    print exprFalse
    print exprNot
    print exprComp
```

---

Fin du document HC2. Tous les extraits de code contiennent un `main` pour être testés indépendamment. Si tu veux que je concatène toutes les tâches dans un seul fichier Haskell exécutable avec un menu pour lancer chaque test, je peux le faire.
