# HC4 - Solutions des tâches (pattern matching)

Ce document propose des solutions Haskell pour HC4T1 → HC4T8. Chaque tâche inclut une fonction demandée et un `main` de test.

---

## HC4T1 - Tâche 1 : Définir une fonction `weatherReport`

```haskell
-- HC4T1
weatherReport :: String -> String
weatherReport "sunny"  = "Il fait beau et ensoleill\233 !"
weatherReport "rainy"  = "N'oublie pas ton parapluie !"
weatherReport "cloudy" = "Un peu gris, mais pas de pluie pour l'instant !"
weatherReport _         = "M\233t\233o inconnue"

main :: IO ()
main = do
    putStrLn (weatherReport "sunny")
    putStrLn (weatherReport "rainy")
    putStrLn (weatherReport "windy")
```

---

## HC4T2 - Tâche 2 : Définir une fonction `dayType`

```haskell
-- HC4T2

dayType :: String -> String
dayType "Saturday" = "C'est le week-end !"
dayType "Sunday"   = "C'est le week-end !"
dayType "Monday"   = "C'est un jour de semaine."
dayType "Tuesday"  = "C'est un jour de semaine."
dayType "Wednesday"= "C'est un jour de semaine."
dayType "Thursday" = "C'est un jour de semaine."
dayType "Friday"   = "C'est un jour de semaine."
dayType _          = "Jour invalide"

main :: IO ()
main = do
    putStrLn (dayType "Saturday")
    putStrLn (dayType "Wednesday")
    putStrLn (dayType "Funday")
```

---

## HC4T3 - Tâche 3 : Définir `gradeComment`

```haskell
-- HC4T3

gradeComment :: Int -> String
gradeComment n
    | n >= 90 && n <= 100 = "Excellent !"
    | n >= 70 && n <= 89  = "Bon travail !"
    | n >= 50 && n <= 69  = "Tu as r\233ussi."
    | n >= 0  && n <= 49  = "Peut mieux faire."
    | otherwise           = "Note invalide."

main :: IO ()
main = do
    putStrLn (gradeComment 95)
    putStrLn (gradeComment 75)
    putStrLn (gradeComment 60)
    putStrLn (gradeComment 30)
    putStrLn (gradeComment 123)
```

---

## HC4T4 - Tâche 4 : R\233\233crire `specialBirthday` avec pattern matching

Voici une r\233\233criture simple avec des cas pr\233d\233finis (exemples).

```haskell
-- HC4T4

specialBirthday :: Int -> String
specialBirthday 0  = "Heureux premier anniversaire !"
specialBirthday 1  = "Bon premier anniversaire !"
specialBirthday 18 = "F\233licitations pour tes 18 ans !"
specialBirthday 21 = "Joyeux 21e anniversaire !"
specialBirthday 50 = "Joyeux demi-si\232cle !"
specialBirthday _  = "Joyeux anniversaire !"

main :: IO ()
main = do
    putStrLn (specialBirthday 1)
    putStrLn (specialBirthday 18)
    putStrLn (specialBirthday 30)
```

---

## HC4T5 - Tâche 5 : Ajouter un cas g\233n\233rique avec message personnalis\233

On enrichit le cas par d\233faut pour inclure l'\226ge dans le message.

```haskell
-- HC4T5

specialBirthday2 :: Int -> String
specialBirthday2 0  = "Heureux premier anniversaire !"
specialBirthday2 1  = "Bon premier anniversaire !"
specialBirthday2 18 = "F\233licitations pour tes 18 ans !"
specialBirthday2 21 = "Joyeux 21e anniversaire !"
specialBirthday2 50 = "Joyeux demi-si\232cle !"
specialBirthday2 age = "Joyeux anniversaire, tu as " ++ show age ++ " ans !"

main :: IO ()
main = do
    putStrLn (specialBirthday2 18)
    putStrLn (specialBirthday2 19)
```

---

## HC4T6 - Tâche 6 : Identifier le contenu d'une liste par pattern matching

```haskell
-- HC4T6

whatsInsideThisList :: [a] -> String
whatsInsideThisList []        = "La liste est vide"
whatsInsideThisList [x]       = "La liste contient un ":" ++ "\"" ++ "(valeur cach\233e)\"" -- on ne peut pas afficher `x` sans contrainte Show
whatsInsideThisList [x,y]     = "La liste contient deux \u00E9l\u00E9ments"
whatsInsideThisList (x:y:z:_) = "La liste contient au moins trois \u00E9l\u00E9ments"

-- Version avec Show pour afficher les valeurs
whatsInsideThisListShow :: Show a => [a] -> String
whatsInsideThisListShow []        = "La liste est vide"
whatsInsideThisListShow [x]       = "Un \u00E9l\u00E9ment: " ++ show x
whatsInsideThisListShow [x,y]     = "Deux \u00E9l\u00E9ments: " ++ show x ++ " et " ++ show y
whatsInsideThisListShow (x:y:z:_) = "Au moins trois \u00E9l\u00E9ments, premiers: " ++ show x ++ ", " ++ show y ++ ", " ++ show z

main :: IO ()
main = do
    putStrLn (whatsInsideThisListShow ([] :: [Int]))
    putStrLn (whatsInsideThisListShow [42])
    putStrLn (whatsInsideThisListShow [1,2])
    putStrLn (whatsInsideThisListShow [1,2,3,4])
```

---

## HC4T7 - T\226che 7 : Ignorer des \u00E9l\u00E9ments dans une liste

Retourner seulement le premier et le troisi\u00E8me \u00E9l\u00E9ment si pr\u00E9sent.

```haskell
-- HC4T7

firstAndThird :: [a] -> Maybe (a,a)
firstAndThird (x:_:z:_) = Just (x,z)
firstAndThird _         = Nothing

main :: IO ()
main = do
    print (firstAndThird [1,2,3,4]) -- Just (1,3)
    print (firstAndThird ["a","b","c"]) -- Just ("a","c")
    print (firstAndThird [1])        -- Nothing
```

---

## HC4T8 - T\226che 8 : Extraire des valeurs de tuples

```haskell
-- HC4T8

describeTuple :: (Show a, Show b) => (a,b) -> String
describeTuple (x,y) = "Le premier \u00E9l\u00E9ment est " ++ show x ++ ", le second est " ++ show y

main :: IO ()
main = do
    putStrLn (describeTuple (1, "Alice"))
    putStrLn (describeTuple (True, 3.14))
```

---

Fin du document HC4. Si tu veux que je regroupe tous les `main` dans un seul fichier ex\u00E9cutable avec un petit menu interactif pour lancer les tests, dis-le-moi.
