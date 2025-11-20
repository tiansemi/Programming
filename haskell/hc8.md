# HC8 - Types de données, synonymes et enregistrements

Solutions pour HC8T1 → HC8T10. Chaque tâche contient le code Haskell demandé et un `main` de test.

---

## HC8T1 : Synonymes de type et fonction de base

```haskell
-- HC8T1

type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to amount = "From: " ++ from ++ ", To: " ++ to ++ ", Amount: " ++ show amount

main :: IO ()
main = do
    putStrLn (generateTx "Alice" "Bob" 100)
```

---

## HC8T2 : Types personnalisés et constructeurs de données

```haskell
-- HC8T2

data PaymentMethod = Cash | Card | Cryptocurrency
    deriving (Show)

data Person = Person { personName :: String, personAddress :: (String, Int), payment :: PaymentMethod }
    deriving (Show)

bob :: Person
bob = Person { personName = "Bob", personAddress = ("Main St", 42), payment = Cash }

main :: IO ()
main = do
    print bob
```

---

## HC8T3 : Types algébriques et fonctions (area)

```haskell
-- HC8T3

data Shape = Circle Float | Rectangle Float Float
    deriving (Show)

area :: Shape -> Float
area (Circle r)     = pi * r * r
area (Rectangle w h) = w * h

main :: IO ()
main = do
    print (area (Circle 5))       -- ~78.53982
    print (area (Rectangle 10 5)) -- 50.0
```

---

## HC8T4 : Syntaxe d'enregistrement pour Employee

```haskell
-- HC8T4

data Employee = Employee { name :: String, experienceInYears :: Float }
    deriving (Show)

richard :: Employee
richard = Employee { name = "Richard", experienceInYears = 7.5 }

main :: IO ()
main = do
    print richard
```

---

## HC8T5 : Syntaxe d'enregistrement pour Person

```haskell
-- HC8T5

data PersonRec = PersonRec { pname :: String, age :: Int, isEmployed :: Bool }
    deriving (Show)

person1 :: PersonRec
person1 = PersonRec { pname = "Alice", age = 30, isEmployed = True }

person2 :: PersonRec
person2 = PersonRec { pname = "Eve", age = 25, isEmployed = False }

main :: IO ()
main = do
    print person1
    print person2
```

---

## HC8T6 : Syntaxe d'enregistrement pour variantes Shape (cercles et rectangles)

```haskell
-- HC8T6

-- Les champs `center` et `color` sont partagés entre constructeurs
data ShapeR = CircleR { center :: (Float,Float), color :: String, radius :: Float }
            | RectangleR { center :: (Float,Float), color :: String, width :: Float, height :: Float }
    deriving (Show)

circleExample :: ShapeR
circleExample = CircleR { center = (0,0), color = "red", radius = 3 }

rectExample :: ShapeR
rectExample = RectangleR { center = (5,5), color = "blue", width = 10, height = 2 }

main :: IO ()
main = do
    print circleExample
    print rectExample
```

---

## HC8T7 : Types de données et description d'animaux

```haskell
-- HC8T7

data Animal = Dog String | Cat String
    deriving (Show)

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "Chien: " ++ name
describeAnimal (Cat name) = "Chat: " ++ name

dog1 :: Animal
cat1 :: Animal
dog1 = Dog "Rex"
cat1 = Cat "Mimi"

main :: IO ()
main = do
    putStrLn (describeAnimal dog1)
    putStrLn (describeAnimal cat1)
```

---

## HC8T8 : Synonymes de type et fonction de salutation

```haskell
-- HC8T8

type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Bonjour " ++ n ++ "! Tu as " ++ show a ++ " ans."

main :: IO ()
main = do
    putStrLn (greet "Paul" 28)
```

---

## HC8T9 : Type enregistrement Transaction et fonction associée

```haskell
-- HC8T9

type Address = String
type Value = Int

data Transaction = Transaction { from :: Address, to :: Address, amount :: Value, transactionId :: String }
    deriving (Show)

createTransaction :: Address -> Address -> Value -> String
createTransaction f t amt =
    let tid = f ++ "->" ++ t ++ ":" ++ show amt
        tx = Transaction { from = f, to = t, amount = amt, transactionId = tid }
    in transactionId tx

main :: IO ()
main = do
    putStrLn (createTransaction "Alice" "Bob" 250)
```

---

## HC8T10 : Deriving Show pour Book

```haskell
-- HC8T10

data Book = Book { title :: String, author :: String, year :: Int }
    deriving (Show)

myBook :: Book
myBook = Book { title = "Le Petit Prince", author = "Antoine de Saint-Exupéry", year = 1943 }

main :: IO ()
main = do
    print myBook
```

---

Fin du document HC8. Souhaites-tu que je génère des fichiers `.hs` individuels pour chaque exercice ?
