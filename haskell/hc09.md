# Haskell Chapitre 9 – Solutions

## HC9T1 : Synonyme de type paramétrique

```haskell
type Entity a = (String, a)
```

## HC9T2 : Type de données paramétrique Box

```haskell
data Box a = Empty | Has a deriving Show
```

## HC9T3 : Fonction addN

```haskell
addN :: Num a => a -> Box a -> Box a
addN n Empty     = Empty
addN n (Has x)   = Has (n + x)
```

## HC9T4 : Fonction extract

```haskell
extract :: a -> Box a -> a
extract def Empty   = def
extract _   (Has x) = x
```

## HC9T5 : Type Shape paramétrique avec enregistrement

```haskell
data Shape a
    = Circle  { color :: a, radius :: Float }
    | Rectangle { color :: a, width :: Float, height :: Float }
    deriving Show
```

## HC9T6 : Type récursif Tweet

```haskell
data Tweet = Tweet
    { content :: String
    , likes :: Int
    , comments :: [Tweet]
    } deriving Show
```

## HC9T7 : Fonction engagement

```haskell
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)
```

## HC9T8 : Type récursif Sequence

```haskell
data Sequence a = End | Node a (Sequence a) deriving Show
```

## HC9T9 : Fonction elemSeq

```haskell
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node v rest) = x == v || elemSeq x rest
```

## HC9T10 : Type BST (Binary Search Tree)

```haskell
data BST a = EmptyBST
           | NodeBST a (BST a) (BST a)
           deriving Show
```
