
import Prelude (print, Show, Eq, (.), (+), ($), (==), (&&), Char, Int, Bool(True, False), not)

class Functor f where
    map :: (a -> b) -> f a -> f b

class FlatMap f where
    flatMap :: (a -> f b) -> f a -> f b

class Empty a where
    empty :: a

class (Eq a, Empty a) => IsEmpty a where
    isEmpty :: a -> Bool
    isEmpty x = x == empty

    nonEmpty :: a -> Bool
    nonEmpty = not . isEmpty

listJoin :: List a -> List a -> List a
listJoin Nil Nil = Nil
listJoin Nil xs = xs
listJoin xs Nil = xs
listJoin (Cons x xs) a = Cons x (xs ++ a)

class Traversable t where
    toList :: (t a) -> List a
    (++) :: (t a) -> (t a) -> List a
    xs ++ ys = listJoin (toList xs) (toList ys)

data Option a = Some a | None
                deriving Show

instance Functor Option where
    map _ None = None
    map f (Some a) = Some (f a)

instance FlatMap Option where
    flatMap _ None = None
    flatMap f (Some a) = f a

instance Empty (Option a) where
    empty = None

instance Eq (Option a) where
    None == None = True
    None == Some x = False
    Some x == None = False
    Some x == Some y = True

instance IsEmpty (Option a)

data List a = Cons a (List a) | Nil
              deriving (Show, Eq)

instance Functor List where
    map _ Nil = Nil
    map f (Cons x xs) = Cons (f x) (map f xs)

instance Traversable List where
    toList xs = xs

instance FlatMap List where
    flatMap _ Nil = Nil
    flatMap f (Cons x xs) = (f x) ++ (flatMap f xs)

instance Empty (List a) where
    empty = Nil

main = print $ listMap && opt
    where
      opt = mapNone && mapSome && optNonEmpty
      mapNone = isEmpty $ map (+ 1) None
      mapSome = (map (+ 1) (Some 1)) == (Some 2)
      optNonEmpty = nonEmpty $ Some 1
      list = Cons 1 (Cons 2 (Cons 3 Nil))
      listMap = (map (+ 1) list) == (Cons 2 (Cons 3 (Cons 4 Nil)))
