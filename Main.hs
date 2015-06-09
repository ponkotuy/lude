
import Prelude (print, Show, Eq, (+), ($), (==), (&&))

class Functor f where
    map :: (a -> b) -> f a -> f b

data Option a = Some a | None deriving (Show, Eq)

instance Functor Option where
    map _ None = None
    map f (Some a) = Some (f a)

data List a = Cons a (List a) | Nil deriving (Show, Eq)

instance Functor List where
    map _ Nil = Nil
    map f (Cons x xs) = Cons (f x) (map f xs)

-- infixr 4 :

-- (:) :: a -> List a -> List a
-- (:) = Cons

main = print $ listMap && opt
    where
      opt = mapNone && mapSome
      mapNone = (map (+ 1) None) == None
      mapSome = (map (+ 1) (Some 1)) == (Some 2)
      list = Cons 1 (Cons 2 (Cons 3 Nil))
      listMap = (map (+ 1) list) == (Cons 2 (Cons 3 (Cons 4 Nil)))
