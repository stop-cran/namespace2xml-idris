module NEListExtras

import Data.NEList

%default total


disjoint : {xs : List a} -> [] = x :: xs -> Void
disjoint p = replace {P=disjointTy} p () where
    disjointTy : List a -> Type
    disjointTy [] = Unit
    disjointTy (x :: xs) = Void

toListStrict : NEList a -> DPair (List a) NonEmpty
toListStrict (MkNEList x xs) with (Data.NEList.toList (MkNEList x xs)) proof p
    | [] = absurd (disjoint p)
    | (y :: ys) = (y :: ys ** IsNonEmpty)

fromListStrict : DPair (List a) NonEmpty -> NEList a
fromListStrict (x :: xs ** _) = MkNEList x xs

reverseOntoStrict : (acc : List a) -> (xs : List a) -> NonEmpty acc -> DPair (List a) NonEmpty
reverseOntoStrict acc [] p = (acc ** p)
reverseOntoStrict acc (x :: xs) p = reverseOntoStrict (x :: acc) xs IsNonEmpty

nonEmptyConcat : NonEmpty xs -> NonEmpty (xs ++ ys)
nonEmptyConcat IsNonEmpty = IsNonEmpty

join' : NEList (NEList a) -> NEList a
join' xs with (toListStrict xs)
    | (y :: ys ** prf) with (map toListStrict (y :: ys)) proof p
        | [] = absurd (disjoint p)
        | (z :: zs) = fromListStrict (join (Prelude.Pairs.DPair.fst z :: map Prelude.Pairs.DPair.fst zs) ** nonEmptyConcat (snd z))

export
toListMaybe : Maybe (NEList a) -> List a
toListMaybe Nothing = []
toListMaybe (Just (MkNEList x xs)) = x :: xs

export
reverse : NEList a -> NEList a
reverse (MkNEList x xs) = fromListStrict $ reverseOntoStrict [x] xs IsNonEmpty

export
append : NEList a -> a -> NEList a
append xs x = fromListStrict $ reverseOntoStrict [x] (reverse (Data.NEList.toList xs)) IsNonEmpty

export
nepack : NEList Char -> String
nepack = pack . Data.NEList.toList

export
Applicative NEList where
    pure = singleton
    f <*> xs = join' $ map (flip map xs) f

export
Monad NEList where
    join = join'

export
Eq a => Eq (NEList a) where
    (MkNEList x xs) == (MkNEList y ys) = x == y && xs == ys