module TParsecExtras

import Control.Monad.State
import TParsec
import TParsec.Running
import Data.NEList
import NEListExtras

%default total
%access public export


Parser' : Type -> Type
Parser' t = All (Parser TParsecU Types.chars t)

sepBy : (Alternative mn, Monad mn) =>
        All (Parser mn p a :-> Parser mn p b :-> Parser mn p (NEList a))
sepBy p sep = map (uncurry cons') $ andopt p (nelist (sep `rand` p)) where
    cons' x xs = MkNEList x (toListMaybe xs)

exactChar : Char -> Parser' Char
exactChar = exact {p=Types.chars}

anyChar : Parser' Char
anyChar = anyTok {p=Types.chars}

noneCharOf : String -> Parser' Char
noneCharOf s = guard (not . flip elem (unpack s)) anyChar

anyCharOf : String -> Parser' Char
anyCharOf = anyOf . unpack
