module ConfigTransform

import ConfigData
import Data.NEList
import NEListExtras

%access export


data RightCounter : a -> b -> Nat -> Type where
    MkRightCounter : RightCounter a b Z
    Left : a -> RightCounter a b n -> RightCounter a b n
    Right : b -> RightCounter a b n -> RightCounter a b (S n)

append : Either a b -> (n ** RightCounter a b n) -> (m ** RightCounter a b m)
append (Left x) (_ ** bundle) = (_ ** Left x bundle)
append (Right x) (_ ** bundle) = (_ ** Right x bundle)

data NameBundle : Nat -> Type where
    NilName : NameBundle Z
    (::) : RightCounter String () n -> NameBundle m -> NameBundle (m+n)

nameTokenToEither : NameToken -> Either String Unit
nameTokenToEither (TextNameToken text) = Left text
nameTokenToEither SubstituteNameToken = Right ()

valueTokenToEither : ValueToken -> Either String (NEList (NEList NameToken))
valueTokenToEither (TextValueToken text) = Left text
valueTokenToEither (ReferenceValueToken ref) = Right ref

total
substBundle : List NameToken -> (n ** RightCounter String Unit n)
substBundle = foldr append (0 ** MkRightCounter) . map nameTokenToEither

total
refBundle : List ValueToken -> (n ** RightCounter String (NEList (NEList NameToken)) n)
refBundle = foldr append (0 ** MkRightCounter) . map valueTokenToEither

total
toValueList : RightCounter String (NEList (NEList NameToken)) n -> List ValueToken
toValueList MkRightCounter = []
toValueList (Left text xs) = TextValueToken text :: toValueList xs
toValueList (Right ref xs) = ReferenceValueToken ref :: toValueList xs


concatenateTextTokens : List ValueToken -> List ValueToken
concatenateTextTokens [] = []
concatenateTextTokens [x] = [x]
concatenateTextTokens (ref@(ReferenceValueToken _) :: xs) = ref :: concatenateTextTokens xs
concatenateTextTokens (text@(TextValueToken _) :: xs@(ReferenceValueToken _ :: _)) = text :: concatenateTextTokens xs
concatenateTextTokens (TextValueToken text1 :: TextValueToken text2 :: xs) = concatenateTextTokens $ TextValueToken (text1 ++ text2) :: xs

isStar : NEList (NEList NameToken) -> Bool
isStar = isCons . filter isSubstitute . Data.NEList.toList . join

keyMatch : ConfigLine -> ConfigLine -> Bool
keyMatch (Payload key _ _ _) (Payload key' _ _ _) = key == key' && (not $ isStar key)
keyMatch _ _ = False

public export
record ConfigLineOverride where
    constructor MkOverride
    old: ConfigLine
    new: ConfigLine

toOverridesList : ConfigLine -> List ConfigLine -> List ConfigLineOverride
toOverridesList override = map $ flip MkOverride override

replaceOverride : ConfigLine -> ConfigLineOverride -> ConfigLineOverride
replaceOverride override x@(MkOverride base _) = if keyMatch base override then MkOverride base override else x

appendConfigLine : ConfigLine -> (List ConfigLine, List ConfigLineOverride) -> (List ConfigLine, List ConfigLineOverride)
appendConfigLine override (base, old) =
    (override :: deleteBy keyMatch override base,
     (map (replaceOverride override) old) ++ (toOverridesList override $ filter (keyMatch override) base))

overrideConfig' : (List ConfigLine, List ConfigLineOverride) -> (List ConfigLine, List ConfigLineOverride)
overrideConfig' ([], old) = ([], old)
overrideConfig' (override :: base, old) = appendConfigLine override (overrideConfig' (base, old))

overrideConfig : List ConfigLine -> (List ConfigLine, List ConfigLineOverride)
overrideConfig x with (overrideConfig' (reverse x, []))
    | (result, old) = (reverse result, old)

--substituteStars : List ConfigLine -> List ConfigLine