
-- data Maybe a = Nothing | Just a
-- maybeMap :: (a->b) -> Maybe a -> Maybe b
-- maybeMap f (Just a) = Just (f a)
-- maybeMap f Nothing = Nothing

--functorii sunt o clasa de tipuri care implementeaza map

{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-- f a = un a incapsulat in alt tip
-}

-- daca am scrie 
-- fmap : : ( a -> b ) -> List a -> List b
-- e fix definitia de la map
-- deci noi cu functori doar generalizam



-- obiecte in obiecte      -   tipuri in tipuri  (ca orice constructor de tip)
-- morfisme in morfisme    -  functii in functii (chiar fmap)

--List e un functor, in ce sens trimite List tipuri in alte tipuri?
--  List aplicat lui Int intoarce tipul List Int

-- FUNCTOR = CLASA CARE TRIMITE FUNCTII IN ALTE FUNCTII, ARE PROPR DE A TRIMITE TIPURI IN TIPURI







newtype Identity a = Identity a --hai sa aratam ca e un functor

instance Functor Identity where 
    -- fmap :: (a->b) -> Identity a -> Identity b
    -- x este de tipul a de fapt, Identity este constructorul
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity x) = Identity $ f x









data Pair a = Pair a a


instance Functor Pair where
    --fmap :: (a -> b) -> (Pair a -> Pair b)
    fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (Pair x y) = Pair (f x) (f y)







data Constant a b = Constant b
--constant din stanga e un constructor de tip
--ceea ce inseamna ca ia niste parametrii si intoarce un tip nou
-- Constant din dreapta este un constructor

-- Constant :: Type -> Type -> Type
--              a       b       Constant a b

--class Functor f where
--  fmap :: (a->b) -> (f a -> f b)
instance Functor (Constant t) where
    --fmap :: (a -> b) -> Constant t a  -> Constant t b
    --                         x :: a               f x :: b
    fmap f (Constant x) = Constant ( f x)







data Two a b = Two a b

instance Functor (Two t) where
    -- fmap:: (a->b) -> Two t a -> Two t b
    --                  x :: t
    --                  y :: a
    -- f:: a -> b
    -- nu pot sa aplic functia f la t(ipul lui x)
    --f stie sa mapeze un a, adica y in contextul nostru
    fmap f (Two x y) = Two x (f y)






data Three a b c = Three a b c
--tb sa ramana un singu tip de aplicat
instance Functor (Three a b) where
    --fmap::( c -> d ) -> Three a b c -> Three a b d
    fmap f (Three x y z) = Three x y (f z)





data Three' a b = Three' a b b
instance Functor (Three' a) where
    -- fmap :: ( c -> d ) -> Three' a c --> Three' a d
    --      y,z :: c
    --      x:: a
    fmap f (Three' x y z ) = Three' x (f y) (f z)
-- Three' x y z --> aici facem pattern matching cu Three' a b b




--maparea mereu se face pe ultima val incapsulata
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    --fmap :: (d -> e) --> Four a b c d --> Four a b c e
    fmap f (Four x y z w) = Four x y z (f w)




data Four'' a b = Four'' a a a b

instance Functor (Four'' a) where
    --fmap ::(b->c) -> Four'' a b -> Four'' a c
    -- x::a
    -- y:: a
    -- z::a
    -- w::b
    fmap f (Four'' x y z w) = Four'' x y z (f w)






data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    --fmap :: (b->c) -> Quant a b -> Quant a c
    fmap f Finance = Finance
    -- x:: a
    -- f :: b-> c
    fmap _ (Desk x) = Desk x
    fmap f (Bloor x) = Bloor (f x)






data LiftItOut f a = LiftItOut (f a)
-- f a ::Type
--LiftItOut ia un singur argument de tipul ( f a)
-- f este un constructor de tipuri
-- f::Type -> Type
-- aici avem o dubla incapsulare
-- capsula nr 1 este (f a) iar incapsulare nr 2 este LiftItOut( f a)

--instantiem doar pt acei f care sunt si eu Functori, asa am definit o constrangere
instance (Functor f) => Functor (LiftItOut f) where
    --fmap :: (a -> b) -> LiftItOut f a -> LiftItOut f b
    -- func :: a-> b
    -- x :: f a
    -- fmapPentru_f :: ( a->b) -> f a -> f b
    --                                  acest fmap de mai jos este cel oferit de instanta (Functor f)
    --                                      fmap::(a->b) -> f a -> f b
    fmap func (LiftItOut x) = LiftItOut (fmap func x)







data Parappa f g a = DaWrappa (f a) (g a)
-- f si g sunt ei insusi constructori de tip

instance (Functor f, Functor g) => Functor (Parappa f g) where
    -- fmap:: ( a -> b) -> Parappa f g a -> Parappa f g b
    -- x:: f a
    -- y:: g a
    --                      Parappa f g b obtinut prin DaWrappa aplicat pe un (f b) si un (g b)
    --                                  fmap func x il avem prin constrangerea Functor f
    -- fmap func y il avem prin constrangerea Functor g
    fmap func (DaWrappa x y) =  DaWrappa (fmap func x) (fmap func y)






data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance ( Functor g) => Functor (IgnoreOne f g a) where
    --fmap::(b->c) ->IgnoreOne f g a b -> IgnoreOne f g a c
    --                                                   g c
    fmap func (IgnoringSomething x y) =  IgnoringSomething x (fmap func y)






data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
    --fmap::(b->c) -> Notorious g o a b -> Notorious g o a c
    fmap func (Notorious x y z) = Notorious  x y (fmap func z)






data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap f NoGoat = NoGoat;
    fmap f (OneGoat a ) = OneGoat ( f a)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)





data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap func (Print str a) = Print str (func a)
  fmap func (Read f) = Read (func . f)
