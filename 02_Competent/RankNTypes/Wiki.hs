{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- * About

-- Definition of "rank"
{-

rank t         = 0
rank (∀a*. p)  = rank p + 1
rank (t1 → t2) = max (rank t1 + 1) (rank t2)

e.g.
rank a == 0
rank (∀a. a) == rank a + 1 == 1
rank (∀a b. a → b) == rank (a → b) + 1 == max (rank a) (rank b) + 1 == 1
rank (∀a. (∀s. → s) → a) 
  == rank ((∀s. → s) → a) + 1
  == max (rank (∀s. → s)) (rank a) + 1
  == max (rank s + 1) 0 + 1
  == max 1 0 + 1
  == 1 + 1
  == 2

-}


-- * Church-encoded Lists

newtype ChurchList a =
  ChurchList { runList :: forall r. (a -> r -> r) -> r -> r }
  -- where
  --   (r) is hidden from LHS (i.e. existential)

fromList :: [a] -> ChurchList a
fromList xs = ChurchList $ \k z -> foldr k z xs
  -- where
  --   k :: (a -> r -> r) -- the node of fold
  --   z :: r             -- the unit of fold
  --   foldr k z xs :: r

cons :: a -> ChurchList a -> ChurchList a
cons x xchs = ChurchList $ \k z -> k x (runList xchs k z)
  -- where
  --   x :: a
  --   xchs :: ChurchList a
  --   k :: a -> r -> r
  --   z :: r
  --   runList xchs :: forall r. (a -> r -> r) -> r -> r
  --   runList xchs k z :: r

nil = ChurchList $ \k z -> z

  
-- * Relation to Existentials

-- ** represented in existentials

-- data <tname> <tparam>* =
--  forall <qparam>* .
--   <constrn>* => <con> <texpr>*

data T a1 a2 = forall t1 t2. (Show t1, Show t2) => MkT a1 a2 t1 t2

-- *** matching

-- case <e> of
--   (<con> <pat>*) -> <expr>

m1 = case e1 of
       (MkT a1 a2 t1 t2) -> show t1 ++ show t2
  where
    e1 = MkT 1 'a' 2 'b'



-- ** represented in rank-N type

-- data <tname> <tparam>* =
--  <con>
--   (forall <k>.
--    (forall <qparam>*.
--      <constrn>* => (<texpr> -> )* <k>

data S a1 a2 = MkS
  (forall k.
   (forall t1 t2.
    (Show t1, Show t2) => a1 -> a2 -> t1 -> t2 -> k) -> k)

e2 = MkS (\a1 a2 t1 t2 (k :: Int) -> k)

-- *** matching

-- case <e> of
--   (<con> f) -> let k1 = f <pat>*
--                in f k
