class ExpAlg t where
  lit :: Int -> t
  add :: t -> t -> t

newtype Eval = Eval { eval :: Int }

instance ExpAlg Eval where
  lit x = Eval x
  add x y = Eval $ eval x + eval y

e1 :: ExpAlg t => t
e1 = lit 1 `add` (lit 2 `add` lit 3)


class ExpAlg t => MulAlg t where
  mul :: t -> t -> t

instance MulAlg Eval where
  mul x y = Eval $ eval x * eval y

e2 :: MulAlg t => t
e2 = lit 4 `mul` (lit 5 `add` lit 6)


newtype View = View { view :: String }

instance ExpAlg View where
  -- lit :: Int -> View
  lit x = View (show x)
  -- add :: View -> View -> View
  -- add x y = View $ view x + view y
  add (View x) (View y) = View ("(" ++ x ++ "+" ++ y ++ ")")

instance MulAlg View where
  mul (View x) (View y) = View ("(" ++ x ++ "*" ++ y ++ ")")

