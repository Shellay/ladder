-- * Taking a walk * --

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

data Direction = L | R deriving (Show, Eq)
type Directions = [Direction]

freeTree :: Tree Char  
freeTree = (n 'P'
            (n 'O'
             (n 'L' (lf 'N') (lf 'T'))
             (n 'Y' (lf 'S') (lf 'A')))  
            (n 'L'  
             (n 'W' (lf 'C') (lf 'R')) 
             (n 'A' (lf 'A') (lf 'C'))))
  where (n,e,lf) = (Node,Empty,\x -> n x e e)

changeTo :: a -> Directions -> Tree a -> Tree a
changeTo a' [] (Node _ l r) = Node a' l r
changeTo a' (d:ds) (Node a l r) = case d of
  L -> Node a (changeTo a' ds l) r
  R -> Node a l (changeTo a' ds r)

elemAt :: Directions -> Tree a -> a
elemAt [] (Node x _ _) = x
elemAt (d:ds) (Node _ l r) = case d of
  L -> elemAt ds l
  R -> elemAt ds r

-- Tests
oldElem = elemAt [R,L] freeTree
newTree = changeTo 'P' [R,L] freeTree
newElem = elemAt [R,L] newTree



-- * A trail of breadcrumbs * ---

-- Breadcrumbs is the *reversed* direction of tracing.
type Breadcrumbs0 = [Direction]

goLeft0 :: (Tree a, Breadcrumbs0) -> (Tree a, Breadcrumbs0)
goLeft0 (Node _ l _, bs) = (l, L:bs)

goRight0 :: (Tree a, Breadcrumbs0) -> (Tree a, Breadcrumbs0)
goRight0 (Node _ _ r, bs) = (r, R:bs)

trav1 = goLeft0 $ goRight0 (freeTree, [])


-- ** The (-:) operator to make pipeline style.
(-:) :: a -> (a -> b) -> b
x -: f = f x

trav2 = (freeTree, []) -: goRight0 -: goLeft0


-- ** Going back up
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show, Eq)
type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node a l r, bs) = (l, LeftCrumb a r:bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node a l r, bs) = (r, RightCrumb a l:bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (l, LeftCrumb  a r:bs) = (Node a l r, bs)
goUp (r, RightCrumb a l:bs) = (Node a l r, bs)

type Zipper a = (Tree a, Breadcrumbs a)


-- ** Manipulating trees under focus

modify :: (a -> a) -> Zipper a -> Zipper a
modify f lf@(Empty, _) = lf
modify f (Node a l r, bs) = (Node (f a) l r, bs)

newFocusTree = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')

-- *** reuse the current zipper for attachment to build a new tree

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)


-- ** Up to top
topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z = topMost (goUp z)



-- * Focusing on lists * --

type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

fore3 = ("hollywood", "") -: goForward -: goForward -: goForward



-- * A very simple file system * --

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem  
myDisk = Folder "root" [ File "goat_yelling_like_man.wmv" "baaaaaa" , File "pope_time.avi" "god bless" , Folder "pics" [ File "ape_throwing_up.jpg" "bleargh" , File "watermelon_smash.gif" "smash!!" , File "skull_man(scary).bmp" "Yikes!"] , File "dijon_poupon.doc" "best mustard" , Folder "programs" [ File "fartwizard.exe" "10gotofart" , File "owl_bandit.dmg" "mov eax, h00t" , File "not_a_virus.exe" "really not a virus" , Folder "source code" [ File "best_hs_prog.hs" "main = print (fix error)" , File "random.hs" "main = print 4"]]] 


-- ** A zipper for our file system
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
-- FSCrum <current-namespace> <preceders> <backtrace-stack>
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

-- focus down onto the file given (name) from within the current folder
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder fldName items, bs) =
  let (ls, item:rs) = break (nameIs name) items
  in  (item, FSCrumb fldName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder fldName _) = name == fldName
nameIs name (File flName _) = name == flName


newFocusFS = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"

-- ** Manipulating our file system
fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

newFocusFS1 = (myDisk, []) -: fsTo "pics" -: fsRename "cspi" -: fsUp

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder fldName items, bs) =
  (Folder fldName (item:items), bs)



-- * Watch your step * --

-- Augment the tracing with Monad

goLeftM :: Zipper a -> Maybe (Zipper a)
goLeftM s@(Node _ _ _, _) = Just $ goLeft s
goLeftM _ = Nothing

goRightM :: Zipper a -> Maybe (Zipper a)
goRightM s@(Node _ _ _, _) = Just $ goRight s
goRightM _ = Nothing


-- * Further way to the `Traversable` and `Lens` topics! * --
