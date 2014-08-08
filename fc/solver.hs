import Data.List
import Data.Maybe
import System.Random
import Data.Tree
import Control.Applicative ((<$>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V

(<:>) :: a -> Vector a -> Vector a
a <:> b = V.cons a b

vectorToSet :: (Ord a) => Vector a -> Set a
vectorToSet = S.fromList . V.toList

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight |
	Nine | Ten | Jack | Queen | King  deriving (Show, Eq, Enum, Ord)

data Suit = Heart | Diamond | Club | Spade deriving (Show, Eq, Enum, Ord)

data Card = Card { rank :: Rank, suit :: Suit } deriving (Show, Eq, Ord)

type Stack = Vector Card
type CardSet = Set Card

data Board = Board {
		cascades :: Vector Stack,
		foundations :: Vector Stack,
		freecells :: CardSet}

instance Eq Board where
	Board cs fd fc == Board cs' fd' fc' = (fd == fd') && (fc == fc') && (vectorToSet cs == vectorToSet cs')

red :: Card -> Bool
red (Card _ Heart) = True
red (Card _ Diamond) = True
red (Card _ _) = False

black :: Card -> Bool
black = not . red

applyAt :: Vector a -> Int -> (a->a) -> Vector a
applyAt list num f = applyAt' list 0
	where
		applyAt' v counter | V.null v = V.empty
						   | counter == num = V.cons (f $ V.head v) $ V.tail v
						   | otherwise = V.cons (V.head v) $ applyAt' (V.tail v) $ counter+1

pushCascade :: Board -> Card -> Int -> Board
pushCascade (Board cs fd fc) cd num = Board cs' fd fc
	where cs' = applyAt cs num (V.cons cd)

popCascade :: Board -> Card -> Board
popCascade (Board cs fd fc) cd = Board cs' fd fc
	where
		cs' = stackf cs
		stackf x | V.null $ V.head x = V.head x <:> stackf (V.tail x)
				 | V.head (V.head x) == cd = V.tail (V.head x) <:> V.tail x
				 | otherwise = V.cons (V.head x) $ stackf $ V.tail x

pushFoundation :: Board -> Card -> Board
pushFoundation (Board cs fd fc) (Card rk st) = Board cs fd' fc
	where 
		fd' = applyAt fd num (\x -> Card rk st <:> x)
		num = fromJust $ elemIndex st [Heart .. Spade]

pushFreeCell :: Board -> Card -> Board
pushFreeCell (Board cs fd fc) cd = Board cs fd $ S.insert cd fc

popFreeCell :: Board -> Card -> Board
popFreeCell (Board cs fd fc) card = Board cs fd fc'
	where fc' = S.delete card fc


playableCascade :: Stack -> Card -> Bool
playableCascade stack cd@(Card rk _) | V.null stack = True
								     | rk == King = False
								     | otherwise = (black cd == red (V.head stack)) && 
								     		(rank (V.head stack) == succ (rank cd))

emptyCascade :: Stack -> Bool
emptyCascade x | V.null x = True
			   | otherwise = False

safeHead :: Vector a -> Vector a
safeHead x | V.null x = V.empty
		   | otherwise = V.singleton $ V.head x

playableCascades :: Board -> Card -> Vector Int
playableCascades (Board stacks _ _) cd = V.findIndices (`playableCascade` cd) stacks V.++ 
										 safeHead (V.findIndices emptyCascade stacks)

playableFoundation :: Board -> Card -> Bool
playableFoundation (Board _ xs _) (Card rk st) = playableFoundation' (xs V.! num)
	where 
		num = fromJust $ elemIndex st [Heart .. Spade]
		playableFoundation' x | V.null x = rk == Ace
							  | otherwise = succ (rank $ V.head x) == rk

playableFreeCell :: Board -> Bool
playableFreeCell (Board _ _ fc) = S.size fc < 4

allCardPlays :: Board -> Card -> Location -> Vector GameState
allCardPlays bd card source = allCardPlaysNoFC bd card source V.++ fcplays
	where
		fcplays = V.fromList [GameState (pushFreeCell bd card) (Move card source FreeCells) | playableFreeCell bd]

allCardPlaysNoFC :: Board -> Card -> Location -> Vector GameState
allCardPlaysNoFC bd card source = pf V.++ stackplays
	where
		pf = V.fromList [GameState (pushFoundation bd card) (Move card source Foundations) | playableFoundation bd card]
		cascadeInts = playableCascades bd card
		cascadeBoards = V.map (pushCascade bd card) cascadeInts
		stackplays = V.map (\(x,y) -> GameState x $ Move card source (Cascades y)) $ V.zip cascadeBoards cascadeInts

availableCascadeCards :: Board -> Vector Card
availableCascadeCards (Board cs _ _) = V.map V.head $ V.filter (not . V.null) cs

availableFreeCellCards :: Board -> Stack
availableFreeCellCards = V.fromList . S.elems . freecells

allPermissable :: Board -> Vector GameState
allPermissable bd = V.concatMap (\(a,b,c)->allCardPlays a b c) (V.zip3 boards cards sources)
	where
		fccards = availableFreeCellCards bd
		fcboards = V.map (popFreeCell bd) fccards
		cscards = availableCascadeCards bd
		csboards = V.map (popCascade bd) cscards
		cards = cscards V.++ fccards
		boards = csboards V.++ fcboards
		sources = V.replicate (V.length cscards) CascadesSource V.++ V.replicate (V.length fccards) FreeCells

solvedBoard :: Board -> Bool
solvedBoard (Board cs _ fc) = V.all V.null cs && S.null fc

data GameState = GameState { gameBoard :: Board, sourceMove :: Move }

type FCTree = Tree (Vector GameState)

buildTree :: Board -> FCTree
buildTree bd = unfoldTree f $ V.singleton $ GameState bd BeginGame
	where 
		f b = (b, moves)
			where moves = V.toList $ if V.null val then V.empty else V.map (`V.cons` b) val
				where val = V.filter (not . (`V.elem` V.map gameBoard b) . gameBoard) $ allPermissable $ gameBoard $ V.head b

filterAndPrune :: [Vector GameState] -> [Vector GameState]
filterAndPrune x = if solvedBoard $ gb $ head x then head x : filterAndPrune filteredTail else filterAndPrune filteredTail
			where 
				gb = gameBoard . V.head
				filteredTail = filter (\y->(gb y/= gb (head x)) && (V.length y < 150)) $ tail x

treeSolverDF :: Board -> Solution
treeSolverDF = Solution . V.toList . V.map sourceMove . V.reverse . head . filter (solvedBoard . gameBoard . V.head) . flatten . buildTree

treeSolverBF :: Board -> Solution
treeSolverBF = Solution . V.toList . V.map sourceMove . V.reverse . head . filterAndPrune . concat . levels . buildTree
		


loadFile :: FilePath -> IO Board
loadFile x = loadBoardFromText <$> readFile x

--the foundations are implemented wrong, natch.
loadBoardFromText :: String -> Board
loadBoardFromText rawtext = loadBoard (lines rawtext) (Board V.empty (V.fromList [V.empty, V.empty, V.empty, V.empty]) S.empty)
	where 
		loadBoard (('C':' ':s):ss) bd = loadBoard ss (bd { cascades = V.fromList (map parser (words s)) <:> cascades bd })
		loadBoard (('F':'C':' ':s):ss) bd = loadBoard ss (bd { freecells = S.fromList $ map parser (words s) })
		loadBoard (('F':' ':s):ss) bd = loadBoard ss (bd { foundations = V.fromList (map parser (words s)) <:> foundations bd })
		loadBoard _ bd = bd

parser :: String -> Card
parser ('2' : ks) = Card Two $ suitParser ks
parser ('3' : ks) = Card Three $ suitParser ks
parser ('4' : ks) = Card Four $ suitParser ks
parser ('5' : ks) = Card Five $ suitParser ks
parser ('6' : ks) = Card Six $ suitParser ks
parser ('7' : ks) = Card Seven $ suitParser ks
parser ('8' : ks) = Card Eight $ suitParser ks
parser ('9' : ks) = Card Nine $ suitParser ks
parser ('T' : ks) = Card Ten $ suitParser ks
parser ('J' : ks) = Card Jack $ suitParser ks
parser ('Q' : ks) = Card Queen $ suitParser ks
parser ('K' : ks) = Card King $ suitParser ks
parser ('A' : ks) = Card Ace $ suitParser ks
parser x = error $ "Bad parse string: " ++ x

cardchar :: Rank -> Char
cardchar Ace = 'A'
cardchar King = 'K'
cardchar Queen = 'Q'
cardchar Jack = 'J'
cardchar Ten = 'T'
cardchar Nine = '9'
cardchar Eight = '8'
cardchar Seven = '7'
cardchar Six = '6'
cardchar Five = '5'
cardchar Four = '4'
cardchar Three = '3'
cardchar Two = '2'

suitchar :: Suit -> Char
suitchar Heart = 'H'
suitchar Club = 'C'
suitchar Diamond = 'D'
suitchar Spade = 'S'

cardstring :: Card -> String
cardstring (Card rk st) = [cardchar rk, suitchar st]

instance Show Board where
	show (Board cs fd fc) = csstring ++ "\n" ++ fdstring ++ "\n" ++ fcstring
		where
			csstring = intercalate "\n" $ map (\x -> "C " ++ unwords (map cardstring x)) cs'
			fdstring = intercalate "\n" $ map (\x -> "FD " ++ unwords (map cardstring x)) fd'
			fcstring = "FC " ++ unwords (map cardstring $ S.elems fc)
			(cs', fd') = (V.toList (V.map V.toList cs), V.toList (V.map V.toList fd))

suitParser :: String -> Suit
suitParser "H" = Heart
suitParser "C" = Club
suitParser "D" = Diamond
suitParser "S" = Spade
suitParser x = error $ "Unrecognized suit: " ++ x

deck :: [Card]
deck = do
	x <- [Ace .. King]
	y <- [Heart .. ]
	return $ Card x y

deckShuffle :: Eq a => [a] -> IO [a]
deckShuffle [] = return []
deckShuffle xs = do
	x <- randomRIO (0, length xs-1) :: IO Int
	let val = xs !! x
	y <- deckShuffle (filter (/=val) xs)
	return $ val : y

makeGame :: IO Board
makeGame = do
	s <- deckShuffle deck
	let
		(s0, l1) = splitAt 7 s
		(s1, l2) = splitAt 7 l1
		(s2, l3) = splitAt 7 l2
		(s3, l4) = splitAt 7 l3
		(s4, l5) = splitAt 6 l4
		(s5, l6) = splitAt 6 l5
		(s6, l7) = splitAt 6 l6
		s7 = l7
		cs = V.fromList $ map V.fromList [s0,s1,s2,s3,s4,s5,s6,s7]
	return $ Board cs (V.fromList [V.empty, V.empty, V.empty, V.empty]) S.empty

data Location = Cascades Int | CascadesSource | Foundations | FreeCells deriving (Show, Eq)

data Move = Move Card Location Location | BeginGame deriving Eq

data Solution = Solution [Move]

instance Show Solution where
	show (Solution (BeginGame:xs)) = show (Solution xs)
	show (Solution (x:xs)) = show x ++ show (Solution xs)
	show _ = ""

instance Show Move where
	show (Move (Card rk st) l1 l2) = show rk ++ " " ++ show st ++ ": " ++ show l1 ++ " -> " ++ show l2 ++ "\n"
	show BeginGame = ""

main :: IO ()
main = do
	x <- makeGame
	print x
	let j = treeSolverDF x
	writeFile "out.txt" (show x ++ show j)
	print j