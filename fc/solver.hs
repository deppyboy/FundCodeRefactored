import Data.List
import Data.Maybe
import System.Random
import Data.Tree
import Control.Applicative ((<$>))
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State.Lazy

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight |
	Nine | Ten | Jack | Queen | King  deriving (Show, Eq, Enum, Ord)

data Suit = Heart | Diamond | Club | Spade deriving (Show, Eq, Enum, Ord)

data Card = Card { rank :: Rank, suit :: Suit } deriving (Show, Eq, Ord)

type Stack = [Card]
type CardSet = Set Card

data Board = Board {
		cascades :: [Stack],
		foundations :: [Stack],
		freecells :: CardSet} deriving Ord

instance Eq Board where
	Board cs fd fc == Board cs' fd' fc' = (fd == fd') && (fc == fc') && (S.fromList cs == S.fromList cs')

red :: Card -> Bool
red (Card _ Heart) = True
red (Card _ Diamond) = True
red (Card _ _) = False

black :: Card -> Bool
black = not . red

applyAt :: [a] -> Int -> (a->a) -> [a]
applyAt list num f = applyAt' list 0
	where
		applyAt' [] _ = []
		applyAt' (v:vs) counter | counter == num = f v : vs
								| otherwise = v : applyAt' vs (counter + 1)

pushCascade :: Board -> Card -> Int -> Board
pushCascade (Board cs fd fc) cd num = Board cs' fd fc
	where cs' = applyAt cs num (\x->cd : x)

popCascade :: Board -> Card -> Board
popCascade (Board cs fd fc) cd = Board cs' fd fc
	where
		cs' = stackf cs
		stackf [] = []
		stackf ([]:xs) = stackf xs
		stackf (x:xs) | head x == cd = tail x : xs
					  | otherwise = x : stackf xs

pushFoundation :: Board -> Card -> Board
pushFoundation (Board cs fd fc) (Card rk st) = Board cs fd' fc
	where 
		fd' = applyAt fd num (\x -> Card rk st : x)
		num = fromJust $ elemIndex st [Heart .. Spade]

pushFreeCell :: Board -> Card -> Board
pushFreeCell (Board cs fd fc) cd = Board cs fd $ S.insert cd fc

popFreeCell :: Board -> Card -> Board
popFreeCell (Board cs fd fc) card = Board cs fd fc'
	where fc' = S.delete card fc


playableCascade :: Stack -> Card -> Bool
playableCascade [] _ = True
playableCascade (Card Ace _:_) _ = False
playableCascade (st:_) cd = (black cd == red st) && 
					          (pred (rank st) == rank cd)

emptyCascade :: Stack -> Bool
emptyCascade [] = True
emptyCascade _ = False

safeHead :: [a] -> [a]
safeHead [] = []
safeHead (x:_) = [x]

playableCascades :: Board -> Card -> [Int]
playableCascades (Board stacks _ _) cd = findIndices (`playableCascade` cd) stacks ++ 
										 safeHead (findIndices emptyCascade stacks)

playableFoundation :: Board -> Card -> Bool
playableFoundation (Board _ xs _) (Card rk st) = playableFoundation' (xs !! num)
	where 
		num = fromJust $ elemIndex st [Heart .. Spade]
		playableFoundation' [] = rk == Ace
		playableFoundation' (y:_) = succ (rank y) == rk

playableFreeCell :: Board -> Bool
playableFreeCell (Board _ _ fc) = S.size fc < 4

allCardPlays :: Board -> Card -> Location -> [GameState]
allCardPlays bd card source = allCardPlaysNoFC bd card source ++ fcplays
	where
		fcplays = [GameState (pushFreeCell bd card) (Move card source FreeCells) | playableFreeCell bd]

allCardPlaysNoFC :: Board -> Card -> Location -> [GameState]
allCardPlaysNoFC bd card source = pf ++ stackplays
	where
		pf = [GameState (pushFoundation bd card) (Move card source Foundations) | playableFoundation bd card]
		cascadeInts = playableCascades bd card
		cascadeBoards = map (pushCascade bd card) cascadeInts
		stackplays = map (\(x,y) -> GameState x $ Move card source (Cascades y)) $ zip cascadeBoards cascadeInts

availableCascadeCards :: Board -> [Card]
availableCascadeCards (Board cs _ _) = map head $ filter (not . null) cs

availableFreeCellCards :: Board -> Stack
availableFreeCellCards = S.elems . freecells

data Color = Red | Black deriving (Eq, Show, Enum)

color :: Card -> Color
color cd = if red cd then Red else Black

highestForceable :: [[Card]] -> Color -> Rank
highestForceable [[],[],_,_] Black = Two
highestForceable [_,_,[],[]] Red = Two
highestForceable [he,di,cl,sp] col | null stack1 = rank $ head stack2
								   | null stack2 = rank $ head stack1
								   | otherwise = lesser
	where 
		(stack1,stack2) = if col == Black then (he, di) else (cl, sp)
		lesser = safesucc $ rank $ head $ if rank (head stack1) > rank (head stack2) then stack2 else stack1
		safesucc King = King
		safesucc x = succ x
highestForceable _ _ = Two

forcedMove :: GameState -> Bool
forcedMove (GameState (Board _ fd _) (Move cd _ Foundations)) = rank cd <= highestForceable fd (color cd)
forcedMove _ = False

allPermissable :: Board -> [GameState]
allPermissable bd = if any forcedMove moves then [head (filter forcedMove moves)] else moves
	where
		fccards = availableFreeCellCards bd
		fcboards = map (popFreeCell bd) fccards
		cscards = availableCascadeCards bd
		csboards = map (popCascade bd) cscards
		cards = fccards ++ cscards
		boards = fcboards ++ csboards
		sources = replicate (length fccards) FreeCells ++ replicate (length cscards) CascadesSource
		moves = concatMap (\(a,b,c)->allCardPlays a b c) (zip3 boards cards sources)

solvedBoard :: Board -> Bool
solvedBoard (Board cs _ fc) = all null cs && S.null fc

data GameState = GameState { gameBoard :: Board, sourceMove :: Move } deriving (Show, Eq)

type FCTree = Tree [GameState]

buildTree :: Board -> FCTree
buildTree bd = unfoldTree f [GameState bd BeginGame]
	where 
		f b = (b, moves)
			where moves = if null val then [] else map (:b) val
				where val = filter (not . (`elem` map gameBoard b) . gameBoard) $ allPermissable $ gameBoard $ head b

filterAndPrune :: [[GameState]] -> [[GameState]]
filterAndPrune x = if solvedBoard $ gb $ head x then head x : filterAndPrune filteredTail else filterAndPrune filteredTail
			where 
				gb = gameBoard . head
				filteredTail = filter (\y->(gb y/= gb (head x)) && (length y < 300)) $ tail x

treeSolverDF :: Board -> Solution
treeSolverDF = Solution . map sourceMove . reverse . head . filter (solvedBoard . gameBoard . head) . flatten . buildTree


treeSolverBF :: Board -> Solution
treeSolverBF = Solution . map sourceMove . reverse . head . filterAndPrune . concat . levels . buildTree
		
treeSolverPruned :: Board -> Solution
treeSolverPruned bd = Solution $ 
					map sourceMove $
					reverse $
					fromJust $
					head $
					filter (/=Nothing) $
					evalState (check $ buildTree bd) S.empty

check :: FCTree -> State (Set Board) [Maybe [GameState]]
check (Node s forests) = do
	bdset <- get
	let bd = gameBoard $ head s
	if solvedBoard bd then return [Just s] else
		if S.member bd bdset then return [Nothing] else do
			modify (S.insert bd) 
			result <- mapM check forests
			return $ concat result


loadFile :: FilePath -> IO Board
loadFile x = loadBoardFromText <$> readFile x

--the foundations are implemented wrong, natch.
loadBoardFromText :: String -> Board
loadBoardFromText rawtext = loadBoard (lines rawtext) (Board [] [[],[],[],[]] S.empty)
	where 
		loadBoard (('C':' ':s):ss) bd = loadBoard ss (bd { cascades = map parser (words s) : cascades bd })
		loadBoard (('F':'C':' ':s):ss) bd = loadBoard ss (bd { freecells = S.fromList $ map parser (words s) })
		loadBoard (('F':' ':s):ss) bd = loadBoard ss (bd { foundations = map parser (words s) : foundations bd })
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
	show (Board cs fd fc) = csstring ++ "\n" ++ fdstring ++ "\n" ++ fcstring ++ "\n"
		where
			csstring = intercalate "\n" $ map (\x -> "C " ++ unwords (map cardstring x)) cs
			fdstring = intercalate "\n" $ map (\x -> "FD " ++ unwords (map cardstring x)) fd
			fcstring = "FC " ++ unwords (map cardstring $ S.elems fc)

suitParser :: String -> Suit
suitParser "H" = Heart
suitParser "C" = Club
suitParser "D" = Diamond
suitParser "S" = Spade
suitParser x = error $ "Unrecognized suit: " ++ x

deck :: [Card]
deck = do
	x <- [Ace .. ]
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
		cs = [s0,s1,s2,s3,s4,s5,s6,s7]
	return $ Board cs [[],[],[],[]] S.empty

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
	let j = treeSolverPruned x
	writeFile "out.txt" (show x ++ show j)
	print j