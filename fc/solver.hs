import Data.List
import Data.Maybe

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight |
	Nine | Ten | Jack | Queen | King deriving (Show, Eq, Enum, Ord)

data Suit = Heart | Diamond | Club | Spade deriving (Show, Eq, Enum)

data Card = Card { rank :: Rank, suit :: Suit } deriving (Show, Eq)

type Stack = [Card]

data Board = Board {
		cascades :: [Stack],
		foundations :: [Stack],
		freecells :: Stack} deriving (Show, Eq)

red :: Card -> Bool
red (Card _ Heart) = True
red (Card _ Diamond) = True
red (Card _ _) = False

black :: Card -> Bool
black = not . red

applyAt :: [a] -> Int -> (a->a) -> [a]
applyAt list num f = applyAt' list 0
	where
		applyAt' (x:xs) counter | counter == num = f x : xs
								| otherwise = x : applyAt' xs (counter+1)
		applyAt' _ _ = []

pushCascade :: Board -> Card -> Int -> Board
pushCascade (Board cs fd fc) cd num = Board cs' fd fc
	where cs' = applyAt cs num (\x->cd : x)

popCascade :: Board -> Card -> Board
popCascade (Board cs fd fc) cd = Board cs' fd fc
	where
		cs' = stackf cs
		stackf (x:xs) | null x = x : stackf xs
					  | head x == cd = tail x : xs
					  | otherwise = x : stackf xs
		stackf _ = []

pushFoundation :: Board -> Card -> Board
pushFoundation (Board cs fd fc) (Card rk st) = Board cs fd' fc
	where 
		fd' = applyAt fd num (\x -> Card rk st : x)
		num = fromJust $ elemIndex st [Heart .. Spade]

pushFreecell :: Board -> Card -> Board
pushFreecell (Board cs fd fc) cd = Board cs fd (cd : fc)

popFreecell :: Board -> Card -> Board
popFreecell (Board cs fd fc) card = Board cs fd fc'
	where fc' = filter (/=card) fc


playableCascade :: Stack -> Card -> Bool
playableCascade (cd:_) pc = (black pc ==  red cd) && (rank cd == succ (rank pc))
playableCascade _ _ = True

playableCascades :: Board -> Card -> [Int]
playableCascades (Board stacks _ _) cd = findIndices (`playableCascade` cd) stacks

playableFoundation :: Board -> Card -> Bool
playableFoundation (Board _ xs _) (Card rk st) = playableFoundation' (xs !! num)
	where 
		num = fromJust $ elemIndex st [Heart .. Spade]
		playableFoundation' (x:_) = succ (rank x) == rk
		playableFoundation' _ = rk == Ace

playableFreecell :: Board -> Bool
playableFreecell (Board _ _ fc) = length fc < 4

allCardPlays :: Board -> Card -> [Board]
allCardPlays bd card = pf ++ fcplays ++ stackplays
	where
		pf = [pushFoundation bd card | playableFoundation bd card]
		fcplays = [pushFreecell bd card | playableFreecell bd]
		stackplays = map (pushCascade bd card) $ playableCascades bd card

availableCascadeCards :: Board -> [Card]
availableCascadeCards (Board cs _ _) = map head $ filter (not . null) cs

availableFreeCellCards :: Board -> [Card]
availableFreeCellCards = freecells

allPermissable :: Board -> [Board]
allPermissable bd = concatMap (uncurry allCardPlays) $ zip boards cards
	where
		fccards = availableFreeCellCards bd
		fcboards = map (popFreecell bd) fccards
		cscards = availableCascadeCards bd
		csboards = map (popCascade bd) cscards
		boards = fcboards ++ csboards
		cards = fccards ++ cscards

solvedBoard :: Board -> Bool
solvedBoard (Board cs _ fc) = all null cs && null fc

solver :: Board -> [Board]
solver board = reverse $ solver' [board] [allPermissable board]
	where
		solver' bds ((guess:guesses):gs) | guess `elem` bds = solver' bds (guesses:gs)
									     | solvedBoard guess = guess : bds
							             | otherwise = solver' (guess:bds) (allPermissable guess:gs)
		solver' (bd:bds) ([]:gs) | solvedBoard bd = bd : bds
							     | otherwise = solver' bds gs
		solver' _ _ = error "Puzzle is unsolvable."

loadBoardFromText :: String -> Board
loadBoardFromText rawtext = loadBoard (lines rawtext) (Board [] [[],[],[],[]] [])
	where 
		loadBoard (('C':' ':s):ss) bd = loadBoard ss (bd { cascades = map parser (words s) : cascades bd })
		loadBoard (('F':'C':' ':s):ss) bd = loadBoard ss (bd { freecells = map parser (words s) })
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
parser ('1' : '0' : ks) = Card Ten $ suitParser ks
parser ('J' : ks) = Card Jack $ suitParser ks
parser ('Q' : ks) = Card Queen $ suitParser ks
parser ('K' : ks) = Card King $ suitParser ks
parser ('A' : ks) = Card Ace $ suitParser ks
parser x = error $ "Bad parse string: " ++ x

suitParser :: String -> Suit
suitParser "H" = Heart
suitParser "C" = Club
suitParser "D" = Diamond
suitParser "S" = Spade
suitParser x = error $ "Unrecognized suit: " ++ x

deck :: Stack
deck = do
	x <- [Ace .. King]
	y <- [Heart .. Spade]
	return $ Card x y

main :: IO [Board]
main = do
	x <- readFile "fc.txt"
	let k = loadBoardFromText x
	return $ solver k

m :: Board
m = Board [[Card Two Heart]] [[Card Ace Heart], [], [], [Card Two Spade, Card Ace Spade]] []