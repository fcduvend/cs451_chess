import System.Exit
import Data.List
import Data.Sequence

data Move = Move Piece [Int] deriving (Show)
data Piece = Piece String Int Int deriving (Show)
instance Eq Piece where
   (Piece x y z) == (Piece x2 y2 z2) = (y == y2) && (z == z2)

main :: IO()
main = do
  let pieces = [ Piece "r" 1 1, Piece "h" 1 2, Piece "b" 1 3, Piece "k" 1 4, Piece "q" 1 5, Piece "b" 1 6, Piece "h" 1 7, Piece "r" 1 8,
                 Piece "p" 2 1, Piece "p" 2 2, Piece "p" 2 3, Piece "p" 2 4, Piece "p" 2 5, Piece "p" 2 6, Piece "p" 2 7, Piece "p" 2 8,
                 Piece "P" 7 1, Piece "P" 7 2, Piece "P" 7 3, Piece "P" 7 4, Piece "P" 7 5, Piece "P" 7 6, Piece "P" 7 7, Piece "P" 7 8,
                 Piece "R" 8 1, Piece "H" 8 2, Piece "B" 8 3, Piece "K" 8 4, Piece "Q" 8 5, Piece "B" 8 6, Piece "H" 8 7, Piece "R" 8 8 ] :: [Piece]

  display pieces
  end

--main loop
display :: [Piece] -> IO()
display pieces = do
  let newPieces = Data.List.sortOn (charIdx) pieces :: [Piece]
  putStrLn (displayBoard newPieces "" 0)

  --if parsecmd doesn't have a proper input just pass and continue
  cmd <- getLine
  parsecmd cmd
  --use the input from cmd to find a move
  display (movePiece cmd (findPiece(Piece "Nan" (head(parseMove cmd)) (head(tail(parseMove cmd)))) newPieces) newPieces)


slist :: [Piece] -> [[Char]]
slist = map (\(Piece a _ _) -> a)

--calculate a piece's 1-D index based off of its 2-D coordinates
charIdx :: Piece -> Int
charIdx (Piece _ x y) = (y - 1) + ((x - 1) * 8)



{-display chess board with pieces
 -Note: pieces must be soretd by charIdx before calling displayBoard -}
displayBoard :: [Piece] -> [Char] -> Int -> [Char]

--counter has reached its max value; end of recursive calls
displayBoard _ str 64 = str ++ "|"

displayBoard (x:xs) str counter =
    if counter == (charIdx x) then
      displayBoard xs (str ++ "|" ++ (getPiece x)) (counter + 1) --if a piece needs to be printed, print it
    else
      printNoPiece (x:xs) str counter --otherwise print a blank space

displayBoard [] str counter = printNoPiece [] str counter --print a blank space



--print a blank square with a newline if necessary
printNoPiece :: [Piece] -> [Char] -> Int -> [Char]
printNoPiece arr str counter = 
  if counter `mod` 8 == 0 then
    displayBoard arr (str ++ "|\n| ") (counter + 1)
  else
    displayBoard arr (str ++ "| ") (counter + 1)

--format a piece for 'displayBoard'
getPiece :: Piece -> [Char]
getPiece (Piece a x y) = 
  if (charIdx (Piece a x y)) `mod` 8 == 0 && (charIdx (Piece a x y)) /= 0 then 
    "\n|" ++ a --break to next line when at the end of a row
  else 
    a
  
findPiece :: Piece -> [Piece] -> Piece
findPiece x y = head[ z | z <- y , z == x]

parseMove :: [Char] -> [Int]
parseMove x = makeList(read x :: Int)

makeList :: Int -> [Int]
makeList x = [] ++ [div (mod x 10000) 1000] ++ [div (mod x 1000) 100] ++ [div (mod x 100) 10] ++ [(mod x 10)]

parsecmd :: [Char] -> IO()
parsecmd x | (x == "q") = end
           | otherwise = return()

movePiece :: [Char] -> Piece -> [Piece] -> [Piece]
movePiece cmd x y = (addPiece(Piece (head(slist [x])) (last(init(parseMove cmd))) (last(parseMove cmd))) (deletePiece x y))

deletePiece :: Piece -> [Piece] -> [Piece]
deletePiece x y = [ z | z <- y , z /= x]

addPiece :: Piece -> [Piece] -> [Piece]
addPiece x y = [x] ++ y

end :: IO()
end = exitWith ExitSuccess
