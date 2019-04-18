import System.Exit
import Data.List
import Data.Sequence

data Move = Move Piece [Int] deriving (Show)
data Piece = Piece String Int Int deriving (Show)
instance Eq Piece where
   (Piece x y z) == (Piece x2 y2 z2) = (y == y2) && (z == z2)

main :: IO()
main = do
  let pieces = [ Piece "r" 0 0, Piece "h" 0 1, Piece "b" 0 2, Piece "k" 0 3, Piece "q" 0 4, Piece "b" 0 5, Piece "h" 0 6, Piece "r" 0 7,
                 Piece "p" 1 0, Piece "p" 1 1, Piece "p" 1 2, Piece "p" 1 3, Piece "p" 1 4, Piece "p" 1 5, Piece "p" 1 6, Piece "p" 1 7,
                 Piece "P" 6 0, Piece "P" 6 1, Piece "P" 6 2, Piece "P" 6 3, Piece "P" 6 4, Piece "P" 6 5, Piece "P" 6 6, Piece "P" 6 7,
                 Piece "R" 7 0, Piece "H" 7 1, Piece "B" 7 2, Piece "K" 7 3, Piece "Q" 7 4, Piece "B" 7 5, Piece "H" 7 6, Piece "R" 7 7 ] :: [Piece]

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
charIdx (Piece _ x y) = y + (x * 8)

{-display chess board with pieces
 -Note: pieces must be soretd by charIdx before calling displayBoard -}
displayBoard :: [Piece] -> [Char] -> Int -> [Char]
displayBoard (x:xs) str counter =
  if counter == 64 then
    str ++ "|" --end of recursive calls
  else
    if counter == (charIdx x) then
      displayBoard xs (str ++ "|" ++ (getPiece x)) (counter + 1) --if a piece needs to be printed, print it
    else
      printNoPiece (x:xs) str counter --otherwise print a blank space

displayBoard [] str counter =
  if counter == 64 then
    str ++ "|" --end of recursive calls
  else
    printNoPiece [] str counter --print a blank space


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
