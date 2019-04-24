import System.Exit
import Data.List
import Data.Sequence

--data Move = Move Piece [Int] deriving (Show)

main :: IO()
main = do
  let board = ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r',
                'p', 'p', 'p', 'p', 'p', 'p', 'p', 'p',
                ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
                ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
                ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
                ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
                'P', 'P', 'P', 'P', 'P', 'P', 'P', 'P',
                'R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R'] :: [Char]

  display board
  end

--main loop
display :: [Char] -> IO()
display board = do

  putStrLn "   1 2 3 4 5 6 7 8"
  putStrLn (printBoard board 0)
  --putStrLn (displayboard board)

  --if parsecmd doesn't have a proper input just pass and continue
  putStrLn "Enter the coordinates of the piece to move as well as the destination coordinates (eg 7161 to move (7,1) to (6,1)):"
  cmd <- getLine
  parsecmd cmd
  
  
  --use the input from cmd to find a move
  display (performCmd cmd board)
  
performCmd :: [Char] -> [Char] -> [Char]
performCmd command board = (Data.List.take elem1Index board) ++ " "  ++ (sublist (elem1Index+1) (elem2Index-1) board) ++ [(board !! elem1Index)] ++ (sublist (elem2Index+1) 64 board) --Only works one way, need to fix to work the other way
    --If elem1Index > elem2Index, check into swapping the order of parsing
   where elem1Index = charIdx (parseMove command)
         elem2Index = charIdx (tail (tail (parseMove command)))
         
sublist :: Int -> Int -> [Char] -> [Char]
sublist startIndex endIndex list = Data.List.take (endIndex-startIndex+1) (removeFromStart startIndex list)
  
removeFromStart :: Int -> [Char] -> [Char]
removeFromStart x [] = []
removeFromStart 0 list = list
removeFromStart x list = removeFromStart (x-1) (tail list)

printBoard :: [Char] -> Int -> [Char]
printBoard board 64 = []
printBoard board elemIndex = (printLineNumber elemIndex) ++ "|" ++ [(board !! elemIndex)] ++ (if ((elemIndex ` mod` 8) == 7) then "|\n" else "") ++ (printBoard board (elemIndex+1))
  
--print line number at the left side of each row
printLineNumber :: Int -> [Char]
printLineNumber elemIndex = if elemIndex `mod` 8 == 0 then 
                              show((ceiling(fromIntegral(elemIndex) / 8.0)) + 1) ++ " " 
                            else 
                              ""

parsecmd :: [Char] -> IO()
parsecmd x | (x == "q") = end
           | otherwise = return()

parseMove :: [Char] -> [Int]
parseMove x = makeList(read x :: Int)
  
makeList :: Int -> [Int]
makeList x = [] ++ [div (mod x 10000) 1000] ++ [div (mod x 1000) 100] ++ [div (mod x 100) 10] ++ [(mod x 10)]

--calculate a piece's 1-D index based off of its 2-D coordinates
charIdx :: [Int] -> Int
charIdx (x:y:_) = (y - 1) + ((x - 1) * 8)

end :: IO()
end = exitWith ExitSuccess
