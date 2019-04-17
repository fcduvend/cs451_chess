import System.Exit
import Data.List

data Piece = Piece String Int Int deriving (Show)

main :: IO()
main = do
  let pieces = [ Piece "r" 0 0, Piece "h" 0 1, Piece "b" 0 2, Piece "k" 0 3, Piece "q" 0 4, Piece "b" 0 5, Piece "h" 0 6, Piece "r" 0 7,
                 Piece "p" 1 0, Piece "p" 1 1, Piece "p" 1 2, Piece "p" 1 3, Piece "p" 1 4, Piece "p" 1 5, Piece "p" 1 6, Piece "p" 1 7,
                 Piece "P" 6 0, Piece "P" 6 1, Piece "P" 6 2, Piece "P" 6 3, Piece "P" 6 4, Piece "P" 6 5, Piece "P" 6 6, Piece "P" 6 7,
                 Piece "R" 7 0, Piece "H" 7 1, Piece "B" 7 2, Piece "K" 7 3, Piece "Q" 7 4, Piece "B" 7 5, Piece "H" 7 6, Piece "R" 7 7 ] :: [Piece]

  --putStrLn (show (indexes pieces "" 0))
  display pieces
  end

slist :: [Piece] -> [[Char]]
slist = map (\(Piece a _ _) -> a)

xlist :: [Piece] -> [Int]
xlist = map (\(Piece _ a _) -> a)

ylist :: [Piece] -> [Int]
ylist = map (\(Piece _ _ a) -> a)

charidx :: Piece -> Int
charidx (Piece _ x y) = y + (x * 8)

indexes :: [Piece] -> [Char] -> Int -> [Char]
indexes (x:xs) str counter =
  if counter == 64 then
    str
  else
    if counter == (charidx x) then
      indexes xs (str ++ (getAwNewLn x)) (counter + 1)
    else
      if counter `mod` 8 == 0 then
        indexes (x:xs) (str ++ "| \n") (counter + 1)
      else
        indexes (x:xs) (str ++ "| ") (counter + 1)



indexes [] str counter =
  if counter == 64 then
    str
  else
      if counter `mod` 8 == 0 then
        indexes [] (str ++ "| \n") (counter + 1)
      else
        indexes [] (str ++ "| ") (counter + 1)

display :: [Piece] -> IO()
display pieces = do

  let newPieces = sortOn (\(Piece _ x y) -> y + (x * 8)) pieces :: [Piece]
  putStrLn (indexes newPieces "" 0)

  cmd <- getLine
  parsecmd cmd
  display newPieces
  
parsecmd :: [Char] -> IO()
parsecmd "q" = end
parsecmd _ = return ()

end :: IO()
end = exitWith ExitSuccess

getX :: Piece -> Int
getX (Piece _ a _) = a

getY :: Piece -> Int
getY (Piece _ _ a) = a

getAwNewLn :: Piece -> [Char]
getAwNewLn (Piece a x y) = if y + (x * 8) `mod` 8 == 0 then "\n|" ++ a else "|" ++ a
