import Data.List
import Data.Word
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import Data.Bits

type Reg = (Word8, Word32)

freq :: Eq a => [a] -> [(a, Int)]
freq [] = []
freq (x:xs) = (x, length l1 + 1) : freq l2
  where (l1, l2) = partition (== x) xs

write :: IO ()
write = 
  do
    putStr "Nome do arquivo: "
    na <- getLine
    txt <- readFile na
    let bs = P.runPut (put $ freq txt)
    L.writeFile (na ++ ".out") bs

put :: [(Char, Int)] -> P.PutM ()
put [] = P.flush
put ((c, f) : xs) =
  do
    P.putWord8 (I.c2w c)
    P.putWord32be (toEnum f)
    put xs

read' :: IO ()
read' =
  do
    putStr "Nome do arquivo: "
    na <- getLine
    bs <- L.readFile na
    let rs = G.runGet getRegs bs
    printRegs rs

getReg :: G.Get (Word8, Word32)
getReg = 
  do
    c <- G.getWord8
    f <- G.getWord32be
    return (c, f)

getRegs :: G.Get [(Word8, Word32)]
getRegs = 
  do 
    empty <- G.isEmpty
    if empty then return []
    else do { r <- getReg; rs <- getRegs; return (r:rs) }

printRegs :: Show a => [(Word8, a)] -> IO()
printRegs [] = return ()
printRegs ((c, f) : rs) =
  do 
    putStrLn $ show (I.w2c c) ++ "-" ++ show f
    printRegs rs
