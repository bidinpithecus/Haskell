import Data.List
import Data.Word ( Word8, Word32 )
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import Data.Bits ()

type Reg = (Word8, Word32)
data Huffman = Leaf Int Char | Node Huffman Int Huffman deriving Show

instance Eq Huffman where
  (==) h0 h1 = weight h0 == weight h1

instance Ord Huffman where
  (<=) h0 h1 = weight h0 <= weight h1

-- Declare uma função que dada uma String retorne uma lista de folhas da árvore contendo o caractere e a frequência com que esse caractere ocorreu na String
freq :: String -> [Huffman]
freq [] = []
freq (x : xs) = Leaf (length l1 + 1) x : freq l2
  where (l1, l2) = partition (== x) xs

-- Declare uma função que dada a lista com a frequência de cada caratere e retorne a árvore de Huffman
makeTree :: [Huffman] -> Huffman
makeTree [] = error "Empty list"
makeTree [h] = h
makeTree (h0 : h1 : hs) = makeTree (sort (merge h0 h1 : hs))

merge :: Huffman -> Huffman -> Huffman
merge h0 h1 = Node h0 (sumFreqs h0 h1) h1

weight :: Huffman -> Int
weight (Leaf w _) = w
weight (Node _ w _) = w

sumFreqs :: Huffman -> Huffman -> Int
sumFreqs h0 h1 = weight h0 + weight h1

-- Declare uma função que dada a árvore de Huffman retorne os códigos de Huffman para cada caractere
codeHuffman :: Huffman -> [(Char, String)]
codeHuffman h = codeHuffmanHelper h ""

codeHuffmanHelper :: Huffman -> String -> [(Char, String)]
codeHuffmanHelper (Leaf _ c) code = [(c, code)]
codeHuffmanHelper (Node l _ r) code = codeHuffmanHelper l (code ++ "0") ++ codeHuffmanHelper r (code ++ "1")

-- Declare uma função que codifique uma String numa sequência binária de códigos de Huffman
encode :: String -> Huffman -> String
encode s h = encodeHelper s (codeHuffman h)

encodeHelper :: String -> [(Char, String)] -> String
encodeHelper [] _ = []
encodeHelper (c : cs) tuples = case findTupleByFst tuples c of
  Just (_, code) -> code ++ encodeHelper cs tuples
  Nothing -> error "Character not found in Huffman encoding table"

findTupleByFst :: Eq a => [(a, b)] -> a -> Maybe (a, b)
findTupleByFst [] elem = Nothing
findTupleByFst ((x, y) : xs) elem
  | x == elem  = Just (x, y)
  | otherwise  = findTupleByFst xs elem

-- Declare uma função que faça o processo inverso que a função `encode`
decode :: String -> Huffman -> String
decode [] _ = []
decode s h =
  let (ch, rest) = traverseHuffmanTree s h
  in ch : decode rest h

traverseHuffmanTree :: String -> Huffman -> (Char, String)
traverseHuffmanTree [] (Leaf _ ch) = (ch, [])
traverseHuffmanTree s (Leaf _ ch) = (ch, s)
traverseHuffmanTree (c : cs) (Node l _ r)
  | c == '0' = traverseHuffmanTree cs l
  | c == '1' = traverseHuffmanTree cs r
  | otherwise = error "Invalid Huffman encoding"

-- write :: IO ()
-- write =
--   do
--     putStr "Nome do arquivo: "
--     na <- getLine
--     txt <- readFile na
--     let bs = P.runPut (put $ freq txt)
--     L.writeFile (na ++ ".out") bs

-- put :: [(Char, Int)] -> P.PutM ()
-- put [] = P.flush
-- put ((c, f) : xs) =
--   do
--     P.putWord8 (I.c2w c)
--     P.putWord32be (toEnum f)
--     put xs

-- read' :: IO ()
-- read' =
--   do
--     putStr "Nome do arquivo: "
--     na <- getLine
--     bs <- L.readFile na
--     let rs = G.runGet getRegs bs
--     printRegs rs

-- getReg :: G.Get (Word8, Word32)
-- getReg =
--   do
--     c <- G.getWord8
--     f <- G.getWord32be
--     return (c, f)

-- getRegs :: G.Get [(Word8, Word32)]
-- getRegs =
--   do
--     empty <- G.isEmpty
--     if empty then return []
--     else do { r <- getReg; rs <- getRegs; return (r:rs) }

-- printRegs :: Show a => [(Word8, a)] -> IO()
-- printRegs [] = return ()
-- printRegs ((c, f) : rs) =
--   do
--     putStrLn $ show (I.w2c c) ++ "-" ++ show f
--     printRegs rs
