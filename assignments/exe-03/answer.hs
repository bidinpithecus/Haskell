import Data.List
import Data.Word ( Word8, Word32 )
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import Data.Bits (Bits (shift, (.&.)))

type Reg = (Word8, Word32)
data Huffman = Leaf Int Char | Node Huffman Int Huffman deriving Show

instance Eq Huffman where
  (==) h0 h1 = weight h0 == weight h1

instance Ord Huffman where
  (<=) h0 h1 = weight h0 <= weight h1

-- Declare uma função que dada uma String retorne uma lista de folhas da árvore contendo o caractere e a frequência com que esse caractere ocorreu na String
freq :: String -> [Huffman]
freq xs = sort (freq' xs)

freq' :: String -> [Huffman]
freq' [] = []
freq' (x : xs) = Leaf (length l1 + 1) x : freq' l2
  where (l1, l2) = partition (== x) xs

-- Declare uma função que dada a lista com a frequência de cada caratere e retorne a árvore de Huffman
makeTree :: [Huffman] -> Huffman
makeTree [] = error "Empty list"
makeTree [h] = h
makeTree (h0 : h1 : hs) = makeTree (insertOrdered (merge h0 h1) hs)

insertOrdered :: Huffman -> [Huffman] -> [Huffman]
insertOrdered h [] = [h]
insertOrdered h (x : xs)
  | h <= x = h : (x : xs)
  | otherwise = x : insertOrdered h xs

merge :: Huffman -> Huffman -> Huffman
merge h0 h1 = Node h0 (sumFreqs h0 h1) h1

weight :: Huffman -> Int
weight (Leaf w _) = w
weight (Node _ w _) = w

char :: Huffman -> Char
char (Leaf _ c) = c
char (Node {}) = error "A node doesn't have a char"

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


--
compress :: IO ()
compress =
  do
    putStr "Input file: "
    inputFile <- getLine
    content <- readFile inputFile
    let frequencies = freq content
    let code = encode content (makeTree frequencies)
    let sizesBS = P.runPut (putFirstPart (length frequencies) (8 - rem (length code) 8))
    let frequenciesBS = P.runPut (putFrequenciesList frequencies)
    let codeBS = P.runPut (putCompressedText code)
    L.writeFile (inputFile ++ ".compressed") (sizesBS <> frequenciesBS <> codeBS)

putFirstPart :: Int -> Int -> P.Put
putFirstPart i0 i1 =
  do
    P.putWord8 (toEnum i0)
    P.putWord32be (toEnum i1)
    P.flush

putFrequenciesList :: [Huffman] -> P.Put
putFrequenciesList [] = P.flush
putFrequenciesList (h : hs) =
  do
    P.putWord8 (I.c2w $ char h)
    P.putWord32be (toEnum $ weight h)
    putFrequenciesList hs

putCompressedText :: String -> P.Put
putCompressedText [] = P.flush
putCompressedText s =
  do
    P.putWord8 (s2w (take 8 s))
    putCompressedText (drop 8 s)

s2w :: String -> Word8
s2w s = s2wHelper s 7

s2wHelper :: String -> Int -> Word8
s2wHelper [] _ = 0
s2wHelper (c : cs) n = shift (read [c]) n + s2wHelper cs (n - 1)

decompress :: IO ()
decompress =
  do
    putStr "Nome do arquivo: "
    compressedFile <- getLine
    bs <- L.readFile compressedFile
    let (diffChars, totalChars) = G.runGet getReg bs
    let frequencies = r2l (G.runGet getRegs (L.take (read (show diffChars) * 5) (L.drop 5 bs)))
    let content = G.runGet getMessage (L.drop (read (show diffChars) * 5 + 5) bs)
    let message = concatMap w2s content
    let encodedMessage = take (length message - fromEnum totalChars) message
    let decodedMessage = decode encodedMessage (makeTree frequencies)
    writeFile (compressedFile ++ ".decompressed") decodedMessage

getReg :: G.Get (Word8, Word32)
getReg =
  do
    c <- G.getWord8
    f <- G.getWord32be
    return (c, f)

r2l :: [Reg] -> [Huffman]
r2l r = sort (r2l' r)

r2l' :: [Reg] -> [Huffman]
r2l' [] = []
r2l' ((c, w) : rs) = Leaf (read (show w)) (I.w2c c) : r2l' rs

w2s :: Word8 -> String
w2s w = w2sHelper w 128

w2sHelper :: Word8 -> Word8 -> String
w2sHelper _ 0 = []
w2sHelper w n
  | w .&. n == 0 = '0' : w2sHelper w (shift n (-1))
  | otherwise = '1' : w2sHelper w (shift n (-1))

getRegs :: G.Get [(Word8, Word32)]
getRegs =
  do
    empty <- G.isEmpty
    if empty then return []
    else do { r <- getReg; rs <- getRegs; return (r : rs) }

getMessage :: G.Get [Word8]
getMessage =
  do
    empty <- G.isEmpty
    if empty then return []
    else do { m <- getCharacter; ms <- getMessage; return (m : ms)}

getCharacter :: G.Get Word8
getCharacter = G.getWord8 >>= \c -> return c

printRegs :: Show a => [(Word8, a)] -> IO()
printRegs [] = return ()
printRegs ((c, f) : rs) =
  do
    putStrLn $ show (I.w2c c) ++ "-" ++ show f
    printRegs rs
