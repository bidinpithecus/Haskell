data Bin =  V | Z Bin | U Bin

data SigBin = Neg Bin | Pos Bin

instance Show Bin where
  show :: Bin -> String
  show V = ""
  show (Z bin) = "0" ++ show bin
  show (U bin) = "1" ++ show bin

instance Show SigBin where
  show :: SigBin -> String
  show (Neg bin) = "-" ++ show bin
  show (Pos bin) = show bin

len :: Bin -> Int
len V = 0
len (Z b) = 1 + len b
len (U b) = 1 + len b

binToIntegerAux :: Bin -> Int -> Integer
binToIntegerAux V _ = 0
binToIntegerAux (Z b) l = binToIntegerAux b (l - 1)
binToIntegerAux (U b) l = 2 ^ l + binToIntegerAux b (l - 1)

binToInteger :: Bin -> Integer
binToInteger V = 0
binToInteger b = binToIntegerAux b (len b - 1)

sigBinToInteger :: SigBin -> Integer
sigBinToInteger (Neg bin) = binToInteger bin * (-1)
sigBinToInteger (Pos bin) = binToInteger bin

concatenate :: Bin -> Bin -> Bin
concatenate a V     = a
concatenate V b     = b
concatenate (Z a) b = Z $ concatenate a b
concatenate (U a) b = U $ concatenate a b

integerToBin :: Integer -> Bin
integerToBin n = case mod n 2 of
  0 -> case div n 2 of 
    0 -> Z V
    _ -> concatenate (integerToBin $ div n 2) (Z V)
  1 -> case div n 2 of 
    0 -> U V
    _ -> concatenate (integerToBin $ div n 2) (U V)

integerToSigBin :: Integer -> SigBin
integerToSigBin n
  | n < 0 = Neg (integerToBin n)
  | otherwise = Pos (integerToBin n)

instance Num Bin where
  (+) :: Bin -> Bin -> Bin
  bin1 + bin2 = integerToBin (binToInteger bin1 + binToInteger bin2)

  (-) :: Bin -> Bin -> Bin
  bin1 - bin2 = integerToBin (binToInteger bin1 - binToInteger bin2)

  (*) :: Bin -> Bin -> Bin
  bin1 * bin2 = integerToBin (binToInteger bin1 * binToInteger bin2)

  fromInteger :: Integer -> Bin
  fromInteger = integerToBin

  abs :: Bin -> Bin
  abs b = b

  signum :: Bin -> Bin
  signum V = 0
  signum (Z b) = 0
  signum (U b) = 1

instance Num SigBin where
  (+) :: SigBin -> SigBin -> SigBin
  bin1 + bin2 = integerToSigBin (sigBinToInteger bin1 + sigBinToInteger bin2)

  (-) :: SigBin -> SigBin -> SigBin
  bin1 - bin2 = integerToSigBin (sigBinToInteger bin1 - sigBinToInteger bin2)

  (*) :: SigBin -> SigBin -> SigBin
  bin1 * bin2 = integerToSigBin (sigBinToInteger bin1 * sigBinToInteger bin2)

  fromInteger :: Integer -> SigBin
  fromInteger = integerToSigBin

  abs :: SigBin -> SigBin
  abs b = b

  signum :: SigBin -> SigBin
  signum (Pos V) = Pos (Z V)
  signum (Pos (Z V)) = Pos (Z V)
  signum (Neg (Z V)) = Pos (Z V)
  signum (Neg b) = Neg (U V)
  signum (Pos b) = Pos (U V)
