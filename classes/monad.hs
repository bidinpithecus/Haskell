import Distribution.FieldGrammar.Parsec (namelessFieldAnn)
fat :: (Eq t, Num t) => t -> t
fat 0 = 1
fat n = n * fat (n - 1)

main1 :: IO ()
main1 = putStr "Qual seu nome? " >> getLine >>= (\n -> putStr ("Alo " ++ n))

main2 :: IO ()
main2 = do  putStr "Qual seu nome? "
            n <- getLine
            putStr $ "Alo " ++ n

main3 :: IO ()
main3 = do  putStr "Digite um numero: "
            num <- getLine
            let n = fat (read num)
            putStrLn ("Resultado: " ++ show n)

main4 :: IO ()
main4 = 
  do 
    putStr "Nome do arquivo: "
    txt <- getLine >>= readFile
    let t = length txt
    putStr $ show t ++ " caracteres\n"

-- State monad

mdcBurro a b
  | a == b = a
  | a > b = mdcBurro (a - b) b
  | otherwise = mdcBurro a (b - a)

mdcTunado a b
  | a == b = do counter; return a
  | a > b = do counter; mdcTunado (a - b) b
  | otherwise = do counter; mdcTunado a (b - a)

type State = Int
newtype SM a = SM (State -> (a, State))

instance Functor SM where
  fmap f (SM m) = SM (\e -> let (a, e') = m e in (f a, e'))

instance Applicative SM where
  pure x = SM (x,)
  SM fs <*> SM vs = SM (\e -> let (f, e') = fs e; (a, e'') = vs e' in (f a, e''))

instance Monad SM where
  SM m >>= f = SM (\e -> let (a, e') = m e; SM fa = f a in fa e')

counter :: SM ()
counter = SM (\x -> ((), x + 1))

runSM :: SM a -> (a, State)
runSM (SM m) = m 0
