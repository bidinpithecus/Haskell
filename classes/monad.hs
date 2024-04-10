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
