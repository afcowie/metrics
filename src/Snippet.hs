{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import Data.ByteString (ByteString, readFile)
import Criterion.Main
import ParseCommandAttoparsec
import ParseCommandTrifecta
import ParseCommandParsec
import GHC.Conc

ex' = "PUTVAL sirius.lhr.operationaldynamics.com/cpu-2/cpu-user interval=10.000 1351756697.945:0.103719\nPUTVAL sirius.lhr.operationaldynamics.com/interface-eth0/if_packets interval=10.000 1351756688.304:1.79711:1.69727\n":: ByteString


test0 :: ByteString -> IO String
test0 x' = do
    re <- return $ processInput x'
    case re of
        Left  e  -> return $ show e
        Right r  -> return $ show r


test2 :: ByteString -> IO String
test2 x' = do
    re <- processInputT x'
    case re of
        Left  e  -> return $ show e
        Right r  -> return $ show r
    

test3 :: ByteString -> IO String
test3 x' = do
    re <- return $ processInput3 x'
    case re of
        Left  e  -> return $ show e
        Right r  -> return $ show r


demo :: IO ()
demo = do
    let d' = ex'
    
    test0 d' >>= putStrLn
    putStrLn "--"
    test2 d' >>= putStrLn
    putStrLn "--"
    test3 d' >>= putStrLn
    return ()

profile = do
    d' <- readFile "tests/sample.txt"
    
    _  <- defaultMain [
            bench "attoparsec" (test0 d'),
            bench "trifecta" (test2 d'),
            bench "parsec" (test3 d')
        ]
    return ()


main :: IO () 
main = do
    GHC.Conc.setNumCapabilities 4
    demo
--  profile
