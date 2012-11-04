{-# LANGUAGE OverloadedStrings #-}


import Data.ByteString (ByteString)

import ParseCommand

x = "PUTVAL sirius.lhr.operationaldynamics.com/cpu-2/cpu-user interval=10.000 1351756697.945:0.103719":: ByteString

main :: IO ()
main = do
   let re = processInput x
   putStrLn $ show re
   return ()
   


