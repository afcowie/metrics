--
-- Statistics collection
--
-- Copyright © 2012 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made available
-- to you by its authors as open source software: you can redistribute it
-- and/or modify it under the terms of the GNU General Public License version
-- 2 ("GPL") as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GPL for more details.
--
-- You should have received a copy of the GPL along with this program. If not,
-- see http://www.gnu.org/licenses/. The authors of this program may be
-- contacted through http://research.operationaldynamics.com/
--

{-# LANGUAGE OverloadedStrings #-}

module ParseCommandTrifecta where

import Prelude hiding (catch)

import Data.ByteString (ByteString)
import Control.Applicative
import Text.Trifecta
import Data.Monoid

--
-- Parse a single "command" line coming from collectd, of the form
--
-- PUTVAL sirius.lhr.operationaldynamics.com/cpu-2/cpu-user interval=10.000 1351756697.945:0.103719
-- PUTVAL sirius.lhr.operationaldynamics.com/interface-eth0/if_packets interval=10.000 1351756688.304:1.79711:1.69727
-- PUTVAL sirius.lhr.operationaldynamics.com/load/load interval=10.000 1351756797.945:0.000000:0.010000:0.050000
--
-- as transmitted by the write_http plugin.
--

data Metric = Metric {
    mIdentifier :: Identifier,
    mInterval :: Double,
    mTime :: Double,
    mValue :: String
} deriving (Show)

-- We'll be making a non linear shift from Collectd's measurements to a
-- datatype here, considering interface-eth0 to be a measurement but all the
-- cpu-* be a single measurement, cpu. We start by ignoring the plugin /
-- plugin-instance and type / type-instance distinction.

data Identifier = Identifier {
    iHostname :: String,
    iPlugin :: String,
    iType :: String
} deriving (Show, Eq)


parseLine :: MonadParser m => m Metric
parseLine = do
    i <- string "PUTVAL" *> space *> label
    l <- space *> interval
    t <- timestamp
    v <- value
    return Metric { mIdentifier = i, mInterval = l, mTime = t,  mValue = v }

--
-- Parse a fragment in the following form
-- sirius.lhr.operationaldynamics.com/cpu-2/cpu-user
--

label :: MonadParser m => m Identifier
label = do
    h <- some character <* char '/'
    p <- some character <* char '/'
    t <- some character -- till space
    return Identifier { iHostname = h, iPlugin = p, iType = t }
  where
    isSlash :: Char -> Bool
    isSlash c = c == '/'
    
    character = letter <|> digit <|> char '_' <|> char '-' <|> char '.'
--    

interval :: MonadParser m => m Double
interval = do
    string "interval=" *> double 

timestamp :: MonadParser m => m Double
timestamp =
    double <* char ':'
    
    

value :: MonadParser m => m String
value =
    some (numeral <|> char ':') <* eol

numeral :: MonadParser m => m Char
numeral =
    digit <|> char '.' <?> "numeral ('0'..'9' or '.')"

eol :: MonadParser m => m Char
eol =
    char '\n' <|> (char '\r' *> char '\n')


parseLines :: MonadParser m => m [Metric]
parseLines =
    many (many eol *> parseLine)


processInputT :: ByteString -> IO (Either String [Metric])
processInputT x' = do
    e <- case parseByteString parseLines mempty x' of
        Failure r   -> do
            displayLn r
            return (Left "Boom")
        Success _ a -> return (Right a)
    return e
