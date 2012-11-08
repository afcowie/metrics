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

module ParseCommand3 where

import Prelude hiding (catch)

import Data.ByteString (ByteString)
import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.ByteString (Parser)

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

data Value =
      CpuUsage {
        vUsage :: Double
      }
    | InterfacePackets {
        packetsTx :: Rational
      }
    | LoadAverage {
        loadShort :: Double,
        loadMedium :: Double,
        loadLong :: Double
      }
  deriving (Show, Eq)


parseLine :: Parser Metric
parseLine = do
    i <- string "PUTVAL" *> space *> identifier
    l <- space *> interval
    t <- space *> timestamp
    v <- value
    return Metric { mIdentifier = i, mInterval = l, mTime = t,  mValue = v }

--
-- Parse a fragment in the following form
-- sirius.lhr.operationaldynamics.com/cpu-2/cpu-user
--

identifier :: Parser Identifier
identifier = do
    h <- some character <* char '/'
    p <- some character <* char '/'
    t <- some character
    return Identifier { iHostname = h, iPlugin = p, iType = t }
  where
    isSlash :: Char -> Bool
    isSlash c = c == '/'
    
    character = letter <|> digit <|> char '_' <|> char '-' <|> char '.'
--    

interval :: Parser Double
interval = do
    string "interval=" *> double 

timestamp :: Parser Double
timestamp = do
    double <* char ':'
    
double :: Parser Double
double = do
    n <- some numeral
    return (read n :: Double)  -- FIXME do better

value :: Parser String
value =
    some (numeral <|> char ':') <* eol


numeral = digit <|> char '.' <?> "numeral (digit or '.')"

eol = char '\n' <|> (char '\r' *> char '\n')


parseLines :: Parser [Metric]
parseLines = many (many eol *> parseLine)

processInput3 :: ByteString -> Either ParseError [Metric]
processInput3 x' = parse parseLines "" x'
