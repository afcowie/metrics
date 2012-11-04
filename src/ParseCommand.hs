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

module ParseCommand where

import Prelude hiding (catch)

import Data.ByteString (ByteString)
import Control.Applicative
import Data.Attoparsec.ByteString.Char8

{-
import Control.Monad.Trans (liftIO)
import Control.Monad.CatchIO (catch)
import Control.Exception (SomeException)
-}

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
    mValue :: ByteString
} deriving (Show)

-- We'll be making a non linear shift from Collectd's measurements to a
-- datatype here, considering interface-eth0 to be a measurement but all the
-- cpu-* be a single measurement, cpu. We start by ignoring the plugin /
-- plugin-instance and type / type-instance distinction.

data Identifier = Identifier {
    iHostname :: ByteString,
    iPlugin :: ByteString,
    iType :: ByteString
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
    i <- string "PUTVAL" *> skipSpace *> identifier
    l <- skipSpace *> interval
    t <- skipSpace *> timestamp
    v <- value
    return Metric { mIdentifier = i, mInterval = l, mTime = t,  mValue = v }

--
-- Parse a fragment in the following form
-- sirius.lhr.operationaldynamics.com/cpu-2/cpu-user
--

identifier :: Parser Identifier
identifier = do
    h <- takeTill isSlash <* char '/'
    p <- takeTill isSlash <* char '/'
    t <- takeTill isSpace
    return Identifier { iHostname = h, iPlugin = p, iType = t }
  where
    isSlash :: Char -> Bool
    isSlash c = c == '/'
--    

interval :: Parser Double
interval = do
    string "interval=" *> double 

timestamp :: Parser Double
timestamp =
    double <* char ':'
    
    

value :: Parser ByteString
value =
    takeTill theEnd

--
-- Annoyingly, isEndOfLine has type (Word8 -> Bool), so we have to cast.
--

theEnd :: Char -> Bool
theEnd c = isEndOfLine $ fromIntegral $ fromEnum c

processInput :: ByteString -> Either String Metric
processInput x' = parseOnly parseLine x'
