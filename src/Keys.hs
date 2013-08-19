-----------------------------------------------------------------------------
--
-- Module      :  Keys
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Keys (
  cintToChar
) where

import Foreign.C.Types (CInt)

cintToChar :: CInt -> Maybe Char
cintToChar c = case c of
    97  -> Just 'a'
    98  -> Just 'b'
    99  -> Just 'c'
    100 -> Just 'd'
    101 -> Just 'e'
    102 -> Just 'f'
    103 -> Just 'g'
    104 -> Just 'h'
    105 -> Just 'i'
    106 -> Just 'j'
    107 -> Just 'k'
    108 -> Just 'l'
    109 -> Just 'm'
    110 -> Just 'n'
    111 -> Just 'o'
    112 -> Just 'p'
    113 -> Just 'q'
    114 -> Just 'r'
    115 -> Just 's'
    116 -> Just 't'
    117 -> Just 'u'
    118 -> Just 'v'
    119 -> Just 'w'
    120 -> Just 'x'
    121 -> Just 'y'
    122 -> Just 'z'
    _   -> Nothing
{-# INLINE cintToChar #-}
