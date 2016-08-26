{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances                             #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Lib where

import Bookkeeper
import Bookkeeper.Internal (Book')
import Data.Default.Class
import Data.Type.Map       ((:\), AsMap)

--------------------------------------------------------------------------------
-- This is the only thing needed from the author perspective:
--------------------------------------------------------------------------------

data Duration = Duration Int deriving Show

instance Default Duration where
  def = Duration 0

data Pitch = C | D | E | F | G | A | B deriving Show

c :: forall note withoutPitch .
  ( withoutPitch ~ (AsMap (note :\ "pitch"))
  , Settable "pitch" Pitch withoutPitch note
  , Default (Book' withoutPitch) )
  => Book' note
c = set #pitch C (def :: Book' withoutPitch)

--------------------------------------------------------------------------------
-- This is what the user of the library has to do:
--------------------------------------------------------------------------------

-- He defines the types in which he's interested in:
type PureNote = Book '["pitch" :=> Pitch]
type NotatedNote = Book '["pitch" :=> Pitch, "duration" :=> Duration]

-- And then he can just write:

-- >>> c :: PureNote
-- Book {pitch = C}
-- >> c :: NotatedNote
-- Book {duration = Duration 0, pitch = C}
