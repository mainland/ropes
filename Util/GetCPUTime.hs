{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.GetCPUTime (
    getCPUTime
  ) where

foreign import ccall "getCPUTime"
  getCPUTime :: IO Double
