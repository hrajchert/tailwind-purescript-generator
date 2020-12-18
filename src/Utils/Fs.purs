module Utils.Fs (ensureDir) where

import Prelude
import Data.Array (dropEnd, length)
import Data.String (Pattern(..), joinWith, split)
import Effect.Aff (Aff)
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as FP

-- Makes sure a given directory exists, creating intermediate directories if needed
ensureDir :: FilePath -> Aff Unit
ensureDir path = do
  let
    directory = FP.dirname path
  -- We check if the directory exists, and if it doesn't
  -- we recursively create the needed directories starting from the end
  directoryExists <- FS.exists directory
  when (not directoryExists)
    let
      -- Split the directory into parts
      directoryParts = split (Pattern FP.sep) directory

      -- Helper to rejoin those parts dropping n elements from the end
      joinParts n = joinWith FP.sep $ dropEnd n directoryParts

      go n = do
        let
          subdir = joinParts n
        if (n == length directoryParts || subdir == "") then
          -- If already recursed the whole array, or we are trying to create an
          -- empty directory, it's our base case of the recursion
          pure unit
        else do
          subdirExists <- FS.exists subdir
          when (not subdirExists) do
            go (n + 1)
            FS.mkdir subdir
    in
      go 0
