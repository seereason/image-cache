module Data.FileCache.Pipify (heifConvert, pipify) where

import Data.List (intercalate)
import System.Process (showCommandForUser)

-- | The heif-convert program in nix package libheif does not accept "-" in place of a filename, to indicate either stdin or stdout.
-- The scale function in ImageIO expects shell commands that it builds into a pipeline.
-- pipify fills the gap.

heifConvert :: String
heifConvert = pipify ".heic" ".jpg" hc
  where hc i o = showCommandForUser "heif-convert"  [i, o]

-- | pipify wraps a shell command that only accepts filenames as input/output arguments and constructs
-- a shell pipeline that reads from stdin, writes to stdout, using tempfiles to construct arguments
-- for the original command.
-- The parameterization of tmp/tempfile is imperfect.

pipify :: String -> String -> (FilePath -> FilePath -> String) -> String
pipify inSuffix outSuffix cmd = (wrap . semis) [ var "in" (tmp ["-s", inSuffix])
                                               , var "out" (tmp ["-s", outSuffix])
                                               , "cat - >${in}"
                                               , wrap (cmd "${in}" "${out}") <> " >/dev/null 2>&1"
                                               , "cat ${out}"
                                               ]
  where wrap s = "(" <> s <> ")"
        semis = intercalate " ; "
        var v c = v <> "=$" <> wrap c
        tmp args = showCommandForUser "tempfile " args

