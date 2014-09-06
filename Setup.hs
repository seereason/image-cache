#!/usr/bin/env runghc

import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
