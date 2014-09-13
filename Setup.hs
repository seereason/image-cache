#!/usr/bin/env runghc

import Distribution.Simple
import System.Exit
import System.Process

main :: IO ()
main = defaultMainWithHooks (withQuickTests simpleUserHooks)

withQuickTests hooks =
    hooks { postBuild = \ args cf pd lbi -> do
                          let cmd = "runhaskell"
                              args = ["-iTests", "Tests/Main.hs"]
                          (code, out, err) <- readProcessWithExitCode cmd args ""
                          case code of
                            ExitFailure _ -> error (showCommandForUser cmd args ++ " -> " ++ show code ++
                                                    "\nstdout:\n" ++ out ++ "\nstderr:\n" ++ err)
                            _ -> putStr out >> postBuild simpleUserHooks args cf pd lbi }
