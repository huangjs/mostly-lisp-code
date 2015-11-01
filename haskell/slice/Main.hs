-- Main for Slice.hs
-- LastChange: Tue Feb 07 08:09:28 2012

module Main where 

import System.Environment
import Data.List
import Debug.Trace
import Slice

main = do
        args0 <- getArgs
        let (flags,args) = getopt args0
        case args of
          [] -> putStr $
                "Usage: slice [-dctfDCTFr] program.prg [StmtNo]\n"++
                "  -d : Data dependency\n"++
                "  -D : Data dependency (inverse relatoin)\n"++
                "  -c : Control dependency\n"++
                "  -C : Control dependency (inverse relatoin)\n"++
                "  -t : Dominator tree\n"++
                "  -T : Dominator tree (of reverse flow graph)\n"++
                "  -f : Dominance frontier\n"++
                "  -F : Dominance frontier (of reverse flow graph)\n"++
                "  -r : Reaching definitions\n"++
                "Note:\n"++
                "  Data dependency is computed from reaching definitions.\n"++
                "  Dominance frontier is computed from dominator tree.\n"++
                "  -C and -F should print the same reslut.\n"
          f:ns -> do str <- readFile f
                     let prg = (read str::Prog)
                     case ns of
                       [] ->  demoSlice   prg flags
                       n:_ -> demoSliceAt prg flags (read n::Int)
        where
        getopt args = foldl f ("",[]) args
                where
                f (flags,as) ('-':f) = (flags++f,as)
                f (flags,as) a = (flags,as++[a])
