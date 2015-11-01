-- Program slicing
-- LastChange: Tue Feb 07 08:00:12 2012

module Slice {-(programSlice)-} where

-- Main entry of this module is
--   programSlice :: Prog -> StmtNo -> [StmtNo]
-- This function takes a program of type Prog (defined below) with
-- a statement of it, and returns the sliced program as a list of
-- their statement numbers.
-- Example: programSlice prog1 10 => [1,3,4,5,9,10]


import Data.List
import Debug.Trace

type Var = String
type ProgLabel = String
type ProgDefs = [Var]
type ProgRefs = [Var]
type ProgConts = [ProgLabel]
type ProgComment = String
type Prog = [(ProgLabel, ProgDefs, ProgRefs, ProgConts, ProgComment)]


-- Test data

prog1 :: Prog -- mizobuchi-03.pdf - a tiny sample of slicing
prog1 = [
        ([],[],[],[],                   "start"),
        ([],["a"],[],[],                "a=1;"),
        ([],["b"],[],[],                "b=2;"),
        ([],[],["a"],["L4","L6"],       "if (a==1) {"),
        ("L4",["a"],[],[],              "  a=3;"),
        ([],["c"],["a"],["L10"],        "  c=a;} else {"),
        ("L6",[],["a"],["L7","L8"],     "  if (a==2)"),
        ("L7",["c"],[],["L9"],          "    c=1;"),
        ("L8",["c"],["a"],["L9"],       "    else c=a+2;"),
        ("L9",["c"],["a"],[],           "  c=a+1;}"),
        ("L10",[],["c"],[],             "return c;")
        ]
fg1 = makeFG prog1

prog2 :: Prog -- Shimomura 95 p.6
prog2 = [
        ([],[],[],[],                   "start"),
        ([],["inw"],[],[],              "inw = FALSE;"),
        ([],["nl"],[],[],               "nl = 0;"),
        ([],["nw"],[],[],               "nw = 0;"),
        ([],["nc"],[],[],               "nc = 0;"),
        ([],["c"],[],[],                "c = read();"),
        ("L6",[],["c"],["L7","L16"],    "while (c != EOF) {"),
        ("L7",["nc"],["nc"],[],         "  nc++;"),
        ([],[],["c"],["L9","L10"],      "  if (c == '\\n')"),
        ("L9",["nl"],["nl"],[],         "    nl++;"),
        ("L10",[],["c"],["L11","L12"],  "  if (c==' '||c=='\\n'||c=='\\t')"),
        ("L11",["inw"],[],[],           "    inw = FALSE;"),
        ("L12",[],["inw"],["L13","L15"],"  else if (inw == FALSE) {"),
        ("L13",["inw"],[],[],           "    inw = TRUE;"),
        ([],["nw"],["nw"],[],           "    nw++; }"),
        ("L15",["c"],[],["L6"],         "  c = read(); }"),
        ("L16",[],["nl"],[],            "write(nl);"),
        ([],[],["nw"],[],               "write(nw);"),
        ([],[],["nc"],[],               "write(nc);")
        ]
fg2 = makeFG prog2

prog3 :: Prog -- Tiger p.383 - for reaching analysis
prog3 = [
        ([],[],[],[],""),
        ([],["a"],[],[],""),
        ([],["c"],[],[],""),
        ("L1",[],["c","a"],["L3","L2"],""),
        ("L3",["c"],["c"],[],""),
        ([],[],[],["L1"],""),
        ("L2",["a"],["c","a"],[],""),
        ([],["c"],[],[],"")
        ]
fg3 = makeFG prog3

prog4 :: Prog -- Tiger p.409 - for dominator tree
prog4 = [
        ([],[],[],[],""),
        ("L1",[],[],["L2"],""),
        ("L2",[],[],["L3","L4"],""),
        ("L3",[],[],["L2"],""),
        ("L4",[],[],["L2","L5","L6"],""),
        ("L5",[],[],["L7","L8"],""),
        ("L6",[],[],["L7"],""),
        ("L7",[],[],["L11"],""),
        ("L8",[],[],["L9"],""),
        ("L9",[],[],["L8","L10"],""),
        ("L10",[],[],["L5","L12"],""),
        ("L11",[],[],["L12"],""),
        ("L12",[],[],[],"")
        ]
fg4 = makeFG prog4

prog5 :: Prog -- Tiger p.433 - for dominance frontier
prog5 = [
        ([],[],[],[],""),
        ("L1",[],[],["L2","L5","L9"],""),
        ("L2",[],[],["L3"],""),
        ("L3",[],[],["L3","L4"],""),
        ("L4",[],[],["L13"],""),
        ("L5",[],[],["L6","L7"],""),
        ("L6",[],[],["L4","L8"],""),
        ("L7",[],[],["L8","L12"],""),
        ("L8",[],[],["L5","L13"],""),
        ("L9",[],[],["L10","L11"],""),
        ("L10",[],[],["L12"],""),
        ("L11",[],[],["L12"],""),
        ("L12",[],[],["L13"],""),
        ("L13",[],[],[],"")
        ]
fg5 = makeFG prog5


-- Flow graph
-- makeFG

type StmtNo = Int
data Stmt = Stmt {stmt_no::StmtNo,
                  stmt_defs::[Var], 
                  stmt_refs::[Var],
                  stmt_succ::[StmtNo],
                  stmt_pred::[StmtNo],
                  stmt_comment::String } deriving (Show,Read)

data FG = FG {fg_stmts::[Stmt]} deriving (Show,Read)

fg_stmtnos :: FG -> [StmtNo]
fg_stmtnos fg = map stmt_no (fg_stmts fg)

fg_startstmt :: FG -> Stmt
fg_startstmt fg = head (fg_stmts fg)

fg_endstmt :: FG -> Stmt
fg_endstmt fg = last (fg_stmts fg)

fg_findstmt :: FG -> StmtNo -> Stmt
fg_findstmt fg no = case find (\s->stmt_no s==no) (fg_stmts fg) of
                    Nothing -> error ("fg_findstmt: not found: "++show no)
                    Just s -> s

ppFG :: FG -> String
ppFG (FG stmts) = unlines $ map ppStmt stmts

ppStmt :: Stmt -> String
ppStmt (Stmt no defs refs succ pred comment) =
                padSpace 5 (show no++":") ++
                padSpace 10 (fa defs refs) ++
                padSpace 30 (show pred++"->"++
                             show no++"->"++
                             show succ) ++
                comment
        where
        fa defs refs = ic defs ++":="++ ic refs
        ic [] = ""
        ic ss = (foldl1 (\x y->x++","++y) ss)

padSpace :: Int -> String -> String
padSpace w s = s ++ replicate (w - length s) ' '

makeFG :: Prog -> FG
makeFG prog = FG stmts
        where
        succ = makeSuccAlist prog
        pred = inverseDigraph succ
        prog2 = zip4 [0..] succ pred prog
        stmts = map mkstmt prog2
        mkstmt (no,(_,succ),(_,pred),(label,defs,refs,cont,comment))
                = Stmt no defs refs succ pred comment

makeSuccAlist :: Prog -> Digraph StmtNo
makeSuccAlist prog = map f prog1
        where
        nstmts = length prog
        prog1 = zip [0..] prog
        f (no,(_,_,_,cont,_)) = (no,ss no cont)
        ss no [] | no+1==nstmts = []
        ss no [] = [no+1]
        ss no ls = map g ls
        g l = case findIndex (\(label,_,_,_,_)->label==l) prog of
              Nothing -> error ("makeSuccAlist: undefined label: "++l)
              Just no -> no

renumberFG :: (StmtNo -> StmtNo) -> FG -> FG
renumberFG renum fg = FG{fg_stmts=map (renumberStmt renum) (fg_stmts fg)}

renumberStmt :: (StmtNo -> StmtNo) -> Stmt -> Stmt
renumberStmt renum stmt@Stmt{stmt_no=no,
                             stmt_succ=succ,
                             stmt_pred=pred}
                = stmt {stmt_no=renum no,
                        stmt_succ=map renum succ,
                        stmt_pred=map renum pred}

reverseFG :: FG -> FG
reverseFG (FG stmts) = FG stmt1
        where
        stmt1 = map f (reverse stmts)
        f stmt@Stmt{stmt_succ=succ, stmt_pred=pred} =
               stmt{stmt_succ=pred, stmt_pred=succ}

filterFG :: (Stmt -> Bool) -> FG -> FG
filterFG f fg = FG (filter f (fg_stmts fg))


-- Digraph utilities

type Digraph a = [(a,[a])]

ppDigraph :: (Ord a,Show a) => Digraph a -> String
ppDigraph dig = concatMap f (sortDigraph dig)
        where
        f (n,[]) = ""
        f (n,ss) = padSpace 3 (show n)
                   ++ " ->"
                   ++ foldl (\x y->x++" "++show y) "" ss
                   ++ "\n"

makeDigraph :: Ord a => [a] -> [(a,a)] -> Digraph a
makeDigraph vs es = succ3
        -- makeDigraph [1,2,3,4,5,6] [(1,2),(3,1),(1,3),(3,4),(5,4)]
        -- => [(1,[2,3]),(2,[]),(3,[1,4]),(4,[]),(5,[4]),(6,[])]
        where
        sot xs = sortBy (\(x,_)(y,_)->compare x y) xs
        succ1 = groupBy (\(x,_)(y,_)->x==y) (sot es)
        succ2 = [(n, map snd ps)|ps@((n,_):_)<-succ1]
        succ3 = sot ([(u,[])|u<-rest]++succ2)
                where
                rest =  vs \\ map fst succ2

unmakeDigraph :: Ord a => Digraph a -> ([a],[(a,a)])
unmakeDigraph dig = (vs,es)
        -- unmakeDigraph [(1,[2,3]),(2,[]),(3,[1,4]),(4,[]),(5,[4]),(6,[])]
        -- => ([1,2,3,4,5,6],[(1,2),(1,3),(3,1),(3,4),(5,4)])
        where
        vs = map head $ group $ sort [n1|(n,ss)<-dig,n1<-n:ss]
        es = sortBy (\(x,_)(y,_)->compare x y) [(n,s)|(n,ss)<-dig,s<-ss]

sortDigraph :: Ord a => Digraph a -> Digraph a
sortDigraph dig = sortBy (\(x,_)(y,_)->compare x y) [(n,sort ss)|(n,ss)<-dig]
        
inverseDigraph :: Ord a => Digraph a -> Digraph a
inverseDigraph dig = makeDigraph vs es'
        -- inverseDigraph [(1,[2,3]),(2,[]),(3,[1,4]),(4,[]),(5,[4])]
        -- => [(1,[3]),(2,[1]),(3,[1]),(4,[3,5]),(5,[])]
        where
        (vs,es) = unmakeDigraph dig
        es' = [(y,x)|(x,y)<-es]

unionDigraph :: Ord a => Digraph a -> Digraph a -> Digraph a
unionDigraph dig1 dig2 = dig
        where
        (vs1,es1) = unmakeDigraph dig1
        (vs2,es2) = unmakeDigraph dig2
        vs = union vs1 vs2
        es = union es1 es2
        dig = makeDigraph vs es

traverseDigraph :: Eq a => Digraph a -> a -> [a]
traverseDigraph succsalist root = visit [root] []
        -- traverseDigraph [(1,[2,3]),(2,[4,5]),(3,[6,4]),(10,[11,12])] 1
        -- => [6,3,5,4,2,1]
        where
        visit roots visited = foldl f visited roots
                where
                f x y | y `elem` x = x
                f x y = visit (succs y) (y:x)
                succs x = case lookup x succsalist of
                          Nothing ->[]
                          Just ss -> ss


-- Reaching definitions

type ReachDef = (StmtNo,Var)
type ReachIn = [ReachDef]
type ReachOut = [ReachDef]
type ReachInfo = (ReachIn,ReachOut)
type ReachAlist = [(StmtNo,ReachInfo)]

ppReachAlist reachalist = concatMap f reachalist
        where
        f (no,(reachin,reachout)) =
               padSpace 5  (show no++":") ++
               padSpace 20 ("I: "++show reachin) ++ "\n" ++
               padSpace 20 ("     O: "++show reachout) ++ "\n"

getReachInfo :: StmtNo -> ReachAlist -> ReachInfo
getReachInfo no reachalist = case lookup no reachalist of
                             Just reachinfo -> reachinfo

getReachIn :: StmtNo -> ReachAlist -> ReachIn
getReachIn no reachalist = fst (getReachInfo no reachalist)

computeReachAlist :: FG -> ReachAlist
computeReachAlist fg = reachalist1
        -- putStr $ ppReachAlist $ computeReachAlist $ makeFG prog2
        where
        (reachalist,_) = until (\(_,changed)->changed==False)
                               (updateReachAlist fg . fst)
                               (updateReachAlist fg [])
        reachalist1 = [(no,getReachInfo no reachalist)|no<-fg_stmtnos fg]
                       
updateReachAlist :: FG -> ReachAlist -> (ReachAlist,Bool)
updateReachAlist (FG stmts) reachalist0 = foldl f (reachalist0,False) stmts
        where
        f (reachalist,changed) stmt = (reachalist1, changed || changed1)
                where
                (reachalist1,changed1) = updateReachAlist1 stmt reachalist

updateReachAlist1 :: Stmt -> ReachAlist -> (ReachAlist,Bool)
updateReachAlist1 stmt@Stmt{stmt_no=no} reachalist = (reachalist1, changed)
        where
        reachinfo = reachInfo stmt reachalist
        changed = case lookup no reachalist of
                  Nothing -> True
                  Just reachinfo1 -> reachinfo1 /= reachinfo
        reachalist1 = if changed then (no, reachinfo):reachalist
                                 else reachalist

reachInfo :: Stmt -> ReachAlist -> ReachInfo
reachInfo (Stmt {stmt_no=no, stmt_defs=defs, stmt_pred=pred}) reachalist =
                        (sort reachin, sort reachout)
        where
        findreachout no = case lookup no reachalist of
                          Nothing -> []
                          Just (_,reachout) -> reachout
        reachin = foldl (\os no->union os (findreachout no)) [] pred
        reachout = union [(no,v)|v<-defs] reachout1
                where
                reachout1 = filter (\(no,v)->v `notElem` defs) reachin


-- Data dependency

type DataDependency = Digraph StmtNo

datadependency1 = computeDataDependency fg1

computeDataDependency :: FG -> DataDependency
computeDataDependency fg = inverseDigraph ddalist
        where
        reachalist = computeReachAlist fg
        ddalist = map f (fg_stmts fg)
        f Stmt{stmt_no=no,stmt_refs=refs} = (no, deps)
                where
                deps = [n|(n,v)<-(getReachIn no reachalist),v `elem` refs]


-- Dominator

type DomInfo = [StmtNo]
type DomAlist = [(StmtNo,DomInfo)]

ppDomAlist :: DomAlist -> String
ppDomAlist domalist = concatMap f domalist
        where
        f (no,ds) = padSpace 5 (show no++":") ++ show ds ++ "\n"

getDomInfo :: StmtNo -> DomAlist -> DomInfo
getDomInfo no domalist = case lookup no domalist of
                         Just dominfo -> dominfo

computeDomAlist :: FG -> DomAlist
computeDomAlist fg = domalist1
        -- putStr $ ppDomAlist $ computeDomAlist $ makeFG prog4
        where
        stmtnos = fg_stmtnos fg
        stmtnos1 = tail stmtnos
        startno = stmt_no (fg_startstmt fg)
        domalist0 = (startno,[startno]):[(no,stmtnos)|no<-stmtnos1]
        (domalist,_) = until (\(_,changed)->changed==False)
                             (updateDomAlist fg . fst)
                             (updateDomAlist fg domalist0)
        domalist1 = [case lookup no domalist of
                     Just dominfo -> (no,dominfo)
                     | no<-stmtnos]

updateDomAlist :: FG -> DomAlist -> (DomAlist,Bool)
updateDomAlist fg@(FG stmts) domalist0 = foldl f (domalist0,False) (tail stmts)
        where
        stmtnos = fg_stmtnos fg
        f (domalist,changed) stmt = (domalist1, changed || changed1)
                where
                (domalist1,changed1) = updateDomAlist1 stmtnos stmt domalist

updateDomAlist1 :: [StmtNo] -> Stmt -> DomAlist -> (DomAlist,Bool)
updateDomAlist1 stmtnos stmt@Stmt{stmt_no=no} domalist = (domalist1,changed)
        where
        dominfo = domInfo stmtnos stmt domalist
        changed = case lookup no domalist of
                  Nothing -> True
                  Just dominfo1 -> dominfo1 /= dominfo
        domalist1 = if changed then (no, dominfo):domalist
                               else domalist      

domInfo :: [StmtNo] -> Stmt -> DomAlist -> DomInfo
domInfo stmtnos (Stmt {stmt_no=no, stmt_pred=pred}) domalist = sort dominfo
        where
        finddom no = case lookup no domalist of
                     Nothing -> []
                     Just dominfo -> dominfo
        dominfo = union [no] dominfo1
                where
                dominfo1 = foldl (\di no->intersect di (finddom no)) 
                                 stmtnos pred

-- Dominator tree

data DomTree = DomTree {domtree_stmtno::StmtNo, 
                        domtree_child::[DomTree]} deriving (Show,Read)

ppDomTree :: DomTree -> String
ppDomTree domtree = ppTree domtree (show.domtree_stmtno) domtree_child

ppTree tree labelfun childfun = fmt tree "" True
        where
        fmt tree vls top = str
                where
                label = labelfun tree
                child = childfun tree
                child1 = takeWhile (\x->length x >= 1) (iterate tail child)
                str = (if top then vls else (take (length vls - 3) vls)++"+--")
                        ++ label ++ "\n"
                        ++ vls ++ (case child of []-> "  "; _->"| ") ++ "\n"
                        ++ concatMap f child1
                f (tr:ts) = fmt tr (vls ++ case ts of []->"   ";_->"|  ") False

computeDomTree :: FG -> DomTree
computeDomTree fg@FG{fg_stmts=stmts} = mktr startno
        -- putStr $ ppDomTree $ computeDomTree fg3
        where
        startno = stmt_no (fg_startstmt fg)
        stmtnos = fg_stmtnos fg
        stmtnos1 = tail stmtnos
        domalist = computeDomAlist fg
        idmap = [(immDom domalist no,no)|no<-stmtnos1]
        child no = [n2|(n1,n2)<-idmap,n1==no]
        mktr no = DomTree no (map mktr (child no))

immDom :: DomAlist -> StmtNo -> StmtNo
immDom domalist no = immdom
        where
        dominfo = getDomInfo no domalist
        dominfo1 = delete no dominfo
        immp n1 = all (\n->n `notElem` dominfo || n==no || n==n1)
                      [d|(d,ds)<-domalist, n1 `elem` ds]
        immdom = case filter immp dominfo1 of
                 [] -> error ("immDom: no immediate dominator: "++show no)
                 [d] -> d

lookupDomTree :: StmtNo -> DomTree -> Maybe (Maybe StmtNo,DomTree)
lookupDomTree no domtree = pno
        where
        pno = case domtree of
              DomTree n _ | n==no -> Just (Nothing, domtree)
              _ -> f domtree 
        f (DomTree n child) = case find (\(DomTree n1 _)->n1==no) child of
                              Nothing -> foldr g Nothing (map f child)
                              Just x -> Just (Just n, x)
                where
                g Nothing y = y
                g x y = x

getImmDom :: DomTree -> StmtNo -> StmtNo
getImmDom domtree no = no1
        where
        (parent, _) = case lookupDomTree no domtree of Just x -> x
        no1 = case parent of
              Nothing -> error ("getImmDom: no immediate dominator: "++show no)
              Just n -> n

getChildDom :: DomTree -> StmtNo -> [StmtNo]
getChildDom domtree no = child1
        where
        (_, DomTree _ child) = case lookupDomTree no domtree of Just x -> x
        child1 = [n|(DomTree n _)<-child]

isaDom :: DomTree -> StmtNo -> StmtNo ->  Bool
isaDom domtree no1 no2 = isadom
        where
        tree1 = case lookupDomTree no1 domtree of
                Just (_, tree) -> tree
                Nothing -> error ("isaDom: not found: "++show no1)
        isadom = case lookupDomTree no2 tree1 of
                 Just _ -> True
                 Nothing -> False

isaStrictDom :: DomTree -> StmtNo -> StmtNo ->  Bool
isaStrictDom domtree no1 no2 = no1/=no2 && isaDom domtree no1 no2


-- Dominance frontier
-- TODO: Improve efficiency

type DomFront = Digraph StmtNo

computeDomFront :: FG -> DomFront
computeDomFront fg = domfront
        where
        domtree = computeDomTree fg
        domfront = [(n,computeDomFrontStmt fg domtree n)
                    |(Stmt {stmt_no=n})<-fg_stmts fg]

computeDomFrontStmt fg domtree n = df
        where
        dflocal = computeDomFrontLocal fg domtree n
        child = getChildDom domtree n
        df = foldl f dflocal child
                where
                f d c = computeDomFrontAddUp fg domtree n d dfc
                        where
                        dfc = computeDomFrontStmt fg domtree c

computeDomFrontLocal fg domtree n = dflocal
        where
        succ = stmt_succ (fg_findstmt fg n)
        dflocal = foldl f [] succ
                where f s y = if getImmDom domtree y /= n && y `notElem` s
                              then y:s
                              else s

computeDomFrontAddUp fg domtree n dflocal dfc = dfup
        where
        dfup = foldl f dflocal dfc
                where
                f s w = if not (isaStrictDom domtree n w) && w `notElem` s
                        then w:s
                        else s

-- Control dependency

type ControlDependency = Digraph StmtNo

computeControlDependency :: FG -> ControlDependency
computeControlDependency fg = inverseDigraph (computeDomFront (reverseFG fg))
        

-- Slice dependency

type SliceDependency = Digraph StmtNo

computeSliceDependency :: FG -> SliceDependency
computeSliceDependency fg = dep
        where
        datadep = computeDataDependency fg
        contdep = computeControlDependency fg
        dep = unionDigraph datadep contdep


-- Slice

computeSlice :: FG -> [StmtNo]
computeSlice fg = computeSliceAt fg (stmt_no (fg_endstmt fg))

computeSliceAt :: FG -> StmtNo -> [StmtNo]
computeSliceAt fg no = sort (traverseDigraph depr no)
        where
        dep = computeSliceDependency fg
        depr = inverseDigraph dep


-- Main entry

programSlice :: Prog -> StmtNo -> [StmtNo]
programSlice prog no = computeSliceAt (makeFG prog) no


-- Demo

demoSlice :: Prog -> String -> IO ()
demoSlice prog flags = demoSliceAt prog flags (length prog - 1)

demoSliceAt :: Prog -> String -> StmtNo -> IO ()
demoSliceAt prog flags no = putStr str
        where
        fg = makeFG prog
        fgr = reverseFG fg
        slice = computeSliceAt fg no
        slicedfg = filterFG (\stmt->stmt_no stmt `elem` slice) fg
        dump c s = if c `elem` flags then s else ""
        --
        reachalist = computeReachAlist fg
        dump_r =  dump 'r' ("*** Reaching definitions:\n" ++
                                ppReachAlist reachalist ++"\n")
        --
        datadep = computeDataDependency fg
        contdep = computeControlDependency fg
        domtree = computeDomTree fg
        domfront = computeDomFront fg
        --
        datadepr = inverseDigraph datadep
        contdepr = inverseDigraph contdep
        domtreer = computeDomTree fgr
        domfrontr = computeDomFront fgr
        --
        dump_d = dump 'd' ("*** Data dependency:\n" ++
                                ppDigraph datadep ++"\n")
        dump_c = dump 'c' ("*** Control dependency:\n" ++
                                ppDigraph contdep ++"\n")
        dump_t = dump 't' ("*** Dominator tree:\n" ++
                                ppDomTree domtree ++"\n")
        dump_f = dump 'f' ("*** Dominance frontier:\n" ++
                                ppDigraph domfront ++"\n")
        dump_D = dump 'D' ("*** Data dependency (inverse relation):\n" ++
                                ppDigraph datadepr ++"\n")
        dump_C = dump 'C' ("*** Control dependency (inverse relation):\n" ++
                                ppDigraph contdepr ++"\n")
        dump_T = dump 'T' ("*** Dominator tree (of reverse flow graph)):\n" ++
                                ppDomTree domtreer ++"\n")
        dump_F = dump 'F'("*** Dominance frontier (of reverse flow graph):\n"++
                       ppDigraph domfrontr ++"\n")
        str = "*** Input program:\n" ++ ppFG fg ++"\n"++
              dump_r++
              dump_d++dump_D++
              dump_c++dump_C++
              dump_t++dump_T++
              dump_f++dump_F++
              "*** Slice at "++show no++":\n"++ ppFG slicedfg
