module JVMGen where

import Control.Monad.State
import Data.Map

import AbsInstant

data TCM a = State (Map Ident Integer, Integer) a 

runTCM :: TCM a -> (a, Integer)
runTCM m = let (res, (_, d)) = runState m (empty, 0) in (res, d)

associate :: Ident -> TCM Integer
associate x = do
    (assocs, n) <- get
    put (insert x n assocs, n + 1)
    return n

getAssoc :: Ident -> TCM Integer
getAssoc x = do
    (assocs, _) <- get
    case lookup x assocs of
        Just assoc -> return assoc
        Nothing -> associate x

commute :: String -> String
commute "idiv" = "swap\n\t"
commute "isub" = "swap\n\t"
commute _ = ""

emitExpGeneral :: String -> Exp -> Exp -> TCM (Integer, String)
emitExpGeneral name e1 e2 = do
    (d1, code1) <- emitExpGeneral e1
    (d2, code2) <- emitExpGeneral e2
    if d1 > d2 then 
        return (max d1 (1 + d2), code1 ++ code2 ++ name ++ "\n\t")
    else
        return (max d2 (1 + d1), code2 ++ code1 ++ commute name ++ name ++ "\n\t")

emitExp :: Exp -> TCM (Integer, String)
emitExp (ExpAdd e1 e2) = emitExpGeneral "iadd" e1 e2
emitExp (ExpSub e1 e2) = emitExpGeneral "isub" e1 e2
emitExp (ExpMul e1 e2) = emitExpGeneral "imul" e1 e2
emitExp (ExpDiv e1 e2) = emitExpGeneral "idiv" e1 e2
emitExp (ExpLit i) = return (1, 
        if i == -1 then
            "iconst_m1\n\t"
        else if 0 <= i && i <= 5 then
            "iconst_" ++ show i ++ "\n\t"
        else if -128 <= i && i <= 127 then
            "bipush " ++ show i ++ "\n\t"
        else "ldc " ++ show i ++ "\n\t" 
    )
emitExp (ExpVar v) = do
    assoc <- getAssoc v
    return (1,
            if 0 <= assoc && assoc <= 3 then
                "iload_" ++ assoc ++ "\n\t"
            else
                "iload " ++ assoc ++ "\n\t"
        )

emitStmt :: Stmt -> TCM (Integer, String)
emitStmt (SAss var expr) = do
    (d, code) <- emitExp expr
    assoc <- getAssoc var
    let space = if 0 <= assoc && assoc <= 3 then "_" else " "
    return (d, code ++ "istore" ++ space ++ assoc ++ "\n\t")
emitStmt (SExp expr) = do
    (d, code) <- emitExp expr
    return (d + 1, "getstatic java/lang/System/out Ljava/io/PrintStream;\n\t" ++ code ++ "invokevirtual java/io/PrintStream/println(I)V\n\t")


genJVM :: [Stmt] -> String
genJVM ss = 
    let codes = mapM emitStmt ss
        (cs, locals) = runTCM codes
        compare (d1, _) (d2, _) = d1 >= d2
        maxd = foldr (\x y -> if compare x y then x else y) cs
        code = concat (map snd cs) in
    ".class public Main\n" ++
    ".super java/lang/Object\n\n" ++
    ".method public <init>()V\n\taload_0\n\tinvokespecial java/lang/Object/<init>()V\n\treturn\n.end method\n\n" ++
    ".method public static main([Ljava/lang/String;)V\n" ++ 
    ".limit stack " ++ maxd ++ "\n\t" ++
    code ++
    "return\n.end method"
