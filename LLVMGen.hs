module LLVMGen where

import AbsInstant
import Control.Monad.State

type FV a = State Integer a

printInt :: String
printInt = "declare i32 @printf(i8*, ...) \n\n@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\ndefine void @printInt(i32 %x) {\n\t%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n\tcall i32 (i8*, ...) @printf(i8* %t0, i32 %x) \n\tret void\n}\n\n"

name :: Ident -> String
name (Ident x) = "%" ++ x

varname :: Ident -> String
varname (Ident x) = "@" ++ x

runFV :: FV a -> a
runFV m = fst $ runState m 0

freshName :: String -> FV String
freshName x = do
    i <- get
    put (i + 1)
    return $ "%" ++ x ++ show i

emitStmt :: Stmt -> FV String

emitStmt (SAss var expr) = do
    (code, local) <- emitExp expr
    return $ code ++ "store i32 " ++ local ++ ", i32* " ++ varname var ++ "\n\t"
emitStmt (SExp expr) = do
    (code, var) <- emitExp expr
    return $ code ++ "call void @printInt(i32 " ++ var ++ ")\n\t"

emitExpText :: String -> Exp -> Exp -> FV (String, String)
emitExpText text e1 e2 = do
    (code1, var1) <- emitExp e1
    (code2, var2) <- emitExp e2
    resVar <- freshName "t"
    return (
        code1 ++ 
        code2 ++ 
        resVar ++ " = " ++ text ++ " i32 " ++ var1 ++ ", " ++ var2 ++ "\n\t",
        resVar)

emitExp :: Exp -> FV (String, String)
emitExp (ExpAdd e1 e2) = emitExpText "add" e1 e2
emitExp (ExpSub e1 e2) = emitExpText "sub" e1 e2
emitExp (ExpMul e1 e2) = emitExpText "mul" e1 e2 
emitExp (ExpDiv e1 e2) = emitExpText "sdiv" e1 e2
emitExp (ExpLit n) = return ("", show n)
emitExp (ExpVar name) = do
    resVar <- freshName "t"
    return (resVar ++ " = load i32, i32* " ++ varname name ++ "\n\t", resVar)


getGlobals :: [Stmt] -> String
getGlobals l =
    let getNames ((SAss var expr):t) = var:getNames t
        getNames ((SExp expr):t) = getNames t
        getNames [] = []
        makeGlobal var = (varname var) ++ " = internal global i32 0\n" in
    concat $ map makeGlobal (getNames l)

genLLVM :: [Stmt] -> String
genLLVM l = 
    printInt ++ 
    getGlobals l ++ 
    "define i32 @main() {\n\t" ++
    concat (runFV (mapM emitStmt l)) ++
    "ret i32 0\n}"