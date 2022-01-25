-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module OptimizationSolutions (optimize) where

import Control.Monad.State

import AbstractSyntax
import InterpreterSolutions

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

import Data.List

type OptStorage = Identifier -> Maybe Integer

emptyOptStorage :: OptStorage
emptyOptStorage i = Nothing

deleteVar :: Identifier -> OptStorage -> OptStorage
deleteVar i m j | i == j    = Nothing
                | otherwise = m j

deleteVars :: [Identifier] -> OptStorage -> OptStorage
deleteVars [] m = m
deleteVars (i:is) m = deleteVars is (deleteVar i m)

{- Copied from Interpreter -}
number :: Bool -> Integer
number False = 0
number True  = 1

boolean :: Integer -> Bool
boolean 0 = False
boolean _ = True

opEval :: OpName -> [Integer] -> Integer
opEval Add     [x, y] = x + y
opEval Sub     [x, y] = x - y
opEval Mul     [x, y] = x * y
opEval Div     [x, y] = x `div` y
opEval Mod     [x, y] = x `mod` y
opEval Eq      [x, y] = number(x == y)
opEval Leq     [x, y] = number(x <= y)
opEval Less    [x, y] = number(x <  y)
opEval Geq     [x, y] = number(x >= y)
opEval Greater [x, y] = number(x >  y)
opEval And     [x, y] = number(boolean x && boolean y)
opEval Or      [x, y] = number(boolean x || boolean y)
opEval Not     [x]    = number(not(boolean x))
opEval op      xs     = error ("Interpreter bug. "
                            ++ "Please contact the software maintainer. "
                            ++ "Tried to apply " ++ show op
                            ++ " to " ++ show xs)
{- -}


--
--  Optimization of Expressions
--

fromConstant :: Expr -> Integer
fromConstant (Constant x) = x
fromConstant _            = undefined

isConstant :: Expr -> Bool
isConstant (Constant x) = True
isConstant _            = False

optExpr' :: OptStorage -> Expr -> Expr
optExpr' m (Constant x)       = Constant x
optExpr' m (Var i)            = case m i of
                                 Nothing -> Var i
                                 Just x  -> Constant x
optExpr' m (Op o es)
  | all isConstant es'        = Constant(opEval o (map fromConstant es'))
      where
        es' = map (optExpr' m) es

optExpr' m (Op Add es)        = case map (optExpr' m) es of
                                 [Constant 0, e] -> e
                                 [e, Constant 0] -> e
                                 es'             -> Op Add es'
optExpr' m (Op Mul es)        = case map (optExpr' m) es of
                                 [Constant 0, e] -> Constant 0
                                 [e, Constant 0] -> Constant 0
                                 es'             -> Op Mul es'
optExpr' m (Op And es)        = case map (optExpr' m) es of
                                 [Constant 0, e] -> Constant 0
                                 -- [Constant 1, e] -> e
                                 [e, Constant 0] -> Constant 0
                                 -- [e, Constant 1] -> e
                                 es'             -> Op And es'
optExpr' m (Op Or es)         = case map (optExpr' m) es of
                                 -- [Constant 0, e] -> e
                                 [Constant 1, e] -> Constant 1
                                 -- [e, Constant 0] -> e
                                 [e, Constant 1] -> Constant 1
                                 es'             -> Op Or es'
optExpr' m (Op o es)           = Op o (map (optExpr' m) es)

-- A stateful wrapper around the above
optExpr :: Expr -> State OptStorage Expr
optExpr e = do m <- get
               return $ optExpr' m e

--
--  Program Optimization
--

updatedVariables, updatedVariables'  :: Program -> [Identifier]
updatedVariables = nub . updatedVariables'
updatedVariables' (i := e) = [i]
updatedVariables' (IfElse e p q) = updatedVariables' p ++ updatedVariables' q
updatedVariables' (If e p) = updatedVariables' p
updatedVariables' (While e p) = updatedVariables' p
updatedVariables' (Block ps) = concat(map updatedVariables' ps)
updatedVariables' (Read i) = [i]
updatedVariables' (Write e) = []
updatedVariables' (Print s) = []
updatedVariables' (For i s e p) = i:(updatedVariables' p)

optUpdate :: Identifier -> Expr -> OptStorage -> OptStorage
optUpdate i e m = case e of
                  Constant x -> m' x
                  _          -> m''
 where
   m' x j | i == j    = Just x
          | otherwise = m j
   m'' j  | i == j    = Nothing
          | otherwise = m j

optPrograms :: [Program] -> State OptStorage [Program]
optPrograms [] = return []
optPrograms (p:ps) =
  do p' <- optProgram p
     case p' of
       Block [] -> optPrograms ps
       _        -> do ps' <- optPrograms ps
                      return (p':ps')

optProgram  :: Program -> State OptStorage Program

optProgram (i := e) =
  do e' <- optExpr e
     modify $ optUpdate i e'
     return (i := e')

optProgram (IfElse e p q) =
  do e' <- optExpr e
     case e' of
       Constant x -> if boolean x
                     then optProgram p
                     else optProgram q
                     -- We throw away the states obtained from optimizing
                     -- the branches ...
       e''        -> do m <- get
                        p' <- optProgram p
                        put m
                        q' <- optProgram q
                        put m
                        return (IfElse e'' p' q')

optProgram (If e p) =
  do e' <- optExpr e
     case e' of
       Constant x -> if boolean x
                     then optProgram p
                     else return (Block [])
                     -- Again, throw away intermediate state ...
       e''        -> do m <- get
                        p' <- optProgram p
                        put m
                        return (If e'' p')

optProgram (While e p) =
  do e' <- optExpr e
     case e' of
       Constant x | (not(boolean x)) -> return (Block [])
       _ -> do m <- get
               let m' = deleteVars (updatedVariables p) m
               put m'
               p' <- optProgram p
               put m'
               e'' <- optExpr e
               return (While e'' p')

optProgram (For i s e p) =
  do s' <- optExpr s
     e' <- optExpr e
     case (s',e') of
       (Constant sv , Constant ev) | ev < sv -> return (i := s')
       _ -> do m <- get
               let m' = deleteVars (i:(updatedVariables p)) m
               put m'
               p' <- optProgram p
               put m'
               s'' <- optExpr s
               e'' <- optExpr e
               return (For i s'' e'' p')

optProgram (Block ps) =
  do ps' <- optPrograms ps
     case ps' of
       [p] -> return p
       _   -> return (Block ps')

optProgram (Write e) =
  do e' <- optExpr e
     return (Write e')

optProgram (Read i) =
  do modify (deleteVar i)
     return (Read i)

optProgram (Print s) = return (Print s)

optimize :: Program -> Program
optimize p = fst $ runState (optProgram p) emptyOptStorage
