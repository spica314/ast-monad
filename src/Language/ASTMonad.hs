{-# LANGUAGE GADTs #-}
module Language.ASTMonad
  ( Code(..)
  , CodeSeq(..)
  , ASTM(..)
  , fromCodeSeq
  , toCodeSeq
  , getParam
  , getEnv
  , putEnv
  , modifyEnv
  , buildAST
  , fromCode
  , fromCode'
  ) where

import Data.Monoid

data Code s = Code s (CodeSeq s)
newtype CodeSeq s = CodeSeq { getCodeSeq :: [Code s] -> [Code s] }

instance Monoid (CodeSeq s) where
  mempty = CodeSeq $ \xs -> [] ++ xs
  mappend (CodeSeq f) (CodeSeq g) = CodeSeq $ f . g

fromCodeSeq :: CodeSeq s -> [Code s]
fromCodeSeq (CodeSeq f) = f []

toCodeSeq :: [Code s] -> CodeSeq s
toCodeSeq xs = CodeSeq $ \ys -> xs ++ ys

newtype ASTM p s e a = ASTM { runASTM :: p -> e -> (a, e, CodeSeq s) }

instance Functor (ASTM p s e) where
  fmap f (ASTM rA) = ASTM $ \p e -> case rA p e of
                                      (a', e', cs') -> (f a', e', cs')

instance Applicative (ASTM p s e) where
  pure x = ASTM $ \p e -> (x, e, mempty)
  (<*>) (ASTM rA) (ASTM rA') = ASTM $ \p e -> case rA p e of
                                                (f', e', cs') -> case rA' p e' of
                                                                   (a'', e'', cs'') -> (f' a'', e'', cs' <> cs'')

instance Monad (ASTM p s e) where
  (>>=) (ASTM rA) f = ASTM $ \p e -> case rA p e of
                                       (a', e', cs') -> case f a' of
                                                          ASTM rA' -> case rA' p e' of
                                                                       (b'', e'', cs'') -> (b'', e'', cs' <> cs'')
                                                                                       
getParam :: (p -> a) -> ASTM p s e a
getParam f = ASTM $ \p e -> (f p, e, mempty)

getEnv :: ASTM p s e e
getEnv = ASTM $ \p e -> (e, e, mempty)

putEnv :: e -> ASTM p s e ()
putEnv e = ASTM $ \p e -> ((), e, mempty)

modifyEnv :: (e -> e) -> ASTM p s e ()
modifyEnv f = ASTM $ \p e -> ((), f e, mempty)

buildAST :: ASTM p s e () -> p -> e -> (e, CodeSeq s)
buildAST (ASTM x) p e = case x p e of
                       (_,e',z) -> (e', z)

fromCode :: (p -> e -> (e, s)) -> ASTM p s e ()
fromCode f = ASTM $ \p e -> let (e', s') = f p e in ((), e', toCodeSeq [Code s' mempty])

fromCode' :: ASTM p s e () -> (p -> e -> (e, s)) -> ASTM p s e ()
fromCode' b f = ASTM $ \p e -> let (e', s') = f p e
                                   (e'', cs'') = buildAST b p e'
                               in ((), e'', toCodeSeq [Code s' cs''])
