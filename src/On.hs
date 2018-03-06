{-# LANGUAGE TemplateHaskell #-}
module On where

import Language.Haskell.TH

on field = do
  f <- newName "f"
  x <- newName "x"
  return (LamE [VarP f, VarP x] (RecUpdE (VarE x) [(field, AppE (VarE f) (AppE (VarE field) (VarE x)))]))
  

-- $(on 'foo) =>
--   \f x -> x {foo = f (foo x)} 
  
