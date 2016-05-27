-- LABORATORIO DE PROGRAMACION FUNCIONAL 2016
-- MODULO PRINCIPAL

module Main where

import Syntax
import TypeChecker
import Generator

import System.Environment


main = do args <- getArgs
          compileFile (args !! 0)


compileFile name = do prg <- readFile (name ++ ".pas")
                      case  compile prg of
                               Right cprg -> writeFile (name ++ ".c") cprg
                               Left  errs -> putStr errs
                               -- habilitar para imprimir errores a archivo
                               --Left errs -> writeFile (name ++ ".ERR")


compile prg = case parser prg of
                   Right ast  -> case checkProgram ast of
                                  Right env  -> Right (genProgram ast env)
                                  Left  err  -> Left  (unlines . map show $ err) 

                   Left  cerr -> Left (show cerr)
