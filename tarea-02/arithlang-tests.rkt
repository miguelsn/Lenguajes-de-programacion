#lang plait

(require "arithlang.rkt")


(module+ test
  (test/exn (eval `()) "operacion aritmetica mal formada")
  (test/exn (eval `(+)) "operacion aritmetica mal formada")
  (test/exn (eval `(-)) "operacion aritmetica mal formada")
  (test/exn (eval `(*)) "operacion aritmetica mal formada")
  (test/exn (eval `(n)) "operacion aritmetica mal formada")
  (test/exn (eval `(+ 2 5 6)) "operacion aritmetica mal formada")
  (test/exn (eval `(- 2 5 6)) "operacion aritmetica mal formada")
  (test/exn (eval `(* 2 5 6)) "operacion aritmetica mal formada")
  (test/exn (eval `(n 5 6)) "operacion aritmetica mal formada")
  (test (eval `(+ 2 5)) 7)
  (test (eval `(- 2 5)) -3)
  (test (eval `(* 2 5)) 10)
  (test (eval `(n 2)) -2))

