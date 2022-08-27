#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
  (test-case "bundle"
             (check-equal? (bundle (explode "abcdefg") 3)
                           (list "abc" "def" "g"))
             (check-equal? (bundle (explode "abcdefgh") 2)
                           (list "ab" "cd" "ef" "gh"))
             (check-equal? (bundle (explode "abcdefgh") 1)
                           (list "a" "b" "c" "d" "e" "f" "g" "h"))
             (check-equal? (bundle '() 2)
                           '())
             (check-equal? (bundle '("a" "b") 3)
                           (list "ab"))
             (check-exn exn:fail? (thunk (bundle '("") 3)))))
(run-tests pruebas 'verbose)
