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
             (check-exn exn:fail? (thunk (bundle '("") 3)))
             (check-exn exn:fail? (thunk (bundle (explode "abcdefgh") 0))))

  (test-case "take"
             (check-equal? (take '(1 2 3 4 5 6) 3)
                           '(1 2 3))
             (check-equal? (take '(1 2 3 4 5 6) 0)
                           '())
             (check-equal? (take '() 3)
                           '())
             (check-equal? (take '(1 2) 3)
                           '(1 2)))

(test-case "drop"
             (check-equal? (drop '(1 2 3 4 5 6) 3)
                           '(4 5 6))
             (check-equal? (drop '(1 2 3 4 5 6) 0)
                           '(1 2 3 4 5 6))
             (check-equal? (drop '() 3)
                           '())
             (check-equal? (drop '(1 2) 3)
                           '()))

  (test-case "qsort-general"
             (check-equal? (general-quicksort '(2 5 6 4 8 4 5 2 18 4) (lambda (x y) (< x y)))
                  '(2 2 4 4 4 5 5 6 8 18))
             (check-equal? (general-quicksort '(2 5 6 4 8 4 5 2 18 4) (lambda (x y) (> x y)))
                  '(18 8 6 5 5 4 4 4 2 2))
             (check-equal? (general-quicksort '("aa" "a" "aaa" "aaaa" "aaaaaaaa" "aa") (lambda (x y) (> (string-length x) (string-length y))))
                  '("aaaaaaaa" "aaaa" "aaa" "aa" "aa" "a"))
             (check-equal? (general-quicksort '("aa" "a" "aaa" "aaaa" "aaaaaaaa" "aa") (lambda (x y) (< (string-length x) (string-length y))))
                  '("a" "aa" "aa" "aaa" "aaaa" "aaaaaaaa"))))
(run-tests pruebas 'verbose)
