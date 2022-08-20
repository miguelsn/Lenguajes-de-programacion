#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
  (test-case "countdown"
    (check-equal? (countdown 5)
                  '(5 4 3 2 1 0))
    (check-equal? (countdown 0)
                  '(0)))
  
  (test-case "insertL"
    (check-equal? (insertL 'x 'y '(x z z x y x))
                  '(y x z z y x y y x))
    (check-equal? (insertL 'x 'y '(z z z z z z z))
                  '(z z z z z z z))
    (check-equal? (insertL 'x 'y '(x x x x x x))
                  '(y x y x y x y x y x y x)))
  
  
  (test-case "remv-1st"
    (check-equal? (remv-1st 'x '(x y z x))
                  '(y z x))
    (check-equal? (remv-1st 'y '(x y z y x))
                  '(x z y x))
    (check-equal? (remv-1st 'z '(a b c))
                  '(a b c))
    (check-equal? (remv-1st 'x '())
                  '()))
  
  (test-case "map"
    (check-equal? (map sub1 '(1 2 3 4))
                  '(0 1 2 3))
    (check-equal? (map countdown '(1 2 3 4))
                  '((1 0) (2 1 0) (3 2 1 0) (4 3 2 1 0)))
    (check-equal? (map countdown '())
                  '()))
  
  (test-case "filter"
    (check-equal? (filter even? '(1 2 3 4 5 6))
                  '(2 4 6))
    (check-equal? (filter odd? '(1 2 3 4 5 6))
                  '(1 3 5))
    (check-equal? (filter zero? '(1 2 3 4 5 6))
                  '())
    (check-equal? (filter null? '(() (1 2)))
                  '(())))
  
  (test-case "zip"
    (check-equal? (zip '(1 2 3) '(a b c))
                  '((1 . a) (2 . b) (3 . c)))
    (check-equal? (zip '(1 2 3 4 5 6) '(a b c))
                  '((1 . a) (2 . b) (3 . c)))
    (check-equal? (zip '(1 2 3) '(a b c d e f))
                  '((1 . a) (2 . b) (3 . c)))
    (check-equal? (zip '(1 2 3) '())
                  '())
    (check-equal? (zip '() '(1 2))
                  '()))
  
  (test-case "list-index-ofv"
    (check-eqv? (list-index-ofv 'x '(x y z x x)) 0)
    (check-eqv? (list-index-ofv 'x '(y z x x)) 2)
    (check-eqv? (list-index-ofv 'n '(x y z x x)) -1)
    (check-eqv? (list-index-ofv 'x '()) -1))
  
  (test-case "append"
    (check-equal? (append '(42 120) '(1 2 3))
                  '(42 120 1 2 3))
    (check-equal? (append '(a b c) '(cat dog))
                  '(a b c cat dog))
    (check-equal? (append '(a b c) '())
                  '(a b c))
    (check-equal? (append '() '(a b c))
                  '(a b c))
    (check-equal? (append '() '())
                  '()))
  
  (test-case "reverse"
    (check-equal? (reverse '(a 3 x))
                  '(x 3 a))
    (check-equal? (reverse '())
                  '()))
  
  (test-case "repeat"
    (check-equal? (repeat '(4 8 11) 4)
                  '(4 8 11 4 8 11 4 8 11 4 8 11))
    (check-equal? (repeat '() 4)
                  '()))
  
  (test-case "same-lists*"
    (check-true (same-lists* '() '()))
    (check-true (same-lists* '(1 2 3 4 5) '(1 2 3 4 5)))
    (check-false (same-lists* '(1 2 3 4) '(1 2 3 4 5)))
    (check-false (same-lists* '(a (b c) d) '(a (b) c d)))
    (check-true (same-lists* '((a) b (c d) d) '((a) b (c d) d))))
  
  (test-case "binary->natural"
    (check-eqv? (binary->natural '()) 0)
    (check-eqv? (binary->natural '(0 0 1)) 4)
    (check-eqv? (binary->natural '(0 0 1 1)) 12)
    (check-eqv? (binary->natural '(1 1 1 1)) 15)
    (check-eqv? (binary->natural '(1 0 1 0 1)) 21)
    (check-eqv? (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191))
  
  (test-case "div"
    (check-eqv? (div 25 5) 5)
    (check-eqv? (div 36 6) 6)
    (check-eqv? (div 36 7) #f))
  
  (test-case "append-map"
    (check-equal? (append-map countdown (countdown 5))
                  '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0))
    (check-equal? (append-map countdown '())
                  '()))
  
  (test-case "set-difference"
    (check-equal? (set-difference '(1 2 3 4 5) '(2 6 4 8))
                  '(1 3 5))
    (check-equal? (set-difference '(1 2 3 4 5) '())
                  '(1 2 3 4 5))
    (check-equal? (set-difference '() '())
                  '()))
  
  (test-case "foldr"
    (check-equal? (foldr cons '() '(1 2 3 4))
                  '(1 2 3 4))
    (check-eqv? (foldr + 0 '(1 2 3 4))
                10)
    (check-eqv? (foldr * 1 '(1 2 3 4))
                24)
    (check-eqv? (foldr * 1 '())
                1))
  
  (test-case "powerset"
    (check-equal? (powerset '(3 2 1))
                  '((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ()))
    (check-equal? (powerset '())
                  '(())))
  
  (test-case "cartesian-product"
    (check-equal? (cartesian-product '((5 4) (3 2 1)))
                  '(((5 3) (5 2) (5 1) (4 3) (4 2) (4 1))))
    (check-equal? (cartesian-product '((a b c) (1 2 3 4)))
                  '(((a 1) (a 2) (a 3) (a 4) (b 1) (b 2) (b 3) (b 4) (c 1) (c 2) (c 3) (c 4))))
    (check-equal? (cartesian-product '((a b c) (1 2 3 4) ("foo" "bar" "baz")))
                  '(((a 1 "foo") (a 1 "bar") (a 1 "baz") (a 2 "foo") (a 2 "bar") (a 2 "baz") (a 3 "foo") (a 3 "bar") (a 3 "baz")
                     (a 4 "foo") (a 4 "bar") (a 4 "baz") (b 1 "foo") (b 1 "bar") (b 1 "baz") (b 2 "foo") (b 2 "bar") (b 2 "baz")
                     (b 3 "foo") (b 3 "bar") (b 3 "baz") (b 4 "foo") (b 4 "bar") (b 4 "baz") (c 1 "foo") (c 1 "bar") (c 1 "baz")
                     (c 2 "foo") (c 2 "bar") (c 2 "baz") (c 3 "foo") (c 3 "bar") (c 3 "baz") (c 4 "foo") (c 4 "bar") (c 4 "baz")))))

  (test-case "foldrs"
     (test-case "foldr-insert"
        (check-equal? (insertL-fr 'x 'y '(x z z x y x))
                  '(y x z z y x y y x))
        (check-equal? (insertL-fr 'x 'y '(z z z z z z z))
                  '(z z z z z z z))
        (check-equal? (insertL-fr 'x 'y '(x x x x x x))
                  '(y x y x y x y x y x y x)))
     (test-case "filter-fr"
         (check-equal? (filter-fr even? '(1 2 3 4 5 6))
                  '(2 4 6))
         (check-equal? (filter-fr odd? '(1 2 3 4 5 6))
                  '(1 3 5))
         (check-equal? (filter-fr zero? '(1 2 3 4 5 6))
                  '())
         (check-equal? (filter-fr null? '(() (1 2)))
                  '(())))
     (test-case "map-fr"
          (check-equal? (map-fr sub1 '(1 2 3 4))
                  '(0 1 2 3))
          (check-equal? (map-fr countdown '(1 2 3 4))
                  '((1 0) (2 1 0) (3 2 1 0) (4 3 2 1 0)))
          (check-equal? (map-fr countdown '())
                  '()))
     (test-case "append-fr"
          (check-equal? (append-fr '(42 120) '(1 2 3))
                  '(42 120 1 2 3))
          (check-equal? (append-fr '(a b c) '(cat dog))
                  '(a b c cat dog))
          (check-equal? (append-fr '(a b c) '())
                  '(a b c))
          (check-equal? (append-fr '() '(a b c))
                  '(a b c))
          (check-equal? (append-fr '() '())
                  '()))
     (test-case "reverse-fr"
          (check-equal? (reverse-fr '(a 3 x))
                  '(x 3 a))
          (check-equal? (reverse-fr '())
                  '()))
     (test-case "binary->natural-fr"
          (check-eqv? (binary->natural-fr '()) 0)
          (check-eqv? (binary->natural-fr '(0 0 1)) 4)
          (check-eqv? (binary->natural-fr '(0 0 1 1)) 12)
          (check-eqv? (binary->natural-fr '(1 1 1 1)) 15)
          (check-eqv? (binary->natural-fr '(1 0 1 0 1)) 21)
          (check-eqv? (binary->natural-fr '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191))
     (test-case "append-map-fr"
          (check-equal? (append-map-fr countdown (countdown 5))
                  '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0))
          (check-equal? (append-map-fr countdown '())
                  '()))
     (test-case "set-difference-fr"
           (check-equal? (set-difference-fr '(1 2 3 4 5) '(2 6 4 8))
                  '(1 3 5))
           (check-equal? (set-difference-fr '(1 2 3 4 5) '())
                  '(1 2 3 4 5))
           (check-equal? (set-difference-fr '() '())
                  '()))
     (test-case "powerset-fr"
           (check-equal? (powerset-fr '(3 2 1))
                  '((3 2 1) (2 1) (3 1) (1) (3 2) (2) (3) ()))
           (check-equal? (powerset-fr '())
                  '(()))))
  
  (test-case "snowball"
    (check-eqv? (snowball 12) 1)
    (check-eqv? (snowball 120) 1)
    (check-eqv? (snowball 9999) 1)))
  

(run-tests pruebas 'verbose)
