#lang racket

(require "../lex/lex.rkt"
         "../reg/reg.rkt"
         "token.rkt"
         "srcloc.rkt")

(define reg-nat
  (reg-repeat 1 +inf.0 (char-set "0123456789")))

(define (lex-nat src lexeme beg end)
  (token 'nat (string->number lexeme) beg end))

(define reg-var
  (reg-conc just-lower-case))

(define (lex-var src lexeme beg end)
  (token 'var lexeme beg end))

(define (lex-terminal name)
  (lambda (src lexeme beg end)
    (token name #f beg end)))

(define reg-space
  (reg-repeat 1 +inf.0 just-whitespace))
        
(define (lex-space src lexeme beg end)
  (lex-imp src))

(define reg-comment
  (reg-conc "#"))

(define (lex-comment src lexeme beg end)
  (read-line src)
  (lex-imp src))

(define reg-kw-delim
  (reg-conc (reg-union "let" "if" "zero?")
            (char-set-comp "(" ")"
                           #\space #\newline #\return #\tab
                            "-")))
(define reg-nat-delim
  (reg-conc reg-nat
            (char-set-comp "(" ")"
                           #\space #\newline #\return #\tab
                           "-")))

(define reg-var-delim
  (reg-conc reg-var
            (char-set-comp "(" ")"
                           #\space #\newline #\return #\tab
                           "-")))


(define (lex-kw-delim src lexeme beg end)
  (error/context "Se esperaba un delimitador después de una palabra reservada"
                 beg end))

(define (lex-nat-delim src lexeme beg end)
  (error/context "Se esperaba un delimitador después de un número"
                 beg end))

(define (lex-var-delim src lexeme beg end)
  (error/context "Se esperaba un delimitador después de una variable"
                 beg end))


(define lex-imp
  (make-lexer
   'imp
   (lex-rule reg-nat lex-nat)
   (lex-rule reg-var lex-var)
   (lex-rule "if" (lex-terminal 'if))
   (lex-rule "let" (lex-terminal 'let))
   (lex-rule "-" (lex-terminal '-))
   (lex-rule "=" (lex-terminal '=))
   (lex-rule "(" (lex-terminal 'oparen))
   (lex-rule ")" (lex-terminal 'cparen))
   (lex-rule "zero?" (lex-terminal 'zero))
   (lex-rule reg-space lex-space)
   (lex-rule reg-kw-delim lex-kw-delim)
   (lex-rule reg-nat-delim lex-nat-delim)
   (lex-rule reg-comment lex-comment)
   (lex-rule reg-var-delim lex-var-delim)))

(define (lex-imp-all src)
  (define tok (lex-imp src))
  (if (eof-object? tok)
      null
      (cons tok (lex-imp-all src))))

(provide lex-imp
         lex-imp-all
         (struct-out pos))
