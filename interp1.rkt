#lang plai

(define-type MisspelledAnimal
  [caml (humps number?)]
  [yacc (height number?)])

(define (good? ma)
  (cond
    [(caml? ma) (>= (caml-humps ma) 2)]
    [(yacc? ma) (> (yacc-height ma) 2.1)]))

(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)]
  [condC (ie ArithC?) (te ArithC?) (ee ArithC?)])

(define (interp a)
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    [condC (ie te ee) (cond [(> (interp ie) 0) (interp te)]
                            [(interp ee)])]))

(define-type ArithS
  [numS (n number?)]
  [plusS (l ArithS?) (r ArithS?)]
  [multS (l ArithS?) (r ArithS?)]
  [bminusS (l ArithS?) (r ArithS?)]
  [uminusS (e ArithS?)])

(define (desugar as)
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]
    [uminusS (e) (plusC (numC 0) (multC (numC -1) (desugar e)))]))

; tests

(test (interp (numC 5)) 5)
(test (interp (desugar (uminusS (numS 5)))) -5)
    