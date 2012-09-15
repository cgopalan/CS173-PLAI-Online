#lang plai-typed

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (s : symbol)]
  [appC (f : symbol) (a : ExprC)])


(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [plusC (l r) (plusC (subst what for l) (subst what for r))]
    [multC (l r) (multC (subst what for l) (subst what for r))]
    [idC (s) (cond [(symbol=? s for) what][else in])]
    [appC (f a) (appC f (subst what for a))]))

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (+ (interp l fds) (interp r fds))]
    [idC (_) (error 'interp "shouldnt get here")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                   (interp (subst a
                    (fdC-arg fd)
                    (fdC-body fd)) fds))]
    )
  )

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond 
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

; tests
(define dbl (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
(test (interp (appC 'double (numC 7)) (list dbl)) 14)