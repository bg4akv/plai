#lang plai-typed

(define-type Exp
  [numExp (n : number)]
  [plusExp (l : Exp) (r : Exp)]
  [mulExp (l : Exp) (r : Exp)])

(define (parse [s : s-expression]) : Exp
  (cond
    [(s-exp-number? s) (numExp (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusExp (parse (second sl)) (parse (third sl)))]
         [(*) (mulExp (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "error")]))]
    [else (error 'parse "error")]))

(define (interp [e : Exp]) : number
  (type-case Exp e
    [numExp (n) n]
    [plusExp (l r) (+ (interp l) (interp r))]
    [mulExp  (l r) (* (interp l) (interp r))]))