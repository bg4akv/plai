#lang plai-typed


(define-type Exp
  (numExp (n : number))
  (plusExp (left : Exp) (right : Exp))
  (mulExp (left : Exp) (right : Exp)))

(define (parse [s : s-expression]) : Exp
  (cond
    [(s-exp-number? s) (numExp (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusExp (parse (second sl)) (parse (third sl)))]
         [(*) (mulExp (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))