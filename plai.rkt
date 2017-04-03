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


(define (parse2 [s : s-expression]) : Exp
  (cond
    [(s-exp-number? s) (numExp (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(s-exp-symbol? (first sl))
          (case (s-exp->symbol (first sl))
            [(+) (plusExp (parse2 (second sl)) (parse2 (third sl)))]
            [(*) (mulExp (parse2 (second sl)) (parse2 (third sl)))]
            [else (error 'parse2 "invalid list input")])]
         [else (error 'parse2 "invalid list")]))]
    [else (error 'parse2 "invalid input")]))


(define (interp (exp : Exp)) : number
  (cond
    [(numExp? exp) (numExp-n exp)]
    [(plusExp? exp) (+ (interp (plusExp-left exp)) (interp (plusExp-right exp)))]
    [(mulExp? exp) (* (interp (mulExp-left exp)) (interp (mulExp-right exp)))]
    [else (error 'interp "error")]))


(define (interp2 (exp : Exp)) : number
  (type-case Exp exp
    [numExp (n) n]
    [plusExp (l r) (+ (interp2 l) (interp2 r))]
    [mulExp (l r) (* (interp2 l) (interp2 r))]))



