#lang plai-typed


(define-type Exp
  [numExp (n : number)]
  [plusExp (left : Exp) (right : Exp)]
  [mulExp (left : Exp) (right : Exp)]
  [subExp (left : Exp) (right : Exp)]
  [negExp (e : Exp)]
  [ifExp (testExp : Exp) (thenExp : Exp) (elseExp : Exp)]
  [idExp (id : symbol)]
  [appFuncExp (fname : symbol) (arg : Exp)])

(define (parse [s : s-expression]) : Exp
  (cond
    [(s-exp-number? s) (numExp (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusExp (parse (second sl)) (parse (third sl)))]
         [(*) (mulExp (parse (second sl)) (parse (third sl)))]
         [(if) (ifExp (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
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
            [(if) (ifExp (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
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
    [mulExp (l r) (* (interp2 l) (interp2 r))]
    [ifExp (testExp thenExp elseExp)
           (case (interp2 testExp)
             [(0) (interp2 elseExp)]
             [(1) (interp2 thenExp)])]
    [else (error 'interp "error")]))


(define (desugar (exp : Exp)) : Exp
  (type-case Exp exp
    [numExp (n) (numExp n)]
    [plusExp (l r) (plusExp (desugar l) (desugar r))]
    [mulExp (l r) (mulExp (desugar l) (desugar r))]
    [subExp (l r) (plusExp (desugar l) (mulExp (numExp -1) (desugar r)))]
    [negExp (e) (desugar (subExp (numExp 0) (desugar e)))]
    [else (error 'desugar "error")]))


(define-type Func
  [func (fname : symbol) (arg : Exp) (body : Exp)])

(func 'double (idExp 'x) (plusExp (idExp 'x) (idExp 'x)))
(func 'quadruple (idExp 'x) (appFuncExp 'double (appFuncExp 'double (idExp 'x))))
(func 'const5 (idExp '_) (numExp 5))
