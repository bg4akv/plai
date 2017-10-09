#lang plai-typed

(define-type Exp
  [numExp (n : number)]
  [negExp (e : Exp)]
  [idExp (id : symbol)]
  [plusExp (l : Exp) (r : Exp)]
  [mulExp (l : Exp) (r : Exp)]
  [minExp (l : Exp) (r : Exp)]
  [funcAppExp (name : symbol) (arg : Exp)])

(define-type FuncDef
  [funcDef (name : symbol) (arg : symbol) (body : Exp)])

(funcDef 'double 'x (plusExp (idExp 'x) (idExp 'x)))
(funcDef 'quad 'x (funcAppExp 'double (funcAppExp 'double (idExp 'x))))
(funcDef 'const5 '_ (numExp 5))

(define (get-func [name : symbol] [funcDefList : (listof FuncDef)]) : FuncDef
  (cond
    [(empty? funcDefList) (error 'get-func "error")]
    [(equal? name (funcDef-name (first funcDefList))) (first funcDefList)]
    [else (get-func name (rest funcDefList))]))

(define (subst [what : Exp] [for : symbol] [in : Exp]) : Exp
  (type-case Exp in
    [numExp (n) in]
    [idExp (id)
           (cond
             [(equal? for id) what]
             [else in])]
    [plusExp (l r) (plusExp (subst what for l) (subst what for r))]
    [mulExp (l r) (mulExp (subst what for l) (subst what for r))]
    [funcAppExp (name arg) (funcAppExp name (subst what for arg))]
    [else (error 'subst "error")]))


(define (parse [s : s-expression]) : Exp
  (cond
    [(s-exp-number? s) (numExp (s-exp->number s))]
    [(s-exp-symbol? s) (idExp (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusExp (parse (second sl)) (parse (third sl)))]
         [(*) (mulExp (parse (second sl)) (parse (third sl)))]
         [(-)
          (case (length sl)
            [(2) (negExp (parse (second sl)))]
            [(3) (minExp (parse (second sl)) (parse (third sl)))]
            [else (error 'parse "error")])]
         [else (funcAppExp (s-exp->symbol (first sl)) (parse (second sl)))]))]
    [else (error 'parse "error")]))

(define (interp [e : Exp] [funcDefList : (listof FuncDef)]) : number
  (type-case Exp e
    [numExp (n) n]
    [plusExp (l r) (+ (interp l funcDefList) (interp r funcDefList))]
    [mulExp (l r) (* (interp l funcDefList) (interp r funcDefList))]
    [funcAppExp (name arg)
                (interp (local ([define func (get-func name funcDefList)])
                  (subst arg (funcDef-name func) (funcDef-body func))) funcDefList)]
    [else (error 'interp "error")]))


(define (desugar [e : Exp]) : Exp
  (type-case Exp e
    [numExp (n) e]
    [plusExp (l r) (plusExp (desugar l) (desugar r))]
    [mulExp (l r) (mulExp (desugar l) (desugar r))]
    [minExp (l r) (plusExp (desugar l) (mulExp (numExp -1) (desugar r)))]
    [negExp (e) (desugar (minExp (numExp 0) e))]
    [else (error 'desugar "error")]))


