#lang plai-typed

(define-type Exp
  [numExp (n : number)]
  [plusExp (l : Exp) (r : Exp)]
  [mulExp (l : Exp) (r : Exp)]
  [minExp (l : Exp) (r : Exp)]
  [negExp (exp : Exp)]
  [idExp (s : symbol)]
  [appFunc (name : symbol) (arg : Exp)])

(define (parse [s : s-expression]) : Exp
  (cond
    [(s-exp-number? s) (numExp (s-exp->number s))]
    [(s-exp-symbol? s) (idExp (s-exp->symbol s))]
    [(s-exp-list? s)
      (let ([sl (s-exp->list s)])
        (case (s-exp->symbol (list-ref sl 0))
          [(+) (plusExp (parse (list-ref sl 1)) (parse (list-ref sl 2)))]
          [(*) (mulExp (parse (list-ref sl 1)) (parse (list-ref sl 2)))]
          [(-) (case (length sl)
                 [(2) (negExp (parse (list-ref sl 1)))]
                 [(3) (minExp (parse (list-ref sl 1)) (parse (list-ref sl 2)))]
                 [else (error 'parse "error")])]
          [else (appFunc (s-exp->symbol (list-ref sl 0)) (parse (list-ref sl 1)))]))]
    [else (error 'parse "error")]))

(define (interp [e : Exp]) : number
  (type-case Exp e
    [numExp (n) n]
    [plusExp (l r) (+ (interp l) (interp r))]
    [mulExp (l r) (* (interp l) (interp r))]
    [minExp (l r) (interp (desugar e))]
    [negExp (exp) (interp (desugar e))]
    [idExp (s) (lookup environment s)]
    [appFunc (name arg)
             (let ([func (getFuncDef name funcList)])
               (interp (subst (funcDef-body func) (funcDef-arg func) arg)))]))

(define (desugar [e : Exp]) : Exp
  (type-case Exp e
    [numExp (n) e]
    [plusExp (l r) (plusExp (desugar l) (desugar r))]
    [mulExp (l r) (mulExp (desugar l) (desugar r))]
    [minExp (l r) (plusExp (desugar l) (mulExp (numExp -1) (desugar r)))]
    [negExp (exp) (desugar (minExp (numExp 0) exp))]
    [idExp (s) e]
    [appFunc (name arg) (appFunc name (desugar arg))]))

(define-type FuncDef
  [funcDef (name : symbol) (arg : symbol) (body : Exp)])

(define (subst [in : Exp] [for : symbol] [what : Exp]) : Exp
  (type-case Exp in
    [numExp (n) in]
    [plusExp (l r) (plusExp (subst l for what) (subst r for what))]
    [mulExp (l r) (mulExp (subst l for what) (subst r for what))]
    [minExp (l r) (minExp (subst l for what) (subst r for what))]
    [negExp (exp) (negExp (subst exp for what))]
    [idExp (s)
           (cond
             [(equal? s for) what]
             [else in])]
    [appFunc (name arg) (appFunc name (subst arg for what))]))

(define (getFuncDef [name : symbol] [funcs : (listof FuncDef)]) : FuncDef
  (cond
    [(empty? funcs) (error 'getFuncDef "function not found")]
    [else
     (cond 
        [(equal? (funcDef-name (first funcs)) name) (first funcs)]
        [else (getFuncDef name (rest funcs))])]))

(define funcList
  (list
   (funcDef 'double 'x (plusExp (idExp 'x) (idExp 'x)))
   (funcDef 'quadruple 'x (appFunc 'double (appFunc 'double (idExp 'x))))))


(define-type Binding
  [bind (name : symbol) (val : number)])
(define-type-alias Env (listof Binding))

(define environment
  (list
   (bind 'x 1)
   (bind 'y 2)
   (bind 'z 3)))


(define (lookup [env : Env] [name : symbol]) : number
  (cond
    [(empty? env) (error 'lookup "not found")]
    [(equal? name (bind-name (first env))) (bind-val (first env))]
    [else (lookup (rest env) name)]))