#lang plai

(define-type FunDef
  [fundef (fun-name symbol?) (arg-name symbol?) (body F1WAE?)])

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (ftn symbol?) (arg F1WAE?)])

; [contract] parse-fd: sexp -> FunDef
; [purpose]
(define (parse-fd sexp)
  (match sexp
    [(list 'deffun (list f x) b) (fundef f x (parse b))]))

; [contract] parse: sexp -> F1WAE
; [purpose]
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [(list f a) (app f (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]))
    
(parse '{with {x {+ 5 5}} {+ x x}})

; [contract] interp: F1WAE list-of-FunDef DefrdSub-> number
; [purpose]
(define (interp f1wae fundefs)
  (type-case F1WAE f1wae
    [num (n) n]
    [add (l r) (+ (interp l fundefs) (interp r fundefs))]
    [sub (l r) (- (interp l fundefs) (interp r fundefs))]
    [with (x i b) (interp (subst b x (interp i fundefs)) fundefs)]
    [id (s) (error 'interp "free identifier")]
    [app (f a)
         (local
           [(define a_fundef (lookup-fundef f fundefs))]
           (interp (subst (fundef-body a_fundef)
                          (fundef-arg-name a_fundef)
                          (interp a fundefs))
                   fundefs))]))
(fundef 'f 'x (add (id 'x) (num 3)))

; [contract] subst: WAE symbol number -> WAE
; [purpose]
(define (subst f1wae idtf val)
  (type-case F1WAE f1wae
    [num (n) f1wae]
    [add (l r) (add (subst l idtf val) (subst r idtf val))]
    [sub (l r) (sub (subst l idtf val) (subst r idtf val))]
    [with (i v e) (with i (subst v idtf val) (if(symbol=? i idtf) e
                                                (subst e idtf val)))]
    [id (s) (if (symbol=? s idtf) (num val) f1wae)]
    [app (f a) (app f (subst a idtf val))]))


;lookup-fundef: symbol list-of-FunDef -> FunDef
(define (lookup-fundef name fundefs)
  (cond
    [(empty? fundefs)
     (error 'lookup-fundef "unknown function")]
    [else
     (if (symbol=? name (fundef-fun-name (first fundefs)))
         (first fundefs)
         (lookup-fundef name (rest fundefs)))]))

(subst (app 'fn (id 'x)) 'x 1)
(subst (app 'fn (with 'y (num 10) (add  (id 'y) (id 'x)))) 'x 1)
(test (interp (app 'f (num 10))
              (list (fundef 'f 'x (sub (num 20)
                                       (app 'twice (id 'x))))
                    (fundef 'twice 'y (add (id 'y) (id 'y)))))
      0)
(test (interp (app 'f (num -1))
              (list (fundef 'f 'y (add (id 'y) (num 5)))))
      4)
