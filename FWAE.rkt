#lang plai

(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [sub (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FWAE?)]
  [app (ftn FWAE?) (arg FWAE?)])

; [contract] parse: sexp -> FWAE
; [purpose]
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]))

(parse '{{fun {x} {+ x 1}} 2})

(test (parse '{{fun {x} {+ x 1}} 10})
                    (app (fun 'x (add (id 'x) (num 1))) (num 10)))

; [contract] interp: FWAE -> FWAE
(define (interp fwae)
  (type-case FWAE fwae
    [num (n) fwae]
    [add (l r) (num+ (interp l) (interp r))]
    [sub (l r) (num- (interp l) (interp r))]
    [with (i v e) (interp (subst e i (interp v)))]
    [id (s) (error 'interp "free identifier")]
    [fun (p b) fwae]
    [app (f a)
         (local
           [(define ftn (interp f))]
           (interp (subst (fun-body ftn)
                          (fun-param ftn)
                          (interp a))))]))

; [contract] num-op: (number number -> number) -> (FWAE FWAE -> FWAE)
(define (num-op op)
  (lambda (x y)
    (num (op (num-n x) (num-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

; [contract] subst: FWAE symbol FWAE -> FWAE
(define (subst exp idtf val)
  (type-case FWAE exp
    [num (n) exp]
    [add (l r) (add (subst l idtf val) (subst r idtf val))]
    [sub (l r) (sub (subst l idtf val) (subst r idtf val))]
    [with (i v e) (with i (subst v idtf val) (if(symbol=? i idtf) e
                                                (subst e idtf val)))]
    [id (name) (cond [(equal? name idtf) val] [else exp])]
    [app (f arg) (app (subst f idtf val)
                      (subst arg idtf val))]
    [fun (id body) (if (equal? idtf id)
                       exp
                       (fun id (subst body idtf val)))]))

(parse '{fun {x} {+ x 10}})
(interp(parse '{{fun {x} {+ x 10}} 33}))
(interp(parse '{+ 2 10}))
(parse '{+ {+ 1 2} 10})
(interp (parse '{+ {+ 1 2} 10}))

(parse '{fun {x} {+ x 1}})
(parse '{{fun {x} {+ x 1}} 2})
(interp (parse '{{fun {x} {+ x 1}} 2}))