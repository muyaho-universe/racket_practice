#lang plai

(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [sub (lhs FAE?) (rhs FAE?)]
 ;[with (name symbol?) (named-expr FAE?) (body FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (ftn FAE?) (arg FAE?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (saved DefrdSub?)])

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body FAE?) (ds DefrdSub?)])

; [contract] interp: FAE DefrdSub -> FAE
(define (interp fae ds)
  (type-case FAE fae
    [num (n) fae]
    [add (l r) (num+ (interp l) (interp r))]
    [sub (l r) (num- (interp l) (interp r))]
    ;[with (i v e) (interp (subst e i (interp v)))]
    [id (s) (lookup s ds)]
    [fun (p b) (closureV p b ds)]
    [app (f a)
         (local
           [(define f-val (interp f ds))
            (define a-val (interp a ds))]
           (interp (closureV-body f-val)
                   (aSub (closureV-param f-val)
                         a-val
                         (closureV-ds f-val))))]))

; [contract] num-op: (number number -> number) -> (FWAE FWAE -> FWAE)
(define (num-op op)
  (lambda (x y)
    (num (op (num-n x) (num-n y)))))
(define num+ (num-op +))
(define num- (num-op -))



; [contract] lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if(symbol=? i name)
                         v
                         (lookup name saved))]))

;(test (interp (parse '{with {y 10} {fun {x} {+ y x}}}) (mtSub))
;        (closureV 'x (add (id 'y) (id 'x))
;                        (aSub 'y (numV 10) (mtSub))))
