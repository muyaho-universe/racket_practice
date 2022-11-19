#lang plai

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (saved DefrdSub?)])

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax: ~a" sexp)]))

;example instance
(aSub 'x 1 (aSub 'y 4 (aSub 'x 2(mtSub))))

;interp: WAE DefrdSub -> number
(define (interp wae ds)
  (type-case WAE wae
    [num (n) n]
    [add (l r) (+(interp l ds) (interp r ds))]
    [sub (l r) (-(interp l ds) (interp r ds))]
    [with (i v e) (interp e (aSub i (interp v ds) ds))]
    [id (s) (lookup s ds)]))

; [contract] lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if(symbol=? i name)
                         v
                         (lookup name saved))]))

; [contract] subst: WAE symbol number -> WAE
; [purpose]
(define (subst wae idtf val)
  (type-case WAE wae
    [num (n) wae]
    [add (l r) (add (subst l idtf val) (subst r idtf val))]
    [sub (l r) (sub (subst l idtf val) (subst r idtf val))]
    [with (i v e) (with i (subst v idtf val) (if(symbol=? i idtf) e
                                                (subst e idtf val)))]
    [id (s) (if (symbol=? s idtf) (num val) wae)]))



(test (lookup 'x (aSub 'x 1 (mtSub))) 1)
(test (lookup 'x (aSub 'y 1 (aSub 'x 4 (mtSub)))) 4)