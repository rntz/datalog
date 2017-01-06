#lang racket

(require (for-template racket/base) syntax/parse "ident.rkt")

;; ---------- Grammar ----------
;; TOP ::= DECLARE | CLAUSE
;; DECLARE ::= (NAME : NAME ...)
;; CLAUSE ::= (HEAD :- TERM ...)
;;
;; HEAD :: = (NAME ARG ...)
;; TERM ::= +TERM | (not +TERM)
;; +TERM ::= (NAME ARG ...)
;;
;; ARG ::= VAR | ATOM
;; ATOM ::= NAME
;;
;; VAR = upper-case identifier
;; NAME = lower-case identifier


;; TODO: declaring & using external predicates
(provide DECLARE CLAUSE TERM +TERM ARGUMENT VARIABLE ATOM PRED NAME)

;; Helper functions
(define (name? id) (first-char char-lower-case? id))
(define (variable? id) (first-char char-upper-case? id))
(define (first-char pred id)
  (define s (symbol->string (syntax->datum id)))
  (and (< 0 (string-length s)) (pred (string-ref s 0))))

(define-syntax-class DECLARE
  #:datum-literals (:)
  (pattern (pred:NAME : arg:NAME ...)))

;; TODO: using arbitrary racket code as a filter. but what about free
;; variables? bind them explicitly, maybe?
(define-syntax-class CLAUSE
  #:datum-literals (:-)
  (pattern ((pred:NAME arg:ARGUMENT ...) :- body:TERM ...)
           #:attr (depends 1) (syntax->list #'(body.pred ...))
           #:attr (negative-depends 1)
           (syntax-parse #'(body ...)
             [((~or ((~datum not) (pred _ ...)) _) ...)
              (syntax->list #'(pred ...))])
           #:attr (refers 1) (syntax->list #'(pred depends ...))))

(define-syntax-class TERM
  #:datum-literals (not)
  (pattern (pred:NAME arg:ARGUMENT ...))
  (pattern (not (pred:NAME arg:ARGUMENT ...))))

;; positive terms
(define-syntax-class +TERM (pattern (pred:NAME arg:ARGUMENT ...)))

(define-syntax-class ARGUMENT
  (pattern v:VARIABLE #:attr as-expr #'v)
  (pattern a:ATOM     #:attr as-expr #'a.as-expr))
(define-syntax-class VARIABLE
  (pattern name:id #:when (variable? #'name)))
(define-syntax-class ATOM
  ;; TODO: numbers!
  (pattern name:NAME #:attr as-expr #''name #:attr as-pattern #''name))

(define-syntax-class PRED (pattern name:NAME))
(define-syntax-class NAME (pattern name:id #:when (name? #'name)))


;; Information about predicates derived from their clauses.
(provide syntax->ident-set (struct-out predicate) clause-info merge-clause-info)

(define (syntax->ident-set x) (list->set (map bound (syntax->list x))))

(struct predicate (clauses depends negative-depends) #:transparent)

(define clause-info
  (syntax-parser
    [c:CLAUSE
     (predicate
      (list #'c)
      (syntax->ident-set #'(c.depends ...))
      (syntax->ident-set #'(c.negative-depends ...)))]))

(define (merge-clause-info x y)
  (match-define (predicate x-clauses x-deps x-neg-deps) x)
  (match-define (predicate y-clauses y-deps y-neg-deps) y)
  (predicate (append x-clauses y-clauses)
             (set-union x-deps y-deps)
             (set-union x-neg-deps y-neg-deps)))
