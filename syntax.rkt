#lang racket

(require (for-template racket/base "core-forms.rkt")
         syntax/parse "ident.rkt")

(provide core-top-forms core-top-form?)
(define core-top-forms (list #': #':- #'begin))
(define (core-top-form? x) (member x core-top-forms free-identifier=?))

;; ---------- Grammar ----------
;; TOP ::= DECLARE | CLAUSE | (begin TOP ...)
;; DECLARE ::= (: NAME NAME ...)
;; CLAUSE ::= (:- HEAD LIT ...)
;;
;; HEAD :: = (NAME ARG ...)
;; LIT ::= +LIT | (~ +LIT)
;; +LIT ::= (NAME ARG ...)
;;
;; ARG ::= VAR | ATOM
;; ATOM ::= NAME
;;
;; VAR = upper-case identifier
;; NAME = lower-case identifier


;; TODO: declaring & using external predicates
(provide TOP DECLARE CLAUSE LIT +LIT ARGUMENT VARIABLE ATOM PRED NAME)

;; Helper functions
(define (name? id) (first-char char-lower-case? id))
(define (variable? id) (first-char char-upper-case? id))
(define (first-char pred id)
  (define s (symbol->string (syntax->datum id)))
  (and (< 0 (string-length s)) (pred (string-ref s 0))))

(define-syntax-class TOP
  #:literals (begin)
  (pattern clause:CLAUSE
           #:attr (clauses 1) (syntax->list #'(clause)))
  (pattern (begin decl:TOP ...)
           #:attr (clauses 1) (syntax->list #'(decl.clauses ... ...)))
  ;; TODO
  ;; (pattern declare:DECLARE)
  )

(define-syntax-class DECLARE
  #:literals (:)
  (pattern (: pred:NAME arg:NAME ...)))

;; TODO: using arbitrary racket code as a filter. but what about free
;; variables? bind them explicitly, maybe?
(define-syntax-class CLAUSE
  #:literals (:- ~)
  (pattern (:- (pred:NAME arg:ARGUMENT ...) body:LIT ...)
           #:attr (depends 1) (syntax->list #'(body.pred ...))
           #:attr (negative-depends 1)
           (syntax-parse #'(body ...)
             #:literals (~)
             [((~or (~ (pred _ ...)) _) ...)
              (syntax->list #'(pred ...))])
           #:attr (refers 1) (syntax->list #'(pred depends ...))))

(define-syntax-class LIT
  #:literals (~)
  (pattern (pred:NAME arg:ARGUMENT ...))
  (pattern (~ (pred:NAME arg:ARGUMENT ...))))

;; positive terms
(define-syntax-class +LIT (pattern (pred:NAME arg:ARGUMENT ...)))

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
(provide syntax->ident-set
         (struct-out predicate) empty-predicate clause-info merge-clause-info)

(define (syntax->ident-set x) (list->set (map bound (syntax->list x))))

(struct predicate (clauses depends negative-depends) #:transparent)

(define empty-predicate (predicate '() (set) (set)))

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
