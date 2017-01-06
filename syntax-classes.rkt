#lang racket

(require syntax/parse)
(provide (all-defined-out))

;; Our grammar

;; TOP ::= DECLARE | CLAUSE
;; DECLARE ::= (pred:NAME : arg:NAME ...)
;; CLAUSE ::= (head:POSITIVE-TERM :- body:TERM ...)
;;
;; TERM ::= POSITIVE-TERM | (not POSITIVE-TERM)
;; POSITIVE-TERM ::= (pred:NAME arg:ARG ...)
;;
;; ARG ::= var:VAR | atom:NAME
;;
;; VAR = upper-case identifier
;; NAME = lower-case identifier

;; TODO: declaring & using external predicates
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
(define-syntax-class ARGUMENT (pattern variable) (pattern name))
(define-syntax-class VARIABLE (pattern name:id #:when (variable? #'name)))
(define-syntax-class NAME (pattern name:id #:when (name? #'name)))

;; Helper functions
(define (first-char pred id)
  (define s (symbol->string (syntax->datum id)))
  (and (< 0 (string-length s)) (pred (string-ref s 0))))
(define (name? id) (first-char char-lower-case? id))
(define (variable? id) (first-char char-upper-case? id))
