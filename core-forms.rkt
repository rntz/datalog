#lang racket

;; The core forms of Datalog.
;;
;; These are defined as syntax, but do nothing (apart from "begin", which we
;; re-export from Racket); they get interpreted by the (datalog ...) macro in
;; datalog.rkt.

(provide (all-defined-out) begin)

(define-syntax : #f)
(define-syntax :- #f)
(define-syntax ~ #f)

