#lang racket

(require "dlog.rkt")

;; TODO: our own #%top-interaction?
(provide #%top-interaction)

(provide (rename-out [datalog-module-begin #%module-begin]) )

(define-syntax-rule (datalog-module-begin blah ...)
  (#%module-begin (datalog blah ...)))
