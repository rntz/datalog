#lang racket

(require "core-forms.rkt" "datalog.rkt")

;; TODO: our own #%top-interaction?
(provide #%top-interaction)

(provide (rename-out [datalog-module-begin #%module-begin])
         (all-from-out "core-forms.rkt"))

(define-syntax-rule (datalog-module-begin blah ...)
  (#%module-begin (datalog blah ...)))

;; arg, datalog macro doesn't do local expansion!
(define-syntax-rule (! decl ...)
  (begin (:- decl) ...))
