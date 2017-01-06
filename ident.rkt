#lang racket

(provide (struct-out free) (struct-out bound))

;; lexi-lambda suggests that bound-identifier=? implies free-identifier=? but
;; not vice-versa.

(struct free (identifier)
  #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc x y equal?)
     (free-identifier=? (free-identifier x) (free-identifier y)))
   ;; XXX this is a fucking hack
   (define (hash-proc x hash) 0)
   (define (hash2-proc x hash) 0)])

(struct bound (identifier)
  #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc x y equal?)
     (bound-identifier=? (bound-identifier x) (bound-identifier y)))
   ;; XXX this is a fucking hack
   (define (hash-proc x hash) 0)
   (define (hash2-proc x hash) 0)])


(module+ example
  ;; An example of when free-identifier=? and bound-identifier=? disagree
  (define x "x here")
  (define-syntax y (make-rename-transformer #'x))
  (displayln (free-identifier=? #'x #'y))  ;; --> #t
  (displayln (bound-identifier=? #'x #'y)) ;; --> #f
  (let ((y "y here"))
    (displayln x)
    (displayln y)))

