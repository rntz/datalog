#lang racket

(require racket syntax/parse/define (for-syntax racket/syntax))
;; re-export syntax/parse/define, it's fantastic.
;; TODO: remove this.
(provide (all-from-out syntax/parse/define))

;; (require (for-syntax syntax/parse))
;; (provide (for-syntax (all-from-out syntax/parse)))


;;; Syntax manipulation utilities

;; (provide (for-syntax format-id))
;; (define-for-syntax (format-id fmt id)
;;   (datum->syntax id (string->symbol (format fmt (syntax->datum id)))))

;; (provide define-syntax-parser)
;; (define-syntax define-syntax-parser
;;   (syntax-parser
;;     [(_ name:id (pattern body ...) ...)
;;       #'(define-syntax name
;;           (syntax-parser
;;             (pattern body ...)
;;             ...))]
;;     [(_ (name:id pattern ...) body ...)
;;       #'(define-syntax-parser name
;;           [(_ pattern ...) body ...])]))


;;; Defining functions
(provide fn)
(begin-for-syntax
  (define-syntax-class fn-clause
    (pattern ((param ...) body ...)
      #:attr pattern #'(list param ...))
    (pattern ((param ... . rest-param:id) body ...)
      #:attr pattern #'(list-rest param ... rest-param))))

(define-syntax-parser fn
  [(_ (name params ...) body ...)
    #'(define (name params ...) body ...)]
  [(_ name:id c:fn-clause ...)
    #'(define/match (name . _)
        [(c.pattern) c.body ...]
        ...)])


;;; Pattern matching
(provide match? match/c)
(begin-for-syntax
  (define-splicing-syntax-class match-branch
    (pattern (~seq pattern #:when condition))
    (pattern pattern #:attr condition #'#t)))

(define-simple-macro (match? e p:match-branch ...)
  (match e [p.pattern #:when p.condition #t] ... [_ #f]))

(define-syntax-rule (match/c pattern ...)
  (lambda (x) (match? x pattern ...)))


;;; Exceptions
(provide exception)

;; FIXME
;; ^ what was this FIXME about?
;; TODO: don't automatically add "exn:" prefix, maybe?
(define-syntax-parser exception
  [(_ name-or-fields) #'(exception name-or-fields exn:fail)]
  [(_ name:id parent:id)
   (with-syntax ([exn:name       (format-id #'name "exn:~a"      #'name)]
                 [make-exn:name  (format-id #'name "make-exn:~a" #'name)]
                 [raise-name     (format-id #'name "raise-~a"    #'name)])
    #'(begin
        (struct exn:name parent ()
          #:transparent
          #:extra-constructor-name make-exn:name)
        (define (raise-name message)
          (raise (make-exn:name message (current-continuation-marks))))))])


;;; Enumeration types
(provide enum enum-case enum/c)

(define-syntax-rule (enum name branch ...)
  (begin
    ;; we hide the constructor by giving it a name that we weren't passed
    (struct name () #:transparent #:constructor-name isnt-hygiene-great)
    (enum-case name branch) ...))

(begin-for-syntax
  (define-syntax-class enum-case-field
    (pattern name:id #:attr contract #'any/c)
    (pattern (name:id contract:expr)))
  ;; Adjusts the lexical context of the outermost piece of a syntax object; i.e.
  ;; changes the context of a syntax pair but not its contents. Got this from
  ;; lexi-lambda in #racket on freenode.
  (define (adjust-outer-context ctx stx)
    (datum->syntax ctx (syntax-e stx))))

(define-syntax-parser enum-case
  ;; If there are no contracts, we don't use define-struct/contract. This means
  ;; in particular we are usable within typed/racket, although I don't know if
  ;; that's a good idea.
  [(enum-case enum-name:id (case-name:id field:id ...))
   #'(struct case-name enum-name (field ...) #:transparent)]
  [(enum-case enum-name:id (case-name:id field:enum-case-field ...))
   ;; works around define-struct/contract being weird and pulling lexical info
   ;; about the parent struct from the type/super-type name pair. Adapted from
   ;; code by lexi-lambda in #racket on freenode.
   (with-syntax ([type/super (adjust-outer-context
                              #'enum-name #'(case-name enum-name))])
     #'(define-struct/contract type/super ((field.name field.contract) ...)
         #:transparent))])

(define-syntax-rule (enum/c (name arg/c ...) ...)
  (or/c (struct/c name arg/c ...) ...))


;;; Contract utilities
(provide define-flat-contract)
(define-syntax-rule (define-flat-contract name branches ...)
  (define name (opt/c (flat-rec-contract name (or/c branches ...)))))


;;; Miscellaneous utilities
(provide TODO assert! warn! flip print-error eta read-file)

(define-syntax-parser TODO
  [_ #'(error "TODO: unimplemented")])

(define (assert! t)
  (unless t (error "ASSERTION FAILURE")))

(define (warn! msg)
  (displayln (format "WARNING: ~a" msg)) )

(define-syntax-rule (eta e)
  (lambda args (apply e args)))

(define ((flip f) x y)
  (f y x))

(define (print-error err)
  (printf "error: ~a\n" (exn-message err)))

(define (read-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ([line (read)] [acc '()])
        (if (eof-object? line)
            (reverse acc)
            (loop (read) (cons line acc)))))))

;;; List utilities
(provide length=? map? foldl1 foldr1 rev-append let*/list)

;; argh. exists in racket 6.7.0.3, but with v/lst arguments swapped.
;; (define (index-of v lst [eq equal?])
;;   (let loop ([i 0] [l lst])
;;     (match l
;;       ['() #f]
;;       [(cons x xs) (if (eq x v) i (loop (+ 1 i) xs))])))

(define (length=? l . lsts)
  (define len (length l))
  (andmap (lambda (l) (= len (length l))) lsts))

(define (map? eq . lsts)
  (and (apply length=? lsts) (apply andmap eq lsts)))

(define (foldl1 f l)
  (foldl f (car l) (cdr l)))

(define (foldr1 f l)
  (foldl1 f (reverse l)))

(define (rev-append x y)
  (append (reverse x) y))

;; (provide for/append)
;; (define-syntax-rule (for/append clauses body ...)
;;   (append* (for/list clauses body ...)))

(define-syntax-rule (let*/list (clause ...) body ...)
  (for*/list (clause ... [x (let () body ...)]) x))


;;; Stream and generator utilities
(require racket/generator)
(provide (all-from-out racket/generator))
(provide stream-take stream-append-lazy streams-interleave
         for/generator generate/stream generate/list for/generate/list
         let*/stream)

(define (stream-take n s)
  ;; NB. order of clauses is important, otherwise we attempt to take one more
  ;; element from `s` than is necessary, which can lead to infinite loops if `s`
  ;; is unproductive after `n` elements.
  (for/list ([_ (in-range n)]
             [x (in-stream s)])
    x))

(define (stream-append-lazy stream stream-thunk)
  (if (stream-empty? stream) (stream-thunk)
      (stream-cons (stream-first stream)
                   (stream-append-lazy (stream-rest stream) stream-thunk))))

(define (streams-interleave streams)
  (match (filter-not stream-empty? streams)
    ['()    empty-stream]
    [`(,s)  s]
    [ss     (stream-append-lazy
             (stream-map stream-first ss)
             (lambda () (streams-interleave (map stream-rest ss))))]))

(define-syntax-rule (let*/stream [clause ...] body ...)
  (for*/stream [clause ... [x (let () body ...)]]
    x))

;; TODO?: cut these down to just the ones I actually use.
(define-syntax-rule (for/generator clauses body ...)
  (in-generator (for clauses (yield (begin body ...)))))
(define-syntax-rule (generate/stream body ...)
  (sequence->stream (in-generator body ...)))
(define-syntax-rule (generate/list body ...)
  (sequence->list (in-generator body ...)))
(define-syntax-rule (for/generate/list clauses body ...)
  (generate/list (for clauses body ...)))


;;; Set utilities
(provide freeze-set sets-union sets-intersect set-filter sequence->set let*/set)

(define (freeze-set s)        (for/set ([x s]) x))
(define (sets-union sets)     (apply set-union (set) sets))
(define (sets-intersect sets) (apply set-intersect sets))
(define (set-filter p s)      (for/set ([x s] #:when (p x)) x))
(define (sequence->set seq)   (for/set ([x seq]) x))

(define-syntax-rule (let*/set (for-clause ...) body ...)
  (for*/set (for-clause ... [x (let () body ...)]) x))


;;; Hash utilities
;; NB. racket/hash now provides hash-union, maybe I can rewrite or drop some of
;; these?
(provide freeze-hash
         hash-union-with hash-union-right hashes-union-right
         hash-intersection-with
         hash-filter-keys ;; hash-select-keys
         hash-keyset hash-map-vals)

(define (freeze-hash h) (for/hash ([(k v) h]) (values k v)))

(define (hash-filter-keys p h)
  (for/hash ([(k v) h] #:when (p k)) (values k v)))

;; TODO: remove if unused.
;; (define (hash-select-keys h k)
;;   (hash-filter-keys (curry set-member? (for/set ([x k]) x)) h))

(define (hash-keyset h) (list->set (hash-keys h)))

(define (hash-map-vals f h)
  (for/hash ([(k v) h])
    (values k (f v))))

(define (hash-union-with a b f)
  (define keys (set-union (list->set (dict-keys a)) (list->set (dict-keys b))))
  (for/hash ([k keys])
    (values k
      (if (not (dict-has-key? a k))
        (dict-ref b k)
        (if (not (dict-has-key? b k))
          (dict-ref a k)
          (f (dict-ref a k) (dict-ref b k)))))))

(define (hash-union-right a b) (hash-union-with a b (lambda (x y) y)))
(define (hashes-union-right hs) (foldl hash-union-right (hash) hs))

(define (hash-intersection-with a b f)
  (for/hash ([k (in-dict-keys a)]
              #:when (dict-has-key? b k))
    (values k (f (dict-ref a k) (dict-ref b k)))))


;; google mapreduce, but without the parallelism.
;; a swiss army knife of turning lists into hashes.
(provide group)
(define (group seq
               #:by     key
               #:map    [mapf list]
               #:reduce [reducef append]
               #:init   [init #f])
  (for/fold ([h (or init (hash))])
            ([elem seq])
    (define k (key elem))
    (define v (mapf elem))
    (if (dict-has-key? h k)
        (dict-update h k (curry reducef v))
        (dict-set h k v))))
