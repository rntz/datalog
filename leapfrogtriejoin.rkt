#lang racket

;; the Leapfrog Triejoin paper: https://arxiv.org/abs/1210.0481
;; it's surprisingly readable! a helpful blog post is
;; http://scattered-thoughts.net/blog/2016/10/11/a-practical-relational-query-compiler-in-500-lines/

;; I can probably do this in typed/racket. It might even improve performance.

(require "util.rkt" racket/generator racket/syntax)

;; Convenient abbreviations for vector operations.
(define @ vector-ref)
(define := vector-set!)
(define len vector-length)
(define (empty? v) (= 0 (len v)))
(define (head v) (@ v 0))

;; Our tests use rackunit & live in a test submodule.
(module+ test (require rackunit))


;; ---------- Galloping iteration ----------
;; returns #f if it runs off the end of the vector.
;; TODO: there has to be a simpler/shorter definition of this.
(define (gallop-to vec ptr target)
  (assert! (< ptr (len vec)))
  (define N (len vec))
  (cond
    [(>= (@ vec ptr) target) ptr]
    [#t
     ;; Jump forward in power-of-two leaps until we hit a larger element.
     (define step
       (let loop ([step 1])
         (define next (+ ptr step))
         (cond
           [(or (<= N next) (<= target (@ vec next))) step]
           [#t (set! ptr next) (loop (* 2 step))])))

     ;; Binary search between i and i+step. It's possible N < i+step, so take
     ;; care not to go out of bounds.
     (let loop ([step step])
       (define next (+ ptr step))
       (when (and (< next N) (< (@ vec next) target))
         (set! ptr next))
       (if (> step 0) (loop (quotient step 2))
           (and (< (+ 1 ptr) N) (+ 1 ptr))))]))

(define (gallop-past vec ptr target) (gallop-to vec ptr (+ 1 target)))

;; A reference implementation
(define (gallop-to-ref vec ptr target)
  (cond
    [(>= ptr (len vec)) #f]
    [(<= target (@ vec ptr)) ptr]
    [#t (gallop-to vec (+ ptr 1) target)]))


;; ---------- Leapfrog intersection (unary join) ----------

;; Takes a list of vectors. Returns a sequence of results from their
;; intersection.
;;
;; TODO: needs to take vectors and *ranges* within those vectors!
(define (intersect columns)
  (define N (length columns))
  (cond
    [(ormap empty? columns) '()]
    [(= 1 N) (first columns)]
    [#t
     (in-generator
      ;; sort columns by their head elements.
      ;; TODO: actually, I think we don't *need* this preprocessing step!
      ;; because, after the first pass over each iterator, they will be sorted anyway!
      ;; I should poke Jamie and email the logicblox folks!
      ;;
      ;; Jamie points out that, if you keep them in sorted order, you can test
      ;; whether you've reached the same element in every column by, after
      ;; leapfrogging, comparing the value you reached to the value at the head
      ;; of the next column. This might possibly have a performance benefit?
      #;(define S (list->vector (sort columns <= #:key head)))
      (define S (list->vector columns))
      ;; (@ P i) is our position/pointer/iterator into (@ S i).
      (define P (make-vector N 0))
      ;; (@@ i) --> the pointed-at value in column i
      (define-syntax-rule (@@ i) (@ (@ S i) (@ P i)))

      ;; `i` is the index of the column we're searching.
      ;; invariant: `target` is the largest value any ptr points at.
      (let/ec done
        (let loop ([i 0])
          (define target (@@ i))

          (do ([target-count 1 (+ 1 target-count)])
              ((= target-count N))
            (set! i (modulo (+ i 1) N)) ;; advance to next sequence
            ;; search for target
            (:= P i (or (gallop-to (@ S i) (@ P i) target) (done)))
            (let ((found (@@ i)))
              (unless (= found target)
                ;; found a bigger, better target! search for it, instead.
                (set! target found)
                (set! target-count 0))))

          ;; we found one!
          (yield target)
          (:= P i (or (gallop-past (@ S i) (@ P i) target) (done)))
          (loop i))))]))


;; Columnar layout simplifies simple galloping iterators, but I'm not sure it
;; simplifies trie-iterators. It might be more performant, though.
;;
;; TODO: explain `ixs`.
;; TODO: probably want `depth` cached for efficiency, or to use a vector.
(struct trie-iter (columns [posns #:mutable]) #:transparent)

(define (trie-iter-depth iter) (len (trie-iter-posns iter)))
(define (trie-iter-posn iter)  (car (trie-iter-posns iter)))

(define (trie-iter-at-end? iter)
  TODO)

(define (trie-iter-get iter)
  (assert! (not (trie-iter-at-end? iter)))
  (@ (@ (trie-iter-columns iter) (trie-iter-depth iter))
     (trie-iter-posn iter)))

(define (trie-iter-up! iter)
  (assert! (not (null? (trie-iter-posns iter))))
  (set-trie-iter-posns! (cdr (trie-iter-posns iter))))

;; we use (open) as an opportunity to find the end of the run of equal values in
;; the current column we're looking at, to establish the bounds for our
;; search of the remaining columns...
(define (trie-iter-open! iter)
  ;; what if trie-iter-posns is empty?
  (define posn (car (trie-iter-posns iter)))
  TODO)

(define (trie-iter-seek! iter target) TODO)


;; Tests for intersection
(module+ test
  (define (test-intersect . xs)
    (define result (sequence->list (intersect (map list->vector xs))))
    ;; test against a reference implementation.
    (check-equal? (list->set result)
                  (apply set-intersect (map list->set xs)))
    result)

  (define evens (for/list ([i (range 51)]) (* 2 i)))
  (define odds  (for/list ([i (range 50)]) (+ 1 (* 2 i))))
  (define primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))
  (define squares (for/list ([i (range 11)]) (* i i)))

  (check-equal? (test-intersect '(0 2 4) '(1 2 3)) '(2))
  (check-equal? (test-intersect evens odds)    '())
  (check-equal? (test-intersect evens primes)  '(2))
  (check-equal? (test-intersect odds primes)   (cdr primes))
  (check-equal? (test-intersect squares evens) '(0 4 16 36 64 100))
  (check-equal? (test-intersect squares odds)  '(1 9 25 49 81)))


;; ---------- Tables and indexing ----------
(define scalar? exact-integer?)
(define row?    (vectorof scalar?))
(define column? (vectorof scalar?))
(define rows?   (vectorof row?))
;; An index is a list of columns. It's sorted lexically by column order.
;; TODO: explain further.
(define index?  (vectorof column?))
;; an "indexing" is a list of column numbers.
(define indexing? (listof exact-nonnegative-integer?))

;; Tables, which can have indexes added to them.
;; indices is a hash from indexings to indexes.
(struct table (rows indices) #:transparent #:constructor-name -make-table)
(define/contract (rows->table rows)
  (-> rows? table?)
  (-make-table rows (make-hash)))

;; Gets an index on a particular table, constructing it if necessary.
(define/contract (table-index! table column-indices)
  (-> table? indexing? index?)
  (hash-ref! (table-indices table)
             column-indices
             (lambda () (make-index (table-rows table) column-indices))))

(define/contract (make-index rows column-indices)
  (-> rows? indexing? index?)
  ;; We take advantage of the stability of vector-sort!. We also update `rows`
  ;; in place, which is harmless as long as we're not multithreaded; what
  ;; matters in `rows` is the set of rows, not their order.
  (for ([i (reverse column-indices)])
    (vector-sort! rows <= #:key (lambda (row) (@ row i))))
  ;; now, construct the columns.
  (for/vector ([i column-indices])
    (vector-map (lambda (row) (@ row i)) rows)))


;; ---------- Global state ----------
(define database/c (hash/c symbol? table?))
(define/contract database (parameter/c database/c) (make-parameter (make-hash)))


;; ---------- Leapfrog triejoin for conjunctive queries ----------
(define arg? (or/c symbol? scalar?))
(define atom? (cons/c symbol? (listof arg?)))
(define query? (listof atom?))

(define/contract (pick-variable-ordering query)
  (-> query? (listof symbol?))
  (define seen (mutable-set))
  (define (unseen?! x)
    (if (set-member? seen x) #f
         (begin0 #t (set-add! seen x))))
  (for*/list ([atom query]
              [arg (cdr atom)]
              #:when (and (symbol? arg) (unseen?! arg)))
    arg))

;; (define/contract (compile-query query)
;;   (-> query? syntax?)

;;   ;; first, pick an order in which to find variables' possible values.
;;   (define variables (pick-variable-ordering query))

;;   (define table-indices
;;     (for/list ([atom query])
;;       (match-define `(,pred ,@args) atom)
;;       ;; figure out what order to index in.
;;       ;; how do we handle scalars?f
;;       #`(table-index! (hash-ref (database) '#,p) #,TODO)))

;;   ;; generate the appropriate indices

;;   (define/with-syntax ((x x-seq) ...)
;;     (for/list ([v vars])
;;       (with-syntax ([x (gensym v)])
;;         (list #'name
;;               #'(intersect TODO)))))

;;   (with-syntax ([((x x-seq) ...) clauses])
;;    #'(for*/vector ([x x-seq] ...)
;;        (vector x ...))))

;; (define (test-query q)
;;   (pretty-print (syntax->datum (compile-query q))))
