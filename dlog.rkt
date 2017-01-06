#lang at-exp racket

(require (for-syntax racket
                     racket/syntax
                     syntax/parse
                     "tarjan-scc.rkt"
                     "util.rkt"
                     "ident.rkt")
         syntax/parse/define
         syntax/parse
         "util.rkt")

(require (for-syntax "syntax-classes.rkt"))

;; Provide the macro that understands datalog.
(provide datalog)


;; Information about predicates derived from their clauses.
(begin-for-syntax
  (struct predicate (clauses depends negative-depends) #:transparent)

  (define clause-info
    (syntax-parser
      [c:CLAUSE
       (predicate
        (list #'c)
        (syntax->ident-set #'(c.depends ...))
        (syntax->ident-set #'(c.negative-depends ...)))]))

  (define (syntax->ident-set x) (list->set (map bound (syntax->list x))))

  (define (merge-clause-info x y)
    (match-define (predicate x-clauses x-deps x-neg-deps) x)
    (match-define (predicate y-clauses y-deps y-neg-deps) y)
    (predicate (append x-clauses y-clauses)
               (set-union x-deps y-deps)
               (set-union x-neg-deps y-neg-deps))))


;; The syntax transformer

;; NB. There is a fundamental tension here between 1. using plain old symbols to
;; represent predicate names, which is easy; and 2. using Racket's syntax-object
;; identifiers to represent them, which is a fucking pain in the ass, but in
;; principle more extensible.
;;
;; For now I prefer (1), but there's a snag: hygiene. If we strip all syntax
;; information from predicate names, then hygiene will prevent the
;; predicate-sets this macro tries to define from being visible to the external
;; world. Gah!
;;
;; So I plan to use a mix of the two approaches, at least until I manage to
;; fully grok Racket's frustratingly complex syntax systems.
;;
;; EDIT: Changed my mind, trying for option (2) now.

;; TODO: error message if (datalog ...) used in non-definition context.
(define-syntax-parser datalog
  [(_ (~or clause:CLAUSE) ...)

   ;; TODO Check arity: Declarations, definitions, and uses of a given predicate
   ;; all have the same arity.

   ;; TODO Check generativity: every variable in the head of a clause occurs in
   ;; a positive position in its body.

   ;; A hash from predicate names to their information.
   (define predicates
     (group (syntax->list #'(clause ...))
       #:by     (syntax-parser [c:CLAUSE (bound #'c.pred)])
       #:map    clause-info
       #:reduce merge-clause-info))

   ;; Some dependency graphs.
   (define depends
     (for/hash ([(name info) predicates])
       (values name (predicate-depends info))))
   (define negative-depends
     (for/hash ([(name info) predicates])
       (values name (predicate-negative-depends info))))

   ;; Find the SCCs in the dependency graph
   (match-define (scc-info components pred->component-index)
     (sccs depends))

   ;; Stratify: Check if any SCC contains a negative edge (negative cycle, no
   ;; stratification).
   (for* ([component components]
          [v component]
          [w (hash-ref negative-depends v)]
          #:when (set-member? component w))
     (error @~a{Cannot stratify: @v uses @w, and @w depends on @|v|!}))

   ;; think about how to do this more modularly, with each clause contributing
   ;; something. maybe each clause adds a function to a list for that predicate,
   ;; and then we put the thing that iteratively runs all the functions at the
   ;; end? but how do we *refer* to the function lists?

   ;; Extract all predicate names referred to. I want bound-identifier=? because
   ;; I'm going to be binding these names myself, not referring to them as
   ;; things already bound.
   (define/with-syntax (pred ...)
     (remove-duplicates (syntax->list #'(clause.refers ... ...))
                        bound-identifier=?))

   (define/with-syntax ((component-clause ...) ...)
     (for/list ([component components])
       (let*/list ([pred component])
         (predicate-clauses (hash-ref predicates pred)))))

   #`(begin
       ;; initialize predicate sets.
       (define pred (mutable-set)) ...

       ;; run the clauses for each SCC in order.
       (run-clauses component-clause ...) ...

       ;; TODO remove this
       (list (sequence->set pred) ...))])

;; Helper macros

;; NB. "foo() :- not bar(X)." is disallowed because its Horn clause
;; interpretation
;;
;;     ∀x (¬bar(x) ⊃ foo)
;;
;; is unimplementable, at least using forward-chaining. All variables must occur
;; at least once positively.
;;
;; BUT, see simrob's response:
;; https://twitter.com/arntzenius/status/815036885051572224
(define-syntax-parser run-clause
  #:datum-literals (:- not)
  [(_ changed:id ((pred:NAME arg:ARGUMENT ...)
                  :-
                  (~or pos-term:+TERM
                       ;; TODO
                       ;; (not neg-term:+term)
                       ) ...))
   #`(begin
       ;; compute the set of things to add to `pred'
       (define tuples (solving (pos-term ...)
                               ;; FIXME: atomic args need to be quoted!
                               (list arg ...)))
       (unless changed
         ;; check whether we added any new elements.
         (set! changed (not (subset? tuples pred))))
       (set-union! pred tuples)
       )])

;; (solving (term ...) body ...)
(define-syntax-parser solving
  [(_ (term ...) body ...)
   (define-values (_ result)
    (for/fold ([bound-vars (set)]
               [body #'(begin body ...)])
              ([term (syntax->list #'(term ...))])
      (define/syntax-parse (pred:NAME arg:ARGUMENT ...) term)
      (define/syntax-parse ((~or var:VARIABLE (~not _:VARIABLE)) ...)
        #'(arg ...))
      (define new-vars (list->set (syntax->datum #'(var ...))))
      (define tuple-patterns
        (for/list ([arg (syntax->list #'(arg ...))])
          (syntax-parse arg
            [atom:NAME #''atom]
            [v:VARIABLE (if (set-member? bound-vars (syntax->datum #'v))
                            #'(== v)
                            #'v)])))
      (values
       (set-union bound-vars new-vars)
       #`(let*/set ([tuple pred])
                   (match tuple
                     [(list #,@tuple-patterns) #,body]
                     [_ (set)])))))
   result])

(define (test e #:n [n 1])
  (pretty-print (syntax->datum (if n (let loop ([n n] [e e])
                                       (if (= n 0) e
                                           (loop (- n 1) (expand-once e))))
                                   (expand e)))))
