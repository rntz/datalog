#lang at-exp racket

(require
 (for-syntax racket
             racket/syntax
             "ident.rkt"
             "syntax.rkt"
             "tarjan-scc.rkt"
             "util.rkt")
 "util.rkt")

;; Provide the macro that understands datalog.
(provide datalog)


;; Information about predicates derived from their clauses.


;; The syntax transformer
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

   ;; Find the SCCs in the dependency graph
   (define depends
     (for/hash ([(name info) predicates])
       (values name (predicate-depends info))))

   (match-define (scc-info components pred->component-index)
     (sccs depends))

   ;; Stratify: Check if any SCC contains a negative edge (negative cycle, no
   ;; stratification).
   (for* ([component components]
          [v component]
          [w (predicate-negative-depends (hash-ref predicates v))]
          #:when (set-member? component w))
     (error @~a{Cannot stratify: @v uses @w, and @w depends on @|v|!}))

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
       (exec-clauses! component-clause ...) ...
       ;; Produce the values of each predicate.
       (list (freeze-set pred) ...))])

;; Helper macros
(define-syntax-parser exec-clauses!
  [(_ clause:CLAUSE ...)
   #'(let loop ()
       (define changed #f)
       (exec-clause! changed clause) ...
       (when changed (loop)))])

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

(define-syntax-parser exec-clause!
  [(_ changed:id clause:CLAUSE)
   #`(let ()
       ;; compute the set of things to add to `pred'
       (define new-tuples (solve-clause clause))
       ;; update changed flag.
       (unless changed (set! changed (not (subset? new-tuples clause.pred))))
       ;; add the new elements to the predicate's set.
       (set-union! clause.pred new-tuples))])

(define-syntax-parser solve-clause
  ;; TODO: negative terms
  #:datum-literals (:- not)
  [(_ ((pred:PRED arg:ARGUMENT ...) :- term:+TERM ...))
   #'(solve-w/bound () (term ...) (set (list arg.as-expr ...)))])

(define-syntax-parser solve-w/bound
  [(_ (_:VARIABLE ...) () body ...) #'(let () body ...)]

  [(_ (bound-var:VARIABLE ...)
      ((pred:PRED arg:ARGUMENT ...) term:+TERM ...)
      body ...)

   (define bound-vars (syntax->list #'(bound-var ...)))

   (define/with-syntax (new-bound-var ...)
    (syntax-parse #'(arg ...)
      [((~or v:VARIABLE _:ATOM) ...)
       (remove-duplicates (syntax->list #'(v ... bound-var ...))
                          bound-identifier=?)]))

   (define/with-syntax (arg-pat ...)
    (for/list ([a (syntax->list #'(arg ...))])
      (syntax-parse a
        [v:VARIABLE #:when (member #'v bound-vars bound-identifier=?) #'(== v)]
        [v:VARIABLE #'v]
        [a:ATOM #'a.as-pattern])))

   ;; maybe define a template-expander here? and use it?
   #'(let*/set ([tuple pred])
       (match tuple
         [(list arg-pat ...)
          (solve-w/bound (new-bound-var ...) (term ...) body ...)]
         [_ (set)]))]

  ;; [(_ (v:VARIABLE ...) (term:+TERM ...) body ...)
  ;;  ;; what the fuck is going on here.
  ;;  ;; this nests in the wrong order!
  ;;  (define-values (_ result)
  ;;   (for/fold ([bound-vars (set)]
  ;;              [body #'(begin body ...)])
  ;;             ([term (syntax->list #'(term ...))])
  ;;     (define/syntax-parse (pred:PRED arg:ARGUMENT ...) term)
  ;;     (define/syntax-parse ((~or var:VARIABLE (~not _:VARIABLE)) ...)
  ;;       #'(arg ...))
  ;;     (define new-vars (list->set (syntax->datum #'(var ...))))
  ;;     (define tuple-patterns
  ;;       (for/list ([arg (syntax->list #'(arg ...))])
  ;;         (syntax-parse arg
  ;;           [a:ATOM #'a.as-expr]
  ;;           [v:VARIABLE (if (set-member? bound-vars (syntax->datum #'v))
  ;;                           #'(== v)
  ;;                           #'v)])))
  ;;     (values
  ;;      (set-union bound-vars new-vars)
  ;;      #`(let*/set ([tuple pred])
  ;;                  (match tuple
  ;;                    [(list #,@tuple-patterns) #,body]
  ;;                    [_ (set)])))))
  ;;  result]
  )


(define (test e #:n [n 1])
  (pretty-print (syntax->datum (if n (let loop ([n n] [e e])
                                       (if (= n 0) e
                                           (loop (- n 1) (expand-once e))))
                                   (expand e)))))

(module+ example
  (datalog
   ((person john) :-)
   ((person mary) :-)
   ((person hilary) :-)
   ((person alice) :-) ((person bob) :-) ((person charlie) :-)

   ((peep X) :- (person X))

   ((edge john mary) :-)

   ((symm X Y) :- (edge X Y))
   ((symm X Y) :- (symm Y X))

   ))

(module+ obama-example
  (datalog
   ;; uh-oh, putting this here causes a problem
   ((person barack) :-) ((person michelle) :-)
   ((person malia) :-)  ((person sasha) :-)

   ((married barack michelle) :-)
   ((parent michelle malia) :-)
   ((parent michelle sasha) :-)
   ;; all of michelle's children are barack's.
   ((parent barack X) :- (parent michelle X))))
