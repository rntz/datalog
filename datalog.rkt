#lang at-exp racket

;; GRIPE: this file is too long for what it does. the core of Datalog is simpler
;; than this.

(require
 (for-syntax racket
             racket/syntax
             "ident.rkt"
             "syntax.rkt"
             "tarjan-scc.rkt"
             "util.rkt")
 syntax/parse
 "util.rkt"
 "core-forms.rkt")

;; Provide the macro that understands datalog and its top-level forms.
(provide datalog (all-from-out "core-forms.rkt"))


;; The syntax transformer

;; in a call to local-expand, if #%app and #%datum are in stop-ids, then they
;; don't get automatically inserted, which is what we want.
;;
;; why #%app and not #%plain-app? I don't know.
(define-for-syntax datalog-top-stop-ids
  (append core-top-forms (list #'#%app #'#%datum)))

(define-syntax-parser datalog
  [(_ top ...)
   (define (visit-all forms)
     (append-map visit (syntax->list forms)))
   (define (visit form)
     (syntax-parse (local-expand form 'module datalog-top-stop-ids)
       #:literals (begin)
       [(begin top ...) (visit-all #'(top ...))]
       [t (list #'t)]))
   #`(datalog-internal #,@(visit-all #'(top ...)))])

;; TODO: error message if (datalog ...) used in non-definition context.
(define-syntax-parser datalog-internal
  #:literals (begin)
  [(_ top:TOP ...)

   ;; munge data.
   (define clauses (syntax->list #'(top.clauses ... ...)))
   (define preds
     (for*/set ([clause clauses]
                [pred (syntax-parse clause
                        [c:CLAUSE (syntax->list #'(c.refers ...))])])
       (bound pred)))
   (define/with-syntax (pred ...) (map bound-identifier (set->list preds)))

   ;; TODO Check arity: Declarations, definitions, and uses of a given predicate
   ;; all have the same arity.

   ;; TODO Check generativity: every variable in the head of a clause occurs in
   ;; a positive position in its body.

   ;; A hash from predicate names to their information.
   (define predicates
     (group clauses
       #:by     (syntax-parser [c:CLAUSE (bound #'c.pred)])
       #:map    clause-info
       #:reduce merge-clause-info
       ;; some predicates referred to may have no defining clauses. this ensures
       ;; they end up with information anyway.
       #:init   (for/hash ([p preds]) (values p empty-predicate))))

   ;; Find the SCCs in the dependency graph
   (define depends
     (for/hash ([(name info) predicates])
       (values name (predicate-depends info))))

   (match-define (scc-info components pred->component-index)
     (sccs depends))

   ;; Check if any SCC contains a negative edge (which implies a negative cycle,
   ;; no stratification).
   (for* ([component components]
          [v component]
          [w (predicate-negative-depends (hash-ref predicates v))]
          #:when (set-member? component w))
     (error @~a{Cannot stratify: @v uses @w, and @w depends on @|v|!}))

   ;; clauses, organized into SCCs, topologically sorted.
   (define/with-syntax ((clause ...) ...)
     (for/list ([component components])
       (let*/list ([pred component])
         (predicate-clauses (hash-ref predicates pred)))))

   #`(begin
       ;; initialize predicate sets.
       (define pred (mutable-set)) ...
       ;; run the clauses for each SCC in order.
       (exec-clauses! clause ...) ...
       ;; Produce the values of each predicate.
       (make-immutable-hash
        (list (cons 'pred pred) ...)))])


;; Helper macros
(define-syntax-parser exec-clauses!
  [(_ clause:CLAUSE ...)
   #'(let loop ()
       (define changed #f)
       (exec-clause! changed clause) ...
       (when changed (loop)))])

;; 2017-12-26: FIXME: the comment below belongs somewhere else. where?
;;
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

;; (solve-clause CLAUSE)
(define-syntax-parser solve-clause
  ;; TODO: negative literals
  #:literals (:- ~)
  [(_ (:- (pred:PRED arg:ARGUMENT ...) lit:+LIT ...))
   #'(solve-w/bound () (lit ...) (set (list arg.as-expr ...)))])

;; (solve-w/bound (VARIABLE ...) (LIT ...) body ...)
(define-syntax-parser solve-w/bound
  [(_ (_:VARIABLE ...) () body ...) #'(let () body ...)]

  [(_ (bound-var:VARIABLE ...)
      ((pred:PRED arg:ARGUMENT ...) lit:+LIT ...)
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

   #'(let*/set ([tuple pred])
       (match tuple
         [(list arg-pat ...)
          (solve-w/bound (new-bound-var ...) (lit ...) body ...)]
         [_ (set)]))])


;; Examples and testing
(define (test e #:n [n 1])
  (pretty-print (syntax->datum (if n (let loop ([n n] [e e])
                                       (if (= n 0) e
                                           (loop (- n 1) (expand-once e))))
                                   (expand e)))))

(module+ example
  (datalog
   (:- (person john))  (:- (person mary)) (:- (person hilary))
   (:- (person alice)) (:- (person bob))  (:- (person charlie))
   (:- (peep X) (person X))))

(module+ graph-example
  (datalog
   (:- (node john)) (:- (node mary)) (:- (node sue))
   (:- (edge john mary))
   (:- (edge mary sue))

   (:- (refl X X) (node X))
   (:- (symm X Y) (edge X Y))
   (:- (symm X Y) (symm Y X))
   (:- (trans X Y) (edge X Y))
   (:- (trans X Z) (trans X Y) (trans Y Z))

   (:- (equiv X Y) (edge X Y))
   (:- (equiv X X) (node X))
   (:- (equiv X Y) (equiv Y X))
   (:- (equiv X Z) (equiv X Y) (equiv Y Z))))


(module+ obama-example
  (datalog
   (:- (person barack)) (:- (person michelle))
   (:- (person malia))  (:- (person sasha))

   (:- (married barack michelle))
   (:- (parent michelle malia))
   (:- (parent michelle sasha))
   ;; all of michelle's children happen to be barack's.
   (:- (parent barack X) (parent michelle X))))
