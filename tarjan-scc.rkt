#lang racket

(provide graph? graph/c scc-info/c scc-info sccs topsort)

(struct scc-info (components node->component-index)
  #:transparent)

(define graph? (hash/c any/c set? #:immutable #t #:flat? #t))
(define (graph/c node/c) (hash/c node/c (set/c node/c) #:immutable #t))

(define (scc-info/c node/c)
  (struct/c scc-info
    (vectorof (set/c node/c) #:immutable #t)
    (hash/c node/c exact-nonnegative-integer? #:immutable #t)))

;; takes a graph, represented as a hash from nodes to sets of nodes.
;; returns a graph of notes
;;
;; Hm, what if the edges of the graph have annotations? (for example,
;; positive/negative?) we could just keep a list of them.
(define/contract (sccs graph)
  (-> (graph/c any/c) (scc-info/c any/c))

  (define sorted-components '())

  (define index 0)
  (define indices (make-hash))
  (define lowlink (make-hash))
  (define (next-index!)
    (begin0 index
      (set! index (+ index 1))))

  (define stack '())
  (define on-stack (mutable-set))

  (define (try-visit node)
    (unless (hash-has-key? indices node)
      (visit node)))

  (define (visit node)
    (define index (next-index!))
    (hash-set! indices node index)
    (hash-set! lowlink node index)

    (set! stack (cons node stack))
    (set-add! on-stack node)

    (define (update-lowlink! node new-lowlink)
      (hash-update! lowlink node (curry min new-lowlink)))

    (for ([next-node (hash-ref graph node (set))])
      (cond
        ;; if next-node is totally unvisited...
        [(not (hash-has-key? indices next-node))
         (visit next-node)
         (update-lowlink! node (hash-ref lowlink next-node))]
        ;; if next-node is on our stack...
        [(set-member? on-stack next-node)
         ;; XXX: why does this use next-node's index, rather than lowlink?
         ;; my current intuition: it should work either way.
         ;; (update-lowlink! node (hash-ref indices next-node))
         (update-lowlink! node (hash-ref lowlink next-node))]
        ;; next-node already completely visited, do nothing
        [#t]))

    ;; if we're done visiting an SCC...
    (when (= index (hash-ref lowlink node))
      ;; pop its nodes off the stack
      (define-values (before after)
        (splitf-at stack (negate (curry equal? node))))
      (set! stack (cdr after))
      (define nodes (list->set (cons node before)))
      (set-subtract! on-stack nodes)
      ;; add the SCC.
      (set! sorted-components (cons nodes sorted-components))))

  ;; The core loop.
  (for ([n (hash-keys graph)]) (try-visit n))

  ;; Postprocessing.
  (set! sorted-components (reverse sorted-components))
  (scc-info
   (apply vector-immutable sorted-components)
   (for*/hash ([(scc index) (in-indexed sorted-components)]
               [node scc])
     (values node index))))

(define/contract (topsort graph)
  (-> (graph/c any/c) list?)
  (match-define (scc-info sorted-components node->component-index) (sccs graph))
  (for*/list ([component sorted-components] [node component]) node))


;; unfortunately graph isomorphism is
;; 1. a pain to implement
;; 2. inefficient
;;
;; so generative testing is not obviously useful here
;;
;; although, I wouldn't be surprised if there were a simple but inefficient
;; implementation of graph isomorphism I haven't thought of. Maybe in Datafun?
;;
;; TODO: more tests.
(module+ test
  (require rackunit)
  (define to-list sequence->list)
  (define to-set (compose list->set to-list))

  (define-syntax-rule (graph [v w ...] ...)
    (make-immutable-hash (list (cons 'v (set 'w ...)) ...)))

  ;; line graph (a -> b -> c)
  (check-equal? '(c b a) (topsort (graph [a b] [b c] [c])))

  ;; two-node cycle (a -> b -> a)
  (define two-cycle (sccs (graph [a b] [b a])))
  (check-equal?
   (vector-immutable (set 'a 'b))
   (scc-info-components two-cycle))

  ;; [a -> b c], [b -> a] graph
  (define abc (sccs (graph [a b c] [b a] [c])))
  (check-equal?
   (list (set 'c) (set 'a 'b))
   (to-list (scc-info-components abc)))

  ;; diamond graph
  (define diamond (graph [a b c] [b d] [c d] [d]))
  (check-equal?
   (to-set (map set '(a b c d)))
   (to-set (scc-info-components (sccs diamond)))))
