#lang racket

(provide dfaisize)
(provide dfa-dfs)
(provide dfa->lr0-table)
(provide dfa->slr1-table)
(provide parse)

(provide %lr0-item-string)
(provide %lr0-item-prod-string)

(require racket/list)
(require racket/set)
(require racket/hash)
(require racket/pretty)
(require racket/string)

(require "parsing_prep.rkt")
(require "tokenizer.rkt")


(struct %lr0-item (lhs rhs dpos) #:transparent)
(define (%lr0-item-at-dpos lr0 dpos) (%lr0-item (%lr0-item-lhs lr0) (%lr0-item-rhs lr0) dpos))
(define (%lr0-item-dpos-sym lr0) (vector-ref (%lr0-item-rhs lr0) (%lr0-item-dpos lr0)))
(define (%lr0-item-string lr0) (format "<lr0 ~a ~a ~a>"
                                       (%lr0-item-lhs lr0)
                                       (%lr0-item-rhs lr0)
                                       (%lr0-item-dpos lr0)))
(define (%lr0-item-prod-string lr0) (format "~a -> ~a"
                                       (%lr0-item-lhs lr0)
                                       (let ([sp (open-output-string)]
                                             [rhs (%lr0-item-rhs lr0)])
                                         (for ([i (in-range (vector-length rhs))])
                                           (when (equal? i (%lr0-item-dpos lr0))
                                             (display " " sp)
                                             (display #\u2022 sp)
                                             (display " " sp))
                                           (display " " sp)
                                           (display (vector-ref rhs i) sp)
                                           (display " " sp))
                                         (when (%lr0-item-dpos-final? lr0)
                                           (display " " sp)
                                           (display #\u2022 sp)
                                           (display " " sp))
                                         (get-output-string sp))))
(define (%lr0-item-dpos-final? lr0) (equal? (%lr0-item-dpos lr0)
                                            (vector-length (%lr0-item-rhs lr0))))


(define (compute-closure items prods)
  (for/fold ([working-items items])
            ([item (in-set items)]
             #:when (and (not (%lr0-item-dpos-final? item))
                         (hash-has-key? prods (%lr0-item-dpos-sym item)))
             [rhs (in-list (hash-ref prods (%lr0-item-dpos-sym item)))])
    (set-add working-items (%lr0-item (%lr0-item-dpos-sym item) rhs 0))))

(define (closureize items prods)
  (let loop ([old-closure items]
             [new-closure (compute-closure items prods)])
    (if (equal? old-closure new-closure)
        old-closure
        (loop new-closure (compute-closure new-closure prods)))))


;; return value is a hash of a set of lr0-items to a hash of symbols to lr0-items.
;; the second value is the start state
;; the symbol represents the transition symbol
;;   the start state has a symbol of ^
;; the lr0-items represent the new state to transition to
(define (dfaisize productions)
  (define augmented-productions (hash-set productions "S'" '(#("S"))))
  (define start-lr0 (%lr0-item "S'" #("S") 0))
  (define start-state (closureize (set start-lr0) augmented-productions))
  
  (define dfa
    (let loop ([dfa (hash)]
               [state start-state])
      (cond
        [(hash-has-key? dfa state) dfa]
        [else
         (define transition-symbols
           (for/fold ([t (hash)])
                     ([lr0 (in-set state)]
                      #:unless (%lr0-item-dpos-final? lr0))
             (define n (%lr0-item-dpos-sym lr0))
             (define new-lr0 (%lr0-item-at-dpos lr0 (+ 1 (%lr0-item-dpos lr0))))
             (hash-update t n
                          (lambda (v) (set-add v new-lr0))
                          (lambda () (set new-lr0)))))
         (define transitions
           (for/hash ([(sym t-set) transition-symbols])
             (values sym (closureize t-set augmented-productions))))
         (for/fold ([w-dfa (hash-set dfa state transitions)])
                   ([t-state (in-hash-values transitions)])
           (hash-union w-dfa (loop w-dfa t-state)
                       #:combine (lambda (a b) a)))]))) ; use previous value on combine
  (values dfa start-state))


(define (dfa-dfs dfa start-state)
  (let search ([visited (set)]
               [to-visit (list start-state)]
               [found '()])
    (cond
      [(equal? to-visit '()) (reverse found)]
      [else
       (define state (car to-visit))
       (cond
         [(set-member? visited state) (search visited (cdr to-visit) found)]
         [else
          (define new-visited (set-add visited state))
          (define unseen (filter
                          (lambda (child) (not (set-member? new-visited child)))
                          (hash-values (hash-ref dfa state))))
          (search new-visited (append unseen (cdr to-visit)) (cons state found))])])))


(define (dfa->lr0-table regexes productions dfa state-list)
  (define variables (hash-keys productions))
  (define terminals (cons "$" (map car regexes)))
  (define symbols (append variables terminals))
  (for/vector ([state (in-list state-list)])
    (define transitions (hash-ref dfa state))
    
    (define var-entries
      (for/hash ([var (in-list variables)]
                 #:when (hash-has-key? transitions var))
        (define tstate (hash-ref transitions var))
        (values var (set (list "T" (~a (index-of state-list tstate)))))))
    (define term-entries
      (for/hash ([term (in-list terminals)]
                 #:when (hash-has-key? transitions term))
        (define tstate (hash-ref transitions term))
        (values term (set (list "S" (~a (index-of state-list tstate)))))))
    (define trans-shifts (hash-union var-entries term-entries))

    ;; right now I'm assuming any conflicts will append to the cell
    (for/fold ([tsr trans-shifts])
              ([lr0 (in-set state)]
               #:when (%lr0-item-dpos-final? lr0))
      (define reductions
        (for/hash ([sym (in-list symbols)])
          (values sym (set (list "R"
                                 (~a (vector-length (%lr0-item-rhs lr0)))
                                 (~a (%lr0-item-lhs lr0)))))))
      (hash-union tsr reductions
                  #:combine (lambda (a b) (set-union a b))))))


(define (dfa->slr1-table regexes productions follows dfa state-list)
  (define variables (hash-keys productions))
  (define terminals (cons "$" (map car regexes)))
  (define symbols (append variables terminals))
  (for/vector ([state (in-list state-list)])
    (define transitions (hash-ref dfa state))
    
    (define var-entries
      (for/hash ([var (in-list variables)]
                 #:when (hash-has-key? transitions var))
        (define tstate (hash-ref transitions var))
        (values var (set (list "T" (~a (index-of state-list tstate)))))))
    (define term-entries
      (for/hash ([term (in-list terminals)]
                 #:when (hash-has-key? transitions term))
        (define tstate (hash-ref transitions term))
        (values term (set (list "S" (~a (index-of state-list tstate)))))))
    (define trans-shifts (hash-union var-entries term-entries))

    ;; right now I'm assuming any conflicts will append to the cell
    (for/fold ([tsr trans-shifts])
              ([lr0 (in-set state)]
               #:when (%lr0-item-dpos-final? lr0))
      (define reductions
        (for/hash ([sym (in-list symbols)]
                   #:when (set-member? (hash-ref follows (%lr0-item-lhs lr0)) sym))
          (values sym (set (list "R"
                                 (~a (vector-length (%lr0-item-rhs lr0)))
                                 (~a (%lr0-item-lhs lr0)))))))
      (hash-union tsr reductions
                  #:combine (lambda (a b) (set-union a b))))))
             
        
(define (parse tokens lr-table)
  (let loop ([state 0]
             [w-tokens tokens]
             [state-stack '(0)]
             [tree-stack '()]
             [is-token #t])
    (define token (car w-tokens))
    (define command (hash-ref (vector-ref lr-table state)
                              (if is-token
                                  (%token-symbol token)
                                  token) ; if reduced, this will be the lhs-state
                              (λ ()
                                (if is-token
                                    (error 'parse "Syntax Error at token ~a" token)
                                    (error 'parse "No valid Transition, state ~a lhs ~a"
                                           state token)))))
    (when (> (set-count command) 1)
      (error 'command-parse "SR or RR conflict! State ~a Token ~a" state token))
    (match (set-first command)
      [(list "S" next-state-s)
       (printf "Shifting from ~a to ~a, token ~a" state next-state-s token)
       (printf "\nStack: ~a" state-stack)
       (newline)
       (define next-state (string->number next-state-s))
       (loop next-state (cdr w-tokens)
             (cons next-state state-stack)
             (cons token tree-stack)
             #t)]
      [(list "T" next-state-s)
       (printf "Transitioning from ~a to ~a, token ~a" state next-state-s token)
       (printf "\nStack: ~a" state-stack)
       (newline)
       (define next-state (string->number next-state-s))
       (loop next-state (cdr w-tokens) (cons next-state state-stack) tree-stack #t)]
      [(list "R" reduce-num-s lhs-state) ; end recursion with cond lhs-state == "S'"
       (define reduce-num (string->number reduce-num-s))
       (printf "Reducing ~a to ~a, state ~a, token ~a" reduce-num lhs-state state token)
       (printf "\nStack: ~a" state-stack)
       (newline)
       (cond
         [(equal? lhs-state "S'") (car tree-stack)] ; only thing left on stack will be S
         [else
          (define reduced (take tree-stack reduce-num))
          (define new-state-stack (list-tail state-stack reduce-num))
          (define new-subtree (list* lhs-state (reverse reduced)))
          (define new-tree-stack (cons new-subtree (list-tail tree-stack reduce-num)))
          (define transition-from-state (car new-state-stack))
          
          (loop (car new-state-stack) (cons lhs-state w-tokens)
                new-state-stack new-tree-stack #f)])]       
      [else
       (error 'command-parse "Invalid command ~a" command)])))
  
    