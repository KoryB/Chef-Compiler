#lang racket

(provide nullablize)
(provide firstisize)
(provide followize)

(require racket/hash)
(require racket/set)


(define (nullable? prod-list nulls)
  (for/or ([prod prod-list])
    (for/and ([sym prod])
      (set-member? nulls sym))))
  
(define (compute-nullables prods nulls)
  (for/set ([prod-pair (in-hash-pairs prods)]
            #:when (nullable? (cdr prod-pair) nulls))
    (car prod-pair)))

;; TODO: the equal? check is ineffecient, use a flag somehow
(define (nullablize prods)
  (let loop ([old-nulls (set)]
             [new-nulls (compute-nullables prods (set))])
    (if (equal? old-nulls new-nulls)
        old-nulls
        (loop new-nulls (compute-nullables prods new-nulls)))))


(define (compute-firsts prods nulls firsts)
  (for*/fold ([working-firsts firsts])
             ([(var prod-list) prods]
              [prod (in-list prod-list)])
    (printf "~a -> ~a\n" var prod)
    (for/fold ([t-firsts working-firsts])
              ([sym prod]
               #:final (not (set-member? nulls sym)))
      (hash-set t-firsts var
                (set-union (hash-ref t-firsts var)
                           (hash-ref t-firsts sym))))))

(define (firstisize regexes prods nulls)
  (define token-firsts
    (for/hash ([regex regexes]) (values (car regex) (set (car regex)))))
  (define prod-firsts
    (for/hash ([sym (in-hash-keys prods)]) (values sym (set))))
  (define init-firsts (hash-union token-firsts prod-firsts))

  (let loop ([old-firsts init-firsts]
             [new-firsts (compute-firsts prods nulls init-firsts)])
    (if (equal? old-firsts new-firsts)
        old-firsts
        (loop new-firsts (compute-firsts prods nulls new-firsts)))))


;; TODO: Figure out a way to do a cleaner loop and avoid the gross double if check
(define (compute-follows prods nulls firsts follows)
  (for/fold ([working-follows follows])  
            ([(var prod-list) prods]
             #:when #t
             [prod (in-list prod-list)]
             #:when #t
             [i (in-range (vector-length prod))]
             [x (in-vector prod)]
             #:when (hash-has-key? prods x))
    (hash-set working-follows x
              (if (equal? i (- (vector-length prod) 1))
                  (set-union (hash-ref working-follows x)
                             (hash-ref working-follows var))
                  (for/fold ([working-set (hash-ref working-follows x)])
                            ([j (in-range (+ 1 i) (vector-length prod))]
                             [y (in-vector prod (+ 1 i) (vector-length prod))]
                             #:final (not (set-member? nulls y)))
                    (if (and (equal? j (- (vector-length prod) 1))
                             (set-member? nulls y))
                        (set-union working-set
                                   (hash-ref firsts y)
                                   (hash-ref working-follows var))
                        (set-union working-set
                                   (hash-ref firsts y))))))))
    
(define (followize prods nulls firsts)
  (define init-follows
    (for/fold ([working-follows (hash)]
               #:result (hash-set working-follows "S" (set "$")))
              ([x (in-hash-keys prods)])
      (hash-set working-follows x (set))))

  (let loop ([old-follows init-follows]
             [new-follows (compute-follows prods nulls firsts init-follows)])
    (if (equal? old-follows new-follows)
        old-follows
        (loop new-follows (compute-follows prods nulls firsts new-follows)))))
