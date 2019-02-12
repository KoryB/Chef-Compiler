;; %id is for structs, they are like beginner classes
;; id% is for classes. 
#lang racket

(provide (struct-out %token))
(provide parse-grammar)
(provide tokenize)

(require racket/string)

(define (string-count str char)
  (for/sum ([i (string-length str)])
    (if (equal? char (string-ref str i)) 1 0)))
     
     
(struct %token (symbol lexeme line-number) #:transparent)
(struct %m-pos (symbol lexeme length) #:transparent)

(define whitespace (list (cons "whitespace" #px"(?i:\\s+)")))
(define start-symbol "S")

(define (count-arrow-lines in)
  (let loop ([line (read-line in)]
             [num-defines 0]
             [num-regexes 0]
             [num-prods 0]
             [in-regexes #t])
    (cond
      [(equal? line eof)
       (values num-defines num-regexes num-prods)]
      [(equal? line "")
       (loop (read-line in) num-defines num-regexes num-prods #f)]
      [(equal? (substring line 0 1) "#")
       (loop (read-line in) (+ num-defines 1)
             num-regexes num-prods in-regexes)] ;TODO add error if regex!=0
      [else
       (if in-regexes
           (loop (read-line in) num-defines (+ num-regexes 1)
                 num-prods in-regexes)
           (loop (read-line in) num-defines num-regexes
                 (+ num-prods 1) in-regexes))])))

(define (parse-grammar fn)
  (define-values (num-defines num-regexes num-prods)
    (call-with-input-file fn #:mode 'text count-arrow-lines))
  (call-with-input-file fn #:mode 'text
    (lambda (in)
      ;; ignoring defines for now
      (define defines (for/list ([i num-defines]) (read-line in)))
      (define regexes
        (append
         (for/list ([i num-regexes])
          (let* ([line (read-line in)]
                 [split-line (string-split line #rx"\\s*-> ")]
                 [id (string-trim (car split-line))]
                 [regex-str (car (cdr split-line))]
                 [regex (pregexp (string-append "(?i:" regex-str ")"))])
            (cons id regex)))
         whitespace))
      (read-line in) ; get rid of empty line
      (define productions
        (for/hash ([i (in-range num-prods)])
          (let* ([line (read-line in)]
                 [split-line (string-split line #rx"\\s*-> ")]
                 [id (string-trim (car split-line))]
                 [prod-str (car (cdr split-line))]
                 [prod-str-rm (string-replace prod-str #px"(\\blambda\\b|λ|\\bepsilon\\b)" "")]
                 [prods-on-pipe (string-split prod-str-rm "|")]
                 [prods-on-space (map string-split prods-on-pipe)]) ; #px"\\s+"
            (values id (map list->vector prods-on-space)))))
      (values regexes productions))))

;; may want to use the peek versions of regex to avoid
 ; loading the entire string into memory
(define (tokenize fn regexes productions)
  (define in (open-input-file fn #:mode 'text))
  (define input-text (port->string in))
  (define input-text-length (string-length input-text))
  
  ;(printf "input-text: ~a\n" input-text)
  (let loop ([i 0]
             [line-num 1]
             [tokens '()])
    (cond
      [(>= i input-text-length)
       (cons (%token "$" "" (+ line-num 1)) tokens )] ; return
      [else
       (define m-pos
         (ormap
          (lambda (regex)
            (define matches (regexp-match-positions (cdr regex) input-text i))
            (if (and matches (equal? i (car (car matches))))
                (let ([start (car (car matches))]
                      [end   (cdr (car matches))])
                  (%m-pos (car regex)
                          (substring input-text start end ) (- end start)))
                #f))
          regexes))
       (cond
         [(not m-pos)
          (define end (min (+ i 10) (- input-text-length 1)))
          (error 'tokenize "invalid token at line ~a\naround: ~a"
                 line-num (substring input-text i end))]
         [(equal? (%m-pos-symbol m-pos) "whitespace")
          (loop
           (+ i (%m-pos-length m-pos))
           (+ line-num (string-count (%m-pos-lexeme m-pos) #\newline)) tokens)]
         [else
          (define token (%token (%m-pos-symbol m-pos)
                                (%m-pos-lexeme m-pos) line-num))
          (loop (+ i (%m-pos-length m-pos))
                (+ line-num (string-count (%token-lexeme token) #\newline))
                (cons token tokens))])])))
