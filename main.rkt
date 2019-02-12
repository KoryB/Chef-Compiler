#lang racket

(require racket/pretty)

(require "tokenizer.rkt")
(require "parsing_prep.rkt")
(require "lr_parser.rkt")
(require "parsing_test_visualizer.rkt")
(require "assemblizer.rkt")

(define argv (current-command-line-arguments))
(define argc (vector-length argv))

; ignoring parse table specification
(define-values (g-path i-path)
  (cond
    [(equal? argc 3) (values (vector-ref argv 1) (vector-ref argv 2))]
    [(equal? argc 2) (values (vector-ref argv 0) (vector-ref argv 1))]
    [else
     (printf "Using default values for g-path and i-path\n")
     (values "exFinal/grammar.txt" "exFinal/input1.txt")]))


(define-values (regexes+white productions) (parse-grammar g-path))
(display "grammar parsed!\n")
(printf "Regexes:\n")
(pretty-print regexes+white)
(define tokens (reverse (tokenize i-path regexes+white productions)))
(display "input tokenized!\n")
(define regexes (remove (cons "whitespace" #px"(?i:\\s+)") regexes+white))
(define nullables (nullablize productions))
(display "nullables calculated!\n")
(define firsts (firstisize regexes productions nullables))
(display "firsts calculated!\n")
(define follows (hash-set (followize productions nullables firsts) "S'" (set "$")))
(display "follows calculated!\n")
(define-values (dfa start-state) (dfaisize productions))
(display "dfa created!\n")
(define state-list (dfa-dfs dfa start-state))
(display "state-list created!\n")
(define table (dfa->lr0-table regexes productions dfa state-list))
(display "table generated!\n")
(define slr-table (dfa->slr1-table regexes productions follows dfa state-list))
(display "slr-table generated!\n")
(define parse-tree (parse tokens slr-table))
(display "parse complete!\n")

(printf "Compiling parse tree\n")
(call-with-output-file "chef.asm" #:exists 'truncate
  (lambda (out)
    (display (assemblize parse-tree) out)))
(printf "Compile complete!\n")

#|
(printf "Regexes:\n")
(pretty-print regexes)
(printf "Grammar:\n")
(pretty-print productions)
(printf "\nNullable Set:\n")
(pretty-print nullables)
(printf "\nFirst Table:\n")
(for ([sym (in-hash-keys productions)])  ; firsts contains terminals as keys
  (printf "\t~a\t~a\n" sym (hash-ref firsts sym)))
(printf "Follow Table:\n")
(for ([(sym follow) follows])
  (printf "\t~a\t~a\n" sym follow))
(printf "Tokens:\n")
(pretty-print tokens)
(printf "LR0 DFA:\n")
(pretty-print dfa)
(pretty-print state-list)

(call-with-output-file "dfa.dot" #:exists 'truncate
  (lambda (out)
    (display (dfa->graphviz dfa state-list) out)))

(call-with-output-file "table.html" #:exists 'truncate
  (lambda (out)
    (display (lr0-table->html regexes productions table) out)))

(call-with-output-file "slr-table.html" #:exists 'truncate
  (lambda (out)
    (display (lr0-table->html regexes productions slr-table) out)))

|#(call-with-output-file "tree.dot" #:exists 'truncate
  (lambda (out)
    (display (tree->graphviz parse-tree) out)))#|

(printf "Parse Tree:\n")
(pretty-print parse-tree) |#
