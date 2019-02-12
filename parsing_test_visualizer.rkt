#lang racket

(provide dfa->graphviz)
(provide lr0-table->html)
(provide tree->graphviz)

(require racket/set)
(require "lr_parser.rkt")
(require "tokenizer.rkt")

(define (dfa->graphviz dfa state-list)
  (define dot (open-output-string))
  (display "digraph dfa {\n" dot)  
  (for ([state (in-list state-list)]
        [index (in-naturals)])
    (fprintf dot "\tq~a [shape = box, label = \"~a\"];\n" index
             (string-join (cons (~a index) (set-map state %lr0-item-prod-string)) "\\n"))
    (for ([(symbol tstate) (hash-ref dfa state)])
      (define tindex (index-of state-list tstate))
      (fprintf dot "\tq~a -> q~a [label = \"~a\"];\n" index tindex symbol)))
  (display "}\n" dot)
  (get-output-string dot))


(define (tree->graphviz tree)
  (define dot (open-output-string))
  (display "digraph dfa {\n" dot)
  (fprintf dot "\t0 [label = \"~a\"];\n" (car tree))
  (let build ([parent-name "0"]
              [parent (cdr tree)])
    (for ([child (in-list parent)]
          [i (in-naturals)])
      (define child-name (string-append parent-name (~a i)))
      (cond
        [(list? child) ; child is a list, car is a string for variable
         (fprintf dot "\t~a [label = \"~a\"];\n" child-name (car child))
         (fprintf dot "\t~a->~a;\n" parent-name child-name)
         (build child-name (cdr child))]
        [else ; child is a token, and leaf
         (fprintf dot "\t~a [label = \"~a\\n[~a]\"];\n"
                  child-name (%token-symbol child) (%token-lexeme child))
         (fprintf dot "\t~a->~a;\n" parent-name child-name)])))
  (fprintf dot "}\n")
  (get-output-string dot))


(define (lr0-table->html regexes productions table)
  (define variables (hash-keys productions))
  (define terminals (append (map car regexes) '("$")))
  (define symbols (append variables terminals))
  (define html (open-output-string))
  
  (display "<HTML>\n<HEAD></HEAD>\n<BODY>\n" html)
  (display "<STYLE>td {border: 1px solid black; }</STYLE>\n" html)
  (display "<TABLE>" html)
  (display "<TR><TH></TH>" html)
  (for ([sym (in-list symbols)])
    (fprintf html "<TH>~a</TH>" sym))

  (display "\n" html)
  (for ([row (in-vector table)]
        [idx (in-naturals)])
    (fprintf html "<TR><TD>~a</TD>" idx)
    (for ([sym (in-list symbols)])
      (fprintf html "<TD>~a</TD>"
               (string-join (set-map (hash-ref row sym '()) string-join) "<br>")))
    (display "</TR>" html))

  (display "\n</TABLE>\n" html)
  (display "</BODY>\n</HTML>\n" html)
  
  (get-output-string html))
