#lang racket

(provide assemblize)

(require "tokenizer.rkt")

(struct %ingredient (initial-value dry label) #:transparent)

(define (assemble-prelude out)
  (fprintf out "\t%ifdef LINUX\n")
  (fprintf out "\t\t%define ARG0 rdi\n")
  (fprintf out "\t\t%define ARG1 rsi\n")
  (fprintf out "\t\t%define ARG2 rdx\n")
  (fprintf out "\t\t%define ARG3 rcx\n")
  (fprintf out "\t%elifdef WINDOWS\n")
  (fprintf out "\t\t%define ARG0 rcx\n")
  (fprintf out "\t\t%define ARG1 rdx\n")
  (fprintf out "\t\t%define ARG2 r8\n")
  (fprintf out "\t\t%define ARG3 r9\n")
  (fprintf out "\t%else\n")
  (fprintf out "\t\t%fatal \"Must define platform\"\n")
  (fprintf out "\t%endif\n")

  (fprintf out "\textern printf\n")
  (fprintf out "\tsection .text\n") ; ?
  (fprintf out "\tglobal main\n")
  (fprintf out "main:\n"))

(define (assemble-align-stack-and-call things-on-stack foreign-function out)
  (when (even? things-on-stack)
    (fprintf out "\tsub rsp,8\n"))
  (fprintf out "\t%ifdef WINDOWS\n")
  (fprintf out "\t\tsub rsp,32\n")
  (fprintf out "\t%endif\n")
  (fprintf out "\tcall ~a\n" foreign-function)
  (fprintf out "\t%ifdef WINDOWS\n")
  (fprintf out "\t\tadd rsp,32\n")
  (fprintf out "\t%endif\n")
  (when (even? things-on-stack)
    (fprintf out "\tadd rsp,8\n")))

(define (assemble-label label out)
  (fprintf out "~a:\n" label))

(define (assemble-cmd cmd out)
  (fprintf out "\t~a\n" cmd))

(define (assemble-words word-list-node)
  (define word-list ; note: cons will reverse the word-list-node giving desired result
    (let build ([word-list-node word-list-node]
                [word-list '()])
      (match word-list-node
        [(list _ remaining-words @word)
         (build remaining-words
                (cons (string-downcase (%token-lexeme @word)) word-list))]
        [(list _ @word)
         (cons (string-downcase (%token-lexeme @word)) word-list)])))
  (string-join word-list " "))
    
       
        
; every mixing bowl is going to need its own stack
; I'll go ahead and assume the stack size will be 256 bytes

; NEEDS:
;  Put into mixing bowl
;  Add
;  Combine
;  Pour into baking dish
;  Serves
;  Ingredients

(define (assemblize parse-tree)
  (define out (open-output-string))
  (define !variable-table (make-hash)) ; name -> (initial value : num, label : str)
  (define !string-table (make-hash))
  (define !id 0)
  (define (label)
    (set! !id (add1 !id))
    (format "v~a" !id))

  (define mixing-bowl-label (label)) ; stacks size 1024
  (define baking-dish-label (label))
  (define mixing-bowl-items (label))   ; label for num items on stack
  (define baking-dish-items (label))
  
  (assemble-prelude out)

  ; @id indicates a token with name id
  (let compile ([node parse-tree])
    (unless (list? node)
      (error 'compile "Cannot compile a token ~a" node))
    (match node
      [(list "S" recipe-list) (compile recipe-list)]
      [(list "recipeList" recipe recipe-list) (compile recipe) (compile recipe-list)]
      [(list "recipeList" recipe) (compile recipe)]

      [(list "recipe" _ _ _ _ ingredients _ _ method serves)
       (compile ingredients)
       (compile method)
       (compile serves)]

      [(list "optionalIngredients") '()]
      [(list "optionalIngredients" _ _ ingredient-list) (compile ingredient-list)]
      
      [(list "ingredientList") '()]
      [(list "ingredientList" ingredient ingredient-list)
       (compile ingredient)
       (compile ingredient-list)]

      [(list "ingredient" initial-value _ word-list _) ; don't need type for the lab
       (define words (assemble-words word-list))
       (when (hash-has-key? !variable-table words)
         (error 'assembler "Already declared ingredient: ~a" words))
       
       (define value
         (if (equal? '() (cdr initial-value))
             0
             (string->number (%token-lexeme (cadr initial-value)))))
       (define ingredient (%ingredient value #f (label)))
       (hash-set! !variable-table words ingredient)]

      [(list "method" _ _ stmt-list) (compile stmt-list)]
      [(list "stmt-list" stmt _ stmt-list) (compile stmt) (compile stmt-list)]
      [(list "stmt-list" stmt _) (compile stmt)]

      ; only need put, add, combine, pour. no chance of SET_ASIDE
      [(list "stmt" specific-stmt) (compile specific-stmt)]

      [(list "putStmt" @put/fold  word-list @into-mixing-bowl) ; assuming
       (define words (assemble-words word-list))
       (define ingredient
         (hash-ref !variable-table words
                   '(error 'compile
                          "Undecalred ingredient: ~a ~a" words !variable-table)))
       (assemble-cmd (format "mov rax, [~a]; ing" (%ingredient-label ingredient)) out)
       (assemble-cmd (format "mov rbx, ~a" mixing-bowl-label) out)
       (assemble-cmd (format "add rbx, [~a]" mixing-bowl-items) out)
       (assemble-cmd "mov [rbx], rax; push" out) ; move ingredient value onto stack
       (assemble-cmd (format "add qword [~a], 8" mixing-bowl-items) out)] ; 8bytes

      [(list "mathStmt" specific-stmt) (compile specific-stmt)]
      [(list "addStmt" _ word-list)
       (define words (assemble-words word-list))
       (define ingredient
         (hash-ref !variable-table words
                   '(error 'compile
                          "Undecalred ingredient: ~a ~a" words !variable-table)))
       (assemble-cmd (format "mov rcx, ~a" mixing-bowl-label) out)
       (assemble-cmd (format "add rcx, [~a]" mixing-bowl-items) out)
       (assemble-cmd "sub rcx, 8" out)
       (assemble-cmd "mov rax, [rcx] ; pop" out)
       (assemble-cmd (format "mov rbx, [~a]; ing" (%ingredient-label ingredient)) out)
       (assemble-cmd "add rax, rbx" out)
       (assemble-cmd "mov [rcx], rax; push" out)]
       

      [(list "mulStmt" _ word-list) 
       (define words (assemble-words word-list))
       (define ingredient
         (hash-ref !variable-table words
                   '(error 'compile
                          "Undecalred ingredient: ~a ~a" words !variable-table)))
       (assemble-cmd (format "mov rcx, ~a" mixing-bowl-label) out)
       (assemble-cmd (format "add rcx, [~a]" mixing-bowl-items) out)
       (assemble-cmd "sub rcx, 8" out)
       (assemble-cmd "mov rax, [rcx] ; pop" out)
       (assemble-cmd (format "mov rbx, [~a]; ing" (%ingredient-label ingredient)) out)
       (assemble-cmd "mul rbx ; not using rdx" out)
       (assemble-cmd "mov [rcx], rax; push" out)]

      [(list "pourStmt" _) ; don't care about ordinals for the lab
       (define jump-back (label))
       (define exit (label))
       (assemble-cmd (format "mov rax, [~a]" mixing-bowl-items) out)
       (assemble-cmd (format "mov rcx, ~a" mixing-bowl-label) out)
       (assemble-cmd (format "mov rdx, ~a" baking-dish-label) out)
       (assemble-cmd (format "mov r8, [~a]" baking-dish-items) out)
       (assemble-cmd "add rdx, r8 ; top of baking dish stack" out)
       (assemble-cmd "mov rbx, 0" out)
       (assemble-label jump-back out)
       (assemble-cmd "cmp rbx, rax" out)
       (assemble-cmd (format "jge ~a" exit) out)
       (assemble-cmd "mov r9, [rcx]" out)
       (assemble-cmd "mov [rdx], r9" out)
       (assemble-cmd "add rdx, 8" out)
       (assemble-cmd "add rcx, 8" out)
       (assemble-cmd "add rbx, 8 ; cmp check" out)
       (assemble-cmd (format "jmp ~a" jump-back) out)
       (assemble-label exit out)

       (assemble-cmd "add r8, rax ; update baking-dish-items" out)
       (assemble-cmd (format "mov [~a], r8" baking-dish-items) out)]

      [(list "serves" _ @num _)
       (define jump-back (label))
       (define exit (label))
       (assemble-cmd (format "mov rax, [~a]" baking-dish-items) out)
       (assemble-cmd (format "mov rbx, ~a" baking-dish-label) out)
       (assemble-cmd "mov rcx, 0" out)
       (assemble-label jump-back out)
       (assemble-cmd "cmp rcx, rax" out)
       (assemble-cmd (format "jge ~a" exit) out)
       (assemble-cmd "push rax" out)
       (assemble-cmd "push rcx" out)
       (assemble-cmd "mov ARG0, percent_d" out)
       (assemble-cmd "mov ARG1, [rbx] ; baking-dish-item" out)
       (assemble-align-stack-and-call 2 "printf" out)
       (assemble-cmd "pop rcx" out)
       (assemble-cmd "pop rax" out)
       (assemble-cmd "add rbx, 8" out)
       (assemble-cmd "add rcx, 8" out)
       (assemble-cmd (format "jmp ~a" jump-back) out)
       (assemble-label exit out)]))
       
  (assemble-cmd "mov rax,0" out)
  (assemble-cmd "ret" out)
  (assemble-cmd "section .data" out)

  (for ([(name ingredient) !variable-table])
    (assemble-label (%ingredient-label ingredient) out)
    (assemble-cmd (format "dq ~a" (%ingredient-initial-value ingredient)) out))

  (assemble-label mixing-bowl-items out)
  (assemble-cmd "dq 0" out) ; dq is 8 bytes
  (assemble-label mixing-bowl-label out)
  (assemble-cmd "times 1024 dq 0" out) 

  (assemble-label baking-dish-items out)
  (assemble-cmd "dq 0" out)
  (assemble-label baking-dish-label out)
  (assemble-cmd "times 1024 dq 0" out)

  (assemble-label "percent_d" out)
  (assemble-cmd "db \"%d\",10,0" out)
  
  (get-output-string out))
       
    