#lang racket
(provide xml-read xml-read-syntax)
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         rackunit)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (remove-open token)
  (substring token 1 (- (string-length token) 1)))

(define (remove-close token)
  (substring token 2 (- (string-length token) 1)))

(define-tokens tokens (open-id close-id arg string num comment))
(define-empty-tokens empty-tokens (eoft))

(define-lex-abbrevs
  (close (:: "</" (complement (:: any-string (:or ">" whitespace) any-string)) ">"))
  (open (:: "<" (:~ "/") (complement (:: any-string (:or ">") any-string)) ">"))
  (comment (:: "<!--" (complement (:: any-string (:or "-->") any-string)) "-->"))
  (args (:+ (:~ "<" ">" whitespace)))
  (string (:: #\" (complement (:: any-string #\" any-string)) #\"))
  (number (:+ (:/ #\0 #\9))))

(define lex
  (lexer-src-pos
   [comment          (token-comment lexeme)]
   [close            (token-close-id (string->symbol (remove-close lexeme)))]
   [open             (token-open-id (string->symbol (remove-open lexeme)))]
   [string           (token-string lexeme)]
   [number           (token-num (string->number lexeme))]
   [args             (token-arg (string->symbol lexeme))]
   [(:+ whitespace)  (return-without-pos (lex input-port))]
   [(eof)            (token-eoft)]))


; Used for testing
(define (str->toks str)
  (let ([p (open-input-string str)])
    (let loop ()
      (let ([next (lex p)])
        (cons (token-name (position-token-token next))
              (if (eq? 'eoft (token-name (position-token-token next)))
                  '()
                  (loop)))))))

(check-equal? (str->toks "<test>") 
              '(open-id eoft))

(check-equal? (str->toks "</hello>") 
              '(close-id eoft))

(check-equal? (str->toks "<cra-zy-!>") 
              '(open-id eoft))

(check-equal? (str->toks "<test></test>") 
              '(open-id close-id eoft))

(check-equal? (str->toks "<lambda x></lambda>") 
              '(open-id close-id eoft))

(check-exn exn:fail?
           (λ () (str->toks "<lambda x></lamb da>")))

(check-exn exn:fail?
           (λ () (str->toks "<lambda x></lamb\tda>")))

(check-exn exn:fail?
           (λ () (str->toks "<lambda x></lamb\n\n\rda>")))

(check-equal? (str->toks "<λ></λ>") 
              '(open-id close-id eoft))

(check-equal? (str->toks "<test>hello world</test>") 
              '(open-id arg arg close-id eoft))

(check-equal? (str->toks "<test>hello? world</test>") 
              '(open-id arg arg close-id eoft))

(check-equal? (str->toks "<test>hello-world</test>") 
              '(open-id arg close-id eoft))

(check-equal? (str->toks "<test>foo<λ>bar</λ></test>") 
              '(open-id arg open-id arg close-id close-id eoft))

(check-equal? (str->toks "<test>hello ) world</test>") 
              '(open-id arg arg arg close-id eoft))

(check-equal? (str->toks "<test>hello \"<test>\" world</test>") 
              '(open-id arg string arg close-id eoft))

(check-equal? (str->toks "<test>hello \"λλλλλλ hello?\" world</test>") 
              '(open-id arg string arg close-id eoft))

(check-equal? (str->toks "")
              '(eoft))

(check-equal? (str->toks "<!-- this is a test -->")
              '(comment eoft))

(check-equal? (str->toks "<!-- foo --><bar></bar>")
              '(comment open-id close-id eoft))

(check-equal? (str->toks "<bar><!-- foo --></bar>")
              '(open-id comment close-id eoft))

; (map (lambda (x) (+ x x)) '(1 2 3))
(check-equal? 
 (str->toks 
  "<map>
\t<lambda x>
\t\t<+>
\t\t\tx x
\t\t</+>
\t</lambda>
\t<list>
\t\t1 2 3
\t</list>
</map>") 
 '(open-id open-id open-id arg arg close-id close-id open-id num num num close-id close-id eoft))

(check-equal? (str->toks "<+>1 2 \"test\"</+>")
              '(open-id num num string close-id eoft))


(define (same-tags? open close)
  (equal? (car (string-split (symbol->string open)))
          (symbol->string close)))

(define (split-param open)
  (let ([params? (string-split (symbol->string open))])
    (match params?
      [`(,id)                `(,(string->symbol id))]
      [`(,id ,params ...)    `(,(string->symbol id) ,(map string->symbol params))]
      [_                     'error])))

(define (match-error a b)
  (error 
   'parse-error 
   (format "open tag (~a) and close tag (~a) don't match" a b)))

(define (convert-comment c)
  (string-append ";" 
                 (substring c 4 (- (string-length c) 3))))

(define (convert-string s)
  (substring s 1 (- (string-length s) 1)))

(define current-source (make-parameter #f))

(define parse
  (parser
   #;[grammar 
      (start [(exprs) $1])
      (exprs [(comment) (convert-comment $1)]
             [(expr) $1]
             [(expr exprs) `(,$1 ,$2)])
      (expr  [(open-id close-id) (if (same-tags? $1 $2)
                                     `(,@(split-param $1))
                                     (match-error $1 $2))]
             
             [(open-id args close-id) (if (same-tags? $1 $3)
                                          `(,@(split-param $1) ,@(flatten $2))
                                          (match-error $1 $3))]
             
             [(open-id exprs close-id) (if (same-tags? $1 $3)
                                           `(,@(split-param $1) ,$2)
                                           (match-error $1 $3))])
      
      (args [(arg)         $1]
            [(arg args)    `(,$1 ,$2)]
            [(num)         $1]
            [(num args)    `(,$1 ,$2)]
            [(string)      $1]
            [(string args) `(,$1 ,$2)])]
   
   
   
   [grammar 
    (start [(exprs) $1])
    (exprs [(expr) (list (add-srcloc $1 $1-start-pos $1-end-pos))]
           [(expr exprs) (cons (add-srcloc $1 $1-start-pos $1-end-pos) $2)])
    
    (expr  [(open-id close-id) `(,@(split-param $1))]  
           [(open-id args close-id) `(,@(split-param $1) ,@$2)]
           [(open-id exprs close-id) (cons $1 $2)])
    
    (args [(arg)         `(,$1)]
          [(arg args)    (cons $1 $2)]
          [(num)         `(,$1)]
          [(num args)    (cons $1 $2)]
          [(string)      `(,(convert-string $1))]
          [(string args) (cons (convert-string $1) $2)])]
   
   [tokens empty-tokens tokens]
   [start start]
   [end eoft]
   [src-pos]
   ;[debug "debug.txt"]
   [error 
    (lambda (tok-ok? tok-name tok-value start-pos end-pos)
      (raise-syntax-error 
       'parse-error
       (format "~a ~a" tok-name tok-value)))]))


(define (add-srcloc stuff start-pos end-pos)
  (datum->syntax #f stuff 
                 (vector
                  (current-source)
                  (position-line start-pos)
                  (position-col start-pos)
                  (position-offset start-pos)
                  (- (position-offset end-pos)
                     (position-offset start-pos)))))



(define (run-parser str)
  (let ([p (open-input-string str)])
    (parse (λ () (lex p)))))

#;(run-parser "")

(run-parser "<+>1</+>")
(run-parser "<+>5 1</+>")
(run-parser "<+>5 1 10</+>")
(run-parser "<+></+>")
(run-parser "<test1>a b c d e f g</test1>")
(run-parser "<define><test></test><+>1 2</+></define>")
(run-parser "<λ x>x</λ>")
(run-parser "<+>1 2</+><+>2 3</+><->432 32</->")
(run-parser "<string-append>\"foo\" \"bar\"</string-append>")

#;(run-parser "<!-- test -->")

#;(check-equal? (run-parser "<test></test>")
                '(test))
#;(check-equal? (run-parser "<test1>a b c d e f g</test1>")
                '(test1 a b c d e f g))
#;(check-equal? (run-parser "<test1><test2></test2></test1>")
                '(test1 (test2)))
#;(check-equal? (run-parser "<λ x>x</λ>")
                '(λ (x) x))

#;(run-parser "<define>
    <test>
        <+>1 2</+>
    </test>
</define>")

#;(run-parser   "<map>
\t<lambda x>
\t\t<+>
\t\t\tx x
\t\t</+>
\t</lambda>
\t<list>
\t\t1 2 3
\t</list>
</map>")


#;(define (my-eval code)
    (match code
      ['()   '()]
      [(list line)       (eval line ns)]
      [(list line ...)   (displayln (eval (car line) ns))
                         (my-eval (cdr line))]))

;(my-eval (run-parser "<+>1 2</+><+>2 3</+><->432 32</->"))

(define (run-p src p)
  (parameterize ([current-source src])
    (parse (λ () (lex p)))))

(define (xml-read [port (current-input-port)])
  (syntax->datum (run-p #f port)))

(define (xml-read-syntax [name #f] [port (current-input-port)])
  (run-p (or name (object-name port))
         port))