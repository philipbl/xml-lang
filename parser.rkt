#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         rackunit)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))


(define-tokens tokens (id parameter arg num string comment))
(define-empty-tokens empty-tokens (open-open-id 
                                   close-id 
                                   close-open-id  
                                   open-comment 
                                   close-comment
                                   quote
                                   eoft))

(define-lex-abbrevs
  [id   (:: ;(:& (:~ "<") (:~ ">") (:~ #\"))
         ;(:& (:~ "<") (:~ ">"))
         ;(:~ "/") 
         (complement (:: any-string (:or ">" "<" "</" "\"" whitespace) any-string)))]
  [number (:+ (:/ #\0 #\9))]
  [string (:: #\" (complement (:: any-string (:or #\") any-string)) #\")])

(define lex
  (lexer-src-pos
   [(:: "<")         (token-open-open-id)]
   [(:: ">")         (token-close-id)]
   [(:: "</")        (token-close-open-id)]
   [(:: "<!--")      (token-open-comment)]
   [(:: "-->")       (token-close-comment)]
   [string           (token-string lexeme)]
   [number           (token-num (string->number lexeme))]
   [id               (token-id lexeme)]
   [(:+ whitespace)  (return-without-pos (lex input-port))]
   [(eof)            (token-eoft)]))

(define string-lex
  (lexer
   [(:: any-string #\")    lexeme]))


; Used for testing
(define (str->toks str)
  (let ([p (open-input-string str)])
    (let loop ()
      (let ([next (lex p)])
        (cons (token-name (position-token-token next))
              #;(position-token-token next)
              (if (eq? 'eoft (token-name (position-token-token next)))
                  '()
                  (loop)))))))

(check-equal? (str->toks "<test>") 
              '(open-open-id id close-id eoft))

(check-equal? (str->toks "</hello>") 
              '(close-open-id id close-id eoft))

(check-equal? (str->toks "<cra-zy-!>") 
              '(open-open-id id close-id eoft))

(check-equal? (str->toks "<test></test>") 
              '(open-open-id id close-id close-open-id id close-id eoft))

(check-equal? (str->toks "<lambda x></lambda>") 
              '(open-open-id id id close-id close-open-id id close-id eoft))

(check-equal? (str->toks "<lambda x>1</lambda>") 
              '(open-open-id id id close-id num close-open-id id close-id eoft))

(check-equal? (str->toks "<test>hello world</test>") 
              '(open-open-id id close-id id id close-open-id id close-id eoft))

(check-equal? (str->toks "<test>hello? world</test>") 
              '(open-open-id id close-id id id close-open-id id close-id eoft))

(check-equal? (str->toks "<test>hello-world</test>") 
              '(open-open-id id close-id id close-open-id id close-id eoft))

(check-equal? (str->toks "<test>foo<λ>bar</λ></test>") 
                '(open-open-id id close-id id open-open-id id close-id id close-open-id id close-id close-open-id id close-id eoft))

(check-equal? (str->toks "<test>hello ) world</test>") 
              '(open-open-id id close-id id id id close-open-id id close-id eoft))

(check-equal? (str->toks "<test>hello \"test\" world</test>") 
              '(open-open-id id close-id id string id close-open-id id close-id eoft))

(check-equal? (str->toks "<test>\"hello\" \"world\"</test>") 
              '(open-open-id id close-id string string close-open-id id close-id eoft))

(check-equal? (str->toks "<test>hello \"λλλλλλ hello?\" world</test>") 
              '(open-open-id id close-id id string id close-open-id id close-id eoft))

(check-equal? (str->toks "")
              '(eoft))

(check-equal? (str->toks "<!-- this is a test -->")
              '(open-comment id id id id close-comment eoft))

(check-equal? (str->toks "<!-- foo --><bar></bar>")
              '(open-comment id close-comment open-open-id id close-id close-open-id id close-id eoft))

(check-equal? (str->toks "<bar><!-- foo --></bar>")
              '(open-open-id id close-id open-comment id close-comment close-open-id id close-id eoft))

; (map (lambda (x) (+ x x)) '(1 2 3))
#;(check-equal? 
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
 '(open-open-id id close-id open-open-id id id close-id 
                open-open-id id close-id id id close-open-id 
                id close-id close-open-id id close-id
                open-open-id id close-id num num num
                close-open-id id close-id close-open-id id close-id
                eoft))

#;(check-equal? (str->toks "<+>1 2 \"test\"</+>")
              '(open-open-id id close-id num num string close-open-id id close-id eoft))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-source (make-parameter #f))

(define (string-> s)
  (substring s 1 (- (string-length s) 1)))

(define parse
  (parser   
   [grammar 
    (start [(exprs) $1])
    (exprs [(expr) (list (add-srcloc $1 $1-start-pos $1-end-pos))]
           [(expr exprs) (cons (add-srcloc $1 $1-start-pos $1-end-pos) $2)])
    
    (expr [(open-tag close-tag)        `(,$1)]
          [(open-tag params close-tag) `(,$1 ,@$2)]
          [(open-tag exprs close-tag)  (cons $1 $2)])
    
    (open-tag [(open-open-id id close-id) (string->symbol $2)])
    (close-tag [(close-open-id id close-id) (string->symbol $2)])
    
    (params [(id)           `(,(string->symbol $1))]
            [(id params)     (cons (string->symbol $1) $2)]
            [(num)          `(,$1)]
            [(num params)    (cons $1 $2)]
            [(string)       `(,(string-> $1))]
            [(string params) (cons (string-> $1) $2)])]
   
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


(run-parser "<+>1 2</+>")
(run-parser "<+>5 1 10</+>")
(run-parser "<+></+>")
(run-parser "<test1>a b c d e f g</test1>")
(run-parser "<define><test></test><+>1 2</+></define>")
(run-parser "<string-append>\"foo\" \"bar\"</string-append>")
;(run-parser "<string-append>\"hello world\" \"foobar\"</string-append>")