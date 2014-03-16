#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         rackunit)

(provide xml-read xml-read-syntax)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))


(define-tokens tokens (id parameter arg num string))
(define-empty-tokens empty-tokens (open-open-id 
                                   close-id 
                                   close-open-id  
                                   quote
                                   eoft))

(define-lex-abbrevs
  [id   (:: ;(:& (:~ "<") (:~ ">") (:~ #\"))
         ;(:& (:~ "<") (:~ ">"))
         ;(:~ "/") 
         (complement (:: any-string (:or ">" "<" "</" "\"" whitespace) any-string)))]
  [number (:+ (:/ #\0 #\9))]
  [string (:: #\" (complement (:: any-string (:or #\") any-string)) #\")]
  [comment (:: "<!--" (complement (:: any-string (:or "-->") any-string)) "-->")])

(define lex
  (lexer-src-pos
   [comment          (return-without-pos (lex input-port))]
   [(:: "<")         (token-open-open-id)]
   [(:: ">")         (token-close-id)]
   [(:: "</")        (token-close-open-id)]
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
              '(eoft))

(check-equal? (str->toks "<!-- foo --><bar></bar>")
              '(open-open-id id close-id close-open-id id close-id eoft))

(check-equal? (str->toks "<bar><!-- foo --></bar>")
              '(open-open-id id close-id close-open-id id close-id eoft))

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
    
    (expr [(open-open-id id close-id close-tag)               `(,(string->symbol $2))]
          [(open-open-id id params close-id close-tag)        `(,(string->symbol $2) ,@$3)]
          [(open-open-id id close-id params close-tag)        `(,(string->symbol $2) ,@$4)]
          [(open-open-id id params close-id params close-tag) `(,(string->symbol $2) ,$3 ,@$5)])
    ; be careful of params here, shouldn't be able to have an open tag
    
    (close-tag [(close-open-id id close-id) (string->symbol $2)])
    
    (params [(id)           `(,(string->symbol $1))]
            [(id params)     (cons (string->symbol $1) $2)]
            [(num)          `(,$1)]
            [(num params)    (cons $1 $2)]
            [(string)       `(,(string-> $1))]
            [(string params) (cons (string-> $1) $2)]
            [(expr)         `(,$1)]
            [(expr params)   (cons $1 $2)])]
   
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


;(run-parser "<+>1 2</+>")
;(run-parser "<+>5 1 10</+>")
;(run-parser "<+></+>")
;(run-parser "<test1>a b c d e f g</test1>")
;(run-parser "<define><test></test><+>1 2</+></define>")
;(run-parser "<string-append>\"foo\" \"bar\"</string-append>")
;(run-parser "<test1 a></test1>")
;(run-parser "<define><test1 a></test1><+>a 2</+></define>")
;(run-parser "<string-append>\"hello world\" \"foobar\"</string-append>")
;(run-parser "<!-- test -->")
#;(run-parser "<map>
  test1
  <list>1 2 3</list>
</map>")
#;(run-parser "<map>
  <lambda x>
    <+>
      x x
    </+>
  </lambda>
  <list>
    1 2 3
  </list>
</map>")

(define (run-p src p)
  (parameterize ([current-source src])
    (parse (λ () (lex p)))))

(define (xml-read [port (current-input-port)])
  (syntax->datum (run-p #f port)))

(define (xml-read-syntax [name #f] [port (current-input-port)])
  (run-p (or name (object-name port))
         port))