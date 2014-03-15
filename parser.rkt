#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         rackunit)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))


(define-tokens tokens (id parameter arg string num comment))
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
  [number (:+ (:/ #\0 #\9))])

(define lex
  (lexer-src-pos
   [(:: "<")         (token-open-open-id)]
   [(:: ">")         (token-close-id)]
   [(:: "</")        (token-close-open-id)]
   [(:: "<!--")      (token-open-comment)]
   [(:: "-->")       (token-close-comment)]
   [#\"              (token-quote)]
   [number           (token-num (string->number lexeme))]
   [id               (token-id lexeme)]
   [(:+ whitespace)  (return-without-pos (lex input-port))]
   [(eof)            (token-eoft)]))


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

#;(check-equal? (str->toks "<test>foo<λ>bar</λ></test>") 
              '(open-open-id id close-id id open-open-id id close-id id close-open-id id close-id close-open-id id close-id eoft))

(check-equal? (str->toks "<test>hello ) world</test>") 
              '(open-open-id id close-id id id id close-open-id id close-id eoft))

(check-equal? (str->toks "<test>hello \"test\" world</test>") 
              '(open-open-id id close-id id quote id quote id close-open-id id close-id eoft))

(check-equal? (str->toks "<test>hello \"λλλλλλ hello?\" world</test>") 
              '(open-open-id id close-id id quote id id quote id close-open-id id close-id eoft))

(check-equal? (str->toks "")
              '(eoft))

(check-equal? (str->toks "<!-- this is a test -->")
              '(open-comment id id id id close-comment eoft))

(check-equal? (str->toks "<!-- foo --><bar></bar>")
              '(open-comment id close-comment open-open-id id close-id close-open-id id close-id eoft))

(check-equal? (str->toks "<bar><!-- foo --></bar>")
              '(open-open-id id close-id open-comment id close-comment close-open-id id close-id eoft))

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
 '(open-open-id id close-id open-open-id id id close-id 
                open-open-id id close-id id id close-open-id 
                id close-id close-open-id id close-id
                open-open-id id close-id num num num
                close-open-id id close-id close-open-id id close-id
                eoft))

(check-equal? (str->toks "<+>1 2 \"test\"</+>")
              '(open-open-id id close-id num num quote id quote close-open-id id close-id eoft))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

