#lang reader "xml-lang.rkt"

<!-- (+ 5 1 10) -->
<+>5 1 10</+>

<!-- (+) -->
<+></+>

<!-- ((lambda (x) (+ x x)) 4) -->
<><lambda><x></x><+>x x</+></lambda> 4</>

<!-- (string-append "foo" "bar") -->
<string-append>"foo" "bar"</string-append>

<!-- (define (test) (print "hello world"))(test) -->
<define><test></test><print>"hello world"</print></define><test></test>

<!-- (define (test1 a) (+ a 2))(test1) -->
<define><test1>a</test1><+>a 2</+></define><test1>5</test1>

<!-- (map test1 (list 1 2 3)) -->
<map>
  test1
  <list>1 2 3</list>
</map>

<!-- (map (lambda (x) (- 2 x))
          (list 1 2 3)) -->
<map>
  <lambda>
    <x></x>
    <->2 x</->
  </lambda>
  <list>
    1 2 3
  </list>
</map>

<!--
;; Finds files in all subdirs
(for ([path (in-directory)])
  (printf "source file: ~a\n" path))
-->
<for>
  <><path> <in-directory></in-directory> </path></>
  <printf>"file: ~a\n" path</printf>
</for>

<!--
; An echo server
(define listener (tcp-listen 12345))
(let echo-server ()
  (define-values (in out) (tcp-accept listener))
  (thread (lambda () (copy-port in out)
                     (close-output-port out)))
  (echo-server))
-->
<define>
  listener
  <tcp-listen>12345</tcp-listen>
</define>
<let>
  echo-server <></>
  <define-values>
    <in>out</in>
    <tcp-accept>listener</tcp-accept>
  </define-values>
  <thread>
    <lambda>
      <></>
      <copy-port>in out</copy-port>
      <close-output-port>out</close-output-port>
    </lambda>
  </thread>
  <echo-server></echo-server>
</let>