#lang reader "xml-lang.rkt"

<+>1 2</+>
<+>5 1 10</+>
<+></+>
<define><test></test><+>1 2</+></define><test></test>
<string-append>"foo" "bar"</string-append>
<define><test1 a></test1><+>a 2</+></define><test1 5></test1>

<map>
  test1
  <list>1 2 3</list>
</map>

<!-- test -->

<map>
  <lambda x>
    <+>
      x x
    </+>
  </lambda>
  <list>
    1 2 3
  </list>
</map>

