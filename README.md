## xml-lang

xml-lang is a programming language written in XML. The XML gets converted into s-expressions so anything that is supported in Racket _should_ be supported in xml-lang (due to weaknesses in the lexer and parser, this might not be true).


### Why?
I first learned about functional programming in a compilers class. A friend and I joked that it would be funny to design a language that instead of using s-expressions, would use XML tags. 

Since then, I have become more interested in functional programming. After reading through most of Racket's documentation, I decided to create my own simple language to gain practice and experience. It made sense to resurrect the idea of a XML based language and xml-lang was born.


### Examples
```
<+>1 2</+>
> 3
```

```
<><lambda><x></x><+>x x</+></lambda> 4</>
> 8
```

```
<string-append>"foo" "bar"</string-append>
> "foobar"
```

To see more examples, check out `examples.xml`.


### How to Run
Currently, all of the xml-lang source files need to be in the same directory as any xml-lang file you are running.

xml-lang is run best in DrRacket because it provides syntax checking, but anything can be used.

To create a xml-lang file add `#lang reader "xml-lang.rkt"` to the top of the file. This tells Racket what parser to use to interpret the code. To run the file from the command line, use `racket source-file`.


### Caveats
xml-lang in theory should support all of Racket, but it doesn't. Some Racket syntax trips up my lexer and parser (i.e. `#'()`). I _might_ fix this if I have time.
