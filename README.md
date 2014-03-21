## xml-lang

xml-lang is a programming language written in XML. The XML gets converted into s-expressions so anything that is supported in Racket _should_[^1] be supported in xml-lang.

### Why?
I first learned about functional programming in a compilers class. I joked with a friend that it would be funny to design a language that instead of using s-expressions, would use XML tags. 

Since then, I have become obsessed with functional programming. After reading through most of Racket's documentation, I decided to create my own simple language to gain practice and experience. It made sense to resurrect the idea of a XML based language and xml-lang was born.

### Example
```
<+>1 2</+>
3
```




[^1]: Due to weaknesses in the lexer and parser, this might not be true.