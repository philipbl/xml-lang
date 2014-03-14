#lang s-exp syntax/module-reader
"runtime.rkt"
#:read xml-read
#:read-syntax xml-read-syntax
#:whole-body-readers? #t
(require "parser.rkt")