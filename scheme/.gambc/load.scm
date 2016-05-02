(include "set-compiler.scm")
(include "set-config.scm")

(c/load "lib/cj-source")
(c/load "lib/define-macro-star")
(c/load "lib/dummy-module")
(c/load "lib/cj-phasing")
(c/load "lib/srfi-1")
(c/load "lib/cj-env-1")
(c/load "lib/cj-warn")
(c/load "lib/simple-match-1")
(c/load "lib/test")
(c/load "lib/cj-inline-1")
(c/load "lib/simple-match")
(c/load "lib/symbol-append")
(c/load "lib/named")
(c/load "lib/cj-env")
(c/load "lib/cj-symbol")
(c/load "lib/list-util-1")
(c/load "lib/string-util-1")
(c/load "lib/improper-list")
(c/load "lib/list-util")
(c/load "lib/srfi-11")
(c/load "lib/cj-functional")
(c/load "lib/cj-source-2")
(c/load "lib/define-nested")
(c/load "lib/slib-sort")
(c/load "lib/C")
(c/load "lib/char-util")
(c/load "lib/string-util-4")
(c/load "lib/predicates")
(c/load "lib/list-util-2")
(c/load "lib/cj-alist")
(c/load "lib/cj-struct")
(c/load "lib/cj-match")
(c/load "lib/cj-source-util-2")
(c/load "lib/cj-expansion")
(c/load "lib/cut")
(c/load "lib/lazy")
(c/load "lib/define-strict-and-lazy")
(c/load "lib/cj-env-2")
(c/load "lib/vector-util")
(c/load "lib/cj-source-quasiquote")
(c/load "lib/scheme-meta")
(c/load "lib/fallible-1")
(c/load "lib/cj-typed-1")
(c/load "lib/cj-typed")
(c/load "lib/cj-source-wraps")
(c/load "lib/cj-inline")
(c/load "lib/cj-cmp")
(c/load "lib/stream")
(c/load "lib/weak-srfi-1")
(c/load "lib/lazy-debug")
(c/load "lib/test-lib")
(c/load "lib/local-test")
(c/load "lib/cj-functional-2")
(c/load "lib/string-util-2")
(c/load "lib/define-module")
(c/load "lib/string-util")
(c/load "lib/dot-oo")
(c/load "lib/enum")
(c/load "lib/easy-1")
(c/load "lib/cj-math")
(c/load "lib/oo-util")
(c/load "lib/more-oo")
(c/load "lib/easy")
(c/load "lib/cj-gambit-sys")
(i/load "lib/typed-list")

(define (load-require)
  (i/load "lib/Maybe")
  (i/load "lib/alist")
  (i/load "lib/unclean")
  (i/load "lib/string-util-3")
  (i/load "lib/cj-path")
  (i/load "lib/keyword-alist")
  (c/load "lib/cj-gambit-sys")
  (i/load "lib/cj-exception")
  (c/load "lib/cj-test")
  (c/load "lib/cj-string")
  (c/load "lib/cj-string-flatten")
  (c/load "lib/cj-c-util")
  (c/load "lib/posix/cj-c-errno_Cpart")
  (c/load "lib/cj-ffi")
  (c/load "lib/posix/cj-c-errno")
  (c/load "lib/posix/cj-posix")
  (i/load "lib/cj-io-util")
  (i/load "lib/Status")
  (i/load "lib/tree-util")
  (i/load "lib/cj-seen")
  (i/load "lib/tsort")
  (i/load "lib/require")
  (i/load "lib/require-util")
  (c/load "lib/failing"))

(define /load i/load)

(/load "ai")
(/load "to-julia")
