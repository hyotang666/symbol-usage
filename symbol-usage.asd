;; vim: ft=lisp et
(in-package :asdf)
(defsystem :symbol-usage
  :version "0.0.11"
  :depends-on
  (
   "alexandria"         ; Utilities implicitly depends on via eclector.
   "trestrul"           ; Utilities for tree structured list.
   "read-as-string"     ; S-Expression string reader.
   "asdf"               ; System builder.
   "quicklisp"          ; Package managing tools.
   "eclector"           ; Package which read symbol as uninterned one.
   "named-readtables"   ; Readtable manager.
   )
  :components((:file "symbol-usage")))
