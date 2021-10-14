;; vim: ft=lisp et
(in-package :asdf)
(defsystem :symbol-usage
  :version "0.0.10"
  :depends-on
  (
   "alexandria"         ; Utilities implicitly depends on via eclector.
   "trestrul"           ; Utilities for tree structured list.
   "read-as-string"     ; S-Expression string reader.
   "uiop"               ; utilities.
   "asdf"               ; System builder.
   "quicklisp"          ; Package managing tools.
   "eclector"           ; Package which read symbol as uninterned one.
   "named-readtables"   ; Readtable manager.
   )
  :components((:file "symbol-usage")))
