;; vim: ft=lisp et
(in-package :asdf)
(defsystem :symbol-usage
  :version "0.0.10"
  :depends-on
  (
   "alexandria"         ; public domain utilities.
   "read-as-string"     ; S-Expression string reader.
   "trestrul"           ; utilities for tree structured list.
   "asdf"               ; system definition facilities.
   "quicklisp"          ; package managing tools.
   "eclector"           ; package which read symbol as uninterned one.
   "uiop"               ; utilities.
   "named-readtables"   ; manage readtabls.
   )
  :components((:file "symbol-usage")))
