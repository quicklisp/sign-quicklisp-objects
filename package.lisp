;;;; package.lisp

(defpackage #:sign-quicklisp-objects
  (:use #:cl)
  (:shadowing-import-from #:commando
                          #:run
                          #:in-temporary-directory)
  (:shadowing-import-from #:zs3
                          #:all-keys
                          #:get-file
                          #:put-file
                          #:name)
  (:shadowing-import-from #:alexandria
                          #:ends-with-subseq)
  (:export #:sign
           #:*signing-user-id*
           #:sign-url
           #:sign-object))
