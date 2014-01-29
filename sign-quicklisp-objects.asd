;;;; sign-quicklisp-objects.asd

(asdf:defsystem #:sign-quicklisp-objects
  :serial t
  :description "Sign Quicklisp files hosted on Amazon S3."
  :author "Zach Beane <zach@quicklisp.org>"
  :license "BSD"
  :depends-on (#:zs3
               #:alexandria
               #:commando)
  :components ((:file "package")
               (:file "sign-quicklisp-objects")))

