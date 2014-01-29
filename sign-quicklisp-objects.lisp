;;;; sign-quicklisp-objects.lisp

(in-package #:sign-quicklisp-objects)

;;; "sign-quicklisp-objects" goes here. Hacks and glory await!

(defparameter *signing-user-id*
  "release@quicklisp.org")

(defun sign (file &key (as *signing-user-id*) output)
  (unless output
    (setf output (parse-namestring (concatenate 'string (namestring file)
                                                ".asc"))))
  (restart-case
      (when (probe-file output)
        (error "Output file ~S already exists" output))
    (delete (&optional v)
      :report "Delete output file and proceed"
      (declare (ignore v))
      (delete-file output)))
  (run "gpg"
       (when as
         (list :local-user as))
       :output output
       :no-tty :sign :detach "-a" (truename file)))

(defun sign-object (bucket key)
  (let ((signed-key (concatenate 'string key ".asc"))
        (object-file "sign-object.dat")
        (signature-file "signature.asc"))
    (in-temporary-directory
      (get-file bucket key object-file)
      (sign object-file :output signature-file)
      (put-file signature-file bucket signed-key
                :public t
                :content-type "text/plain")
      (format t "; Signed ~A ~A as ~A~%"
              bucket key signed-key))))

(defun sign-keys (bucket keys)
  "KEYS here is a vector of key objects as returned by ZS3:ALL-KEYS."
  (map nil
       (lambda (key)
         (let ((name (name key)))
           (unless (ends-with-subseq ".asc" name)
             (sign-object bucket name))))
       keys))

(defun test-signing (file)
  (sign file))
