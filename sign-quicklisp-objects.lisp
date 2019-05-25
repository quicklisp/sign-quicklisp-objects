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

(defun s3-components (url)
  (let ((uri (puri:parse-uri url)))
    (let ((host (puri:uri-host uri))
          (path (puri:uri-path uri)))
      (values host
              (string-left-trim "/" path)))))

(defun sign-url (url)
  (multiple-value-bind (bucket key)
      (s3-components url)
    (sign-object bucket key)))

(defun publish-to-url (file url &key (content-type "text/plain"))
  (multiple-value-bind (bucket key)
      (s3-components url)
    (put-file file bucket key
              :content-type content-type
              :public t)))

(defun bucket-distribution (bucket)
  (declare (ignore bucket))
  ;;; FIXME
  (first (zs3:all-distributions)))

(defun invalidate-signature-file (bucket key)
  (let ((distribution (bucket-distribution bucket)))
    (zs3:invalidate-paths distribution (list key))))

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
      (invalidate-signature-file bucket (format nil "/~A" signed-key))
      (format t "; Signed ~A ~A as ~A~%"
              bucket key signed-key))))



(defun =suffix (suffix)
  (lambda (name)
    (alexandria:ends-with-subseq suffix name)))

(defun sign-keys (bucket keys &key (matching 'identity))
  "KEYS here is a vector of key objects as returned by ZS3:ALL-KEYS."
  (flet ((already-signed (name)
           (search (concatenate 'string name ".asc") keys
                   :key 'name)))
    (map nil
         (lambda (key)
           (let ((name (name key)))
             (when (funcall matching name)
               (unless (or (already-signed name)
                           (ends-with-subseq ".asc" name))
                 (sign-object bucket name)))))
         keys)))

(defun test-signing (file)
  (sign file))
