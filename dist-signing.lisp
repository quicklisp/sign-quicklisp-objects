;;;; dist-signing.lisp

(in-package #:sign-quicklisp-objects)

(defun delete-file-if-exists (pathname)
  (when (probe-file pathname)
    (delete-file pathname)))

(defun temporary-directory ()
  (uiop:temporary-directory))

(defvar *random-pathname-alphabet*
  (concatenate 'string
               "abcdefghijklmnopqrstuvwxyz"
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               "0123456789"))

(defun random-pathname-string (&optional (length 8))
  (let ((string (make-string length)))
    (map-into string (lambda () (char *random-pathname-alphabet*
                                      (random (length *random-pathname-alphabet*)))))))

(defun temp-output-file (template-pathname)
  (let* ((temp-string (random-pathname-string))
         (temp-name (format nil "~A-~A"
                            (pathname-name template-pathname)
                            temp-string)))
    (merge-pathnames (make-pathname :name temp-name
                                    :type (pathname-type template-pathname))
                     (temporary-directory))))

(defun call-with-temp-output-file (template-pathname fun)
  (let ((file (temp-output-file template-pathname)))
    (ensure-directories-exist file)
    (unwind-protect
         (funcall fun file)
      (delete-file-if-exists file))))

(defmacro with-temp-output-file ((var template-pathname) &body body)
  `(call-with-temp-output-file ,template-pathname (lambda (,var) ,@body)))

(defmacro with-temp-output-files (bindings &body body)
  (labels ((expand (bindings body)
             (let ((binding (first bindings)))
               (if (rest bindings)
                   `(with-temp-output-file (,(first binding) ,(second binding))
                      ,(expand (rest bindings) body))
                   `(with-temp-output-file (,(first binding) ,(second binding))
                      ,@body)))))
    (expand bindings body)))


(defun raw-distinfo (url)
  (values (drakma:http-request url)))

(defun parse-distinfo (raw-distinfo)
  "Turn raw distinfo data into a table."
  (let ((table (make-hash-table :test 'equalp)))
    (with-input-from-string (stream raw-distinfo)
      (loop for line = (read-line stream nil)
            while line do
              (ppcre:register-groups-bind (key value)
                  ("^(.*?): *(.*)$" line)
                (setf (gethash key table) value))))
    table))

(defun distinfo-table (url)
  (parse-distinfo (raw-distinfo url)))

(defun file-sha256-string (file)
  (let ((digest (ironclad:digest-file :sha256 file)))
    (string-downcase (ironclad:byte-array-to-hex-string digest))))

(defvar *sha256-cache* (make-hash-table :test 'equal))

(defun url-sha256 (url)
  (or (gethash url *sha256-cache*)
      (setf (gethash url *sha256-cache*)
            (with-temp-output-file (file "file.dat")
              (loop
                (with-simple-restart (try-again "Try again")
                  (when (ql-http:fetch url file)
                    (return (file-sha256-string file)))))))))

(defun raw-releases (url)
  (values (drakma:http-request url)))

(defun skip-header-comment (stream)
  (read-line stream))

(defun parse-releases (raw-releases)
  (let ((table (make-hash-table :test 'equalp)))
    (with-input-from-string (stream raw-releases)
      (skip-header-comment stream)
      (loop for line = (read-line stream nil)
            while line
            do
               (let ((parts (ppcre:split "\\s+" line)))
                 (let ((key (first parts))
                       (url (second parts)))
                   (setf (gethash key table) url)))))
    table))

(defun releases-table (distinfo)
  (let ((url (gethash "release-index-url" distinfo)))
    (parse-releases (raw-releases url))))

(defun release-digests (releases-table)
  (let ((digests (make-hash-table :test 'equalp)))
    (maphash (lambda (release-name url)
               (setf (gethash release-name digests)
                     (url-sha256 url)))
             releases-table)
    digests))



(defun write-dist-digests (digests output-file)
  (with-open-file (stream output-file
                          :direction :output
                          :if-exists :supersede)
    (format stream "# name sha256~%")
    (let ((keys (sort (alexandria:hash-table-keys digests)
                      #'string<)))
      (dolist (key keys)
        (format stream "release/~A ~A~%" key (gethash key digests)))))
  (probe-file output-file))


(defclass signed-dist ()
  ((url
    :initarg :url
    :reader url)
   (releases
    :initarg :releases
    :accessor releases)
   (distinfo
    :initarg :distinfo
    :accessor distinfo)
   (digests
    :initarg :digests
    :accessor digests))
  (:documentation
   "A dist for signing."))


(defun reset (dist)
  (slot-makunbound dist 'distinfo)
  (slot-makunbound dist 'releases)
  (slot-makunbound dist 'digests))

(defmethod slot-unbound ((class t) (dist signed-dist) (slot (eql 'distinfo)))
  (setf (distinfo dist)
        (distinfo-table (url dist))))

(defmethod slot-unbound ((class t) (dist signed-dist) (slot (eql 'releases)))
  (setf (releases dist)
        (releases-table (distinfo dist))))

(defmethod slot-unbound ((class t) (dist signed-dist) (slot (eql 'digests)))
  (setf (digests dist)
        (release-digests (releases dist))))

(defun relative-uri (path dist)
  (puri:render-uri
   (puri:merge-uris path (url dist))
   nil))

(defun digests-url (dist)
  (relative-uri "digests.txt" dist))

(defun releases-url (dist)
  (relative-uri "releases.txt" dist))

(defun systems-url (dist)
  (relative-uri "systems.txt" dist))

(defun signed-url (url)
  (format nil "~A.asc" url))

(defun release-url (dist project-name)
  (gethash project-name (releases dist)))

(defun all-project-names (dist)
  (sort (alexandria:hash-table-keys (releases dist)) #'string<))

(defun url-exists-p (url)
  (let ((status-code
          (nth-value 1
                     (drakma:http-request url
                                          :method :head))))
    (= status-code 200)))

(defgeneric signedp (object)
  (:method ((url string))
    (url-exists-p (signed-url url)))
  (:method ((dist signed-dist))
    (let ((signed-urls
            (list (releases-url dist)
                  (systems-url dist)
                  (digests-url dist)
                  (url dist))))
      (every #'signedp signed-urls))))

(defun sign-dist (dist)
  (when (stringp dist)
    (setf dist (make-instance 'signed-dist :url dist)))
  (with-temp-output-file (file "digests.txt")
    (write-dist-digests (digests dist) file)
    (publish-to-url file (digests-url dist))
    (sign-url (digests-url dist))
    (sign-url (systems-url dist))
    (sign-url (releases-url dist))
    (sign-url (url dist)))
  dist)
