
(defpackage :things.cffi
  (:documentation "CFFI Experiments.")
  (:use #:cl #:cffi)
  (:export #:main))

(in-package :things.cffi)

;; Public

(defun main ()
  (curl-global-init 0)
  (curl-easy-setopt *easy-handle* :nosignal :long 1)
  (set-curl-option-url *easy-handle* "http://www.cliki.net/CFFI"))

;; CFFI configurations

(pushnew '(merge-pathnames #p".local/lib/" (user-homedir-pathname))
         *foreign-library-directories*
         :test #'equal)
(pushnew '(merge-pathnames #p".local/lib64/" (user-homedir-pathname))
         *foreign-library-directories*
         :test #'equal)
(pushnew #p"/usr/local/lib/" *foreign-library-directories*
         :test #'equal)
(pushnew #p"/usr/local/lib64/" *foreign-library-directories*
         :test #'equal)

;; SFML libraries

(define-foreign-library sfml-system
  (:unix (:or "libsfml-system.so.2.3" "libsfml-system.so"))
  (T (:default "sfml-system-2")))

(use-foreign-library sfml-system)

;; libcurl libraries

(define-foreign-library libcurl
  (:unix (:or "libcurl.so.4" "libcurl.so"))
  (T (:default "libcurl")))

(use-foreign-library libcurl)

(defctype easy-handle :pointer)
(defctype curl-code :int)

(defcfun "curl_global_init" curl-code
  (flags :long))

(defcfun "curl_easy_init" easy-handle)

(defcfun "curl_easy_cleanup" :void
  (easy-handle easy-handle))

(defparameter *curl-error-size* 257)
(defparameter *easy-handle* (curl-easy-init))
(defvar *easy-handle-cstrings* (make-hash-table))
(defvar *easy-handle-errorbuffers* (make-hash-table))

(defun add-curl-handle-cstring (handle cstring)
  (car (push cstring (gethash handle *easy-handle-cstrings*))))

(defun curry-curl-option-setter (function-name option-keyword)
  "Wrap the function named by FUNCTION-NAME with a version that
curries the second argument as OPTION-KEYWORD"
  ;; For DEFINE-CURL-OPTION-SETTER
  (setf (symbol-function function-name)
        (let ((c-function (symbol-function function-name)))
          (lambda (easy-handle new-value)
            (funcall c-function easy-handle option-keyword
                     (if (stringp new-value)
                         (add-curl-handle-cstring
                          easy-handle
                          (foreign-string-alloc new-value))
                         new-value))))))

(defun make-easy-handle ()
  (let ((easy-handle (curl-easy-init)))
    (setf (gethash easy-handle *easy-handle-cstrings*) '())
    (setf (gethash easy-handle *easy-handle-errorbuffers*)
          (foreign-alloc :char :count *curl-error-size*
                         :initial-element 0))
    easy-handle))

(defun get-easy-handle-error (handle)
  (foreign-string-to-lisp
   (gethash handle *easy-handle-errorbuffers*)))

(defun free-easy-handle (handle)
  (curl-easy-cleanup handle)
  (foreign-free (gethash handle *easy-handle-errorbuffers*))
  (remhash handle *easy-handle-errorbuffers*)
  (mapc #'foreign-string-free
        (gethash handle *easy-handle-cstrings*))
  (remhash handle *easy-handle-cstrings*))

(defmacro curl-easy-setopt (easy-handle enumerated-name
                            value-type new-value)
  `(foreign-funcall "curl_easy_setopt"
                    easy-handle ,easy-handle
                    curl-option ,enumerated-name
                    ,value-type ,new-value curl-code))

(defmacro define-curl-option-setter (name option-type
                                     option-value foreign-type)
  "Define with DEFCFUN a function name that calls
curl_easy_setopt. OPTION-TYPE and OPTION-VALUE are CFFI
foreign type and value to be passed as the second argument to
easy_setopt, and FOREIGN-TYPE is the CFFI foreign type to be used
for the resultant functions's third argument"
  ;; For DEFINE-CURL-OPTIONS
  `(progn
     (defcfun ("curl_easy_setopt" ,name) curl-code
       (easy-handle easy-handle)
       (option ,option-type)
       (new-value ,foreign-type))
     (curry-curl-option-setter ',name ',option-value)))

(defmacro define-curl-options (type-name type-offsets &rest enum-args)
  "Each ENUM-ARGS is (NAME TYPE NUMBER). 
   Please give a look at curl.h"
  (flet ((enumerated-value (type offset)
           (+ (getf type-offsets type) offset))
         (map-enum-args (procedure)
           (mapcar (lambda (arg) (apply procedure arg)) enum-args))
         (make-setter-name (option-name)
           (intern (concatenate
                    'string "SET-" (symbol-name type-name)
                    "-" (symbol-name option-name)))))
    `(progn
       (defcenum ,type-name
         ,@ (map-enum-args
             (lambda (name type number)
               (list name (enumerated-value type number)))))
       ,@(map-enum-args
          (lambda (name type number)
            (declare (ignore number))
            `(define-curl-option-setter ,(make-setter-name name)
               ,type-name ,name ,(ecase type
                                   (long :long)
                                   (objectpoint :pointer)
                                   (functionpoint :pointer)
                                   (off-t :long)))))
       ',type-name)))

(define-curl-options curl-option
    (long 0 objectpoint 10000 functionpoint 20000 off-t 30000)
  ;; These are defined in curl.h
  (:noprogress long 43)
  (:nosignal long 99)
  (:errorbuffer objectpoint 10)
  (:url objectpoint 2))


