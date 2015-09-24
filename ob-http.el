;;; ob-http.el --- http request in org-mode babel

;; Copyright (C) 2015 ZHOU Feng

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-http
;; Version: 0.0.1
;; Package-Requires: ((s "1.9.0"))

;;; Commentary:
;;
;; http request in org-mode babel
;;

;;; Code:
(require 'org)
(require 'ob)
(require 's)
(require 'json)
(require 'ob-http-mode)

(defconst org-babel-header-args:http
  '((pretty . :any)
    (proxy . :any)
    (cookie . :any)
    (schema . :any)
    (host . :any)
    (port . :any)
    (username . :any)
    (password . :any)
    (max-time . :any))
  "http header arguments")

(defgroup ob-http nil
  "org-mode blocks for http request"
  :group 'org)

(defcustom ob-http:max-time 10
  "maximum time in seconds that you allow the whole operation to take"
  :group 'ob-http
  :type 'integer)

(defcustom ob-http:remove-cr nil
  "remove carriage return from header"
  :group 'ob-http
  :type 'boolean)

(defstruct ob-http-request method url headers body)
(defstruct ob-http-response headers body headers-map)

(defun ob-http-parse-request (input)
  (let* ((headers-body (ob-http-split-header-body input))
         (headers (s-split-up-to "\\(\r\n\\|[\n\r]\\)" (car headers-body) 1))
         (method-url (split-string (car headers) " ")))
    (make-ob-http-request
     :method (car method-url)
     :url (cadr method-url)
     :headers (if (cadr headers) (s-lines (cadr headers)))
     :body (cadr headers-body))))

(defun ob-http-parse-response (response)
  (let* ((headers-body (ob-http-split-header-body response))
         (headers-map (mapcar 'ob-http-parse-header (s-lines (car headers-body)))))
    (make-ob-http-response
     :headers (car headers-body)
     :body (cadr headers-body)
     :headers-map headers-map)))

(defun ob-http-split-header-body (input)
  (s-split-up-to "\\(\r\n\\|[\n\r]\\)[ \t]*\\1" input 1))

(defun ob-http-pretty-json (str)
  (if (executable-find "jq")
      (with-temp-buffer
        (insert str)
        (shell-command-on-region (point-min) (point-max) "jq -r ." nil 't)
        (buffer-string))
    (with-temp-buffer
      (insert str)
      (json-pretty-print-buffer)
      (buffer-string))))

(defun ob-http-parse-header (line)
  (let ((key-value (s-split-up-to ": " line 1)))
    `(,(s-downcase (car key-value)) . ,(cadr key-value))))

(defun ob-http-parse-content-type (content-type)
  (if (s-contains? "json" content-type) "json" nil))

(defun ob-http-pretty (body content-type)
  (let ((type (if content-type (ob-http-parse-content-type content-type) "json")))
    (cond ((string= "json" type) (ob-http-pretty-json body))
          (t body))))

(defun ob-http-pretty-response (response content-type)
  (setf (ob-http-response-body response)
        (ob-http-pretty (ob-http-response-body response)
                        (or content-type
                            (ob-http-get-response-header response "content-type")))))

(defun ob-http-select (str path)
  (if (executable-find "jq")
      (with-temp-buffer
        (insert str)
        (shell-command-on-region (point-min) (point-max) (format "jq -r \"%s\"" path) nil 't)
        (buffer-string))
    str))

(defun ob-http-alist-key-to-string (pair)
  `(,(symbol-name (car pair)) . ,(cdr pair)))

(defun org-babel-expand-body:http (body params)
  (s-format body 'aget
            (mapcar 'ob-http-alist-key-to-string
             (mapcar #'cdr (org-babel-get-header params :var)))))

(defun ob-http-get-response-header (response header)
  (cdr (assoc (s-downcase header) (ob-http-response-headers-map response))))

(defun ob-http-remove-carriage-return (response)
  (setf (ob-http-response-headers response)
        (s-join "\n" (s-lines (ob-http-response-headers response))))
  response)

(defun ob-http-flatten (l)
  (cond
   ((null l) nil)
   ((atom l) (list l))
   (t
    (append (ob-http-flatten (car l)) (ob-http-flatten (cdr l))))))

(defun ob-http-aget (key alist)
  (assoc-default (intern key) alist))

(defun ob-http-construct-url (path params)
  (if (s-starts-with? "/" path)
      (s-concat
       (format "%s://" (or (aget params :schema) "http"))
       (when (and (aget params :username) (aget params :password))
         (s-format "${:username}:${:password}@" 'ob-http-aget params))
       (aget params :host)
       (when (aget params :port)
             (format ":%s" (aget params :port)))
       path)
    path))

(defun org-babel-execute:http (body params)
  (let* ((request (ob-http-parse-request (org-babel-expand-body:http body params)))
         (proxy (cdr (assoc :proxy params)))
         (pretty (assoc :pretty params))
         (get-header (cdr (assoc :get-header params)))
         (cookie-jar (cdr (assoc :cookie-jar params)))
         (cookie (cdr (assoc :cookie params)))
         (select (cdr (assoc :select params)))
         (request-body (ob-http-request-body request))
         (error-output (org-babel-temp-file "curl-error"))
         (args (list "-i"
                     (when proxy `("-x" ,proxy))
                     (let ((method (ob-http-request-method request)))
                       (if (string= "HEAD" method) "-I" `("-X" ,method)))
                     (mapcar (lambda (x) `("-H" ,x)) (ob-http-request-headers request))
                     (when (s-present? request-body)
                       (let ((tmp (org-babel-temp-file "http-")))
                         (with-temp-file tmp (insert request-body))
                         `("-d" ,(format "@%s" tmp))))
                     (when cookie-jar `("--cookie-jar" ,cookie-jar))
                     (when cookie `("--cookie" ,cookie))
                     "--max-time"
                     (int-to-string (or (cdr (assoc :max-time params))
                                        ob-http:max-time))
                     (ob-http-construct-url (ob-http-request-url request) params))))
    (with-current-buffer (get-buffer-create "*curl output*")
      (erase-buffer)
      (if (= 0 (apply 'call-process "curl" nil `(t ,error-output) nil (ob-http-flatten args)))
          (let ((response (ob-http-parse-response (buffer-string))))
            (when pretty (ob-http-pretty-response response (cdr pretty)))
            (when ob-http:remove-cr (ob-http-remove-carriage-return response))
            (cond (get-header (ob-http-get-response-header response get-header))
                  (select (ob-http-select (ob-http-response-body response) select))
                  (pretty (ob-http-response-body response))
                  (t (s-join "\n\n" (list (ob-http-response-headers response) (ob-http-response-body response))))))
        (with-output-to-temp-buffer "*curl error"
          (princ (with-temp-buffer
                   (insert-file-contents-literally error-output)
                   (s-join "\n" (s-lines (buffer-string)))))
          "")))))

(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("http" . "ob-http")))

(provide 'ob-http)
;;; ob-http.el ends here
