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

(defstruct ob-http/request method url headers body)

(defun ob-http/parse-input (input)
  (let* ((headers-body (ob-http/split-header-body input))
         (headers (s-split-up-to "\\(\r\n\\|[\n\r]\\)" (car headers-body) 1))
         (method-url (split-string (car  headers) " ")))
    (make-ob-http/request
     :method (car method-url)
     :url (cadr method-url)
     :headers (if (cadr headers) (s-lines (cadr headers)))
     :body (cadr headers-body))))

(defun ob-http/split-header-body (input)
  (s-split-up-to "\\(\r\n\\|[\n\r]\\)[ \t]*\\1" input 1))

(defun ob-http/pretty-json (str)
  (if (executable-find "jq")
      (with-temp-buffer
        (insert str)
        (shell-command-on-region (point-min) (point-max) "jq -r ." nil 't)
        (buffer-string))
    (with-temp-buffer
      (insert str)
      (json-pretty-print-buffer)
      (buffer-string))))

(defun parse-header (line)
  (let ((key-value (s-split-up-to ": " line 1)))
    `(,(s-downcase (car key-value)) . ,(cadr key-value))))

(defun parse-content-type (content-type)
  (if (s-contains? "json" content-type) "json" nil))

(defun ob-http/pretty (str content-type)
  (let ((type (if content-type (parse-content-type content-type) "json")))
    (cond ((string= "json" type) (ob-http/pretty-json str))
          (t str))))

(defun ob-http/select (str path)
  (if (executable-find "jq")
      (with-temp-buffer
        (insert str)
        (shell-command-on-region (point-min) (point-max) (format "jq -r \"%s\"" path) nil 't)
        (buffer-string))
    str))

(defun ob-http/alist-key-to-string (pair)
  `(,(symbol-name (car pair)) . ,(cdr pair)))

(defun org-babel-expand-body:http (body params)
  (s-format body 'aget
            (mapcar 'ob-http/alist-key-to-string
             (mapcar #'cdr (org-babel-get-header params :var)))))

(defun ob-http/get-header (headers header)
  (cdr (assoc (s-downcase header) headers)))

(defun ob-http/remove-carriage-return (header-body)
  (list (s-join "\n" (s-lines (car header-body))) (cadr header-body)))

(defun org-babel-execute:http (body params)
  (let* ((body (org-babel-expand-body:http body params))
         (req (ob-http/parse-input body))
         (proxy (cdr (assoc :proxy params)))
         (pretty (assoc :pretty params))
         (pretty-format (if pretty (cdr pretty)))
         (get-header (cdr (assoc :get-header params)))
         (cookie-jar (cdr (assoc :cookie-jar params)))
         (cookie (cdr (assoc :cookie params)))
         (max-time (cdr (assoc :max-time params)))
         (select (cdr (assoc :select params)))
         (body (ob-http/request-body req))
         (cmd (s-format "curl -is ${proxy} ${method} ${headers} ${cookie-jar} ${cookie} ${body} \"${url}\" --max-time ${max-time} | tr -d '\r'" 'aget
                        `(("proxy" . ,(if proxy (format "-x %s" proxy) ""))
                          ("method" . ,(let ((method (ob-http/request-method req)))
                                         (if (string= "HEAD" method) "-I" (format "-X %s" method))))
                          ("headers" . ,(mapconcat (lambda (x) (format " -H \"%s\"" x))
                                                   (ob-http/request-headers req) ""))
                          ("body" . ,(if (s-present? body)
                                         (let ((tmp (org-babel-temp-file "http-")))
                                           (with-temp-file tmp (insert body))
                                           (format "-d @\"%s\"" tmp))
                                       ""))
                          ("url" . ,(ob-http/request-url req))
                          ("cookie-jar" . ,(if cookie-jar (format "--cookie-jar %s" cookie-jar) ""))
                          ("cookie" . ,(if cookie (format "--cookie %s" cookie) ""))
                          ("max-time" . ,(int-to-string (or max-time ob-http:max-time))))))
         (result (shell-command-to-string cmd))
         (header-body (ob-http/split-header-body result))
         (result-headers (mapcar 'parse-header (s-lines (car header-body))))
         (result-body (if pretty (ob-http/pretty (cadr header-body)
                                                 (or (cdr pretty)
                                                     (cdr (assoc "content-type" result-headers))))))
         (result-body (if select (ob-http/select (cadr header-body) select)
                        result-body))
         (result-header (if get-header (ob-http/get-header result-headers get-header)))
         (result (if ob-http:remove-cr (s-join "\n\n" (ob-http/remove-carriage-return header-body)) result)))
    (message cmd)
    (if get-header result-header (if result-body result-body result))))

(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("http" . "ob-http")))

(provide 'ob-http)
;;; ob-http.el ends here
