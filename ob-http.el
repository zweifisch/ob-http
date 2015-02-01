(require 'org)
(require 'ob)
(require 's)
(require 'json)

(defgroup ob-http nil
  "org-mode blocks for http request"
  :group 'org)

(defcustom ob-http:max-time 10
  "maximum time in seconds that you allow the whole operation to take"
  :group 'ob-http
  :type 'integer)

(defstruct ob-http/request method url headers body)

(defun ob-http/take-while (pred coll)
  (cond ((not coll) '())
        (t (if (apply pred (list (car coll)))
             (cons (car coll) (ob-http/take-while pred (cdr coll)))
             '()))))

(defun ob-http/drop-while (pred coll)
  (cond ((not coll) '())
        (t (if (apply pred (list (car coll)))
             (ob-http/drop-while pred (cdr coll))
             coll))))

(defun ob-http/split-with (pred coll)
  (list (ob-http/take-while pred coll) (ob-http/drop-while pred coll)))

(defun ob-http/string-is-empty (s)
  (or (string= "" s) (if (string-match "\\`[ \t\n\r]+\\'" s) t nil)))

(defun ob-http/parse-input (input)
  (let* ((header-body (ob-http/split-with
                       (lambda (x) (not (ob-http/string-is-empty x)))
                       (s-lines input)))
         (body (cadr header-body))
         (headers (car header-body))
         (method-url (split-string (car headers) " "))
         (method (car method-url))
         (url (cadr method-url)))
    (make-ob-http/request
     :method method
     :url url
     :headers (nthcdr 1 headers)
     :body (mapconcat 'identity (nthcdr 1 body) "\n"))))

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
  (let ((type (parse-content-type content-type)))
    (cond ((string= "json" type) (ob-http/pretty-json str))
          (t (ob-http/pretty-json str)))))

(defun org-babel-execute:http (body params)
  (let* ((req (ob-http/parse-input body))
         (proxy (cdr (assoc :proxy params)))
         (pretty (assoc :pretty params))
         (pretty-format (if pretty (cdr pretty)))
         (cookie-jar (cdr (assoc :cookie-jar params)))
         (cookie (cdr (assoc :cookie params)))
         (max-time (cdr (assoc :max-time params)))
         (body (ob-http/request-body req))
         (cmd (s-format "curl -is ${proxy} ${method} ${headers} ${cookie-jar} ${cookie} ${body} \"${url}\" --max-time ${max-time}" 'aget
                        `(("proxy" . ,(if proxy (format "-x %s" proxy) ""))
                          ("method" . ,(let ((method (ob-http/request-method req)))
                                        (if (string= "HEAD" method) "-I" (format "-X %s" method))))
                          ("headers" . ,(mapconcat (lambda (x) (format "-H \"%s\"" x))
                                                  (ob-http/request-headers req) ""))
                          ("body" . ,(if (not (ob-http/string-is-empty body))
                                        (let ((tmp (org-babel-temp-file "http-")))
                                          (with-temp-file tmp (insert body))
                                          (format "-d @\"%s\"" tmp))
                                      ""))
                          ("url" . ,(ob-http/request-url req))
                          ("cookie-jar" . ,(if cookie-jar (format "--cookie-jar %s" cookie-jar) ""))
                          ("cookie" . ,(if cookie (format "--cookie %s" cookie) ""))
                          ("max-time" . ,(int-to-string (or max-time ob-http:max-time))))))
         (result (shell-command-to-string cmd))
         (header-body (ob-http/split-with
                       (lambda (x) (not (ob-http/string-is-empty x)))
                       (s-lines result)))
         (result-headers (mapcar 'parse-header (car header-body)))
         (result-body (s-join "\n" (nthcdr 1 (cadr header-body)))))
    
    (message cmd)
    (if pretty
        (ob-http/pretty result-body (or (cdr pretty)
                                        (cdr (assoc "content-type" result-headers))))
      result)))

(provide 'ob-http)
