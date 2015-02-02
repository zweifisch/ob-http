;;; ob-http-mode.el --- syntax highlight for ob-http
(require 's)

(setq ob-http-mode-keywords
      (let* ((ob-http-methods
              '(GET POST PUT PATCH DELETE OPTIONS HEAD TRACE CONNECT))
             (ob-http-headers
              '(Accept Accept-Charset Accept-Encoding Accept-Language
                       Accept-Datetime Authorization Cache-Control
                       Connection Cookie Content-Length Content-MD5
                       Content-Type Date Expect From Host If-Match
                       If-Modified-Since If-None-Match If-Range
                       If-Unmodified-Since Max-Forwards Origin Pragma
                       Proxy-Authorization Range Referer TE User-Agent
                       Upgrade Via Warning))
             (ob-http-methods-regexp
              (regexp-opt (mapcar 'symbol-name ob-http-methods) 'words))
             (ob-http-headers-regexp
              (s-concat "^\\(" (s-join "\\|" (mapcar 'symbol-name ob-http-headers)) "\\): \\(.*\\)$"))
             (ob-http-custome-headers-regexp
              "\\(^X-[^ :]+\\): \\(.*\\)$")
             (ob-http-variable-regexp
              "\\([^ ?&=\n]+\\)=\\([^&\n]*\\)")
             (ob-http-misc-regexp
              "\\(&\\|=\\|?\\|{\\|}\\|\\[\\|\\]\\|\\,\\|:\\)"))
        `((,ob-http-headers-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,ob-http-custome-headers-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,ob-http-misc-regexp (1 font-lock-comment-face))
          (,ob-http-variable-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,ob-http-methods-regexp . font-lock-constant-face))))

(define-derived-mode ob-http-mode fundamental-mode "ob http"
  (set (make-local-variable 'font-lock-defaults) '(ob-http-mode-keywords)))

(provide 'ob-http-mode)
;;; ob-http-mode.el ends here
