(require 's)

(setq http-methods '(GET POST PUT PATCH DELETE OPTIONS HEAD TRACE CONNECT))

(setq http-headers
      '(Accept Accept-Charset Accept-Encoding Accept-Language
        Accept-Datetime Authorization Cache-Control Connection Cookie
        Content-Length Content-MD5 Content-Type Date Expect From Host
        If-Match If-Modified-Since If-None-Match If-Range
        If-Unmodified-Since Max-Forwards Origin Pragma
        Proxy-Authorization Range Referer TE User-Agent Upgrade Via
        Warning))

(setq http-methods-regexp (regexp-opt (mapcar 'symbol-name http-methods) 'words))

(setq http-headers-regexp (s-concat "^\\("
                                    (s-join "\\|" (mapcar 'symbol-name http-headers))
                                    "\\): \\(.*\\)$"))

(setq http-custome-headers-regexp "^X-\\([^ :]+\\): \\(.*\\)$")

(setq http-font-lock-keywords
      `((,http-headers-regexp . font-lock-constant-face)
        (,http-custome-headers-regexp . font-lock-constant-face)
        (,http-methods-regexp . font-lock-function-name-face)))

(define-derived-mode http-mode fundamental-mode
  "http mode"
  "Major mode for editing http request"
  (setq font-lock-defaults '((http-font-lock-keywords)))
  (setq http-headers-regexp nil)
  (setq http-custome-headers-regexp nil)
  (setq http-methods-regexp nil))

(provide 'http-mode)
