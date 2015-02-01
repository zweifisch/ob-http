
(setq http-keywords '("=" ";" ":"))
(setq http-constants '("applicationx-www-form-urlencoded" "application/json" "UTF-8"))
(setq http-functions '("GET" "POST" "PUT" "PATCH" "DELETE" "OPTIONS" "HEAD"))

(setq http-keywords-regexp (regexp-opt http-keywords 'words))
(setq http-constants-regexp (regexp-opt http-constants 'words))
(setq http-functions-regexp (regexp-opt http-functions 'words))

(setq http-headers-regexp "^\\([^ :]+\\): \\(.*\\)$")

(setq http-font-lock-keywords
      `((,http-headers-regexp . font-lock-constant-face)
        (,http-constants-regexp . font-lock-constant-face)
        (,http-functions-regexp . font-lock-function-name-face)
        (,http-keywords-regexp . font-lock-keyword-face)))

(define-derived-mode http-mode fundamental-mode
  "http mode"
  "Major mode for editing http request"

  (setq font-lock-defaults '((http-font-lock-keywords)))

  (setq http-headers-regexp nil)
  (setq http-constants-regexp nil)
  (setq http-keywords-regexp nil)
  (setq http-functions-regexp nil))

(provide 'http-mode)
