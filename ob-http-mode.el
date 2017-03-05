;;; ob-http-mode.el --- syntax highlight for ob-http

;; Copyright (C) 2015 Feng Zhou

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
              (rx-to-string
               `(seq
                 bol
                 (? (1+ space))
                 (group-n 1 (or ,@(mapcar 'symbol-name ob-http-methods)))
                 space
                 (group-n 2 (1+ any))
                 eol)))
             (ob-http-headers-regexp
              (rx-to-string
               `(seq
                 bol
                 (? (1+ space))
                 (group-n 1 (or ,@(mapcar 'symbol-name ob-http-headers)))
                 ": "
                 (group-n 2 (1+ any))
                 eol)))
             (ob-http-custom-headers-regexp
              "\\(^X-[^ :]+\\): \\(.*\\)$")
             (ob-http-variable-regexp
              "\\([^ ?&=\n]+\\)=\\([^&\n]*\\)")
             (ob-http-misc-regexp
              "\\(&\\|=\\|?\\|{\\|}\\|\\[\\|\\]\\|\\,\\|:\\)"))
        `((,ob-http-headers-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,ob-http-custom-headers-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,ob-http-variable-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,ob-http-methods-regexp  (1 font-lock-constant-face) (2 font-lock-function-name-face))
          (,ob-http-misc-regexp (1 font-lock-comment-face)))))

(define-derived-mode ob-http-mode fundamental-mode "ob http"
  (set (make-local-variable 'font-lock-defaults) '(ob-http-mode-keywords)))

(provide 'ob-http-mode)
;;; ob-http-mode.el ends here
