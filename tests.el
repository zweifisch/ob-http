(require 'ob-http)

(ert-deftest ob-http-test-construct-url ()
  (let ((params '((:username . "demo")
                  (:password . "secret")
                  (:host . "localhost"))))
    (should (equal (ob-http-construct-url "/" params)
                   "http://localhost/")))

  (let ((params '((:host . "localhost"))))
    (should (equal (ob-http-construct-url "/" params)
                   "http://localhost/")))

  (let ((params '((:host . "localhost"))))
    (should (equal (ob-http-construct-url "http://local" params)
                   "http://local")))

  (let ((params '((:host . "localhost")
                  (:port . 8080)
                  (:scheme . "https"))))
    (should (equal (ob-http-construct-url "/" params)
                   "https://localhost:8080/"))))

(ert-deftest ob-http-test-construct-url-schema-alias ()
  (should (equal (ob-http-construct-url "/" '((:host . "localhost")
                                             (:port . 8080)
                                             (:schema . "https")))
                 "https://localhost:8080/")))

(ert-deftest ob-http-test-construct-url-scheme-precedence ()
  (dolist (params '(((:host . "localhost") (:scheme . "https") (:schema . "http"))
                    ((:host . "localhost") (:schema . "http") (:scheme . "https"))))
    (should (equal (ob-http-construct-url "/" params)
                   "https://localhost/"))))

(ert-deftest ob-http-test-construct-url-absolute-scheme ()
  (should (equal (ob-http-construct-url "http://localhost/"
                                       '((:scheme . "https") (:schema . "https")))
                 "http://localhost/")))

(ert-deftest ob-http-test-scheme-header-arguments ()
  (should (assq 'scheme org-babel-header-args:http))
  (should (assq 'schema org-babel-header-args:http)))
