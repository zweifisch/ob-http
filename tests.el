(require 'ob-http)

(ert-deftest ob-http-test-construct-url ()
  (let ((params '((:username . "demo")
                  (:password . "secret")
                  (:host . "localhost"))))
    (should (equal (ob-http/construct-url "/" params)
                   "http://demo:secret@localhost/")))

  (let ((params '((:host . "localhost"))))
    (should (equal (ob-http/construct-url "/" params)
                   "http://localhost/")))

  (let ((params '((:host . "localhost"))))
    (should (equal (ob-http/construct-url "http://local" params)
                   "http://local")))

  (let ((params '((:host . "localhost")
                  (:port . "8080")
                  (:schema . "https"))))
    (should (equal (ob-http/construct-url "/" params)
                   "https://localhost:8080/"))))
