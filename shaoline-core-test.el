;;; shaoline-core-test.el --- Tests for Shaoline pure core functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'shaoline) ;; or relative require if used as submodule

(ert-deftest shaoline-compose-modeline-output-is-string ()
  "shaoline-compose-modeline должен возвращать строку без вызова Emacs I/O."
  (let ((result (shaoline-compose-modeline)))
    (should (stringp result))))

(ert-deftest shaoline-compose-modeline-segment-errors-never-break ()
  "Ошибки сегмента не должны сбивать весь вывод."
  (puthash 'crazy-segment
           (lambda (_buffer) (error "OOPS"))
           shaoline--segment-table)
  (let* ((shaoline-segments '((:left crazy-segment)))
         (s (shaoline-compose-modeline)))
    (should (string-match-p "\\[SEGMENT ERROR:" s))))

;;; Add more property-based or ERT tests here as needed

(provide 'shaoline-core-test)
