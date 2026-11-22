(defpackage #:rogue-company/tests
  (:use #:cl #:fiveam))

(in-package #:rogue-company/tests)

(defun near (a b &optional (tolerance 1f-5))
  "A et B à moins de TOLERANCE l'un de l'autre."
  (< (abs (- a b)) tolerance))
