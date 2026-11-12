(in-package #:rogue-company/tests)

(def-suite text :description "Texte sans allocation : entiers et UTF-8.")
(in-suite text)

(test integers-are-written-in-decimal
  (let ((buffer (rogue-company::make-text-buffer)))
    (is (equal '("0" "7" "42" "1234" "-56" "4611686018427387903")
               (mapcar (lambda (n) (copy-seq (rogue-company::write-integer-into n buffer)))
                       (list 0 7 42 1234 -56 most-positive-fixnum))))))

(test utf8-round-trips
  "ASCII, accents, symbole sur 3 octets et caractère sur 4 octets, quel que soit
le type de chaîne."
  (cffi:with-foreign-pointer (pointer 64)
    (flet ((round-trip (string)
             (rogue-company::encode-utf8-into string pointer)
             (cffi:foreign-string-to-lisp pointer :encoding :utf-8)))
      (dolist (string (list "abc" (coerce "x2" 'simple-base-string) "Échap é ç" "€ 𝄞" ""))
        (is (string= string (round-trip string)))))))

(test no-allocation
  (let ((buffer (rogue-company::make-text-buffer))
        (base (coerce "Tick" 'simple-base-string)))
    (cffi:with-foreign-pointer (pointer 64)
      (let ((before (sb-ext:get-bytes-consed)))
        (dotimes (i 200000)
          (rogue-company::write-integer-into i buffer)
          (rogue-company::encode-utf8-into base pointer)
          (rogue-company::encode-utf8-into "Échap" pointer))
        (is (< (- (sb-ext:get-bytes-consed) before) 131072))))))
