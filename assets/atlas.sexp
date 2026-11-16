;;; Écrit par scripts/build-atlas.lisp : ne pas modifier à la main.

(:cell 48 :directions (:east :south-east :south :north-east :north) :units
 ((:pikeman
   ((:walk :row 0 :frames 8 :tips
     (((0 -14) (0 -14) (0 -15) (-1 -14) (-1 -14) (-1 -14) (0 -14) (-1 -14))
      ((-1 -15) (-1 -15) (-1 -15) (-1 -15) (-1 -15) (-1 -15) (-1 -15) (-1 -15))
      ((0 -16) (-1 -16) (-1 -16) (-1 -16) (-1 -16) (-1 -15) (0 -16) (0 -16))
      ((-1 -15) (-1 -15) (-1 -15) (-1 -15) (-1 -15) (-1 -15) (-1 -15) (-1 -15))
      ((-1 -15) (-1 -15) (-1 -15) (-1 -15) (-1 -15) (-1 -15) (-1 -15) (-1 -15))))))
  (:standard-bearer
   ((:walk :row 10 :frames 8 :tips
     (((2 -15) (2 -15) (1 -15) (2 -15) (2 -15) (2 -16) (2 -15) (2 -15))
      ((2 -15) (0 -13) (3 -15) (2 -16) (0 -13) (3 -14) (2 -15) (3 -15))
      ((-4 -15) (-4 -15) (-4 -15) (-4 -15) (-4 -15) (-4 -15) (-4 -15) (-4 -15))
      ((6 -16) (7 -16) (7 -16) (6 -16) (5 -16) (3 -15) (5 -15) (5 -16))
      ((7 -16) (6 -17) (5 -17) (5 -17) (4 -16) (4 -16) (6 -16) (7 -16)))))))
 :overlays
 ((:flag :row 20 :frames 4 :width 23 :height 17)
  (:part-body :row 22 :frames 5 :width 14 :height 13)
  (:part-foot :row 24 :frames 1 :width 6 :height 4)
  (:part-hand :row 26 :frames 1 :width 5 :height 5)
  (:part-head :row 28 :frames 5 :width 14 :height 13)
  (:solid :row 30 :frames 1 :width 4 :height 4))) 
