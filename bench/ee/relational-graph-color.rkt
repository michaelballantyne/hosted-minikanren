#lang racket

(require "../../main.rkt")

(provide
 texas-colors
 green-texas
 green-colors-texas
 colors-of-texas
 color-japan
 color-a-cycle
 color-korea
 color-middle-earth
 color-iberia
 ways-to-color-iberia)

(include "../common/relational-graph-color.scm")

(module+ test
  (require rackunit)
  (check-equal? (texas-colors) '(_.0))
  (check-equal? (green-texas) '())
  (check-equal? (green-colors-texas) '())
  (check-equal? (colors-of-texas) '(blue yellow red))
  (check-equal?
   (color-japan)
   '(((japan . green))
     ((japan . blue))
     ((japan . yellow))
     ((japan . purple))))
  (check-equal?
   (color-a-cycle)
   '(((a . red) (b . orange) (c . purple))))
  (check-equal?
   (color-korea)
   '(((skorea . green) (nkorea . blue))
     ((skorea . green) (nkorea . yellow))
     ((skorea . green) (nkorea . purple))
     ((skorea . blue) (nkorea . green))
     ((skorea . blue) (nkorea . yellow))
     ((skorea . blue) (nkorea . purple))
     ((skorea . yellow) (nkorea . green))
     ((skorea . yellow) (nkorea . blue))))
  (check-equal?
   (color-iberia)
   '(((spain . green)
      (portugal . blue)
      (andorra . blue)
      (cerdagne . blue)
      (gibraltar . blue))))
  (check-equal?
   (ways-to-color-iberia)
   48))
