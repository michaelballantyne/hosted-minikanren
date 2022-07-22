#lang racket

(require "../main.rkt" racket/pretty)

(define-relation (conjo q)
  (fresh (x)
    (conde
      [(== x 5) (== q x)]
      [(== q 6)])))

(define-relation (disjo q)
  (conde
    [(fresh (x) (== q 6))]
    [(== q 7)]
    [(== q 8)]))

;;     [(== q 5)]
;;     [(fresh (x) (== q 6))]
;;     [(== q 7)]
;;     [(== q 8)]))

;; (conde
;;   [(== q 5)]
;;   [(conde
;;      [(fresh (x) (== q 6))]
;;      [(== q 7)])])
;; =>
;; (lambda (st)
;;   (suspend
;;     (let ([scope (subst-scope (state-S st))])
;;       (mplus*
;;         (bind* ((== q 5) st))
;;         (bind* ((conde [(fresh (x) (== q 6))] [(== q 7)]) st))))))
;; =>
;; (lambda (st)
;;   (suspend
;;     (let ([st (state-with-scope st (new-scope))])
;;       (mplus*
;;         (bind* ((== q 5) st))
;;         (bind*
;;           ((lambda (st)
;;              (suspend
;;                (let ([st (state-with-scope st (new-scope))])
;;                  (mplus*
;;                    (bind* ((fresh () (== q 6)) st))
;;                    (bind* ((== q 7) st))))))
;;             st))))))
;; =>
;; (lambda (st)
;;   (suspend
;;     (let ([st (state-with-scope st (new-scope))])
;;       (mplus*
;;         (bind* ((== q 5) st))
;;         (bind*
;;           ((lambda (st)
;;              (suspend
;;                (let ([st (state-with-scope st (new-scope))])
;;                  (mplus*
;;                    (bind* ((fresh () (== q 6)) st))
;;                    (bind* ((== q 7) st))))))
;;             st))))))
;;


(module+ test
  (require rackunit)

  (check-equal?
    (run* (q) (conjo q))
    '(5 6))

  (check-equal?
    (run* (q) (disjo q))
    '(7 6 8)))
