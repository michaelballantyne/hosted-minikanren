(define sea-of-japan
  '((japan)))

(define korea
  '((skorea nkorea)
    (nkorea skorea)))

(define iberia
  '((spain portugal cerdagne andorra gibraltar)
    (portugal spain)
    (andorra spain)
    (cerdagne spain)
    (gibraltar spain)))

(define colors
  '(green blue yellow purple))

(defrel (color-grapho colors graph alist)
  (conde
    ((== `() graph) (== '() alist))
    ((fresh (node neighbors d)
       (== `((,node . ,neighbors) . ,d) graph)
       (fresh (rec-alist n-color)
         (== `((,node . ,n-color) . ,rec-alist) alist)
         (find-working-color colors node neighbors rec-alist n-color)
         (color-grapho colors d rec-alist))))))

(defrel (find-working-color colors node neighbors rec-alist n-color)
  (fresh (color res-col)
    (== `(,color . ,res-col) colors)
    (absento color res-col)
    (conde
      ((== color n-color) (colors-node color node neighbors rec-alist))
      ((=/= color n-color) (find-working-color res-col node neighbors rec-alist n-color)))))

(defrel (colors-node color node neighbors rec-alist)
  (conde
	((== '() rec-alist))
	((fresh (aa da d)
	   (== `((,aa . ,da) . ,d) rec-alist)
	   (=/= aa node) ;; no two places with the same name
	   (distinct-cols color node neighbors aa da)
	   (colors-node color node neighbors d)))))

(defrel (distinct-cols color node neighbors aa da)
  (conde
    ((== '() neighbors))
    ((fresh (neighbor res)
       (== `(,neighbor . ,res) neighbors)
       (=/= node neighbor)
       (absento neighbor res)
       (conde
         ((== neighbor aa)
          (=/= color da)
          (distinct-cols color node res aa da))
         ((=/= neighbor aa)
          (distinct-cols color node res aa da)))))))

(define (texas-colors)
  (run 1 (q) (colors-node 'orange 'texas
						  '(n-mexico oklahoma arkansas louisiana)
						  '((n-mexico . green) (nebraska . orange) (arkansas . red)))))


(define (green-texas)
  (run 10 (q) (distinct-cols 'green 'texas '(n-mexico oklahoma arkansas louisiana) 'oklahoma 'green)))


(define (green-colors-texas)
  (run 10 (q)
	   (colors-node 'green
					'texas
					'(n-mexico oklahoma arkansas louisiana)
					'((oklahoma . green) (nebraska . blue)))))


(define (colors-of-texas)
  (run 10 (q) (find-working-color
               '(green blue yellow red)
               'texas
               '(n-mexico oklahoma arkansas louisiana)
               '((oklahoma . green) (nebraska . blue))
               q)))


(define (color-japan)
  (run 4 (q) (color-grapho colors sea-of-japan q)))


(define (color-a-cycle)
  (run 1 (q) (color-grapho
              '(red orange purple black)
              '((a b c) (b a c) (c a b))
              q)))


(define (color-korea)
  (run 8 (q) (color-grapho colors korea q)))


(define (color-iberia)
  (run 1 (q) (color-grapho colors iberia q)))


(define (ways-to-color-iberia)
  (length (run 48 (q) (color-grapho '(red white blue)
                                    iberia
                                    q))))


(define (color-middle-earth)
 (run 1 (q)
      (color-grapho colors '((lindon eriador forodwaith)
                             (forodwaith lindon rhovanion eriador)
                             (eriador lindon forodwaith rhovanion enedwaith)
                             (rhovanion forodwaith eriador enedwaith rohan rhun)
                             (enedwaith eriador rhovanion rohan gondor)
                             (rohan enedwaith rhovanion rhun gondor mordor)
                             (gondor enedwaith rohan mordor)
                             (rhun rohan rhovanion khand mordor)
                             (mordor gondor rohan rhun khand harad)
                             (khand mordor rhun harad)
                             (harad mordor khand))
					q)))
