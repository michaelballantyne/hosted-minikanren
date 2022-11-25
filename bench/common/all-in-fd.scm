;; Modified version of an example due to David Nolen
;; on a miniKanren Hangouts

(defrel (infd0-29 x)
  (conde
   [(== x 0)]
   [(conde
	 [(== x 1)]
	 [(conde
	   [(== x 2)]
	   [(conde
		 [(== x 3)]
		 [(conde
		   [(== x 4)]
		   [(conde
			 [(== x 5)]
			 [(conde
			   [(== x 6)]
			   [(conde
				 [(== x 7)]
				 [(conde
				   [(== x 8)]
				   [(conde
					 [(== x 9)]
					 [(conde
					   [(== x 10)]
					   [(conde
						 [(== x 11)]
						 [(conde
						   [(== x 12)]
						   [(conde
							 [(== x 13)]
							 [(conde
							   [(== x 14)]
							   [(conde
								 [(== x 15)]
								 [(conde
								   [(== x 16)]
								   [(conde
									 [(== x 17)]
									 [(conde
									   [(== x 18)]
									   [(conde
										 [(== x 19)]
										 [(conde
										   [(== x 20)]
										   [(conde
											 [(== x 21)]
											 [(conde
											   [(== x 22)]
											   [(conde
												 [(== x 23)]
												 [(conde
												   [(== x 24)]
												   [(conde
													 [(== x 25)]
													 [(conde
													   [(== x 26)]
													   [(conde
														 [(== x 27)]
														 [(conde
														   [(== x 28)]
														   [(== x 29)])])])])])])])])])])])])])])])])])])])])])])])])])])])])]))


(defrel (all-in0-29 xs)
  (conde
    ((== xs '())
     (== 'cat 'cat))
    ((fresh (a d)
       (== xs `(,a . ,d))
       (infd0-29 a)
       (all-in0-29 d)))))


;; Note that in the following we're simply asking for the first answer.

(define all-in-fd
  (let ((vs (reverse (build-list 30 identity))))
	(λ ()
	  (run 30 (q) (== q vs) (all-in0-29 vs)))))
