#lang racket
(provide (all-defined-out))

(define nodes
  '(western-australia
    northern-territory
    south-australia
    queensland
    new-south-wales
    victoria
    tasmania))

(define edges
  '((western-australia northern-territory)
    (western-australia south-australia)
    (northern-territory south-australia)
    (northern-territory queensland)
    (south-australia queensland)
    (south-australia new-south-wales)
    (south-australia victoria)
    (queensland new-south-wales)
    (new-south-wales victoria)))
