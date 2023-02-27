#lang racket/base

(provide run-with-redirections)

(define (resolve-redirects table)
  (for/hash ([(k v) table])
    (values k (resolved-module-path-name (module-path-index-resolve (module-path-index-join v #f))))))

(define (make-redirecting-resolver redirects [base-resolver-arg #f])
  (define base-resolver (or base-resolver-arg (current-module-name-resolver)))
  (define resolved-redirects (resolve-redirects redirects))

  (case-lambda
    [(resolved-path maybe-namespace) (base-resolver resolved-path maybe-namespace)]
    [(path relative-to error-stx load?)
     (let ([maybe-redirected-path (or (hash-ref resolved-redirects path (lambda () #f)) path)])
       (base-resolver maybe-redirected-path relative-to error-stx load?))]))


;; Hack to share test log
(define-namespace-anchor runner-ns)
(require (only-in rackunit))

;; (-> module-path? (hashof symbol? module-path?) any)
;;
;; Loads the module at the given module path while redirecting uses of the
;; module path keys in the redirect hash to the module paths given as the hash
;; values.
;;
;; The module path to run and the values in the redirects table are resolved relative
;; to (or (current-load-relative-directory) (current-directory))
;;
;; The relative path at the time of each redirected resolution is currently ignored,
;; so redirection is only supported for module path keys that are symbols.
(define (run-with-redirections module-path redirects)
  (parameterize ([current-namespace (make-base-empty-namespace)]
                 [current-module-name-resolver (make-redirecting-resolver redirects)])

    ;; Hack to share test log
    (namespace-attach-module (namespace-anchor->namespace runner-ns) 'rackunit)

    (namespace-require module-path)))

(module+ main
  (require racket/match)
  (match-define (list-rest minikanren-impl to-run submod-path) (vector->list (current-command-line-arguments)))
  (define require-spec
    (if (null? submod-path)
        to-run
        `(submod ,to-run . ,(map string->symbol submod-path))))
  (run-with-redirections require-spec (hash 'benchmark-minikanren minikanren-impl)))
