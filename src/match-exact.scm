;match-exact.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Tell if a path exactly matches

;Module description
(module sxml-match-exact
  (import
    (sxml-match "match.scm")
    (sxml-manip "manip.scm")
  )
  (export
    (make-match-exact-path path)
) )

(define (make-match-exact-path path)
  (define (take-step obj step)
    (make-match-exact-path
      (cond
        ((not (pair? obj)) '(no-match))
        ((equal? (get-tag-name step) (car obj)) (cdr obj))
        (#t '(no-match))
  ) ) )

  (define (matches? obj)
    (null? obj)
  )

  (make-match take-step matches? path)
)
