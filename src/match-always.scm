;match-always.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Unconditionally say that it matches

;Module description
(module sxml-match-always
  (export
    (make-match-always)
) )

(define (make-match-always)
  (define (take-step obj step)
    (make-match-always)
  )

  (define (matches? obj)
    #t
  )

  (cons (list take-step matches?) '())
)
