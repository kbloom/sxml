;match-always.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Unconditionally say that it matches

;Module description
(module sxml-match-always
  (import
    (sxml-match "match.scm")
  )
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

  (make-match take-step matches? '())
)
