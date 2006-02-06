;match.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Tell if a path matches an expression (XPath)

;Module description
(module sxml-match
  (export
    (class match take-step matches? pos)
    (take-step obj step)
    (matches? obj)
    (all-step templates tagname)
) )

;Interface definition

;1st function
(define (take-step obj step)
  ((match-take-step obj) (match-pos obj) step)
)

;2nd function
(define (matches? obj)
  ((match-matches? obj) (match-pos obj))
)

;Take a step in the tree
(define (all-step templates tagname)
  (if (and (pair? templates) (pair? (car templates)))
    (cons
      (cons (take-step (caar templates) tagname) (cdar templates))
      (all-step (cdr templates) tagname)
    )
   '()
) )
