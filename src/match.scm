;match.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Tell if a path matches an expression (XPath)

;Module description
(module sxml-match
  (from
    (sxml-match-exact "match-exact.scm")
    (sxml-match-always "match-always.scm")
  )
  (export
    (take-step obj step)
    (matches? obj)
    (all-step templates tagname)
) )

;Simple class
;(vtable . struct)
;vtable -> (take-step matches?)

;Interface definition

;1st function
(define (take-step obj step)
  ((caar obj) (cdr obj) step)
)

;2nd function
(define (matches? obj)
  ((cadar obj) (cdr obj))
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
