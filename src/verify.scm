;verify.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Verify that the sxml is actually valid

;Module description
(module sxml-verify
  (import
    (sxml-manip "manip.scm")
  )
  (export
    (valid-attr attr)
    (valid-tag tag)
) )

(define (valid-attr attr)
  (cond
    ((null? attr) #t)
    ((pair? attr)
      (and
        (caar? attr)
        (string? (caar attr))
        (string? (cdar attr))
        (valid-attr (cdr attr))
    ) )
    (#t #f)
) )

(define (valid-tag tag)
  (letrec
    ( (valid-children
        (lambda (children)
          (cond
            ((null? children) #t)
            ( (and
                (pair? children)
                (or (string? (car children)) (valid-tag (car children)))
              )
              (valid-children (cdr children))
            )
            (#t #f)
    ) ) ) )

    (and
      (tag? tag)
      (string? (car tag))
      (valid-attr (cadr tag))
      (valid-children (get-children tag))
) ) )
