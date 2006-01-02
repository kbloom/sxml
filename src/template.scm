;template.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Apply templates to sxml to transform it (XSLT)

;Module description
(module sxml-template
  (from
    (sxml-match "match.scm")
  )
  (import
    (sxml-manip "manip.scm")
  )
  (export
    (apply-templates root templates param)
    (apply-templates-list root sxml templates param)
    (apply-templates-children root sxml templates param)
) )

;templates = list of (matcher . function)
;function = lambda (root branch templates param position)

(define (find-match templates)
  (cond
    ((not (pair? templates)) #f)
    ((matches? (caar templates)) (cdar templates))
    (#t (find-match (cdr templates)))
) )

(define (do-match root sxml next templates param position)
  (letrec
      ;Traverse tree looking for matches
    ( (apply-templates-loop
        (lambda (sxml next templates position)
          (if (not (pair? sxml))
            next
            (let
              (
                (a (car sxml))
                (b (apply-templates-loop (cdr sxml) next templates (+ position 1)))
              )
              (if (tag? a)
                (do-match-loop a b templates position)
                b
      ) ) ) ) )

      ;See if this tag matches, and if so apply the template
      (do-match-loop
        (lambda (sxml next templates position)
          (let*
            (
              (t (all-step templates (get-tag-name sxml)))
              (m (find-match t))
            )

            (if m
              (cons (m root sxml templates param position) next)
              (apply-templates-loop (get-children sxml) next t 0)
    ) ) ) ) )

    (do-match-loop sxml next templates position)
) )

;Apply all of the templates to an sxml tree
;Returns list of applied nodes
;Note that for root, you will only want 1 node, so take car of list
(define (apply-templates root templates param)
  (if (tag? root)
    (do-match root root '() templates param 0)
   '()
) )

(define (apply-templates-list root sxml templates param)
  (letrec
    ( (apply-templates-list-loop
        (lambda (sxml position)
          (if (pair? sxml)
            (let
              (
                (a (car sxml))
                (b (apply-templates-list-loop (cdr sxml) (+ position 1)))
              )

              (if (tag? a)
                (do-match root a b templates param position)
                b
            ) )
           '()
    ) ) ) )

    (apply-templates-list-loop sxml 0)
) )

(define (apply-templates-children root sxml templates param)
  (if (tag? sxml)
    (apply-templates-list
      root
      (get-children sxml)
      (all-step templates (get-tag-name sxml))
      param
    )
   '()
) )
