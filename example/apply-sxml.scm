;apply-sxml.scm
;Sterling Stuart Stein
;Try the different functions in sxml

;Module description
(module apply-sxml
  (main main)
  (library sxml)
)

(define (apply1 root branch templates param position)
 `("value" ()
   ,(let ((attr (get-attr branch "tags")))
      (if attr
        attr
        "0"
    ) )
    .
   ,(apply-templates-children root branch templates param)
) )

(define (apply2 root branch templates param)
  '(chained ())
)

(define (main argv)
  (let*
    (
      (parsed (parse-xml (current-input-port)))
      (templates
        (list
          (cons (make-match-exact-path '("xmlness" "inner")) apply1)
          (cons (make-match-exact-path '("xmlness" "inner" "self-close")) apply2)
      ) )
      (returned
        (apply-templates parsed templates '())
    ) )

    (display-xml
     `(matched () . ,returned)
      (current-output-port)
) ) )
