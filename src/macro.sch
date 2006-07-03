;struct.sch
;Sterling Stuart Stein
;Miscellaneous macros

;Constants

(define-macro (NUMBITS) 32)



;Macro to make structs with constructors

(define-macro (structure name . members)
 `(begin
    (define-struct ,name . ,members)
    (define-constructor ,name . ,members)
) )

(define-macro (define-constructor name . members)
  (letrec
    ( (define-construct-loop
        (lambda (mem)
          (if (pair? mem)
            (cons
             `(,(symbol-append name '- (car mem) '-set!) x ,(car mem))
              (define-construct-loop (cdr mem))
            )
           '(x)
    ) ) ) )

   `(define (,(symbol-append 'construct- name)  . ,members)
      (let ((x (,(symbol-append 'make- name))))
        .
       ,(define-construct-loop members)
) ) ) )
