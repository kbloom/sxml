;test-sxml.scm
;Sterling Stuart Stein
;Try the different functions in sxml

;Module description
(module test-sxml
  (main main)
  (library sxml)
)

;Let for structs
(define-macro (letlist a b . c)
  (letrec
    ( (sym (gensym))

      (build-list
        (lambda (x y)
          (if (pair? x)
            (let ((nextsym (gensym)))
              (cons
               `(,(car x) (car ,y))
                (cons
                 `(,nextsym (cdr ,y))
                  (build-list (cdr x) nextsym)
            ) ) )

           '()
    ) ) ) )

   `(let ((,sym ,b))
      (let*
       ,(build-list a sym)
      ,@c
) ) ) )

;Make triplets of test cases: (name function result)
;equal? used to test that result matches
(define-macro (test-cases reg . x)
  (letrec
    ( (make-test-case
        (lambda (x)
          (if (pair? x)
            (letlist (name func res) (car x)
              (let*
                ( (sym (gensym))
                  (definition
                   `(define (,sym) ,func)
                  )
                  (rest
                    (list name (list 'unquote sym) res)
                ) )

                (cons (cons definition rest) (make-test-case (cdr x)))
            ) )

           '()
    ) ) ) )

    (let ((parts (make-test-case x)))
     `(begin
      ,@(map car parts)
        (define ,reg
          (,'quasiquote
           ,(map cdr parts)
) ) ) ) ) )

(test-cases
  registry

  ("Sanity check"
    (+ 1 2)
    3
  )
)

;Find the length of the longest name
(define (longest x)
  (apply max (map (lambda (x) (string-length (car x))) x))
)

(define (display-dots x)
  (if (< x 1)
    #unspecified

    (begin
      (display ".")
      (display-dots (- x 1))
) ) )

(define (run-tests reg)
  (letrec
    ( (len (+ (longest reg) 3))
      (run-test
        (lambda (t)
          (if (pair? t)
            (letlist (name func expected) (car t)
              (display name)
              (display-dots (- len (string-length name)))
              (let ((actual (func)))
                (if (equal? actual expected)
                  (begin
                    (display "Pass\n")
                    (run-test (cdr t))
                  )

                  (begin
                    (display "Fail\n")
                    (display "Expected:\n")
                    (display expected)
                    (display "\n\nGot:\n")
                    (display actual)
                    (display "\n\n")

                    1 ;Return error code
            ) ) ) )

            0 ;Success
    ) ) ) )

    (run-test reg)
) )

(define (main argv)
  (run-tests registry)
)
