;test-sxml.scm
;Sterling Stuart Stein
;Try the different functions in sxml
;
;Copyright (C) 2006  Sterling Stuart Stein
;
;This program is free software: you can redistribute it and/or modify
;it under the terms of the GNU Lesser General Public License as published by
;the Free Software Foundation, either version 3 of the License, or (at your
;option) any later version.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

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

;Here are all of the actual tests to run
(test-cases
  registry

  ("Sanity check"
    (+ 1 2)
    3
  )

  ("Parse"
    (parse-xml (open-input-string
      "<html><head><title>Fluffy</title></head><body class=\"colorful\" style=\"fuzzy\">abc <br /> def</body></html>"
    ))

    ("html" ()
      ("head" ()
        ("title" () "Fluffy")
      )
      ("body" (("class"."colorful") ("style"."fuzzy"))
        "abc "
        ("br" ())
        " def"
  ) ) )

  ("Parse and trim"
    (trim-all (parse-xml (open-input-string
      "<a>
  Indented
  <b>
Enclosed
    <c de=\"fg\" />
  </b>
  After
</a>
"
    )))

    ("a" ()
      " Indented "
      ("b" ()
        " Enclosed "
        ("c" (("de"."fg")))
      )
      " After "
  ) )

  ("Display"
    (let ((str (open-output-string)))
      (display-xml
       '("a" ()
          "abc"
          ("b" (("a"."1") ("b"."2"))
            "ghi"
            ("c" ())
          )
          "def"
        )
        str
      )
      (close-output-port str)
    )

    "<a>abc<b a=\"1\" b=\"2\">ghi<c/></b>def</a>"
  )

  ("Get node name"
    (get-tag-name '("root" () ("internal" () "text")))
    "root"
  )

  ("Get attribute"
    (get-attr '("root" (("mood"."odd") ("state"."happy") ("music"."earthbound")) "text") "state")
    "happy"
  )

  ("Add attribute"
    (set-attr '("root" (("a"."1") ("b"."2")) "text") "c" "3")
    ("root" (("a"."1") ("b"."2") ("c"."3")) "text")
  )

  ("Change attribute"
    (set-attr '("root" (("a"."1") ("b"."2") ("c"."3")) "text") "b" "off")
    ("root" (("a"."1") ("b"."off") ("c"."3")) "text")
  )

  ("Delete attribute"
    (del-attr '("root" (("a"."1") ("b"."2") ("c"."3")) "text") "a")
    ("root" (("b"."2") ("c"."3")) "text")
  )

  ("Select nodes"
    (select-tags
     '("root" () ("yes" () "1") ("no" () "2") ("yes" () "3") ("no" () ("yes" () "4")) ("yes" () "5" ("yes" () "6")))
      (lambda (x) (equal? (get-tag-name x) "yes"))
    )

    (("yes" () "1") ("yes" () "3") ("yes" () "5" ("yes" () "6")))
  )

  ("Recursively select nodes"
    (select-tags-rec
     '("root" () ("yes" () "1") ("no" () "2") ("yes" () "3") ("no" () ("yes" () "4")) ("yes" () "5" ("yes" () "6")))
      (lambda (x) (equal? (get-tag-name x) "yes"))
    )

    (("yes" () "1") ("yes" () "3") ("yes" () "4") ("yes" () "5" ("yes" () "6")))
  )

  ("Get text"
    (get-text '("root" () "a" ("z" () "b") "c" ("y" () "d" ("x" () "e")) "f"))
    "acf"
  )

  ("Recursively get text"
    (get-text-rec '("root" () "a" ("z" () "b") "c" ("y" () "d" ("x" () "e")) "f"))
    "abcdef"
  )

  ("Sort"
    (sort
     '(("a" () "p") ("b" () "n") ("c" () "o") ("d" () "m"))
      (make-comparison-int
        (list
          (make-comparison-string-int value-of-text)
    ) ) )

    (("d" () "m") ("b" () "n") ("c" () "o") ("a" () "p"))
  )

  ("Reverse sort"
    (sort
     '( ("a" (("up"."1") ("down"."1")))
        ("b" (("up"."3") ("down"."0")))
        ("c" (("up"."2") ("down"."1")))
        ("d" (("up"."2") ("down"."0")))
        ("e" (("up"."1") ("down"."0")))
        ("f" (("up"."3") ("down"."1")))
      )
      (make-comparison-int-desc
        (list
          ;Double negative just for fun
          (reverse-sort-order-int (make-comparison-number-int (make-value-of-attr "up")))
          (make-comparison-number-int (make-value-of-attr "down"))
    ) ) )

    ( ("a" (("up"."1") ("down"."1")))
      ("e" (("up"."1") ("down"."0")))
      ("c" (("up"."2") ("down"."1")))
      ("d" (("up"."2") ("down"."0")))
      ("f" (("up"."3") ("down"."1")))
      ("b" (("up"."3") ("down"."0")))
) ) )

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
