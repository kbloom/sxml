;test-sxml.scm
;Sterling Stuart Stein
;Try the different functions in sxml

;Module description
(module test-sxml
  (main main)
  (library sxml)
)

(define (separator)
  (print "\n--------------------\n")
)

(define (main argv)
  (let
    ((parsed (parse-xml (current-input-port))))

    (separator)
    (print "sxml:\n" parsed)
    (separator)
    (print "xml:")
    (display-xml parsed (current-output-port))
    (separator)
    (let ((trimmed (trim-all parsed)))
      (print "Trimmed xml:")
      (display-xml trimmed (current-output-port))
      (separator)
      (print "Root node name: " (get-tag-name parsed))
      (separator)
      (print "Attribute print's value: " (get-attr parsed "print"))
      (separator)
      (let
        ( (short
           '("test" (("present" . "here") ("possible" . "maybe")) "text")
        ) )

        (print "Add attribute happy:")
        (display-xml (set-attr short "happy" "meal") (current-output-port))
        (separator)
        (print "Change attribute possible:")
        (display-xml (set-attr short "possible" "different") (current-output-port))
        (separator)
        (print "Delete attribute possible:")
        (display-xml (del-attr short "possible") (current-output-port))
      )
      (separator)

      (let
        ( (short
           '("root" ()
              ("match" () "1")
              ("match" () "2")
              "some text"
              ("no" () "3")
              ("nested" ()
                "some text"
                ("match" () "4")
                ("morenested" ()
                  ("match" () "5")
                  ("no" () "6")
                  ("match" () "7"
                    ("match () "a")
                    "some text"
                    ("no" ())
                    ("match () "b")
                  )
                )
                ("no" () "8")
                ("match" () "9")
              )
            )
        ) )

        (print "Find node named match:")
        (print (select-tags short (lambda (x) (equal? (get-tag-name x) "match"))))
        (separator)
        (print "Find node named match recursively:")
        (print (select-tags-rec short (lambda (x) (equal? (get-tag-name x) "match"))))
      )

      (separator)
      (print "Get all of the text on one level:")
      (print (get-text parsed))
      (separator)
      (print "Get all of the text:")
      (print (get-text-rec parsed))
      (separator)
      (let*
        ( (value-of
            value-of-text
          )
          (criteria 
            (make-comparison-int
              (list
                (make-comparison-string-int value-of)
          ) ) )
          (tags
            (select-tags-rec trimmed (equals-tag-name "inner"))
          )
          (sorted
            (sort tags criteria)
        ) )

        (print "-- Sort by contained text --")
        (print "Unsorted value of tags:")
        (print (map value-of tags))
        (separator)
        (print "Sorted value of tags:")
        (print (map value-of sorted))
        (separator)
        (print "Sorted tags:")
        (display-xml (connect-tag trimmed sorted) (current-output-port))
        (separator)
      )

      (let*
        ( (value-of
            (make-value-of-attr "tags")
          )
          (criteria 
            (make-comparison-int
              (list
                (reverse-sort-order-int (make-comparison-string-int value-of))
          ) ) )
          (tags
            (select-tags-rec trimmed (equals-tag-name "inner"))
          )
          (sorted
            (sort tags criteria)
        ) )

        (print "-- Sort by tags attribute (reversed) --")
        (print "Unsorted value of tags:")
        (print (map value-of tags))
        (separator)
        (print "Sorted value of tags:")
        (print (map value-of sorted))
        (separator)
        (print "Sorted tags:")
        (display-xml (connect-tag trimmed sorted) (current-output-port))
        (separator)
      )

      (let*
        ( (unsorted
           '("number-list" ()
              ("number" () "9")
              ("number" () "07")
              ("number" () "0")
              ("number" () ".8")
              ("number" () "1e2")
              ("number" () "12")
              ("number" () "-1")
              ("number" () "5")
          ) )
          (value-of
            value-of-text
          )
          (criteria
            (make-comparison-int
              (list
                (make-comparison-number-int value-of)
          ) ) )
          (tags
            (select-tags-rec unsorted (equals-tag-name "number"))
          )
          (sorted
            (sort tags criteria)
        ) )

        (print "-- Sort by number --")
        (print "Sorted tags:")
        (display-xml (connect-tag unsorted sorted) (current-output-port))
        (separator)
      )

      (let*
        ( (unsorted
           '("database" ()
              ("record" () "z" ("sub" (("tie" . "c")) "a" ))
              ("record" () "y" ("sub" (("tie" . "b")) "a" ))
              ("record" () "x" ("sub" (("tie" . "a")) "a" ))
              ("record" () "w" ("sub" (("tie" . "a")) "e" ))
              ("record" () "v" ("sub" (("tie" . "a")) "d" ))
          ) )
          (value-of-1
            (make-value-of-select (equals-tag-name "sub") value-of-text)
          )
          (value-of-2
            (make-value-of-select (equals-tag-name "sub") (make-value-of-attr "tie"))
          )
          (criteria
            (make-comparison-int-desc
              (list
                (make-comparison-string-int value-of-1)
                (make-comparison-string-int value-of-2)
          ) ) )
          (tags
            (select-tags-rec unsorted (equals-tag-name "record"))
          )
          (sorted
            (sort tags criteria)
        ) )

        (print "-- Sort by text of sub-tag with attribute as tie-breaker (reversed) --")
        (print "Sorted tags:")
        (display-xml (connect-tag unsorted sorted) (current-output-port))
        (separator)
      )

      (let
        ( (sxml
           '("stuff" ()
              ("record" () ("group" () "a") ("info" () "1"))
              ("record" () ("group" () "b") ("info" () "2"))
              ("record" () ("group" () "a") ("info" () "3"))
              ("record" () ("group" () "b") ("info" () "4"))
              ("record" () ("group" () "b") ("info" () "5"))
              ("record" () ("group" () "a") ("info" () "6"))
          ) )
          (value-of-1
            (make-value-of-select (equals-tag-name "group") value-of-text)
        ) )

        (print (group-by (get-children sxml) value-of-1))
      )

) ) )
