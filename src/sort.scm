;sort.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Sort tags, including by multiple criteria

;Module description
(module sxml-sort
  (import
    (sxml-manip "manip.scm")
  )
  (export
    (make-comparison comparators)
    (make-comparison-int comparators)
    (reverse-sort-order-int comp)
    (equals-tag-name name)
    (value-of-text tag)
    (make-value-of-attr attr)
    (make-value-of-select equals value-of)
    (make-value-of-select-rec equals value-of)
    (make-value-of-first value-of)
    (make-comparison-string-int value-of)
    (make-comparison-number-int value-of)
    (just-children branch tagname)
    (just-text branch tagname)
    (group-by x value-of)
) )

;Could be optimized to do 1 less comparison at end
(define (do-comparison comparators x y)
  (if (pair? comparators)
    (cond
      (((car comparators) x y) #t)               ;-1, <
      (((car comparators) y x) #f)               ; 1, >
      (#t (do-comparison (cdr comparators) x y)) ; 0, =, next compare
    )
    #f
) )

(define (make-comparison comparators)
  (lambda (x y)
    (do-comparison comparators x y) ;Curry to other function
) )

(define (do-comparison-int comparators x y)
  (if (pair? comparators)
    (let ((r ((car comparators) x y)))
      (cond
        ((< r 0) #t)                                   ;-1, <
        ((> r 0) #f)                                   ; 1, >
        (#t (do-comparison-int (cdr comparators) x y)) ; 0, =, next compare
    ) )
    #f
) )

(define (make-comparison-int comparators)
  (lambda (x y)
    (do-comparison-int comparators x y) ;Curry to other function
) )

;Make comparison go in opposite direction
(define (reverse-sort-order-int comp)
  (lambda (x y)
    (- (comp x y))
) )

(define (equals-tag-name name)
  (lambda (x)
    (equal? name (get-tag-name x))
) )

(define value-of-text get-text-rec)

(define (make-value-of-attr attr)
  (lambda (x)
    (coalesce (get-attr x attr) "")
) )

(define (value-of-select-loop tags value-of)
  (letrec
    ( (vosloop
        (lambda (x)
          (if (pair? x)
            (string-append (value-of (car x)) (vosloop (cdr x)))
            ""
    ) ) ) )

    (vosloop tags)
) )

(define (make-value-of-select equals value-of)
  (lambda (x)
    (value-of-select-loop (select-tags x equals) value-of)
) )

(define (make-value-of-select-rec equals value-of)
  (lambda (x)
    (value-of-select-loop (select-tags-rec x equals) value-of)
) )

(define (make-value-of-first value-of)
  (letrec
    ( (vof
        (lambda (x)
          (if (pair? x)
            (or (value-of (car x)) (vof (cdr x)))
            #f
    ) ) ) )

    vof
) )

(define (make-comparison-string-int value-of)
  (lambda (x y)
    (let
      (
        (a (value-of x))
        (b (value-of y))
      )
      (cond
        ((string<? a b) -1)
        ((string>? a b)  1)
        (#t 0)
) ) ) )

(define (make-comparison-number-int value-of)
  (lambda (x y)
    (let
      (
        (a (string->number (value-of x)))
        (b (string->number (value-of y)))
      )
      (cond
        ((not (number? a)) -1)
        ((not (number? b))  1)
        ((< a b) -1)
        ((> a b)  1)
        (#t 0)
) ) ) )

(define (just-children branch tagname)
  (append-list
    (map get-children (select-tags branch (equals-tag-name tagname)))
) )

(define (just-text branch tagname)
  (get-text-rec
    (connect-tag branch (select-tags branch (equals-tag-name tagname)))
) )

(define (group-by x value-of)
  (letrec
    ( (hash
        (make-hashtable)
      )
      (group-by-loop!
        (lambda (pos)
          (if (pair? pos)
            (let*
              (
                (item (car pos))
                (key (value-of item))
                (has (hashtable-get hash key))
                (newitem
                  (cons
                    item
                    (coalesce has '())
              ) ) )

              (hashtable-put! hash key newitem)
              (group-by-loop! (cdr pos))
            )

            #unspecified
    ) ) ) )

    (group-by-loop! x)
    (hashtable->list hash)
) )
