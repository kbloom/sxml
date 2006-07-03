;sxml.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Manipulate tags

;Module description
(module sxml-manip
  (export
    (caar? x)
    (cadr? x)
    (cadar? x)
    (tag? tag)
    (coalesce x y)
    (append-list l)
    (string-append-list l)
    (hashtable-keyval->vector h)
    (hashtable-key->vector h)
    (get-children tag)
    (get-tag-name tag)
    (get-attr tag name)
    (set-attr tag name val)
    (del-attr tag name)
    (select-tags sxml equals)
    (select-tags-rec sxml equals)
    (get-text tag)
    (get-text-rec tag)
    (connect-tag head body)
) )

;
;
;Miscellaneous Helper functions
;
;

;Alway start with a because if can car, can cdr
;car? = pair?

(define (caar? x)
  (and (pair? x) (pair? (car x)))
)

(define (cadr? x)
  (and (pair? x) (pair? (cdr x)))
)

(define (cadar? x)
  (and (pair? x) (pair? (car x)) (pair? (cdar x)))
)

(define tag? cadr?)

(define get-children cddr)

;
;
;Other misc functions
;
;

(define (coalesce x y)
  (if x
    x
    y
) )

(define (append-list l)
  (if (pair? l)
    (append (car l) (append-list (cdr l)))
    l
) )

(define (string-append-list l)
  (if (pair? l)
    (string-append (car l) (string-append-list (cdr l)))
    l
) )

(define (hashtable-keyval->vector h)
  (let
    ( (index 0)
      (ret (make-vector (hashtable-size h)))
    )

    ;Have to use conventional for loop because restricted for-each format
    (hashtable-for-each h
      (lambda (key val)
        (vector-set! ret index (cons key val))
        (set! index (+ index 1))
    ) )
    ret
) )

(define (hashtable-key->vector h)
  (let
    ( (index 0)
      (ret (make-vector (hashtable-size h)))
    )

    (hashtable-for-each h
      (lambda (key val)
        (vector-set! ret index key)
        (set! index (+ index 1))
    ) )
    ret
) )

;
;
;Methods for manipulating tags
;
;

(define (get-tag-name tag)
  (cond
    ((not (tag? tag)) #f)
    (#t (car tag))
) )

(define (get-attr tag name)
  (letrec
    ( (find-attr
        (lambda (pos)
          (cond
            ((not (caar? pos)) #f)
            ((equal? name (caar pos)) (cdar pos))
            (#t (find-attr (cdr pos)))
    ) ) ) )

    (cond
      ((not (tag? tag)) #f)
      (#t (find-attr (cadr tag)))
) ) )

(define (set-attr tag name val)
  (letrec
    ( (set-attr-loop
        (lambda (pos)
          (cond
            ((not (caar? pos)) (cons (cons name val) '()))
            ((equal? name (caar pos)) (cons (cons name val) (cdr pos)))
            (#t (cons (car pos) (set-attr-loop (cdr pos))))
    ) ) ) )

    (cond
      ((not (tag? tag)) #f)
      (#t (cons (car tag) (cons (set-attr-loop (cadr tag)) (cddr tag))))
) ) )

(define (del-attr tag name)
  (letrec
    ( (del-attr-loop
        (lambda (pos)
          (cond
            ((not (caar? pos)) '())
            ((equal? name (caar pos)) (cdr pos))
            (#t (cons (car pos) (del-attr-loop (cdr pos))))
    ) ) ) )

    (cond
      ((not (tag? tag)) #f)
      (#t (cons (car tag) (cons (del-attr-loop (cadr tag)) (cddr tag))))
) ) )

(define (select-tags sxml equals)
  (letrec
    ( (select-tags-loop
        (lambda (sxml)
          (if (pair? sxml)
            (let
              (
                (a (car sxml))
                (b (select-tags-loop (cdr sxml)))
              )

              (if (tag? a)
                (if (equals a)
                  (cons a b)
                  b
                )
                b
            ) )

           '()
    ) ) ) )

    (cond
      ((not (tag? sxml)) '())
      (#t (select-tags-loop (get-children sxml)))
) ) )

(define (select-tags-rec sxml equals)
  (letrec
    ( (select-tags-rec-loop
        (lambda (sxml next)
          (if (pair? sxml)
            (let
              (
                (a (car sxml))
                (b (select-tags-rec-loop (cdr sxml) next))
              )

              (if (tag? a)
                (if (equals a)
                  (cons a b)
                  (select-tags-rec-loop (get-children a) b)
                )
                b
            ) )

            next
    ) ) ) )

    (cond
      ((not (tag? sxml)) '())
      ((equals sxml) sxml)
      (#t (select-tags-rec-loop (get-children sxml) '()))
) ) )

(define (get-text tag)
  (letrec
    ( (get-text-loop
        (lambda (elements)
          (if (not (pair? elements))
            ""
            (let
              (
                (a (car elements))
                (b (get-text-loop (cdr elements)))
              )
              (if (string? a)
                (string-append a b)
                b
    ) ) ) ) ) )

    (if (not (tag? tag))
      ""
      (get-text-loop (cddr tag))
) ) )

(define (get-text-rec tag)
  (letrec
    ( (get-text-loop
        (lambda (elements next)
          (if (not (pair? elements))
            next
            (let
              (
                (a (car elements))
                (b (get-text-loop (cdr elements) next))
              )
              (cond
                ((tag? a) (get-text-loop (cddr a) b))
                ((string? a) (string-append a b))
                (#t b)
    ) ) ) ) ) )

    (if (not (tag? tag))
      ""
      (get-text-loop (cddr tag) "")
) ) )

(define (connect-tag head body)
  (if (tag? head)
    (cons
      (car head)
      (cons
        (cadr head)
        body
    ) )
    head
) )
