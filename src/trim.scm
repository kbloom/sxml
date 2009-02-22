;trim.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Trim trailing and leading white spaces out
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
(module sxml-trim
  (import
    (sxml-template "template.scm")
    (sxml-manip    "manip.scm")
  )
  (export
    (trim str)
    (trim-all sxml)
) )

(define (leading-spaces str len)
  (letrec
    ( (leading-spaces-loop
        (lambda (pos)
          (if (>= pos len)
            pos
            (let ((c (string-ref str pos)))
              (if (<= (char->integer c) 32)
                (leading-spaces-loop (+ pos 1))
                pos
    ) ) ) ) ) )

    (leading-spaces-loop 0)
) )

(define (trailing-spaces str len)
  (letrec
    ( (trailing-spaces-loop
        (lambda (pos)
          (if (< pos 0)
            (+ pos 1)
            (let ((c (string-ref str pos)))
              (if (<= (char->integer c) 32)
                (trailing-spaces-loop (- pos 1))
                (+ pos 1)
    ) ) ) ) ) )

    (trailing-spaces-loop (- len 1))
) )

;Gak.  Imperative style.
(define (trim str)
  (let*
    (
      (len (string-length str))
      (lead  (leading-spaces  str len))
      (trail (trailing-spaces str len))
      (empty (<= trail lead))
      (str2
        (if empty
          ""
          (substring str lead trail)
      ) )
      (rl
        (if (or empty (<= lead 0))
          str2
          (string-append " " str2)
      ) )
      (rt
        (if (or empty (>= trail len))
          rl
          (string-append rl " ")
    ) ) )

    rt
) )

(define (trim-all-apply root branch templates param position)
  (letrec
    ( (trim-children
        (lambda (sxml processed)
          (if (not (pair? sxml))
            sxml
            (let*
              (
                (a (car sxml))
                (b
                  (trim-children
                    (cdr sxml)
                    (if (tag? a)
                      (cdr processed)
                      processed
              ) ) ) )

              (cond
                ((string? a)
                  (let ((trimmed (trim a)))
                    (if (= (string-length trimmed) 0)
                      b
                      (cons trimmed b)
                ) ) )
                ((tag? a) (cons (car processed) b))
                (#t (cons a b))
    ) ) ) ) ) )

    (connect-tag
      branch
      (trim-children
        (get-children branch)
        (apply-templates-children root branch templates param)
) ) ) )

(define (trim-all sxml)
  (let*
    ( (templates
        (list
          (cons (make-match-always) trim-all-apply)
      ) )
      (result
        (apply-templates sxml templates '())
    ) )

    (if (pair? result)
      (car result)
      result
) ) )
