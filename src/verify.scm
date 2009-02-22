;verify.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Verify that the sxml is actually valid
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
