;match.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Tell if a path matches an expression (XPath)
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
(module sxml-match
  (export
    (class match take-step matches? pos)
    (take-step obj step)
    (matches? obj)
    (all-step templates tagname)
) )

;Interface definition

;1st function
(define (take-step obj step)
  ((match-take-step obj) (match-pos obj) step)
)

;2nd function
(define (matches? obj)
  ((match-matches? obj) (match-pos obj))
)

;Take a step in the tree
(define (all-step templates tagname)
  (if (and (pair? templates) (pair? (car templates)))
    (cons
      (cons (take-step (caar templates) tagname) (cdar templates))
      (all-step (cdr templates) tagname)
    )
   '()
) )
