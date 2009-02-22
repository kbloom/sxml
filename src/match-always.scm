;match-always.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Unconditionally say that it matches
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
(module sxml-match-always
  (import
    (sxml-match "match.scm")
  )
  (export
    (make-match-always)
) )

(define (make-match-always)
  (define (take-step obj step)
    (make-match-always)
  )

  (define (matches? obj)
    #t
  )

  (make-match take-step matches? '())
)
