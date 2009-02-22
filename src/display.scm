;display.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Display SXML as XML
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
(module sxml-display
  (import
    (sxml-manip "manip.scm")
  )
  (export
    (display-xml sxml output-port)
) )

(define (read-to delimiter input-port)
  (letrec
    ( (delimiter2 (cons #eof-object delimiter)) ;Always quit at end
      (read-to-loop
        (lambda (accumulator buffer pos)
          (let
            ((next (read-char input-port)))

            (cond
              ( (member next delimiter2)
                (cons next (string-append accumulator (substring buffer 0 pos)))
              )
              ( #t
                (string-set! buffer pos next)
                (if (= (+ pos 1) (string-length buffer))
                  (read-to-loop (string-append accumulator buffer) (make-string (+ (string-length accumulator) (string-length buffer))) 0) ;Fibonacci growth
                  (read-to-loop accumulator buffer (+ pos 1))
    ) ) ) ) ) ) )

    (read-to-loop "" (make-string 10) 0) ;Default string length
) )

;Convert sxml to XML and display to output-port
(define (display-xml sxml output-port)
  (letrec
    (
      (display-xml-attributes
        (lambda (attributes)
          (cond
            ((not (caar? attributes)) #unspecified)
            (#t
              (display " " output-port)
              (display (caar attributes) output-port)
              (display "=\"" output-port)
              (display (cdar attributes) output-port)
              (display "\"" output-port)

              (display-xml-attributes (cdr attributes))
      ) ) ) )

      (display-xml-children
        (lambda (children)
           (cond
            ((not (pair? children)) #unspecified)
            (#t
              (display-xml          (car children) output-port)
              (display-xml-children (cdr children))
    ) ) ) ) )

    (cond
      ((not (tag? sxml)) (display sxml output-port))
      (#t
        (let
          ( (attributes (cadr sxml))
            (children   (cddr sxml))
          )

          (display "<" output-port)
          (display (car sxml) output-port)
          (display-xml-attributes attributes)
          (cond
            ((null? children)
              (display "/>" output-port)
            )
            (#t
              (display ">" output-port)

              (display-xml-children children)

              (display "</" output-port)
              (display (car sxml) output-port)
              (display ">" output-port)
) ) ) ) ) ) )
