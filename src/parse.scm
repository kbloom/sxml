;parse.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Parses XML into SXML
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
(module sxml-parse
  (export
    (parse-xml port)
) )

#|
State transition summary
state   val newstate
outside   < tagname
tagname   > outside
tagname   ? ques
tagname   ! decl
tagname     attr
attr      " quote
attr      > outside
quote     " attr
decl      - decl1
decl      > outside
decl1     > outside
decl1     - comment
decl1    !- decl
comment   - comment1
comment1 !- comment
comment1  - decl
ques      ? ques1
ques1    !> ques
ques1     > outside
|#
(define rg
  (let ((state 'outside))
  (regular-grammar ()
      ;state outside
      ((when (equal? state 'outside) #\<)
        (set! state 'tagname)
        (cons 'lt (the-string))
      )
      ((when (equal? state 'outside) (+ (out "<")))
        (cons 'str (the-string))
      )

      ;state tagname
      ((when (equal? state 'tagname) #\>)
        (set! state 'outside)
        (cons 'gt (the-string))
      )
      ((when (equal? state 'tagname) #\?)
        (set! state 'ques)
        (cons 'ques (the-string))
      )
      ((when (equal? state 'tagname) #\!)
        (set! state 'decl)
        (cons 'excl (the-string))
      )
      ((when (equal? state 'tagname) #\/)
        (cons 'slash (the-string))
      )
      ((when (equal? state 'tagname) (+ (in " \t\n\r")))
        (set! state 'attr)
        (ignore)
      )
      ((when (equal? state 'tagname) (+ (out ">?!/ \t\n\r")))
        (cons 'str (the-string))
      )

      ;state attr
      ((when (equal? state 'attr) #\>)
        (set! state 'outside)
        (cons 'gt (the-string))
      )
      ((when (equal? state 'attr) #\")
        (set! state 'quote)
        (cons 'quote (the-string))
      )
      ((when (equal? state 'attr) #\=)
        (cons 'equal (the-string))
      )
      ((when (equal? state 'attr) #\/)
        (cons 'slash (the-string))
      )
      ((when (equal? state 'attr) (+ (in " \t\n\r")))
        (ignore)
      )
      ((when (equal? state 'attr) (+ (out ">\"=/ \t\n\r")))
        (cons 'attr (the-string))
      )

      ;state quote
      ((when (equal? state 'quote) #\")
        (set! state 'attr)
        (cons 'quote (the-string))
      )
      ((when (equal? state 'quote) (+ (out "\"")))
        (cons 'quotestr (the-string))
      )

      ;state decl
      ((when (equal? state 'decl) #\>)
        (set! state 'outside)
        (cons 'gt (the-string))
      )
      ((when (equal? state 'decl) #\-)
        (set! state 'decl1)
        (ignore)
      )
      ((when (equal? state 'decl) (+ (out ">-")))
        (ignore)
      )

      ;state decl1
      ((when (equal? state 'decl1) #\>)
        (set! state 'outside)
        (cons 'gt (the-string))
      )
      ((when (equal? state 'decl1) #\-)
        (set! state 'comment)
        (ignore)
      )
      ((when (equal? state 'decl1) (out ">-"))
        (set! state 'decl)
        (ignore)
      )

      ;state comment
      ((when (equal? state 'comment) #\-)
        (set! state 'comment1)
        (ignore)
      )
      ((when (equal? state 'comment) (+ (out "-")))
        (ignore)
      )

      ;state comment1
      ((when (equal? state 'comment1) #\-)
        (set! state 'decl)
        (ignore)
      )
      ((when (equal? state 'comment1) (out "-"))
        (set! state 'comment)
        (ignore)
      )

      ;state ques
      ((when (equal? state 'ques) #\?)
        (set! state 'ques1)
        (ignore)
      )
      ((when (equal? state 'ques) (+ (out "?")))
        (ignore)
      )

      ;state ques1
      ((when (equal? state 'ques1) #\>)
        (set! state 'outside)
        (cons 'gt (the-string))
      )
      ((when (equal? state 'ques1) (out ">"))
        (set! state 'ques)
        (ignore)
) ) ) )

(define (parse-xml port)
  (letrec
    (
      ;Read in 1 token
      (tokenizer
        (lambda ()
          (read/rp rg port)
      ) )

      ;Checks if this is a tag that we are currently ignoring
      (ignore-tag
        (lambda (tag)
          (or
            (not (pair? tag))
            (not (pair? (cdr tag)))
            (equal? (cadr tag) "!")
            (equal? (cadr tag) "?")
      ) ) )

      ;Get first tag as root of XML tree
      (get-first-tag
        (lambda (parsetree)
          (cond
            ((not (pair? parsetree)) "No tags found")
            ((not (pair? (car parsetree))) (get-first-tag (cdr parsetree)))
            (#t (car parsetree))
      ) ) )

      ;Inserts x into list l at index 1
      (insert-1
        (lambda (l x)
          (cons (car l) (cons x (cdr l)))
      ) )

      ;Reads outside of tags
      ;Returns (close tag . list of outside elements) or "error string"
      (get-outside
        (lambda ()
          (let ((token (tokenizer)))
            (cond
              ((not (pair? token)) '(noclose . ()) ) ; .() for readability
              ((equal? (car token) 'lt)
                (let ((tag (get-tag)))
                  (cond
                    ;Is an error
                    ((not (and (pair? tag) (pair? (cdr tag)))) tag)

                    ;Is a closing tag
                    ((car tag)
                      (cons (cadr tag) '())
                    )

                    ;Is a tag to ignore
                    ((ignore-tag tag)
                      (get-outside)
                    )

                    ;Is an ordinary tag
                    (#t
                      (let ((next (get-outside)))
                        (cond
                          ((not (pair? next)) next)
                          (#t (insert-1 next (cdr tag)))
              ) ) ) ) ) )

              ;Is not a tag
              (#t 
                (let ((next (get-outside)))
                (cond
                  ((not (pair? next)) next)
                    (#t (insert-1 next (cdr token)))
      ) ) ) ) ) ) )

      ;Reads in a tag
      ;Returns (close? . tag) or "error string"
      (get-tag
        (lambda ()
          (let ((token (tokenizer)))
            (cond
              ((not (pair? token)) token)
              ((equal? 'slash (car token))
                (let ((token (tokenizer)))
                  (cond
                    ((not (pair? token)) token)
                    ((equal? 'str (car token)) (named-tag (cdr token) #t))
                    (#t "Error inside of close tag")
              ) ) )

              ((member (car token) '(str excl ques))
                (named-tag (cdr token) #f)
              )

              (#t "Error inside tag")
      ) ) ) )

      ;Reads in a tag after the name
      ;Returns tag or "error string"
      (named-tag
        (lambda (name close)
          (cons close
            (cond

              ;Skip special tags
              ((member name '("!" "?"))
                (skip-tag)
                (cons name '())
              )

              ;Close tag
              (close
                (let ((token (tokenizer)))
                  (cond
                    ((not (pair? token)) "Error in closed named-tag")
                    ((equal? 'gt (car token)) (cons name '(())))
                    (#t "Extra junk in close tag")
              ) ) )

              ;Regular tag
              (#t
                (let ((attr (get-attr)))
                  (cond
                    ;Error message
                    ((not (pair? attr)) attr)

                    ;Self-closing tag
                    ((car attr)
                      (cons name (list (cdr attr)))
                    )

                    ;Ordinary tag
                    (#t
                      (let ((contained (get-outside)))
                        (cond
                          ;Error message
                          ((not (pair? contained)) contained)

                          ;Wrong closing tag
                          ((not (equal? name (car contained)))
                            "Close tag does not match open tag"
                          )

                          ;Finally, a complete tag!
                          (#t (cons name (cons (cdr attr) (cdr contained))))
      ) ) ) ) ) ) ) ) ) )

      ;Skips ahead until tag close
      (skip-tag
        (lambda ()
          (let ((token (tokenizer)))
            (cond
              ((not (pair? token)) token)
              ((equal? 'gt (car token)) 'gt)
              (#t (skip-tag))
      ) ) ) )

      ;Reads the attributes in a tag
      ;Returns (self-close? . attr)
      (get-attr
        (lambda ()
          (let ((token (tokenizer)))
            (cond
              ;Error message
              ((not (pair? token)) token)

              ;Self-close
              ((equal? 'slash (car token))
                (let ((token (tokenizer)))
                  (cond
                    ;Error message
                    ((not (pair? token)) token)

                    ;Not a > to close the tag
                    ((not (equal? 'gt (car token)))
                      "Expect close tag after self-close tag slash"
                    )

                    ;The end of a self-closing tag
                    (#t '(#t . ())) ; .() for readability
              ) ) )

              ;Attribute name
              ((equal? 'attr (car token))
                (let
                  (
                    (attrname (cdr token))
                    (token (tokenizer))
                  )

                  (cond
                    ((not (pair? token)) token)
                    ((not (equal? 'equal (car token)))
                      "Expect = after attribute name"
                    )
                    (#t
                      (let ((token (tokenizer)))
                        (cond
                          ((not (pair? token)) token)
                          ((not (equal? 'quote (car token)))
                            "Expect quote after = in attribute"
                          )
                          (#t
                            (let ((token (tokenizer)))
                              (cond
                                ((not (pair? token)) token)

                                ;Empty string
                                ((equal? 'quote (car token))
                                  (named-attr (cons attrname ""))
                                )
                                ((not (equal? 'quotestr (car token)))
                                  "Expect quote after = in attribute"
                                )

                                (#t
                                  (let
                                    (
                                      (attrstr (cdr token))
                                      (token (tokenizer))
                                    )
                                    (cond
                                      ((not (pair? token)) token)
                                      ((not (equal? 'quote (car token)))
                                        "Expect quote after = in attribute"
                                      )
                                      (#t
                                        (named-attr (cons attrname attrstr))
              ) ) ) ) ) ) ) ) ) ) ) ) )

              ;The end of an ordinary tag
              (#t '(#f . ())) ; .() for readability

              ;Out-of-place character
              (#t "Unexpected character in tag attributes")
      ) ) ) )

      ;Handles recursive call to get-attr
      ;Returns (self-close? . attr)
      (named-attr
        (lambda (attr)
          (let ((next (get-attr)))
            (cond
              ((not (pair? next)) next)
              (#t (insert-1 next attr))
    ) ) ) ) )

    (let ((parsetree (get-outside)))
      (cond
        ((not (pair? parsetree)) parsetree)
        ((not (equal? 'noclose (car parsetree))) "Extra close tag")
        (#t (get-first-tag (cdr parsetree)))
) ) ) )
