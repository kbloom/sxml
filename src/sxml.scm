;sxml.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;This is the main include.  It includes all of the other small pieces

;Module description
(module sxml
  (from
    (sxml-parse    "parse.scm")
    (sxml-display  "display.scm")
    (sxml-manip    "manip.scm")
    (sxml-sort     "sort.scm")
    (sxml-template "template.scm")
    (sxml-trim     "trim.scm")
    (sxml-verify   "verify.scm")
) )
