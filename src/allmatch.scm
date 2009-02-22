;allmatch.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Include files that fill in the matching class
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
(module sxml-allmatch
  (from
    (sxml-match        "match.scm")
    (sxml-match-exact  "match-exact.scm")
    (sxml-match-path   "match-path.scm")
    (sxml-match-always "match-always.scm")
) )

