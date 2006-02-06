;match-path.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Tell if a path matches

;Compiles expression into finite state machine (FSM).

;Module description
(module sxml-match-path
  (import
    (sxml-match "match.scm")
    (sxml-manip "manip.scm")
  )
  ;(current state, hash of alphabet, table of state transitions, vector of final states)
  ;Alphabet = string -> int, 0 for unknown
  (static
    (class fsm state alphabet trans finals)
  )
  (export
    (make-match-path path)
) )

(define (make-match-path path)
  (letrec
    ( (get-alpha
        (lambda (machine step)
          (coalesce (hashtable-get (fsm-alphabet machine) step) 0)
      ) )

      (take-step
        (lambda (obj step)
          ;Copy fsm except set to new state
          (let ((obj2 (duplicate::fsm obj (state
              ;state=trans[state][alphabet.get(step.name)]
              (vector-ref
                (vector-ref (fsm-trans obj) (fsm-state obj))
                (get-alpha obj (get-tag-name step))
              )
            ))))
            (make-match take-step matches? obj2)
      ) ) )

      (matches?
        (lambda (obj)
          (hashtable-get (fsm-finals obj) (fsm-state obj))
      ) )

      ;Destructively build hash table of alphabet of fsm
      ;Construct vector with string is index, -1=?, -2=*
      (build-alphabet-loop!
        (lambda (path hash vec pos index)
          (if (pair? path)
            (let*
              (
                (step (car path))
                (lookup (hashtable-get hash step))
              )

              (if (and (string? step) (not lookup))
                (begin
                  (hashtable-put! hash step index)
                  (vector-set! vec pos index)
                  (build-alphabet-loop! (cdr path) hash vec (+ pos 1) (+ index 1))
                )

                (cond
                  (lookup 
                    (vector-set! vec pos lookup)
                    (build-alphabet-loop! (cdr path) hash vec (+ pos 1) index)
                  )
                  ((equal? step '?)
                    (vector-set! vec pos -1)
                    (build-alphabet-loop! (cdr path) hash vec (+ pos 1) index)
                  )
                  ((equal? step '*)
                    (vector-set! vec pos -2)
                    (build-alphabet-loop! (cdr path) hash vec (+ pos 1) index)
                  )
                  (#t #f) ;Error, unknown type
            ) ) )

            hash
      ) ) )

      (build-alphabet
        (lambda (path)
          (if (> (length path) 29)
             #f
            (let*
              (
                (vec (make-vector (length path)))
                (hash (build-alphabet-loop! path (make-hashtable) vec 0 1))
              )
              (cons hash vec)
      ) ) ) )

      ;Find which states are reachable and make transition table
      (search-states
        (lambda (path numbranches)
          (letrec
            (
              (hash (make-hashtable))
              (start-state (epsilon-closure 1))

              ;Find where you can get from here without additional input
              (epsilon-closure
                (lambda (state)
0
              ) )

              ;Find where you can go from this state
              (search-states-loop!
                (lambda (state)
;Check if have already been in this state
0
              ) )
            )

            (search-states-loop! start-state)
            (cons hash start-state)
      ) ) )

      ;Destroy hashtable to turn it into a regular table
      (hash->table!
        (lambda (hash)
0
      ) )
    )

;Make fsm
;  Build alphabet
;    Only 29 elements maximum (since 32 bit integer - 2(
;  Search for reachable states
;  Turn hash of states into table

    (make-match take-step matches? path)
) )
