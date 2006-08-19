;match-path.scm
;Sterling Stuart Stein
;Library to work with xml as s-expressions (sxml)
;Tell if a path matches (regular-expression-like)

;Compiles expression into finite state machine (FSM).
;30 states maximum for FSM and NFSM since using bitsets
;Language: "string" = itself, ? = any string, * = 0 or more ?
;Examples:
;("html" "body")     = /html/body
;(* "div" "b")       = div/b
;(* "div" * "table") = div//table
;(* "table" ? "tr")  = table/*/tr

;Module description
(module sxml-match-path
  (include
    "macro.sch"
  )
  (import
    (sxml-match "match.scm")
    (sxml-manip "manip.scm")
  )
  (export
    (make-match-path path)
) )

;Structs used
(define-struct fsm states finalmask startstate alphahash)
(define-struct deter index state) ;For numbering the states
;states is a vector (state) of vector (transition) of int (destination state)
;alphahash is hashtable of string -> index, last+1=other

(define-struct edge letter transition)
(define-struct nfsm states finalmask alphahash)
;states is a vector of edges
;letter can be number from alphahash, ? for any, or epsilon for no symbol
;start at state 0

;Make FSM

(define (get-alphahash x)
  (letrec
    ( (hash (make-hashtable))
      (vec 0)

      (unique-loop!
        (lambda (p)
          (if (pair? p)
            (begin
              (let ((str (car p)))
                (if (string? str)
                  (hashtable-put! hash str #f) ;Put all strings in hashtable
                  #unspecified
              ) )
              (unique-loop! (cdr p))
            )
            #unspecified
      ) ) )

      (index-loop!
        (lambda (i)
          (if (< i 0)
            #unspecified
            (begin
              (hashtable-put! hash (vector-ref vec i) i)
              (index-loop! (- i 1))
    ) ) ) ) )

    (unique-loop! x)
    (set! vec (hashtable-key->vector hash))
    (index-loop! (- (vector-length vec) 1))
    hash
) )

(define (list->nfsm exp)
  (let*
    ( (alpha (get-alphahash exp))
      (numstates (+ (length exp) 1))
      (n (nfsm (make-vector numstates)
            (bit-lsh 1 (- numstates 1)) alpha))
    )

    (letrec
      ( (state-loop!
          (lambda (p i)
            (if (pair? p)
              (let ((l (car p)))
                (vector-set! (nfsm-states n) i
                  (cond
                    ((string? l) ;Ordinary string
                     `(,(edge (hashtable-get alpha l) (+ i 1)))
                    )
                    ((eq? l '?) ;Match any
                     `(,(edge '? (+ i 1)))
                    )
                    ((eq? l '*) ;Kleene star
                     `(
                       ,(edge '? i)
                       ,(edge 'epsilon (+ i 1))
                    ) )
                    (#t (print "Junk in NFSM expression")) ;Something else
                ) )
                (state-loop! (cdr p) (+ i 1))
              )
              (begin
                (vector-set! (nfsm-states n) i '()) ;No way out of final state
                n
      ) ) ) ) )

      (state-loop! exp 0)
) ) )

;Very destructive internally
(define (nfsm->fsm n)
  (letrec
      ;Find epsilon closure of state in nfsm
    ( (epsilon-closure
        (lambda (statemask)
          (letrec
              ;Follow epsilon edges from given state
            ( (epsilon-edge
                (lambda (mask state)
                  (letrec
                    ( (edge-loop
                        (lambda (edge mask)
                          (if (pair? edge)
                            (edge-loop (cdr edge)
                              (if (eq? (edge-letter (car edge)) 'epsilon)
                                (let ((next (edge-transition (car edge))))
                                  ;if you haven't been there yet
                                  (if (= 0 (bit-and mask (bit-lsh 1 next)))
                                    (bit-or mask (epsilon-edge mask next))
                                    mask
                                ) )
                                mask
                            ) )
                            mask
                    ) ) ) )

                    (edge-loop (vector-ref (nfsm-states n) state)
                      (bit-or mask (bit-lsh 1 state))
              ) ) ) )

              ;Check which states are in mask
              (state-iterate-loop
                (lambda (statemask index)
                  (if (< index (NUMBITS))
                    (state-iterate-loop
                      ;If state is not in mask
                      (if (= 0 (bit-and statemask (bit-lsh 1 index)))
                        statemask
                        ;Union in states reachable by epsilon edges
                        (bit-or statemask (epsilon-edge statemask index))
                      )
                      (+ index 1)
                    )
                    statemask
            ) ) ) )

            (state-iterate-loop statemask 0)
      ) ) )

      ;Which states have been found in dfs
      (hash (make-hashtable))

      ;Search through states in nfsm to find which are reachable
      (dfs!
        (lambda (mask)
          (if (hashtable-get hash mask)
            #unspecified ;Already did this one

            (letrec
                ;State transition table (for newly found state)
              ( (v (make-vector (+ (hashtable-size (nfsm-alphahash n)) 1) 0))

                ;or with all elements in a vector
                (vector-or!
                  (lambda (v x)
                    (letrec
                      ( (or-loop!
                          (lambda (i)
                            (if (< i 0)
                              #unspecified

                              (begin
                                (vector-set! v i
                                  (bit-or (vector-ref v i) x)
                                )
                                (or-loop! (- i 1))
                      ) ) ) ) )

                      (or-loop! (- (vector-length v) 1))
                ) ) )

                ;Follow all edges in nfsm state
                (edge-loop!
                  (lambda (edge)
                    (if (pair? edge)
                      (let*
                        ( (e (car edge))
                          (letter (edge-letter e))
                          (trans (bit-lsh 1 (edge-transition e)))
                        )

                        (cond
                          ( (eq? letter '?) (vector-or! v trans))
                          ( (integer? letter)
                              (vector-set! v letter
                                (bit-or (vector-ref v letter) trans)
                          ) )
                          ;Else ignore
                        )
                        (edge-loop! (cdr edge))
                      )

                      #unspecified
                ) ) )

                (state-loop!
                  (lambda (state)
                    (if (< state (NUMBITS))
                      (begin
                        (if (= (bit-and mask (bit-lsh 1 state)) 0)
                          #unspecified
                          (edge-loop! (vector-ref (nfsm-states n) state))
                        )
                        (state-loop! (+ state 1))
                      )

                      #unspecified
                ) ) )

                (explore-loop!
                  (lambda (i)
                    (if (< i 0)
                      #unspecified

                      (begin
                        (vector-set! v i (epsilon-closure (vector-ref v i)))
                        (dfs! (vector-ref v i))
                        (explore-loop! (- i 1))
              ) ) ) ) )

              (hashtable-put! hash mask (deter 0 v))
              (state-loop! 0)
              (explore-loop! (- (vector-length v) 1))
      ) ) ) )

      (startstate (epsilon-closure 1))
      (finalmask #unspecified)
      (states #unspecified)

      ;Calculate finalmask and convert hash to states
      (index-loop!
        (lambda ()
          (letrec
            ( (v (hashtable-keyval->vector hash))
              ;Add array indexes to hash
              (ind-loop!
                (lambda (fm i)
                  (if (< i 0)
                    fm

                    (begin
                      (deter-index-set! (cdr (vector-ref v i)) i)
                      (ind-loop!
                        (if (= (bit-and (car (vector-ref v i))
                                        (nfsm-finalmask n)) 0)
                          fm
                          (bit-or fm (bit-lsh 1 i))
                        )
                        (- i 1)
              ) ) ) ) )

              ;Changes from bitset state numbers in transitions to simple index
              (clean-state!
                (lambda (s)
                  (letrec
                    ( (trans-loop!
                        (lambda (i)
                          (if (< i 0)
                            #unspecified

                            (begin
                              (vector-set! s i (deter-index
                                  (hashtable-get hash (vector-ref s i)))
                              )
                              (trans-loop! (- i 1))
                    ) ) ) ) )

                    (trans-loop! (- (vector-length s) 1))
              ) ) )

              ;Extract hash to states
              (extract-loop!
                (lambda (i)
                  (if (< i 0)
                    #unspecified

                    (let ((s (deter-state (cdr (vector-ref v i)))))
                      (vector-set! states i s)
                      (clean-state! s)
                      (extract-loop! (- i 1))
            ) ) ) ) )

            (set! states (make-vector (vector-length v)))
            (set! finalmask (ind-loop! 0 (- (vector-length v) 1)))
            (extract-loop! (- (vector-length v) 1))
    ) ) ) )

    (dfs! startstate) ;Populate hash
    (index-loop!)     ;Index hash, set finalmask
    (fsm states finalmask
      (deter-index (hashtable-get hash startstate))
      (nfsm-alphahash n)
) ) )





;Actual make-match-path definition

(define (make-match-path path)
  (letrec
    ( (f (nfsm->fsm (list->nfsm path)))
      (take-step
        (lambda (obj step)
          (let*
            ( (c (hashtable-get (fsm-alphahash f) (get-tag-name step)))
              (next
                (vector-ref (vector-ref (fsm-states f) obj)
                  (if c c (hashtable-size (fsm-alphahash f)))
            ) ) )

            (make-match take-step matches? next)
      ) ) )

      (matches?
        (lambda (obj)
          (not (= (bit-and (fsm-finalmask f) (bit-lsh 1 obj)) 0))
    ) ) )

    (make-match take-step matches? (fsm-startstate f))
) )
