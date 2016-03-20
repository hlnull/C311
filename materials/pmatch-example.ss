; Consider the following grammar describing binary trees as Scheme pairs:
; empty_tree ::= ()
; binary_tree ::= (t0 . t1)

; We can use pmatch to directly use this grammar to define recursive 
; functions over binary trees. For instance, consider the simple function
; which counts the number of nodes in a binary tree:
(load "pmatch.scm")

(define count-nodes
  (lambda (tree)
    (pmatch tree
      [() () 0]
      [(,t0 . ,t1) () (+ 2 (count-nodes t0) (count-nodes t1))]
      [,else (error 'count-nodes "Error: ~ is not a binary tree" else)])))
      
; In general, pmatch can be used to directly correspond to given
; grammars that describe languages or data structures. It is best to
; include a clause for every expansion in the grammar, and an error
; clause to specifically handle with input that is not part of the grammar
; (although pmatch will give an unmatched error message for this).
