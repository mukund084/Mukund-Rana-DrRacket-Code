;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bs-min) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define-struct node (key left right))

;; A Node is a (make-node Nat BT BT)
;; A binary tree (BT) is one of:
;; * empty
;; * Node


;; (bst-min t) consumes a binary tree and produces the smallest value in the tree 

;;bst-min: BT -> Nat
;; Requires: t to be a non-empty BST
(define (bst-min t)
  (cond[(empty? (node-left t)) (node-key t)]
       [else (min (node-key t) (bst-min (node-left t)))]))



(bst-min (make-node 15 (make-node 8 (make-node 6 empty empty) (make-node 9 empty empty)) (make-node 20 (make-node 16 empty empty) (make-node 22 empty empty))))  