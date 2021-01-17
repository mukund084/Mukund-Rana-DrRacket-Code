;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname not-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***********************************************************
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignment 5, Q1 
;; ***********************************************************
;;

;; ***********************************************************
;; Q1 a)
;; ***********************************************************

(define-struct ls (first rest)) 

;; A (lsof X) is one of:
;; * 'nothing
;; * (make-ls X (lsof X))

;;(ls-length lsany)  produces the length of the list
;;Examples
(check-expect (ls-length
(make-ls "!" (make-ls 'huh (make-ls 42 'nothing)))) 3)

;;ls-length: (lsof Any) -> Nat
;; Requires: 'noting to be the last element 
(define (ls-length lsany)
  [cond
    [(and (symbol? (ls-rest lsany)) (symbol=? (ls-rest lsany) 'nothing)) 1]
    [else (+ 1 (ls-length (ls-rest lsany)))]])


;;tests
(check-expect (ls-length
(make-ls "!" (make-ls 'huh (make-ls 42 (make-ls 56 'nothing))))) 4)

;; ***********************************************************
;; Q1 b)
;; ***********************************************************

;;(ls-max lsnums) produces the largest valune in a (lsof Num)
;; Examples
(check-expect (ls-max (make-ls 5 (make-ls 9 (make-ls 7 'nothing)))) 9)

;;ls-max: (lsof Num) -> Nat
(define (ls-max lsnums)
  (cond
    [(and (symbol? (ls-rest lsnums)) (symbol=? (ls-rest lsnums) 'nothing)) (ls-first lsnums)]
    [else (max (ls-first lsnums) (ls-max (ls-rest lsnums)))]))

;;tests 
(check-expect (ls-max (make-ls 0 (make-ls 0 (make-ls 0 'nothing)))) 0)
(check-expect (ls-max (make-ls 0 (make-ls 7 (make-ls 0 'nothing)))) 7)






