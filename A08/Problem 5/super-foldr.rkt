;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname super-foldr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ==========================================================
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignmnet 8, Q5
;; ==========================================================
;;

;;
;; =============================================================================
;; Q3 a) 
;; =============================================================================
;;

;; A nested list of X (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))


;;(super-foldr f base nested-lst) applies foldr to a nested list of elements
;;  Examples
(check-expect (super-foldr + 0 (list 1 (list 5 5 (list 1 3)) (list 10 2) 2)) 29)
(check-expect (super-foldr + 0 empty) 0)

;;super-foldr: (X Y -> Y) Y (nested-listof X) -> Y
(define (super-foldr combine base nested-lst)
  (cond
    [(empty? nested-lst) base]
    [(list? (first nested-lst))
     (combine(super-foldr combine base (first nested-lst))
             (super-foldr combine base (rest nested-lst)))]
    [else (combine(first nested-lst)
                  (super-foldr combine base (rest nested-lst)))]))

;;tests
;;(check-expect (super-foldr - 0 (list 1 (list 5 5 (list 1 3)) (list 10 2) 2)) 9)
;;(check-expect
;; (super-foldr
;;  (lambda (val sofar)
;;    (cond [(string? val) (+ (string-length val) sofar)]
;;          [(number? val) (+ val sofar)]))
;;  0
;;  '("pancho" "lefty" ("my" "proud" "mountains")
;;             ("tecumseh" "valley")))
;; 41)




;;
;; =============================================================================
;; Q3 b) 
;; =============================================================================
;;

;;(magnitudes nl) produes the sum of all the absolute values of the numbers in a
;;  nested list
;;Examples
(check-expect (magnitudes empty) 0)

;;magnitudes: (listof Num) -> Num
(define (magnitudes nl)
  (super-foldr (lambda (first-num rorr) (+ (abs first-num) rorr)) 0  nl))

;;tests 
(check-expect (magnitudes '(1 (-5 -5 (1 -3)) (-10 2) 2)) 29)
;;(check-expect (magnitudes '(1 (5 5 (1 -3)) (10 2) 2)) 29)



;;
;; =============================================================================
;; Q3 c) 
;; =============================================================================
;;

;;(super-filter pred? nested-list) generlizes the filter to work on a nested
;;  lists
;; Examples
(check-expect
 (super-filter odd? '(1 (2 (2 3 4) 5 6 (7 8 9)) 10 11 12))
 '(1 ( (3) 5 (7 9)) 11))


;;super-filter: (X -> Bool) (nested-listof X) --> (nested-listof X)
(define (super-filter pred? nested-list)
  (super-foldr (lambda (x filtered)
                 (cond
                   [(list? x) (cons x filtered)]
                   [(pred? x) (cons x filtered)]
                   [else filtered]))
               empty nested-list))

;;tests 
;;(check-expect(super-filter odd? empty) empty)
;;(check-expect(super-filter even? empty) empty)
;;(check-expect(super-filter even?
;;                           '(1 (2 (2 4 3) 4 6 (7 8 9)) 10 11 12))
;;             '((2 (2 4) 4 6 (8)) 10 12))










             









