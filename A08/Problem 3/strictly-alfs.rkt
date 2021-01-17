;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname strictly-alfs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ==========================================================
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignmnet 8, Q3 
;; ==========================================================
;;

;;
;; =============================================================================
;; Q3 a) 
;; =============================================================================
;;

;;(occurrences num lst-num) counts the number of occurrences of num in lst-num
;;Example
(check-expect (occurrences 2 '(1 2 1 2 2 3 1)) 3)
(check-expect (occurrences 2 empty) 0)

;;occurrences: Num (listof Num) -> Num
(define (occurrences num lst-num)
  (length (filter (lambda (is-num) (= is-num num)) lst-num)))

;;tests
(check-expect (occurrences 1 '(1 2 1 2 2 3 1)) 3)
(check-expect (occurrences 3 '(1 2 1 2 2 3 1)) 1)



;;
;; =============================================================================
;; Q3 b) 
;; =============================================================================
;;

;;(zip lst1 lst2) produces a lst of pairs where the ith pair contains the ith element
;;  from the first list followed by ith element of the second list
;;Examples
(check-expect (zip '(1 2 3) '(a b c)) '((1 a)(2 b)(3 c)))

;;zip: (listof Any) (listof Any) -> (listof (list Any Any)) 
;;  Requries: Both lst1 lst2 must be the same length
(define (zip lst1 lst2)
  (foldr (lambda (a b rorr) (cons (list a b) rorr)) empty lst1 lst2))

;;tests
(check-expect (zip '(1 2 3) '(4 5 6)) '((1 4)(2 5)(3 6)))
(check-expect (zip empty empty) empty)

;;
;; =============================================================================
;; Q3 c) 
;; =============================================================================
;;


;;(unzip lst-pairs) produces two list, where the first list contains elements from
;;  each pair and second list contains second element from each pair
;;Examples
(check-expect (unzip '()) '(()()))

;;unzip: (listof (list Any Any)) -> (listof Any) (listof Any)
(define (unzip lst-pairs)
  (list (map (lambda (a) (first a)) lst-pairs)
        (map (lambda (b) (second b)) lst-pairs)))

;;tests 
(check-expect (unzip '((1 a)(2 b)(3 c))) '((1 2 3) (a b c)))




;;
;; =============================================================================
;; Q3 d) 
;; =============================================================================
;;

;;(subsequence lst from to) produces the subsequence from lst that begins at index
;; from and ends just before index to
;;Example
(check-expect (subsequence '(a b c d e f g) 1 4) '(b c d))
(check-expect (subsequence empty 0 400) empty)

;;subsequence: (listof Any) Nat Nat -> (listof Any) 
(define (subsequence lst from to)
  (map (lambda (b) (second b))
       (filter (lambda (x) (and (>= (first x) from) (<= (first x) (- to 1))))
               (foldr (lambda (a b rorr) (cons (list a b) rorr)) empty
                      (build-list (length lst)  (lambda (x) x)) lst ))))

;;tests 
(check-expect (subsequence '(a b c d e f g) 1 1) '())
(check-expect (subsequence '(a b c d) 0 400) '(a b c d))

