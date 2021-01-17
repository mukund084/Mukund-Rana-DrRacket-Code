;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname vector) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***********************************************************
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignment 4, Q2
;; ***********************************************************
;;

;;
;; ***********************************************************
;; Q2 a)
;; ***********************************************************
;;

;;(euclidean-norm lstvectors) produces the Euclidean Norm of a vector
;; Example:
;;(check-within (euclidean-norm (list 3 4)) 5 0.01)

;; euclidean-norm: (listof Nums)  → Num
(define (euclidean-norm lstvectors)
  (sqrt (sum-square lstvectors)))

;;tests
(check-within (euclidean-norm (list 3 4 3 1)) 5.91 0.01)
(check-within (euclidean-norm (list 0 1))1 0.01)
(check-within (euclidean-norm (list 1 1))1.41 0.01)
(check-within (euclidean-norm (list -2 1)) 2.23 0.01)
(check-within (euclidean-norm (list -2 -3)) 3.60 0.01)


;;(sum-square lstvectors) produces the squared sum of the componenets of a
;;  vector
;;Examples

;;sum-square: (listof Nums) -> Num
(define (sum-square lstvectors)
  (cond
    [(empty? lstvectors) 0]
    [else (+(expt (first lstvectors) 2) (sum-square (rest lstvectors)))]))

;;tests


;;
;; ***********************************************************
;; Q2 b)
;; ***********************************************************
;;

;;(unit-vector lstvectors) Produces the unit vector of a vector
;; Examples
(check-within (unit-vector (list 3 4)) (list 0.6 0.8) 0.01)

;;unit-vector: (listof Nums) -> (listof Nums) 
(define (unit-vector lstvectors)
  (unit-vectorcalcution lstvectors (euclidean-norm lstvectors)))

;;test
(check-within (unit-vector (list 3 4 3 1)) (list 0.5 0.67 0.5 0.16) 0.01)
(check-within (unit-vector (list 0 1)) (list 0 1) 0.01)
(check-within (unit-vector (list 1 1)) (list 0.70 0.70) 0.01)
(check-within (unit-vector (list -2 -3)) (list -0.55 -0.83) 0.01)
(check-within (unit-vector (list -2 1)) (list -0.89 0.44) 0.01)



;;(unit-vectorcalcution lstvectors) Produces the values of the
;;  unit vector of a vector
;; Examples

;;unit-vector: (listof Nums) -> (listof Nums) 
(define (unit-vectorcalcution lstvectors euclidean-normvector)
  (cond
    [(empty? lstvectors) empty]
    {else (cons (/ (first lstvectors)  euclidean-normvector)
          (unit-vectorcalcution (rest lstvectors) euclidean-normvector))}))

;;tests 

;;
;; ***********************************************************
;; Q2 c)
;; ***********************************************************
;;

;;(cos-between lon1 lon2) computes the dot product
;;  the unit vectors of lon1 and lon2 to find the
;;  cosine of the angle vector lon1 and lon2
;;Examples
(check-within (cos-between (list 3 4) (list 0 6)) 0.8 0.01)

;;cos-between: (listof Num) (listof Num) → Num
(define (cos-between lon1 lon2)
  (dot-product  (unit-vector lon1)  (unit-vector lon2)))

;;tests 
(check-within (cos-between (list 3 7) (list 8 9)) 0.94 0.01)
(check-within (cos-between empty (list 8 9)) 0 0.01)


;;(dot-product lon1 lon2) computes the dot product
;;  of the two vectors 1on1 and lon2
;;Examples 

;; dot-product: (listof Num) (listof Num) → Num
;; requires: lon1 and lon2 are the same length
(define (dot-product lon1 lon2)
(cond
[(empty? lon1) 0]
[else (+ (* (first lon1) (first lon2))
(dot-product (rest lon1) (rest lon2)))]))


;;tests 






