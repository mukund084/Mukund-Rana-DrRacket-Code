;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ==========================================================
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignmnet 9, Q1 
;; ==========================================================
;;

;;
;; =============================================================================
;; Q1 a) 
;; =============================================================================
;;


;;(flip-case lst-string) procues a new list depending on the length of lst-string
;;  if the length of lst-string is between 0 and 2 its returns the same string and
;;  if the length of lst-string is greater than 3 it applies the changes in the
;;  assignment

;;Examples
(check-expect (flip-case '("Mr" "Goose!")) '("Mr" "Goose!"))
(check-expect
 (flip-case '("OnLiNE" "ClAsSEs" "ArE" "sOo" "mUcH" "FuN!"))
 '("OnLiNE" "ClAsSEs" "are" "SOO" "MUCH" "fun!"))

;;flip-case: (listof Strings) -> (listof Strings)
(define (flip-case lst-string) 
  (local
    [

     (define (sum first-lst second-lst)
       (+ (length (string->list first-lst)) (length (string->list second-lst))))
     
     (define (third-element lst-string)
       (cond
         [(< (length lst-string) 3) empty]
         [(even? (sum (first lst-string) (second lst-string)))
          (cons (string-upcase (third lst-string))
                (third-element (rest lst-string)))]
         [(odd?  (sum (first lst-string) (second lst-string)))
          (cons (string-downcase (third lst-string))
                (third-element (rest lst-string)))]))
     
     (define (flip-case/acc lst-string)
       (cond
         [(> (length lst-string) 3)
          (append (list (first lst-string)) (list (second lst-string))
                  (third-element lst-string))]
         [(or (= (length lst-string) 0)
              (= (length lst-string) 1) (= (length lst-string) 2)) lst-string]))]

    (flip-case/acc lst-string)))


;;tests
(check-expect 
 (flip-case '("OnLiNE" "ClAsSEs" "ArE" "sOo" "mUcH" "FuN!" "Sometimes"))
 '("OnLiNE" "ClAsSEs" "are" "SOO" "MUCH" "fun!" "SOMETIMES"))


;;
;; =============================================================================
;; Q1 b) 
;; =============================================================================
;;

;;(function-go-round fn-list data-list) applies each function in fn-list to the
;;  the corresponding position in the data-list and if the the data-list is longer
;;  than the fn-list, then the list of function rotates to apply the first element
;;  of fn-list to the next element data list

;;Examples
(check-expect (function-go-round (list string-length) empty) empty)


;;function-go-round: (listof X) (listof X) -> (listof (listof X (listof X)))
(define (function-go-round fn-list data-list)
  (local
    [(define new-list-constant fn-list)

     (define (function-go-round/acc fn-list-1 data-list-1)
       (cond
         [(empty? data-list-1) empty]
         [(empty? fn-list-1)  (function-go-round/acc new-list-constant data-list-1) ]
         [else (cons ((first fn-list-1) (first data-list-1))
                     (function-go-round/acc (rest fn-list-1) (rest data-list-1)))]))]
  
    (function-go-round/acc fn-list  data-list)))

;;tests
(check-expect
 (function-go-round (list string-length string-upcase
                          (lambda (x) (string-append x "!!")))
                    '("joy" "anger" "disgust" "sadness" "fear"))
 '(3 "ANGER" "disgust!!" 7 "FEAR"))

(check-expect
 (function-go-round
  (list even? odd? add1 (lambda (x) (> 3 x)) even?) '(8 9 2 1 4))
 (list true true 3 true true))
