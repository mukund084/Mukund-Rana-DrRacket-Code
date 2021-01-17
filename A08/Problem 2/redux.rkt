;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname redux) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ==========================================================
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignmnet 8, Q2 
;; ==========================================================
;;


;;
;; =============================================================================
;; Q2 a) 
;; =============================================================================
;;

;;(parity  binary) returns 'odd or 'even depending on 
;;  how many times #\1 appears in list
;;Examples:
(check-expect (parity  "110101") 'even)
(check-expect (parity  "1110011") 'odd)
;;parity: Str → Sym
(define (parity binary)
  (cond
    [(=(remainder (count-1(string->list binary)) 2 ) 0) 'even]
    [else 'odd]))

;;tests
(check-expect (parity  "111111") 'even)
(check-expect (parity  "000000") 'even)
(check-expect (parity  "100000") 'odd)
(check-expect (parity  "") 'even)



;;(count-1  binary) counts the number of times
;;  #\1 appears in list
;;Examples:
(check-expect (count-1 empty) 0)
(check-expect (count-1 (string->list "110101")) 4)
(check-expect (count-1 (string->list "1110011")) 5)

;;count-1: Str -> Nat 
(define (count-1 binary)
  (length (filter (lambda (is-char-1) (char=? is-char-1 #\1)) binary)))

;;tests
(check-expect (count-1 (string->list "")) 0)
(check-expect (count-1 (string->list "0000")) 0)
(check-expect (count-1 (string->list "110")) 2)
(check-expect (count-1 (string->list "111")) 3)


;;
;; =============================================================================
;; Q2 b) 
;; =============================================================================
;;

;;(replace-word word1 word2 lstreplace) produces a list of strings
;;  that replaces one word with another

;;Examples
(check-expect
 (replace-word "exam" "assessment" (list "exam" "content" "assignment"))
 (list "assessment" "content" "assignment"))

  
;;replace-word: Str Str -> (listof Str)
(define(replace-word word1 word2 lstreplace)
  (map (lambda (is-word1) (cond                               
                            [(string=? word1 is-word1) word2 ]
                            [else is-word1])) lstreplace))


;;tests
(check-expect
 (replace-word "exam" "assessment" (list "exam" "content" "assignment"))
 (list "assessment" "content" "assignment"))


(check-expect
 (replace-word "exam" "assessment" (list "content" "content" "exam"))
 (list "content" "content" "assessment"))

(check-expect
 (replace-word "exam" "assessment"
               (list "exam" "exam" "exam"))
 (list "assessment" "assessment" "assessment"))

(check-expect
 (replace-word "exam" "assessment"
               (list "ice-cream" "ice-cream" "ice-cream"))
 (list "ice-cream" "ice-cream" "ice-cream"))



;;
;; =============================================================================
;; Q2 c) 
;; =============================================================================
;;

;;(all-factor num) produces a list of the factors of num
;; Examples
(check-expect(all-factors 30)
 (cons 1 (cons 2 (cons 3 (cons 5
 (cons 6 (cons 10 (cons 15 empty))))))))


;;all-factor: Nat -> (listof Nat)
;; Requires: num > 0
(define (all-factors num)
  (cond
    [(= num 1 ) (cons 1 empty)]
    [else (factors num (rest (countup-to num)))]))

;;tests
(check-expect(all-factors 10) (list 1 2 5))
(check-expect(all-factors 1) (list 1))


;;(countup-to  n num) produces an increasing list from n to num-1
;; Example:
(check-expect(countup-to 3) (list 0 1 2))

;;countup-to: Int Int → (listof Int)
;; Requires:
;;   n > 1
;;   n > num

;;countup-to: Int → (listof Int)
(define (countup-to n)
  (build-list n (lambda (x) x)))

;;tests 
(check-expect(countup-to 2) (list 0 1))


;;(factors lstnums) produces the factors of each number
;; Examples
(check-expect(factors 4 (list 1 2 3)) (list 1 2))


;;factors: Nat (listof Nat) -> (listof Nat)
(define (factors num lstnums)
  (filter (lambda (first-count-num) (= (remainder num first-count-num) 0)) lstnums))

;;tests 
(check-expect(factors 5 (list 1 2 3 4))(list 1))


;;
;; =============================================================================
;; Q2 d) 
;; =============================================================================
;;

;;(mean-relative lstnums) produces a list that determines if each
;; value is above, below, or equal to the mean
;;Examples
(check-expect (mean-relative (cons 5 (cons 7
                             (cons 9 (cons 12 empty)))))
                             (cons 'below-mean (cons 'below-mean
                             (cons 'above-mean
                             (cons 'above-mean empty)))))

;;mean-relative: (listof Int) -> (listof Sym)
(define (mean-relative lstnums)
  (check-mean-relative lstnums (mean lstnums)))

;;tests
(check-expect (mean-relative (cons 0 (cons 0
                (cons 0 (cons 0 empty)))))
                (cons 'mean (cons 'mean
                (cons 'mean (cons 'mean empty)))))

(check-expect (mean-relative (cons 1 (cons 0
               (cons 1 (cons 0 empty)))))
                (cons 'above-mean (cons 'below-mean
              (cons 'above-mean (cons 'below-mean empty)))) )

(check-expect (mean-relative (cons -1 (cons -6
              (cons -8 (cons -2 empty)))))
              (cons 'above-mean (cons 'below-mean
              (cons 'below-mean (cons 'above-mean empty)))) )



;;(mean lstnums) Calculates the mean of a list
;;Examples
(check-expect (mean (list 1)) 1)
(check-expect (mean (list 5 7 9 12)) 8.25)

;;mean: (listof Int) -> Num
(define (mean lstnums)
  (/  (foldl + 0 lstnums) (length lstnums)))

;;tests
(check-expect (mean (list 5 7 9 20)) 10.25)
(check-expect (mean (list -5 -7 -9 -20)) -10.25)
(check-expect (mean (list 5 7 -9 -20)) -4.25)



;;(check-mean-relative lstnums m) Determines if each value in the
;;  list is above, below or equal to the mean
(check-expect (check-mean-relative empty 1) empty)

;; check-mean-relative: (listof Int) -> (listof Sym)
(define (check-mean-relative lstnums m)
  (foldr (lambda (first-lstnums rorr)
           (cond
             [(> (- m first-lstnums) 0) (cons 'below-mean rorr)]
             [(< (- m first-lstnums) 0) (cons 'above-mean rorr)]
             [else (cons 'mean rorr)])) empty lstnums))


;;tests 
(check-expect (check-mean-relative (cons 5 (cons 7
              (cons 9 (cons 12 empty)))) 8.25)
              (cons 'below-mean (cons 'below-mean
              (cons 'above-mean (cons 'above-mean empty)))))
             





