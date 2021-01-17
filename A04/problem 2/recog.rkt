;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recog) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***********************************************************
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignment 4, Q3
;; ***********************************************************
;;

;;
;; ***********************************************************
;; Q3 a)
;; ***********************************************************
;;


;; 3a)
;; These are helper functions. See assignment for design recipe requirements.s get-x

;;(get-x lstpoint) produces the x-coordinate of a point
;;Examples
(check-within (get-x (list 3 4)) 3 0.01)
(check-within (get-x (list 2 0)) 2 0.01)


;;get-x: (listof Nums) -> Num
(define (get-x lstpoint)
  (first lstpoint))


;;(get-y lstpoint) produces the y-coordinate of a point
;;Examples
(check-within (get-y (list 3 4)) 4 0.01)
(check-within (get-y (list 3 0)) 0 0.01)

;;get-y: (listof Nums) -> Num
(define (get-y lstpoint)
  (second lstpoint))


;;(translate-gesture lstnums x-offset y-offset) produces a new gesture onces
;;  once x-offset and y-offset are applied to each x-coordinate and y-coordinate
;;  in the list
;;Examples
(check-within (translate-gesture (list(list 3 4)) 1 2) (list(list 4 6))  0.01)
(check-within (translate-gesture (list(list 3 4) (list 4 5)) 1 2) (list(list 4 6)
                                                                       (list 5 7))  0.01)


;;translate-gesture: (listof (list Num Num)) Num Num -> (lisof Nums)
(define (translate-gesture lstpoints x-offset y-offset)
  (cond
    [(empty? lstpoints) empty]
    [else (cons (list (+ (first (first lstpoints)) x-offset)
                      (+ (second (first lstpoints)) y-offset))
                (translate-gesture (rest lstpoints) x-offset y-offset))]))


;;(scale-gesture lstnums x-scale y-scale) produces a new gesture onces
;;  once x-scale and y-scale are applied to each x-coordinate and y-coordinate
;;  in the list
;;Examples
(check-within (scale-gesture (list(list 3 4)) 1 2) (list(list 3 8))  0.01)
(check-within (scale-gesture (list(list 3 4) (list 4 5)) 1 2)
              (list(list 3 8) (list 4 10)) 0.01)


;;translate-gesture: (listof (list Num Num)) Num Num -> (lisof Nums)
(define (scale-gesture lstpoints x-scale y-scale)
  (cond
    [(empty? lstpoints) empty]
    [else (cons (list (* (first (first lstpoints)) x-scale)
                      (* (second (first lstpoints)) y-scale))
                (scale-gesture (rest lstpoints) x-scale y-scale))]))


;;(get-b-box lstgesture) produces the gestures Bounding Box
;; Examples
(check-within (get-b-box (list (list 100 0) (list 200 100) (list 100 200)
                               (list 0 100) (list 100 0))) (list (list 0 0)
                                                                 (list 200 200)) 0.01)

;;get-b-box: (listof (list Num Num)) -> (listof (list Num Num))
(define (get-b-box lstgesture)
  (list (list (min-gesture (gesturex lstgesture)) (min-gesture
                                                   (gesturey lstgesture))) 
        (list (max-gesture (gesturex lstgesture)) (max-gesture
                                                   (gesturey lstgesture)))))


;;(get-b-box (list (list 1 2)))

;;(gesturex lstgesture) produces the minimum of the x gesture
;; Examples

;;gesturex: (listof (list Num Num)) -> (listof Num Num)
(define (gesturex lstgesture)
  (cond
    [(empty? (rest lstgesture)) (cons (first (first lstgesture)) empty)]
    [else  (cons (first (first lstgesture)) (gesturex (rest lstgesture))) ]))

;;(gesturey lstgesture) produces the minimum of the y gesture
;; Examples

;;gesturey: (listof (list Num Num)) -> (listof Num Num)
(define (gesturey lstgesture)
  (cond
    [(empty? (rest lstgesture)) (cons (second (first lstgesture)) empty)]
    [else  (cons (second (first lstgesture)) (gesturey (rest lstgesture))) ]))

;;(min-gesture lstgesturexy) finds the min of gesuture values in a list
;;Examples 

;;min-gesture: (listof Num) -> Num
(define (min-gesture lstgesturexy)
  (cond [(empty? (rest lstgesturexy)) (first lstgesturexy)]
        [else (min (first lstgesturexy)
                   (min-gesture (rest lstgesturexy)))]))

;;(max-gesture lstgesturexy) finds the max of gesuture values in a list
;;Examples 

;;max-gesture: (listof Num) -> Num
(define (max-gesture lstgesturexy)
  (cond [(empty? (rest lstgesturexy)) (first lstgesturexy)]
        [else (max (first lstgesturexy)
                   (max-gesture (rest lstgesturexy)))]))




                


;; 3b)
;; Full design recipe required.

;;(distance lstpoint1 lstpoint2) calculates the distance between 2 adjecent points
;; Examples
;;(check-within (distance (list 3 4) (list 7 4)) 16  0.01)

;;distance: (listof Num Num) (listof Num Num) -> Num
(define (distance lstpoint1 lstpoint2)
  (+(expt (- (first lstpoint2) (first lstpoint1)) 2) (expt (- (second lstpoint2)
                                                              (second lstpoint1)) 2)))

;;tests
;;(check-within (distance (list 0 0) (list 7 4)) 65  0.01)
;;(check-within (distance (list 0 0) (list 0 0)) 0  0.01)


;;(gesture-length lstgesture) calculates the sum of distances between adjacent
;;  points in the gesture
;; Examples
;;(check-within (gesture-length (list (list 7 4) (list 3 4) (list 8 9) (list 3 4))) 18.14  0.01)

;;gesture-length: (listof (list Num Num))-> Num
(define (gesture-length lstgesture)
  (cond
    [(empty? lstgesture) 0]
    [(empty? (rest lstgesture)) 0]
    [else (+(sqrt (distance (first lstgesture) (second lstgesture)))
            (gesture-length (rest lstgesture)))]))

;;tests
;;(check-within (gesture-length (list )) 0 0.01)
;;(check-within (gesture-length (list(list 2 4))) 0 0.01)
;;(check-within (gesture-length (list (list 7 4) (list 3 4) (list 8 9) (list 3 4) (list 6 5))) 21.30  0.01)
(gesture-length (list (list 7 4) (list 3 4)))



;;
;; ***********************************************************
;; Q3 bii) 
;; ***********************************************************
;;

;;Constant for testing 
(define mygest (list (list 100 0) (list 200 100) (list 100 200)
                     (list 0 100) (list 100 50)))

;;Constants for index and point 
(define (index-key indexkpoint) (first indexkpoint))
(define (point indexkpoint) (second indexkpoint))

;;(get-points g lstindex) produces a list of points based on the index of each point
;;  in a gesture
;;Examples
(check-expect (get-points mygest (list 0 0 2 4 4))
              (list (list 100 0) (list 100 0) (list 100 200) (list 100 50)
                    (list 100 50)))

;;get-points: (listof (list Num Num)) -> (listof (list Num Num))
(define (get-points g lstindex)
  (cond
    [(or (empty? lstindex) (empty? g)) empty]
    [else (cons (lookup-point (first lstindex) (combine-index g)) (get-points g (rest lstindex)))]))

;;tests
(check-expect (get-points mygest (list )) empty)
(check-expect (get-points (list ) (list 0 1)) empty)
(check-expect (get-points (list ) (list )) empty)
(check-expect (get-points mygest (list 0)) (list(list 100 0)))




;;(countup-index-index-index n) produces an increasing list of from n to glength - 1
;; Example:
(check-expect (countup-index 0 3) (list 0 1 2))

;;countup-index: Int -> (listof Int)
;; Requires: n >= 0 and glength > n 
(define (countup-index n glength)
  (cond [(= (- glength 1) n) (cons n empty)]
        [else (cons n (countup-index (add1 n) glength))]))

;;tests
(check-expect (countup-index 1 7) (list 1 2 3 4 5 6))
(check-expect (countup-index -10 -5) (list -10 -9 -8 -7 -6))
(check-expect (countup-index -2 1) (list -2 -1 0))



;;(insert n glst) inserts the number ccorresponding index number to each point  in
;;  a gesture  
;;Examples:
(check-expect (insert (list 0 1 2) (list (list 3 4) (list 5 6) (list 7 8)))
              (list (list 0 (list 3 4)) (list 1 (list 5 6)) (list 2 (list 7 8))))

;;insert: (listof Num) (listof (list Num Num)) → (listof (list Nat (list Num Num)))
(define (insert indexlst glst)
  (cond
    [(or (empty? glst) (empty? indexlst)) empty]
    [else (cons (list (first indexlst) (first glst))
                (insert (rest indexlst)(rest glst)))]))

;;tests
(check-expect (insert(list ) (list (list 3 4))) empty)
(check-expect (insert (list 0 1 3) (list )) empty)
(check-expect (insert (list ) (list )) empty)


;;(combine-index glst) combines the index with each point in a list of gesture
;;Examples
(check-expect (combine-index (list (list 2 3) (list 4 5) (list 6 7)))
              (list (list 0 (list 2 3)) (list 1 (list 4 5)) (list 2 (list 6 7))))

;;combine-index: (list0f (list Num Num)) -> (listof (list Nat (list Num Num)))
(define (combine-index glst)
  (cond
    [(empty? glst) empty]
    [else (insert (countup-index 0 (length glst)) glst)]))

;;tests
(check-expect (combine-index (list (list 3 4))) (list (list 0 (list 3 4))))
(check-expect (combine-index (list )) empty)

 
;;(lookup-point k glst) produces the value corresponding the point of the
;;  corresponding index or false if index not present
;;Example
(check-expect (lookup-point 0 (list (list 0 (list 2 3)) (list 1 (list 4 5))
                                    (list 2 (list 6 7)))) (list 2 3))


;;lookup-point: Nat (listof (list Nat (list Num Num))) -> (list Num Num)
(define (lookup-point index glst)
  (cond
    [(empty? glst) false]
    [(= index (index-key (first glst))) (point (first glst))]
    [else (lookup-point index (rest glst))]))

;;tests 
(check-expect (lookup-point 10 (list (list 0 (list 2 3)) (list 1 (list 4 5))
                                     (list 2 (list 6 7)))) false)






;; 3c) Starter code definitions

;; 3ci)
;;(five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))



;; five-sample: Gesture -> Gesture
;; requires: gesture is non-empty
(define (five-sample gesture)
  (get-points gesture (newgesuture-index gesture)))



;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))




;;(newgesuture-index glst) produces the new gesture index for non-empty gesture
;; Examples

;;newgesture-index: Gesture -> (listof Nat)
(define (newgesuture-index gesture)
  (list 0 (floor (* 0.25 (length gesture))) (floor (* 0.50 (length gesture)))
        (floor (* 0.75 (length gesture))) (- (length gesture) 1)))

;;tests 



;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale)
;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0

(define (move-and-scale gesture x-scale y-scale)
  (scale-gesture (translate-gesture gesture (- 0 (min-gesture (gesturex gesture))) (- 0(min-gesture (gesturey gesture)))) x-scale  y-scale))

;; Test:

(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))






(define min-width 30)
(define min-height 30)
(define norm-size 200)

;;(normalize-gesture gesture) normalizes gesture to (0,0) and a standard size
;; Examples:
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50) (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01)

;; normalize-gesture: Gesture -> Gesture
;; requires: gesture is not both vertical and horizontal
;;           gesture is non-empty
(define (normalize-gesture gesture)
  (cond
    [(< (width gesture) min-width) (move-and-scale gesture 1 (/ norm-size (height gesture)))]      ;; Vertical 
    [(< (height gesture) min-height) (move-and-scale gesture (/ norm-size (width gesture)) 1)]     ;; Horizontal 
    [else (move-and-scale gesture (/ norm-size (width gesture)) (/ norm-size (height gesture)))]))  ;; Neither 



;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) 0.01)


;;(width gesture) calculates the width of a gesture
;; Examples

;;width: Gesture -> Num
(define (width gesture)
  (- (first(second(get-b-box gesture))) (first(first (get-b-box gesture)))))


;;(height gesture) calculates the height of a gesture
;; Examples

;;height: Gesture -> Num
(define (height gesture)
  (- (second(second(get-b-box gesture))) (second(first (get-b-box gesture)))))



;; 3civ)

;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points
;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
              16.16 0.01)

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
(define (geometric-5match gesture1 gesture2)
   (+ (gesture-length (list (first (normalize-gesture (five-sample gesture1))) (first (normalize-gesture (five-sample gesture2)))))
             (gesture-length (list (second (normalize-gesture (five-sample gesture1))) (second (normalize-gesture (five-sample gesture2)))))
             (gesture-length (list (third (normalize-gesture (five-sample gesture1))) (third (normalize-gesture (five-sample gesture2)))))
             (gesture-length (list (fourth (normalize-gesture (five-sample gesture1))) (fourth (normalize-gesture (five-sample gesture2)))))
             (gesture-length (list (fifth (normalize-gesture (five-sample gesture1))) (fifth (normalize-gesture (five-sample gesture2)))))))



;; Tests:

(geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))




;;(+ (/ (sqrt(distance (list 10 10) (list 10 10))) 5) (/ (sqrt (distance (list 30 30) (list 20 20))) 5) (/ (sqrt(distance (list 50 50) (list 30 30))) 5) (/ (sqrt(distance (list 70 70) (list 40 40))) 5) (/ (sqrt(distance (list 80 80) (list 40 40))) 5))
;;#i28.284271247461902











;;Constant
(define starting-index 0)

;;tests 
  

;;(geometric-length gesture1 gesture2 k n) produces the average distance between
;;  points in gesture1 and gesture2
;;Examples

;;geometric-match: Gesture Gesture Nat Nat -> Num
(define (geometric-length gesture1 gesture2 k n)
  (cond
    [(= n k) 0]
    [else (+ (gesture-length (list (first (get-points gesture1 (list n))) (first (get-points gesture2 (list n))))) (geometric-length gesture1 gesture2 k (add1 n)))]))

;;tests 


;;(geometric-match  gesture1 gesture2 k) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points
;;Examples

;; geometric-match: Gesture Gesture Nat -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
(define (geometric-match  gesture1 gesture2 k)
  (geometric-length (normalize-gesture (sub-sample gesture1 k)) (normalize-gesture (sub-sample gesture1 k)) k starting-index))

(geometric-match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)) 5)





