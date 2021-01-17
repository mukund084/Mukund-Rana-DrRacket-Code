;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recognize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***********************************************************
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignment 4, Q3
;; ***********************************************************
;;

(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       



;;
;; ***********************************************************
;; Q3 a)
;; ***********************************************************
;;

;;
;; ***********************************************************
;; Q3 ai) 
;; ***********************************************************
;;


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

;;
;; ***********************************************************
;; Q3 aii) 
;; ***********************************************************
;;

;;(translate-gesture lstnums x-offset y-offset) produces a new gesture onces
;;  once x-offset and y-offset are applied to each x-coordinate and y-coordinate
;;  in the list
;;Examples
(check-within (translate-gesture (list(list 3 4)) 1 2) (list(list 4 6))  0.01)
(check-within (translate-gesture (list(list 3 4) (list 4 5)) 1 2)
              (list(list 4 6) (list 5 7)) 0.01)


;;translate-gesture: Gesture Num Num -> (lisof Nums)
(define (translate-gesture lstpoints x-offset y-offset)
  (cond
    [(empty? lstpoints) empty]
    [else (cons (list (+ (first (first lstpoints)) x-offset)
                      (+ (second (first lstpoints)) y-offset))
                (translate-gesture (rest lstpoints) x-offset y-offset))]))

;;
;; ***********************************************************
;; Q3 aiii) 
;; ***********************************************************
;;

;;(scale-gesture lstnums x-scale y-scale) produces a new gesture onces
;;  once x-scale and y-scale are applied to each x-coordinate and y-coordinate
;;  in the list
;;Examples
(check-within (scale-gesture (list(list 3 4)) 1 2) (list(list 3 8))  0.01)
(check-within (scale-gesture (list(list 3 4) (list 4 5)) 1 2)
              (list(list 3 8) (list 4 10)) 0.01)


;;scale-gesture: Gesture Num Num -> (lisof Nums)
;; Requires : x-scale and y-scale to be positive non-zero numbers
(define (scale-gesture lstpoints x-scale y-scale)
  (cond
    [(empty? lstpoints) empty]
    [else (cons (list (* (first (first lstpoints)) x-scale)
                      (* (second (first lstpoints)) y-scale))
                (scale-gesture (rest lstpoints) x-scale y-scale))]))

;;
;; ***********************************************************
;; Q3 aiv) 
;; ***********************************************************
;;

;;(get-b-box lstgesture) produces the gestures Bounding Box
;; Examples
(check-within (get-b-box (list (list 100 0) (list 200 100) (list 100 200)
                         (list 0 100) (list 100 0)))
              (list (list 0 0)(list 200 200)) 0.01)

;;get-b-box: Gesture -> Gesture
(define (get-b-box lstgesture)
  (list (list (min-gesture (gesturex lstgesture)) (min-gesture (gesturey lstgesture))) 
        (list (max-gesture (gesturex lstgesture)) (max-gesture (gesturey lstgesture)))))

;;(gesturex lstgesture) produces the minimum of the x gesture
;; Examples

;;gesturex: Gesture -> (listof Num Num)
(define (gesturex lstgesture)
  (cond
    [(empty? (rest lstgesture)) (cons (first (first lstgesture)) empty)]
    [else  (cons (first (first lstgesture)) (gesturex (rest lstgesture))) ]))

;;(gesturey lstgesture) produces the minimum of the y gesture
;; Examples

;;gesturey: Gesture -> (listof Num Num)
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

;;
;; ***********************************************************
;; Q3 bi) 
;; ***********************************************************
;;

;;(gesture-length lstgesture) calculates the sum of distances between adjacent
;;  points in the gesture
;; Examples
(check-within (gesture-length (list (list 7 4) (list 3 4) (list 8 9)
                                    (list 3 4))) 18.14  0.01)

;;gesture-length: Gesture-> Num
(define (gesture-length lstgesture)
  (cond
    [(empty? lstgesture) 0]
    [(empty? (rest lstgesture)) 0]
    [else (+(sqrt (distance (first lstgesture) (second lstgesture)))
            (gesture-length (rest lstgesture)))]))

;; Tests:
(check-within (gesture-length (list )) 0 0.01)
(check-within (gesture-length (list(list 2 4))) 0 0.01)
(check-within (gesture-length (list (list 7 4) (list 3 4) (list 8 9) (list 3 4)
                                    (list 6 5))) 21.30  0.01)

;;(distance lstpoint1 lstpoint2) calculates the distance between 2 adjecent points
;; Examples
(check-within (distance (list 3 4) (list 7 4)) 16  0.01)

;;distance: (listof Num Num) (listof Num Num) -> Num
(define (distance lstpoint1 lstpoint2)
  (+(expt (- (first lstpoint2) (first lstpoint1)) 2) (expt (- (second lstpoint2)
                                                     (second lstpoint1)) 2)))

;; Tests:
(check-within (distance (list 0 0) (list 7 4)) 65  0.01)
(check-within (distance (list 0 0) (list 0 0)) 0  0.01)

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

;;get-points: Gesture -> Gesture
(define (get-points g lstindex)
  (cond
    [(or (empty? lstindex) (empty? g)) empty]
    [else (cons (lookup-point (first lstindex) (combine-index g))
                (get-points g (rest lstindex)))]))

;; Tests:
(check-expect (get-points mygest (list )) empty)
(check-expect (get-points (list ) (list 0 1)) empty)
(check-expect (get-points (list ) (list )) empty)
(check-expect (get-points mygest (list 0)) (list(list 100 0)))




;;(countup-index n) produces an increasing list of from n to glength - 1
;; Example:
(check-expect (countup-index 0 3) (list 0 1 2))

;;countup-index: Int -> (listof Int)
;; Requires: n >= 0 and glength > n 
(define (countup-index n glength)
  (cond [(= (- glength 1) n) (cons n empty)]
        [else (cons n (countup-index (add1 n) glength))]))

;; Tests:
(check-expect (countup-index 1 7) (list 1 2 3 4 5 6))
(check-expect (countup-index -10 -5) (list -10 -9 -8 -7 -6))
(check-expect (countup-index -2 1) (list -2 -1 0))



;;(insert n glst) inserts the number ccorresponding index number to each point  in
;;  a gesture  
;;Examples:
(check-expect (insert (list 0 1 2) (list (list 3 4) (list 5 6) (list 7 8)))
              (list (list 0 (list 3 4)) (list 1 (list 5 6)) (list 2 (list 7 8))))

;;insert: (listof Num) Gesture → (listof (list Nat (list Num Num)))
(define (insert indexlst glst)
  (cond
    [(or (empty? glst) (empty? indexlst)) empty]
    [else (cons (list (first indexlst) (first glst))
                (insert (rest indexlst)(rest glst)))]))

;; Tests:
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

;; Tests:
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

;; Tests: 
(check-expect (lookup-point 10 (list (list 0 (list 2 3)) (list 1 (list 4 5))
                                     (list 2 (list 6 7)))) false)



;;
;; ***********************************************************
;; Q3 c) 
;; ***********************************************************
;;

;;
;; ***********************************************************
;; Q3 ci) 
;; ***********************************************************
;;

;;(five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;;Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))



;;five-sample: Gesture -> Gesture
;; Requires: gesture is non-empty
(define (five-sample gesture)
  (get-points gesture (newgesuture-index gesture)))



;; Tests::
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
 
;;
;; ***********************************************************
;; Q3 cii) 
;; ***********************************************************
;;

;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale)
;;Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0

(define (move-and-scale gesture x-scale y-scale)
  (scale-gesture (translate-gesture gesture (- 0 (min-gesture (gesturex gesture)))
                 (- 0(min-gesture (gesturey gesture)))) x-scale  y-scale))

;;Test:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))

;;
;; ***********************************************************
;; Q3 ciii) 
;; ***********************************************************
;;

;;Constants for checking minimum width, height and normalize size  
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
    [(< (width gesture) min-width) (move-and-scale gesture 1 (/ norm-size (height gesture)))]       ;; Vertical 
    [(< (height gesture) min-height) (move-and-scale gesture (/ norm-size (width gesture)) 1)]      ;; Horizontal 
    [else (move-and-scale gesture (/ norm-size (width gesture)) (/ norm-size (height gesture)))]))  ;; Neither 



;;  Tests::
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


;;
;; ***********************************************************
;; Q3 civ) 
;; ***********************************************************
;;

;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with 5 points
;;Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               16.16 0.01)

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
(define (geometric-5match gesture1 gesture2)
   (/(+ (gesture-length (list (first (normalize-gesture (five-sample gesture1)))
                              (first (normalize-gesture (five-sample gesture2)))))
             (gesture-length (list (second (normalize-gesture (five-sample gesture1)))
                                   (second (normalize-gesture (five-sample gesture2)))))
             (gesture-length (list (third (normalize-gesture (five-sample gesture1)))
                                   (third (normalize-gesture (five-sample gesture2)))))
             (gesture-length (list (fourth (normalize-gesture (five-sample gesture1)))
                                   (fourth (normalize-gesture (five-sample gesture2)))))
             (gesture-length (list (fifth (normalize-gesture (five-sample gesture1)))
                                   (fifth (normalize-gesture (five-sample gesture2)))))) 5))


;;  Tests::
(check-within (geometric-5match
               (list (list 11 11) (list 36 31) (list 50 50) (list 71 79) (list 80 80))
               (list (list 10 10) (list 24 20) (list 30 30) (list 40 40) (list 40 40)))
               15.51 0.01)


;;
;; ***********************************************************
;; Q3 cv) 
;; ***********************************************************
;;

;;(five-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;;Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)


;;five-point-rec: Gesture TL -> Sym
;; Requires: candidate is not both vertical and horizontal
(define (five-point-rec candidate template-library)
  (lookup-letter (min-geo-match (min-letter candidate template-library))
                 (min-letter candidate template-library)))


;; Tests:
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testy templates) 'y)

;;(min-letter candidate template-library) produces a list with letter and minimun
;;  distance between the candidate and letter reference in the template-library
;;Examples

;;min-letter: Gesture TL -> (listof (list Sym Num))
(define (min-letter candidate template-library)
  (cond
    [(empty? template-library) empty]
    [else  (cons (list (first (first template-library))
                       (geometric-5match candidate (second (first template-library))))
                 (min-letter candidate (rest template-library)))]))


;;(min-geo-match lstlettermin) produces the minimum value of the geometeric-match
;;  distance 
;;Examples

;;min-geo-match (listof Num) -> Num
(define (min-geo-match lstlettermin)
  (cond [(empty? (rest lstlettermin)) (second(first lstlettermin))]
        [else (min (second (first lstlettermin))
                   (min-geo-match (rest lstlettermin)))]))


;;Constants for index and point 
(define (min-key indexkpoint) (second indexkpoint))
(define (letter indexkpoint) (first indexkpoint))

;;(lookup-letter min-value min-lettterlst) produces the letter to its corresponding value. 
;;Example
(check-expect(lookup-letter 10 empty) false)

;;lookup-point: Nat (listof (list Nat (list Num Num))) -> (list Num Num)
(define (lookup-letter min-value min-lettterlst)
  (cond
    [(empty? min-lettterlst) false]
    [(= min-value (min-key (first min-lettterlst))) (letter (first min-lettterlst))]
    [else (lookup-letter min-value (rest min-lettterlst))]))


;;
;; ***********************************************************
;; Q3 d) 
;; ***********************************************************
;;

;;
;; ***********************************************************
;; Q3 di) 
;; ***********************************************************
;;

;;Constant
(define start-factor 1)

;;(sub-sample k gesture) produces a sampling of gesture of k points
;;Examples
(check-expect (sub-sample(list (list 1 1) (list 2 2)) 8) (list (list 1 1)
                         (list 1 1) (list 1 1) (list 1 1) (list 2 2) (list 2 2)
                         (list 2 2) (list 2 2)))

;;sub-sample: Nat Gesture -> Gesture
;; Requires: gesture is non-empty and k > 2
(define (sub-sample gesture k)
  (get-points gesture (cons 0 (subgesuture-index k start-factor (length gesture)))))

;; Tests: 
(check-expect (sub-sample (list (list 1 1) (list 2 2)) 3)
  (list (list 1 1) (list 2 2) (list 2 2)))
  
(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)) 5)
  (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
 
(check-expect (sub-sample (list (list 1 1)) 3)
  (list (list 1 1) (list 1 1) (list 1 1)))
 
(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)) 6)
  (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5) (list 5 5)))
  
(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)) 3)
  (list (list 1 1) (list 3 3) (list 5 5)))

;;(subgesuture-index k lengthgesture gesture) produces the new gesture index for
;;  non-empty gesture for k - 1 number of list. 
;; Examples

;;subgesture-index: Nat Nat Nat -> (listof Nat)                                  
(define (subgesuture-index k n lengthgesture)
  (cond
    [(= n (- k 1)) (cons (- lengthgesture 1) empty)]
    [else (cons (floor (* lengthgesture (* n (/ 1 (- k 1)))))
                (subgesuture-index k (add1 n) lengthgesture))]))

;;
;; ***********************************************************
;; Q3 dii) 
;; ***********************************************************
;;

;;Constant
(define starting-index 0)

;;(geometric-match  gesture1 gesture2 k) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points
;;Examples
(check-within (geometric-match
  (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
  (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)) 5)
  16.16 0.01)

;; geometric-match: Gesture Gesture Nat -> Num
;; Requires:
;;   gesture1 and gesture2 are each not both vertical and horizontal
;;   gesture1 and gesture2 to be non-empty list
;;   k <= length of either gesture1 or gesture2 
(define (geometric-match  gesture1 gesture2 k)
  (/ (geometric-length (normalize-gesture (sub-sample gesture1 k))
                       (normalize-gesture (sub-sample gesture2 k)) k starting-index) k))

;; Tests:
(check-within (geometric-match
  (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
  (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)) 4)
  13.46 0.01)


;;(geometric-length gesture1 gesture2 k n) produces the average distance between
;;  points in gesture1 and gesture2
;;Examples

;;geometric-length: Gesture Gesture Nat Nat -> Num
(define (geometric-length gesture1 gesture2 k n)
  (cond
    [(= n k) 0]
    [else (+ (gesture-length (list (first (get-points gesture1 (list n)))
                                   (first (get-points gesture2 (list n)))))
             (geometric-length gesture1 gesture2 k (add1 n)))]))

;; Tests:

;;
;; ***********************************************************
;; Q3 diii) 
;; ***********************************************************
;;

;(k-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (k-point-rec  testd templates 5) 'd)
(check-expect (k-point-rec  testk templates 5) 'k)

;;k-point-rec : Gesture TL Nat -> Sym
;; requires: candidate is not both vertical and horizontal
(define (k-point-rec  candidate template-library k)
  (lookup-letter (min-geo-match (min-letter-k candidate template-library k))
                 (min-letter-k candidate template-library k)))

;; Tests:
(check-expect (k-point-rec tests templates 8) 's)
(check-expect (k-point-rec testy templates 10) 'y)


;;(min-letter-k candidate template-library) produces a list with letter and minimun
;;  distance between the candidate and letter reference in the template-library for
;;  k number of points 
;;Examples
(check-expect (min-letter-k testd empty 5) empty)

;; min-letter-k: Gesture TL Nat -> (listof (list Sym Num))
(define (min-letter-k candidate template-library k)
  (cond
    [(empty? template-library) empty]
    [else  (cons (list (first (first template-library))
                       (geometric-match candidate (second (first template-library)) k))
                 (min-letter candidate (rest template-library)))]))







