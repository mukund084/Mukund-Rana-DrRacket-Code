;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname groceries) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***********************************************************
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignment 5, Q2 
;; ***********************************************************
;;

;; ***********************************************************
;; Q2 a)
;; ***********************************************************

(define-struct grocery (dept name cost mass))

;; A Grocery is a (make-grocery Str Str Num Num)
;; Requires: cost >= 0, mass > 0.
;; A Store is a (listof Grocery)
;; Requires: no two items have both the same dept and same name.

(define try-n-save
  (list (make-grocery "produce" "apple" 2.49 600)
        (make-grocery "seed" "rice" 0.95 1000)
        (make-grocery "dairy" "milk" 3.99 4000)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "produce" "potato" 2.99 5000)
        (make-grocery "chips" "potato" 1.99 250)
        (make-grocery "chips" "corn" 1.99 275)
        (make-grocery "seed" "wheat" 0.49 500)
        (make-grocery "produce" "banana" 0.69 450)
        (make-grocery "dairy" "cheese" 6.49 900)
        (make-grocery "chips" "banana" 1.99 50)
        (make-grocery "produce" "peach" 3.99 400)
        (make-grocery "seed" "lentil" 2.99 800)
        (make-grocery "produce" "corn" 0.99 100)
        (make-grocery "seed" "corn" 4.99 850)
        (make-grocery "dairy" "kefir" 5.99 1000)))



(define kwik-e-mart
  (list (make-grocery "seed" "rice" 0.38 400)
        (make-grocery "can" "corn" 4.00 400)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "produce" "apple" 2.99 400)
        (make-grocery "can" "creamed eels" 2.19 350)
        (make-grocery "produce" "pineapple" 3.17 250)))

(define kwik-e-mart-2.0
  (list (make-grocery "seed" "rice" 0.38 400)
        (make-grocery "can" "corn" 4.00 400)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "produce" "apple" 1.00 400)
        (make-grocery "can" "creamed eels" 2.19 350)
        (make-grocery "produce" "pineapple" 3.17 250)))

(define student-shop
  (list (make-grocery "snacks" "chocolates" 4.99 10)
        (make-grocery "snacks" "ice-cream"  5.99 20)
        (make-grocery "games" "Mario Kart" 50.99 40)
        (make-grocery "games" "Pokemon" 70.99 60)
        (make-grocery "vegetables" "cauliflower" 3.99 300)
        (make-grocery "vegetables" "carrot" 2.99 200)
        (make-grocery "baked goods" "Donuts" 1.99 30)
        (make-grocery "baked goods" "cookies" 0.99 15)
        (make-grocery "baked goods" "pie" 10.99 500)
        (make-grocery "baked goods" "bagel" 10.99 15)))


;; ***********************************************************
;; Q2 b)
;; ***********************************************************

(define-struct interval (lo hi))
;; An Interval is a (make-interval (anyof 'dontcare Num)
;; (anyof 'dontcare Num))

;;(in-interval? num lsinterval) produces true if num is in the interval
;;  otherwise produces false if num is not in the interval.

;;Examples
(check-expect (in-interval? 42
                            (make-interval 'dontcare 'dontcare)) true)
(check-expect (in-interval? 34
                            (make-interval 35 'dontcare)) false)
(check-expect (in-interval? 34
                            (make-interval 'dontcare 35)) true)


;;in-interval?: Num Interval -> Bool (Check Style Guide)
(define (in-interval? num lsinterval)
  (cond
    [(and (symbol?(interval-hi lsinterval)) (symbol? (interval-lo lsinterval))) true]  ;; (-infinity, +infinity) 
    [(and (and (not(symbol?(interval-hi lsinterval)))
               (not(symbol?(interval-lo lsinterval))))
          (>= num (interval-lo lsinterval)) (<= num (interval-hi lsinterval))) true]   ;; (a <= x <= b) 
    [(and (symbol? (interval-lo lsinterval))(<= num (interval-hi lsinterval))) true]   ;; (-infinity, x)  
    [(and (symbol?(interval-hi lsinterval))(>= num (interval-lo lsinterval))) true]    ;; (x, +infinity)
    [else false]))

;;tests
(check-expect (in-interval? 4.5
  (make-interval 4.5 4.5)) true)

(check-expect (in-interval? 6
  (make-interval 5 4)) false)

(check-expect (in-interval? 34
  (make-interval 33 'dontcare)) true)

(check-expect (in-interval? 32
  (make-interval 33 'dontcare)) false)

(check-expect (in-interval? 5
  (make-interval 4.5 4.5)) false)

(check-expect (in-interval? 4
  (make-interval 5 4)) false)


;; ***********************************************************
;; Q2 c)
;; ***********************************************************

;; A StrPatt is a (anyof Str 'dontcare)

(define-struct query (dept name cost mass))
;; A GroceryQuery is a
;; (make-query StrPatt StrPatt Interval Interval)



;;(find-matches grocery g-query) produces a list that contains items that satisfy
;;  the grocery qurey

;;Examples
(check-expect
 (find-matches try-n-save (make-query "seed" 'dontcare
                                      (make-interval 'dontcare 'dontcare)
                                      (make-interval 'dontcare 'dontcare)))
 (list
  (make-grocery "seed" "rice" 0.95 1000)
  (make-grocery "seed" "pinto" 2.49 500)
  (make-grocery "seed" "wheat" 0.49 500)
  (make-grocery "seed" "lentil" 2.99 800)
  (make-grocery "seed" "corn" 4.99 850)))


;;find-matches: (listof Grocery)  GroceryQuery ->  (listof Grocery)
(define (find-matches grocery g-query)
  (cond
    [(empty? grocery) empty]
    [(and (or (symbol? (query-dept g-query )) (string=? (query-dept g-query)
            (grocery-dept (first grocery)))
          (or (symbol? (query-name g-query)) (string=? (query-name g-query)
            (grocery-name (first grocery))))
          (in-interval? (grocery-cost (first grocery)) (query-cost g-query ))
          (in-interval? (grocery-mass (first grocery)) (query-mass g-query )))
     (cons (first grocery) (find-matches (rest grocery) g-query))]
    [else (find-matches (rest grocery) g-query)]))



;;tests
(check-expect
 (find-matches try-n-save (make-query 'dontcare "corn"
                                      (make-interval 'dontcare 'dontcare)
                                      (make-interval 'dontcare 'dontcare)))
 (list (make-grocery "chips" "corn" 1.99 275)
       (make-grocery "produce" "corn" 0.99 100)
       (make-grocery "seed" "corn" 4.99 850)))

(check-expect
 (find-matches try-n-save (make-query "seed" 'dontcare
                                      (make-interval 'dontcare 3.00)
                                      (make-interval 600 'dontcare)))
 (list
  (make-grocery "seed" "rice" 0.95 1000)
  (make-grocery "seed" "lentil" 2.99 800)))

(check-expect
 (find-matches try-n-save (make-query "seed" "rice"
                                      (make-interval 'dontcare 3.00)
                                      (make-interval 600 'dontcare)))
 (list
  (make-grocery "seed" "rice" 0.95 1000)))


;; ***********************************************************
;; Q2 d)
;; ***********************************************************

;;(sort-dept-name store) sorts a store in alphabetical order by deparment and then
;;  by name
;;Examples
(check-expect (sort-dept-name try-n-save)
              (list
               (make-grocery "chips" "banana" 1.99 50)
               (make-grocery "chips" "corn" 1.99 275)
               (make-grocery "chips" "potato" 1.99 250)
               (make-grocery "dairy" "cheese" 6.49 900)
               (make-grocery "dairy" "kefir" 5.99 1000)
               (make-grocery "dairy" "milk" 3.99 4000)
               (make-grocery "produce" "apple" 2.49 600)
               (make-grocery "produce" "banana" 0.69 450)
               (make-grocery "produce" "corn" 0.99 100)
               (make-grocery "produce" "peach" 3.99 400)
               (make-grocery "produce" "potato" 2.99 5000)
               (make-grocery "seed" "corn" 4.99 850)
               (make-grocery "seed" "lentil" 2.99 800)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "rice" 0.95 1000)
               (make-grocery "seed" "wheat" 0.49 500)))

;;sort-dept-name: Store -> Store
(define (sort-dept-name store)
  (cond [(empty? store) empty]
        [else (insert (first store) (sort-dept-name (rest store)))]))

;;tests

(check-expect (sort-dept-name kwik-e-mart)
              (list
               (make-grocery "can" "corn" 4 400)
               (make-grocery "can" "creamed eels" 2.19 350)
               (make-grocery "produce" "apple" 2.99 400)
               (make-grocery "produce" "pineapple" 3.17 250)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "rice" 0.38 400)))

;;(insert dept-name store) inserts the Grocery into the sorted list
;;  so that the resulting list is also alphabetical sorted.
;; insert: Str Store -> Store
(define (insert n storelst)
  (cond [(empty? storelst) (cons n empty)]
        [(and (string=? (grocery-dept n) (grocery-dept (first storelst)))
              (string<=? (grocery-name n) (grocery-name (first storelst)))) (cons n storelst)]
        [(string<? (grocery-dept n) (grocery-dept (first storelst))) (cons n storelst)]
        [else (cons (first storelst) (insert n (rest storelst)))]))


;; ***********************************************************
;; Q2 e)
;; ***********************************************************

;;(overlap/acc store1 store2) produces a store containing items that are available
;; in both stores. 
;;Examples
(check-expect
 (overlap kwik-e-mart try-n-save)
 (list
  (make-grocery "produce" "apple" 2.49 600) ; Buy cheaper.
  (make-grocery "seed" "pinto" 2.49 500) ; Same price and size.
  (make-grocery "seed" "rice" 0.38 400))) ; Same price; buy smaller

;;overlap: Store Store -> Store
(define (overlap store1 store2)
  (cond
    [(empty? (sort-dept-name store1)) empty]
    [(empty? (overlap/item (sort-dept-name store1) (sort-dept-name store2)))
             (overlap (rest (sort-dept-name store1)) (sort-dept-name store2))]
    [else (cons (overlap/item (sort-dept-name store1) (sort-dept-name store2))
                (overlap (rest (sort-dept-name store1)) (sort-dept-name store2)))]))

;;tests
(check-expect
 (overlap kwik-e-mart-2.0 try-n-save)
 (list
  (make-grocery "produce" "apple" 1 400)    ;; Buy cheaper.
  (make-grocery "seed" "pinto" 2.49 500)    ;; Same price and size.
  (make-grocery "seed" "rice" 0.38 400)))   ;; Same price; buy smaller


;;(overlap/item store1 store2) produces a item are available in both stores
;;overlap/item: Store Store -> Store
(define (overlap/item store1 store2)
  (cond
    [(empty? store2) empty]
    [(and (string=? (grocery-dept (first store1)) (grocery-dept (first store2))) (string=? (grocery-name (first store1)) (grocery-name (first store2))))
     (cond
       [(< (/ (grocery-cost (first store1)) (grocery-mass (first store1)))  (/ (grocery-cost (first store2)) (grocery-mass (first store2)))) (first store1)]
       [(< (/ (grocery-cost (first store2)) (grocery-mass (first store2)))  (/ (grocery-cost (first store1)) (grocery-mass (first store1)))) (first store2)]
       [(< (grocery-mass (first store1)) (grocery-mass (first store2))) (first store1)]
       [else (first store2)])]
    [else (overlap/item store1 (rest store2))]))


;; ***********************************************************
;; Q2 f)
;; *********************************************************
         

;;(scale-prices store g-query scale) sclaes the prices of the items that match
;;  GroceryQuery
;;Examples
(check-expect (scale-prices
               kwik-e-mart
               (make-query "can"
                           'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 1.10)
              (list (make-grocery "seed" "rice" 0.38 400)
                    (make-grocery "can" "corn" 4.40 400)
                    ;; corn goes from 4.00 to 4.40.
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "produce" "apple" 2.99 400)
                    (make-grocery "can" "creamed eels" 2.41 350)
                    ;; eels goes from 2.19 to 2.409, rounded to 2.41.
                    (make-grocery "produce" "pineapple" 3.17 250)))


;;scale-prices: Store GroceryQuery -> Store
(define (scale-prices store g-query scale)
  (cond
    [(empty? (rest store)) (cons (first store) empty)]
    [(equal? (first store) (first (find-matches store g-query)))
     (cons (make-grocery (grocery-dept (first store))
                         (grocery-name (first store))
                         (/(round (* (* scale (grocery-cost (first store))) 100)) 100)
                         (grocery-mass (first store)))(scale-prices (rest store) g-query scale))]
    [else (cons (first store) (scale-prices (rest store) g-query scale))]))

;;tests
(check-expect (scale-prices
               kwik-e-mart
               (make-query "can"
                           'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 0)
              (list (make-grocery "seed" "rice" 0.38 400)
                    (make-grocery "can" "corn" 0 400)
                    ;; corn goes from 4.00 to 0.
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "produce" "apple" 2.99 400)
                    (make-grocery "can" "creamed eels" 0 350)
                    ;; eels goes from 2.19 to 0. 
                    (make-grocery "produce" "pineapple" 3.17 250)))

(check-expect (scale-prices
               kwik-e-mart
               (make-query "can"
                           'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 0.5)
              (list (make-grocery "seed" "rice" 0.38 400)
                    (make-grocery "can" "corn" 2 400)
                    ;; corn goes from 4.00 to 2.
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "produce" "apple" 2.99 400)
                    (make-grocery "can" "creamed eels" 1.1 350)
                    ;; eels goes from 2.19 to 1.095, rounded to 1.1. 
                    (make-grocery "produce" "pineapple" 3.17 250)))




