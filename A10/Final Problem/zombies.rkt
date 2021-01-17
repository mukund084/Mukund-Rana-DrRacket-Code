;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname zombies) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ==========================================================
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignmnet 10, Q1 
;; ==========================================================
;;

;; A Location is a Nat

;; A Town is a (listof (list Location (listof Location)))

;; Requires: Town represents a valid graph as defined in Module 16

(define waterloo '((0 (1 2 3))
                   (1 (2 3))
                   (2 (0 4))
                   (3 (1))
                   (4 (5))
                   (5 (3))))

(define toronto  '((0 (1))
                   (1 (2))
                   (2 (3 5))
                   (3 (4))
                   (4 (0))
                   (5 (1))))




;; A Horde is a (listof (list Location Nat))

;;
;; =============================================================================
;; I 
;; =============================================================================
;;


;;(infect town zombies) produces a horde where each location has the same number
;;  zombies
;;Examples 
(check-expect (infect empty 1000) empty)

;;infect: Town Nat -> Horde
(define (infect town zombies)
  (map (lambda (x) (cons (first x) (cons zombies empty))) town))

;;tests
(check-expect (infect waterloo 1000)
              (list (list 0 1000) (list 1 1000) (list 2 1000)
                    (list 3 1000) (list 4 1000) (list 5 1000)))

;;(infect toronto 1000) 
  



;;
;; =============================================================================
;; II 
;; =============================================================================
;;

;;constants
(define decrease-in-town 0.95)

;;(sink horde) produces a list of two elements where the first element is the total
;;  number of zombies that sink into the earth and the second element is the horde
;;  after the zombies have sunken into the earth

;;Examples

;;sink: Horde -> (listof Num Horde)
;; Requires: Horde to be non-empty
(define (sink horde)
  (local
    [(define (split-list-pairs a)
       (cond
         [(empty? a) empty]
         [else (cons (second (first a)) (split-list-pairs (rest a)))]))


     (define (sum-of-list lst) (foldr +  0 lst))

     (define (new-decreased-horde horde)
       (map (lambda (x) (cons (first x)
                              (cons (round (* decrease-in-town(second x))) empty))) horde))

     (define (sunken-zombies lst-1 lst-2)
       (- (sum-of-list lst-1) (sum-of-list lst-2)))]

    (list (sunken-zombies (split-list-pairs horde) (split-list-pairs (new-decreased-horde horde))) (new-decreased-horde horde))))

;;tests
(check-expect
 (sink (infect waterloo 1000))
 (list 300 (list (list 0 950) (list 1 950) (list 2 950)
                 (list 3 950) (list 4 950) (list 5 950))))


;;
;; =============================================================================
;; III
;; =============================================================================
;;

;;(apportion zombies n) produces a list of n numbers, which states that number of
;;  ways the zombies can be divided into equal groups according the condition that
;;  the all numbers must add up to number zombies and difference between two numbers
;;  cannot be > 1

;;Examples
(check-expect (apportion 10 2) (list 5 5))

;;apportion: Nat Nat -> (listof Nat)
(define (apportion zombies n)
  (cond
    [(> n zombies) (local
                     [(define (improper-list zombies n)
                        (cond
                          [(= 1 n) (cons zombies empty)]
                          [(= 0 zombies) (cons zombies (improper-list zombies  (sub1 n)))]
                          [else (cons 1 (improper-list  (sub1 zombies) (sub1 n)))]))]
                     
                     (improper-list zombies n))]
                     
    
    [else (local
            [(define (groups num-zombies quotient-reduce count)
               (cond
                 [(= (+ quotient-reduce (remainder zombies n)) num-zombies)
                  (cons (+ quotient-reduce (remainder zombies n)) empty)]
                 [else (cons quotient-reduce
                             (groups (- zombies (* quotient-reduce count))
                                     quotient-reduce (add1 count)))]))]

            (reverse (groups zombies (quotient zombies n) 1)))]))



;;tests 
(check-expect (apportion 100 3) (list 34 33 33))
(check-expect (apportion 1 3) (list 1 0 0))


;;
;; =============================================================================
;; IV
;; =============================================================================
;;

(define braaaaains (second (sink (infect waterloo 1000))))
(define braaaaains-1 (second (sink (infect toronto 1000))))

;;(shamble town horde) produces a horde thats results from all the zombies at each
;;  location apporting themselves into nearly rqual groups and shambling along
;;  edges connecting the locations
;; Examples

;;shamble: Town Horde -> Horde
(define (shamble town horde)
  (local
    [(define (apportion-list town horde)
       (map (lambda (x y) (apportion (second y) (length (second x)))) town horde))


     (define (combine-list node-list apportion-list-1)
       (map (lambda (x y) (cons x (cons y empty))) node-list apportion-list-1))


     (define (zip town apportion-list-2 acc)
       (cond
         [(empty? town) acc]
         [else (zip (rest town) (rest apportion-list-2)
                    (append (combine-list (second (first town))
                                          (first apportion-list-2)) acc))]))


     (define (find-same-nodes lst node)
       (filter (lambda (x) (= (first x) node)) lst))

     (define (combine-nodes lst)
       (cond
         [(empty? lst) 0]
         [else (+ (second (first lst))
                  (combine-nodes (rest lst)))]))

     (define (split-list-nodes a)
       (cond
         [(empty? a) empty]
         [else (cons (first (first a)) (split-list-nodes (rest a)))]))

     (define (find-list Nat lst-2)
       (cond
         [(empty? lst-2) empty]
         [(= Nat (first (first lst-2))) (cons (first lst-2) (find-list Nat (rest lst-2)))]
         [else (find-list Nat (rest lst-2))]))


     (define (new-sorted-list lst-1 lst-2)
       (cond
         [(empty? lst-1) empty]
         [else (append (find-list (first lst-1) lst-2)
                       (new-sorted-list (rest lst-1) lst-2))]))

     
     (define (remove-and-add lst-zip lst-nodes)
       (cond
         [(empty? lst-nodes) empty]
         [(member? (first (first lst-zip)) lst-nodes)
          (cons (cons (first (first lst-zip))
                      (cons  (combine-nodes (find-same-nodes lst-zip
                                                             (first (first lst-zip)))) empty))
                (remove-and-add (rest lst-zip)
                                (remove-all (first (first lst-zip)) (rest lst-nodes))))]))]


    (remove-and-add (zip town (apportion-list town horde) empty)
                    (split-list-nodes (zip town (apportion-list town horde) empty)))))
    
                                                            
;;tests 
(check-expect
 (shamble waterloo braaaaains)
 (list (list 3 1741) (list 5 950) (list 1 1268) (list 0 475) (list 4 475) (list 2 791)))


;;(shamble toronto braaaaains-1)



;;
;; =============================================================================
;; V
;; =============================================================================
;;

(define braaaaaaains (shamble waterloo braaaaains))
(define braaaaaaains-1 (shamble toronto braaaaains))

;;(rise zombies horde) produces a new horde with new zombies added to the horde have
;;  been apportioned equally as possible between the loactions
;;Examples

;;rise: Nat Horde -> Horde
(define (rise zombies horde)
  (local
    [(define (apportioned-zombies zombies)
       (apportion zombies (length horde)))

     (define (new-zombies-added apportioned-new-zombies horde)
       (map (lambda (x y) (cons (first y) (cons (+ x (second y)) empty))) apportioned-new-zombies horde))]


    
    (new-zombies-added (apportioned-zombies zombies) horde)))


;;tests 
(check-expect (rise 300 braaaaaaains)
              (list (list 3 1791) (list 5 1000)
                    (list 1 1318) (list 0 525) (list 4 525) (list 2 841))) 



;;
;; =============================================================================
;; VI
;; =============================================================================
;;

;;(night town horde) produces a new horde after the horrors of single night have
;;  passed
;;Examples

;;night: Town Horde -> Horde
(define (night town horde)
  (rise (first (sink horde)) (shamble town (sort-list (second (sink horde))))))

;;(sort-list lst) sorts the list in increasing order
;; Examples


;;sort-list: Horde -> Horde
(define (sort-list lst)
  (local
    [(define (split-list-nums a)
       (cond
         [(empty? a) empty]
         [else (cons (first (first a)) (split-list-nums (rest a)))]))


     (define (find-list Nat lst-2)
       (cond
         [(empty? lst-2) empty]
         [(= Nat (first (first lst-2))) (cons (first lst-2) (find-list Nat (rest lst-2)))]
         [else (find-list Nat (rest lst-2))]))


     (define (new-sorted-list lst-1 lst-2)
       (cond
         [(empty? lst-1) empty]
         [else (append (find-list (first lst-1) lst-2)
                       (new-sorted-list (rest lst-1) lst-2))]))]


    (new-sorted-list (sort (split-list-nums lst) <) lst)))



;;tests
(check-expect (night waterloo (infect waterloo 1000))
              (list (list 3 1791) (list 5 1000) (list 1 1318)
                    (list 0 525) (list 4 525) (list 2 841)))


;;
;; =============================================================================
;; VII
;; =============================================================================
;;

;;(apocalypse town infection nights) produces the horde after a number of nights
;;  passed

;;Examples

;;apocalypse: Town Nat Nat -> Horde
(define (apocalypse town infection nights)
  (local
    [(define (apocalypse/acc horde num-nights)
       (cond
         [(= 1 num-nights) horde]
         [else (apocalypse/acc (night town horde) (sub1 num-nights))]))]


    (apocalypse/acc (night town (infect town infection)) nights)))

;;tests
(check-expect (apocalypse waterloo 1000 3)
              (list (list 3 1629) (list 5 476)
                    (list 1 1895) (list 0 449) (list 4 449) (list 2 1102)))


     




  
 










    















    
    

 

    
    







    
         










  
  






