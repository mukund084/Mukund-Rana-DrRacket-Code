;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Exercise-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define g
  '((A (C D E))
    (B (E J))
    (C ())
    (D (F J))
    (E (K))
    (F (K H))
    (H ())
    (J (H))
    (K ()))
  )



;;(count-in-neighbours g) consumes a Graph and produces
;;  a (listof Nat) indicating how many in-neighbours each node has.


;;Examples
(check-expect (count-in-neighbours g)
              (list 0 0 1 1 2 1 2 2 2))

;;super-filter: (X -> Bool) (nested-listof X) --> (nested-listof X)
(define (super-filter pred? nested-lst)
  (cond
    [(empty? nested-lst) empty]
    [(list? (first nested-lst))
     (cons  (super-filter pred? (first nested-lst))
            (super-filter pred? (rest nested-lst)))]
    [(pred? (first nested-lst))
     (cons (first nested-lst)
           (super-filter pred? (rest nested-lst)))]
    [else (super-filter pred? (rest nested-lst))]))



;;count-in-neighbours: Graph -> (listof Nat)
(define (count-in-neighbours g)
  (local
    [(define (split-list-nodes a)
       (cond
         [(empty? a) empty]
         [else (cons (first (first a)) (split-list-nodes (rest a)))]))


     (define (split-list-pairs a)
       (cond
         [(empty? a) empty]
         [else (cons (second (first a)) (split-list-pairs (rest a)))]))


     (define (check-node a b) (super-filter (lambda (x) (symbol=? a x)) b))


     (define (remove-empty g)
       (filter (lambda (x) (not (empty? x))) g))

     (define (count-in-neighbours/acc lst-1 lst-2)
       (cond
         [(empty? lst-1) empty]
         [else (cons (length (remove-empty (check-node (first lst-1) lst-2)))
                     (count-in-neighbours/acc (rest lst-1) lst-2))]))]


    (count-in-neighbours/acc (split-list-nodes g) (split-list-pairs g))))


(count-in-neighbours g)




    

    

     
       
    



     