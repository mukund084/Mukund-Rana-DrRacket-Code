;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ==========================================================
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignmnet 7, Q2
;; ==========================================================
;;
(require "animals.rkt")

;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; Global Helper function
;;(new-list lstexamples) creates a new list of  attributes from the examples
;;new-list: (listof Example) -> (listof Sym) 
(define (new-list lstexamples)
  (cond
    [(empty? lstexamples) empty]
    [else (append (rest (first lstexamples)) (new-list (rest lstexamples)))]))

;;
;; =============================================================================
;; Q2 a) 
;; =============================================================================
;;

;; constanst for testing
(define seen
  (list
   (list 'squirrel 'small 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'crow 'medium 'flies 'angry)))

(define seen-1
  (list
   (list 'duck 'medium 'swims)
   (list 'squirrel 'small 'angry)
   (list 'crow 'small 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'duck 'large 'swims 'flies)))

;;(collect-attributes examples) produces a list of attributes contained in the
;;  examples with no duplicates
;; Examples
(check-expect (collect-attributes  seen)
              (list 'small 'large 'swims 'medium 'flies 'angry))

;;collect-attributes: (listof Example) -> (listof Sym)
(define (collect-attributes examples)
  (local
    [(define (remove-duplicate-attributes lst-attributes)
       (cond
         [(empty? lst-attributes) empty]
         [(member? (first lst-attributes) (rest lst-attributes))
          (remove-duplicate-attributes (rest lst-attributes))]
         [else (cons (first lst-attributes)
                     (remove-duplicate-attributes (rest lst-attributes)))]))]
    
    (remove-duplicate-attributes (new-list examples))))

;;tests
(check-expect (collect-attributes seen-1)
              (list 'medium 'small 'angry 'large 'swims 'flies))

(check-expect (collect-attributes empty)
              empty)

;;
;; =============================================================================
;; Q2 b) 
;; =============================================================================
;;

;;(split-examples examples symbol) splits the list of examples into two list
;;  where the first list contains the attributes/label in symbol and the second list
;;  does not contain it 

;;Examples
(check-expect (split-examples seen 'goose)
              (list
               (list
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry))
               (list
                (list 'squirrel 'small 'angry)
                (list 'crow 'medium 'flies 'angry))))
              

;;split-examples: (listof Example) Sym -> (listof (listof Example) (listof Example))
(define (split-examples examples symbol)
  (local
    [(define (contain-symbol? x)(member? symbol x))
     
     (define (not-contain-symbol? x) (not (member? symbol x)))

     (define (combine-split-list lst1 lst2)
       (list lst1 lst2))]

    (combine-split-list (filter contain-symbol? examples)
                        (filter not-contain-symbol? examples))))


;;tests
(check-expect (split-examples seen 'small)
              (list
               (list
                (list 'squirrel 'small 'angry))
               (list
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'crow 'medium 'flies 'angry))))

;;
;; =============================================================================
;; Q2 c) 
;; =============================================================================
;;

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.


;;(histogram examples) produces a list of attributes/countpairs with pair indicating
;;  the number of times that attribute appears in the examples
;;Examples
(check-expect (histogram seen)
              (list
               (list 'small 1) (list 'large 2) (list 'swims 2)
               (list 'medium 1) (list 'flies 3) (list 'angry 4)))


;; histogram: (listof Example) -> Histogram
(define (histogram examples)
  (local
    [(define (count-occurrences symbol lst-attributes)
       (cond
         [(empty? lst-attributes) 0]
         [(symbol=? symbol (first lst-attributes))
          (+ 1 (count-occurrences symbol (rest lst-attributes)))]
         [else (count-occurrences symbol (rest lst-attributes))]))

     (define (histogram lst-attributes short-list)
       (cond
         [(empty? short-list) empty]
         [else (cons (list (first short-list)
                           (count-occurrences (first short-list) lst-attributes))
                     (histogram lst-attributes (rest short-list)))]))]
    
    (histogram (new-list examples) (collect-attributes examples))))                   

;;tests 
(check-expect
 (histogram seen-1)
 (list
  (list 'medium 1) (list 'small 2) (list 'angry 3)
  (list 'large 2) (list 'swims 4) (list 'flies 3)))

(check-expect (histogram empty) empty)

;;
;; =============================================================================
;; Q2 d) 
;; =============================================================================
;;

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;;(augment-histogram histogram attributes total) produces augumented list that first
;;  adds any missing attributes to the histogram and second adds to the histogram
;;  number of examples not containing the attribute
;; Examples
(check-expect
 (augment-histogram
  (list (list 'a 100) (list 'c 50))
  (list 'a 'b 'c)
  200)
 (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))

;;augment-histogram: Histogram (listof Sym) Nat -> Augmented Histogram
(define (augment-histogram histogram attributes total)
  (local
    [(define (sym-list lstexamples)
       (cond
         [(empty? lstexamples) empty]
         [else (cons (first (first lstexamples)) (sym-list (rest lstexamples)))]))
     
     (define (augment-histogram/acc histogram attributes total histogram-attributes )
       (cond
         [(empty? attributes) empty ]
         [(empty? histogram)
          (cons (list (first attributes) 0  total)
                (augment-histogram/acc histogram
                                       (rest attributes) total histogram-attributes))]
         [(member? (first attributes) histogram-attributes)
          (cons (list (first (first histogram)) (second (first histogram))
                      (- total (second (first histogram))))
                (augment-histogram/acc (rest histogram)
                                       (rest attributes) total histogram-attributes))]
         [else (cons (list (first attributes) 0  total)
                     (augment-histogram/acc histogram
                                            (rest attributes) total histogram-attributes))]))]

    (augment-histogram/acc histogram attributes total (sym-list histogram))))

;;tests
(check-expect
 (augment-histogram empty (list 'x 'y) 10)
 (list (list 'x 0 10) (list 'y 0 10)))

;;
;; =============================================================================
;; Q2 e) 
;; =============================================================================
;;

;;(entropy positive-counts negative-counts) produces the entropy of two
;;  augmented histograms lists
;;Examples
(check-within (entropy (list 'large 126 59) (list 'large 146 669)) 0.5663948489858 0.01)

;;entropy:  Augmented Histogram  Augmented Histogram -> Num
(define (entropy positive-counts negative-counts)
  (local
    [
     (define (a lst-labels) (first lst-labels))
     (define (c lst-labels) (second lst-labels))
     (define (b lst-labels) (third lst-labels))
     (define (d lst-labels) (fourth lst-labels))

     (define (a-plus-b row1)
       (+ (first row1) (third row1)))

     (define (c-plus-d row2)
       (+ (second row2) (fourth row2)))

     (define (prob-1 n m)
       (cond
         [(> (+ n m) 0) (/ n (+ n m))]
         [else 0.5]))

     (define (e-prob-2 p)
       (cond
         [(= p 0) 0]
         [(and (> p 0) (<= p 1)) (* -1 p (log p 2))]))

     (define (final-entropy lst-labels)
       (+ (* (prob-1 (a-plus-b lst-labels) (c-plus-d lst-labels))
             (+ (e-prob-2 (prob-1 (a lst-labels) (b lst-labels)))
                (e-prob-2 (prob-1 (b lst-labels) (a lst-labels)))))
       
          (* (prob-1 (c-plus-d lst-labels) (a-plus-b lst-labels))
             (+ (e-prob-2 (prob-1 (c lst-labels) (d lst-labels)))
                (e-prob-2 (prob-1 (d lst-labels) (c lst-labels)))))))]


    (final-entropy (new-list (list positive-counts negative-counts)))))


;;tests
(check-within (entropy (list 'small 17 168) (list 'small 454 361)) 0.5825593868115 0.001)
(check-within (entropy (list 'a 0 100) (list 'b 100 0)) 0.0 0.001)
(check-within (entropy (list 'a 0 100) (list 'b 0 100)) 1.0 0.001)

;;
;; =============================================================================
;; Q2 f) 
;; =============================================================================
;;

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;;(entropy-attributes positive negative) computes the entropy of each attribute in
;;  an augmented histograms

;;entropy-attributes: AH AH -> EAL
(define (entropy-attributes positive negative)
  (cond
    [(empty? positive) empty]
    [else (cons (list (first (first positive))
                      (entropy (first positive) (first negative)))
                (entropy-attributes (rest positive) (rest negative)))]))
    

;;tests
(check-within (entropy-attributes
               (list
                (list 'large 126 59) (list 'angry 161 24)
                (list 'small 17 168) (list 'flies 170 15)
                (list 'swims 162 23) (list 'medium 42 143))
               (list
                (list 'large 146 669) (list 'angry 469 346)
                (list 'small 454 361) (list 'flies 615 200)
                (list 'swims 365 450) (list 'medium 215 600)))
              (list
               (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
               (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
               (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)) 0.001)

;;              
;; =============================================================================
;; Q2 g) 
;; =============================================================================
;;

;;(best-attribute entropies) finds the attribute with the minimum entropy or any
;;  such attribute if there's a tie
;; Examples
(check-expect (best-attribute
               (list
                (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
                (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
                (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)))
              'large)


;;best-attribute: EAL -> Sym
;;  Requires: entropies to be a non-empty list


(define (best-attribute entropies)
  (local
    [(define (min-entropy lst-entropy)
       (cond
         [(empty? (rest lst-entropy)) (first lst-entropy)]
         [else (min (first lst-entropy)
                    (min-entropy (rest lst-entropy)))]))

     (define (find-min-attribute nums lst-entroy-pairs)
       (cond
         [(empty? (rest lst-entroy-pairs)) (first (first lst-entroy-pairs))]
         [(= nums (second (first lst-entroy-pairs)))
          (first (first lst-entroy-pairs))]
         [else (find-min-attribute nums (rest lst-entroy-pairs))]))]
    
    (find-min-attribute (min-entropy (new-list entropies)) entropies)))

;;tests 
(check-expect (best-attribute
               (list
                (list 'large #i0.8663948489858) (list 'angry #i0.6447688190492)
                (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
                (list 'swims #i0.6017998773730) (list 'medium #i0.5663948489858)))
              'medium)

(check-expect (best-attribute
               (list
                (list 'large #i0.8663948489858) (list 'angry #i0.6447688190492)
                (list 'small #i0.5663948489858) (list 'flies #i0.6702490498564)
                (list 'swims #i0.6017998773730) (list 'medium #i0.5663948489858)))
              'small)

;;              
;; =============================================================================
;; Q2 h) 
;; =============================================================================
;;

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)


;;(build-dt examples label) builds the a decision tree using the ID3 algorithm
;; build-dt: (listof Examples) Sym -> DT 
(define (build-dt examples label)
  (local
    [(define (collect-attributes-dt examples)
       (collect-attributes examples))
      
     (define (postive-examples examples)
       (first (split-examples examples label)))

     (define (negative-examples examples)
       (second (split-examples examples label)))]
    
    (cond
      [(empty? (postive-examples examples)) false]
      [(empty? (negative-examples examples)) true]
      [(empty? (collect-attributes-dt examples))
       (cond
         [(> (length (postive-examples examples))
             (length (negative-examples examples))) true]
         [else false])]
         
      [else (local
              [(define root-attribute 
                 (best-attribute
                  (entropy-attributes
                   (augment-histogram
                    (histogram (postive-examples examples))
                    (collect-attributes examples)
                    (length (postive-examples examples)))
                   (augment-histogram
                    (histogram (negative-examples examples))
                    (collect-attributes examples)
                    (length (negative-examples examples))))))

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

               (define with-root 
                 (first (split-examples examples root-attribute)))

               (define without-root
                 (second (split-examples examples root-attribute)))

               (define (remove-root lst-examples)
                 (local [(define (not-root? root)
                           (not (symbol=? root root-attribute)))]
                   (super-filter not-root? lst-examples)))

               (define remove-with-root  
                 (remove-root with-root))


               (define left-subtree (build-dt remove-with-root label))
               (define right-subtree (build-dt without-root label))]

              (cond
                [(equal? left-subtree right-subtree) left-subtree]
                [else (cons root-attribute (list left-subtree right-subtree))]))])))

;;              
;; =============================================================================
;; Q2 i) 
;; =============================================================================
;;

;;(train-classifier examples label) generate a decision tree and then produces
;; a predicate that consumes a list a list of attributes and produces a decision

;;train-classifier: (listof Examples) Sym -> ((listof Sym) -> Bool)
(define (train-classifier examples label)
  (local
    [(define decision-tree
       (build-dt examples label))

     (define (pred? decision-tree lst-attributes)
       (cond
         [(boolean? decision-tree) decision-tree]
         [(member? (first decision-tree) lst-attributes)
          (pred? (second decision-tree) lst-attributes) ]
         [else (pred? (third decision-tree) lst-attributes)]))

     (define (final-pred? lst-attributes)
       (pred? decision-tree lst-attributes))]
    final-pred?))

;;tests 
(define goose? (train-classifier (random-animals 1000) 'goose))
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)
(define squirrel? (train-classifier (random-animals 1000) 'squirrel))
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small 'angry)) true)
(define crow? (train-classifier (random-animals 1000) 'crow))
(check-expect (crow? (list 'angry 'flies 'medium)) true)
    


     
                                               


   
      
      




    







         









