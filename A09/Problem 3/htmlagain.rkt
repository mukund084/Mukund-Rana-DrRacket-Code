;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname htmlagain) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ==========================================================
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignmnet 9, Q3 
;; ==========================================================
;;

;;
;; =============================================================================
;; Q3 a) 
;; =============================================================================
;;

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag
;; A Tag is (cons Sym (listof HI))

;;(tokenize str-html) produces a list of strings representing the opening tags,
;;  closing tags and strings in the document


;;Examples
(check-expect (tokenize "<p><h1>Heading</h1>Text</p>")
              '("<p>" "<h1>" "Heading" "</h1>" "Text" "</p>"))
(check-expect (tokenize "a b c") '("a b c"))



;;tokenize: Str -> (listof Str)
(define (tokenize s)
  (local
    [(define (opening-tag loc)
       (cond
         [(empty? loc) empty]
         [(= (length loc) 4) loc]
         [(char=? (first loc) #\>) (cons #\> empty)]
         [(char=? (third loc) #\/) (cons (first loc) empty)]
         [else (cons (first loc) (opening-tag (rest loc)))]))

     (define (closing-tag loc)
       (cond
         [(empty? loc) empty]
         [(= (length loc) 4) loc]
         [(char=? (first loc) #\>) (rest loc)]
         [(char=? (third loc) #\/) (rest loc)]
         [else (closing-tag (rest loc))]))

     (define (loc->lol loc)
       (local [(define fline (opening-tag loc))
               (define rlines (closing-tag loc))]
         (cond
           [(empty? loc) empty]
           [(not (member? #\> loc))  (list (list->string loc))]
           [(= (length loc) 4) (list (list->string loc))]
           [else (cons (list->string fline)
                       (loc->lol rlines))])))]

    (loc->lol (string->list s))))

;;tests
(check-expect (tokenize "") empty)

 
;;
;; =============================================================================
;; Q3 b) 
;; =============================================================================
;;

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag
;; A Tag is (cons Sym (listof HI))




;;(string->html str-html) produces the HI representation of str-html
;;Examples


;;string->html: Str -> HI
(define (string->html str-html)
  (local
    [(define (seperate-open-tag str-tag)
       (string->symbol (substring str-tag 1 (- (length (string->list str-tag)) 1))))

     (define (check-open-tag open-tag)
       (char=? (first (string->list open-tag)) #\<))

     (define (check-close-tag open-tag)
       (and (char=? (first (string->list open-tag)) #\<)
            (char=? (second (string->list open-tag)) #\/)))


     (define (change-into-nested stack acc)
       (cond
         [(and (string? (first stack))  (member? #\< (string->list (first stack ))))
          (cons (cons (seperate-open-tag (first stack)) acc) (rest stack))]
         [else (change-into-nested (rest stack)
                                   (cons (first stack) acc))]))

     (define (reverse-list lst acc)
       (cond
         [(empty? lst) acc]
         [else (reverse-list (rest lst)
                             (cons (first lst) acc))]))
         

         
     (define (string->html/acc html-lst stack-1)
       (cond
         [(empty? html-lst) stack-1]
         [(check-close-tag (first html-lst)) (string->html/acc (rest html-lst)
                                                               (change-into-nested stack-1 empty))]
         [(check-open-tag (first html-lst)) (string->html/acc (rest html-lst)
                                                              (cons (first html-lst) stack-1))]
         [(string? (first html-lst)) (string->html/acc (rest html-lst)
                                                       (cons (first html-lst) stack-1))]))]

    (first (reverse-list (string->html/acc (tokenize str-html) empty) empty))))



;;tests 

(check-expect (string->html
               "<p><h1>Heading</h1>Text</p>")
              '(p (h1 "Heading") "Text"))


(check-expect (string->html "<p><h1>Heading</h1>Text</p>")
              '(p (h1 "Heading") "Text"))


(check-expect (string->html (string-append
                             "<html><head><title>CS135</title></head>"
                             "<body><h1>Welcome</h1>More text...</body></html>"))
              '(html (head (title "CS135"))
                     (body (h1 "Welcome")
                           "More text...")))
             
    



