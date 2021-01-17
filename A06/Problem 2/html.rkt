;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname html) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =============================================================================
;; data definitions For Q2
;; =============================================================================

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag

;; A Tag is (cons Sym (listof HI))


;; =============================================================================
;; constants For Q2 examples
;; =============================================================================

(define just-text "Hello, world!")
(define short-example '(p (h1 "Heading") "Text"))
(define html-example '(html (head (title "CS135"))
                            (body (h1 "Welcome")
                                  "More text...")))

;;
;; =============================================================================
;; Q2 a) 
;; =============================================================================
;;


;;(html->string html-item) produces  the equivalent HTML text
;;Examples

(check-expect (html->string "text") "text")
(check-expect
 (html->string html-example)
 (string-append
  "<html><head><title>CS135</title></head>"
  "<body><h1>Welcome</h1>More text...</body></html>"))

;;html->string: (listof HI) -> Str
(define (html->string html-item)
  (cond
    [(string? html-item) html-item]
    [else (string-append (start-tag (first html-item)) (covert-to-string-hi-list (rest html-item))   (end-tag (first html-item)) )]))

;;tests
(check-expect (html->string short-example) "<p><h1>Heading</h1>Text</p>")
(check-expect (html->string just-text) just-text)

;;(covert-to-string-hi-list html-item) converts the (listof HI) into a list of string
;; covert-to-hi-list: (listof HI) -> Str
;;(define (covert-to-string html-item))
(define (covert-to-string-hi-list html-item)
  (cond
    [(empty? (rest html-item)) (html->string (first html-item))]
    [else (string-append (html->string (first html-item)) (covert-to-string-hi-list (rest html-item)))]))




;;(start-tag tag-name) produces the starting tag in html string
;; Examples
(check-expect (start-tag 'html) "<html>")

;;start-tag: Sym -> String
(define (start-tag tag-name)
  (string-append "<" (symbol->string tag-name) ">"))

;;tests
(check-expect (start-tag 'p) "<p>")

;;(end-tag tag-name) produces the starting tag in html string
;; Examples
(check-expect (end-tag 'html) "</html>")

;;start-tag: Sym -> String
(define (end-tag tag-name)
  (string-append "<" "/"(symbol->string tag-name) ">"))

;;tests
(check-expect (end-tag 'p) "</p>")



;;
;; =============================================================================
;; Q2 b) 
;; =============================================================================
;;

;;(remove-tag/part2 html-item) Breaks the (listof HI) in smaller parts for recursion
;; Examples

;;remove-tag: (listof HI) -> (anyof (listof HI)) 
(define (remove-tag-hi tag-sym html-item)
  (cond
    [(empty? html-item) empty]
    [(and (string? (first html-item))  (cons (first html-item) (remove-tag-hi tag-sym (rest html-item)))]
    [else (append (remove-tag tag-sym (first html-item)) (remove-tag-hi tag-sym (rest html-item)))]))



;;(remove-tag tag-sym html-item) removes the all removes all
;;  occurrences of that tag from HI, and moves it content to parent and if the
;;  root is removed then functtion should return a list of the children

;;Examples

;;remove-tag/acc: Sym  HI -> (anyof HI (listof (anyof Sym Str)))
(define (remove-tag tag-sym html-item)
  (cond
    [(string? html-item) html-item]
    [(symbol=? tag-sym (first html-item)) (remove-tag-hi tag-sym (rest html-item))]
    [else (cons (first html-item) (remove-tag-hi tag-sym (rest html-item)))]))


 (remove-tag 'b '(p "Hello, " (b "World") "!"))
;;
;; =============================================================================
;; Q2 c) 
;; =============================================================================
;;


;;(okay-tags? html-item) produces true if it has followed the Validation rules
;;  otherwise false

;; Examples


;;okay-tags?: HI -> Bool
;;(define (okay-tags? html-item)
;;  (cond
;;  




                  

     
