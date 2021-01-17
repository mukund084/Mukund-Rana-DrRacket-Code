;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname roster) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***********************************************************
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignment 6, Q1
;; ***********************************************************
;;

;; =============================================================================
;; struct and data definitions For Q1
;; =============================================================================

;; A StudentID is a Nat with at most 8 digits (i.e. 0 <= id <= 99999999)

;; A Grade is one of:
;; * false
;; * Nat
;;   Requires: Nat is between 0 and 100 (inclusive)

(define-struct student (id name grade))
;; A Student is a (make-student StudentID Str Grade)


(define-struct rnode (student left right))
;; A Roster Node (RN) is a (make-rnode Student Roster Roster)
;; Requires: all students in the left subtree have an ID < student's ID
;;           all students in the right subtree have an ID > student's ID

;; A Roster is one of 
;; * empty
;; * RN


;; =============================================================================
;; constants used in Q1 examples
;; =============================================================================

(define beth (make-student 12345678 "Beth" 96))
(define jenny (make-student 08675309 "Jenny" 81))
(define john1 (make-student 48975311 "John" 95))
(define jenny/new (make-student 08675309 "Jen" 81))
(define bob (make-student 01675309 "Bob" false))
(define john2 (make-student 20488192 "John" false))

(define sample-roster
  (make-rnode beth ; root
              (make-rnode jenny empty empty)   ; left child
              (make-rnode john1 empty empty))) ; right child

(define sample-roster-2
  (make-rnode beth 
              (make-rnode jenny/new empty empty)
              (make-rnode john1
                          (make-rnode john2 empty empty)
                          empty)))

(define sample-roster-3
  (make-rnode beth 
             (make-rnode jenny/new (make-rnode bob empty empty) empty)
              (make-rnode john1
                          (make-rnode john2 empty empty)
                          empty)))

;;
;; =============================================================================
;; Q1 a) 
;; =============================================================================
;;

;;(find-student student-id roster) consumes a StuendID and RN and produces the
;;  the matching student
;; Examples
(check-expect (find-student 12345678 sample-roster ) beth)
(check-expect (find-student 87654321 sample-roster ) false)

;;find-student: StudentID Roster -> Str ;; (question: shoould I include requires roster to be binary search tree? or non-empty roster?  Question: Can I name my functions like this?
;; Questions about testing the code?
(define (find-student studentID roster)
  (cond
    [(empty? roster) false]
    [(= studentID (student-id (rnode-student roster))) (rnode-student roster)]
    [(< studentID (student-id (rnode-student roster)))
     (find-student studentID (rnode-left roster))]
    [(> studentID (student-id  (rnode-student roster)))
     (find-student studentID (rnode-right roster))]))

;;tests
(check-expect (find-student 08675309 sample-roster ) jenny)


;;
;; =============================================================================
;; Q1 b) 
;; =============================================================================
;;

;;(class-average class-roster) produrces the average mean of the class in the Roster
;; Examples 
(check-expect (class-average sample-roster) (+ 90 2/3))
(check-expect (class-average empty) 'N/A)

;;class-average: Roster -> Num
(define (class-average class-roster)
  (cond
    [(empty? class-roster) 'N/A]
    [else (/ (sum-grade class-roster) (valid-grade class-roster))]))

;;tests ;;
(check-expect (class-average sample-roster-2) (+ 90 2/3))

;;(valid-grade class-roster) produces the number of students with a valid grade 
;;Examples
(check-expect (valid-grade sample-roster) 3)
(check-expect (valid-grade sample-roster-2) 3)

;;valid-grade: RN -> Nat
(define (valid-grade class-roster)
  (cond
    [(empty? class-roster)  0]
    [else (+ (cond
               [(number? (student-grade (rnode-student class-roster))) 1]
               [else 0])
             (valid-grade (rnode-left class-roster))
             (valid-grade (rnode-right class-roster)))]))

;;(sum-grade class-roster) produces the sum of all the grades in roster ;; Question about base case and format of helper functions? 
;; Examples
(check-expect (sum-grade sample-roster) 272)
(check-expect (sum-grade sample-roster-2) 272)

;;sum-grade: RN -> Nat
(define (sum-grade class-roster)
  (cond
    [(empty? class-roster) 0]
    [else (+ (cond
               [(number? (student-grade (rnode-student class-roster))) (student-grade (rnode-student class-roster))]
               [else 0])
             (sum-grade (rnode-left class-roster))
             (sum-grade (rnode-right class-roster)))]))


;;
;; =============================================================================
;; Q1 c) 
;; =============================================================================
;;

;;(find-student/name name class-roster) produces of list of students who have the
;;  same corresponding name from the Roster
;;Examples
(check-expect (find-student/name "Beth" sample-roster) (list beth))
(check-expect (find-student/name "Dan" sample-roster) empty)

;; find-student/name: Str Roster -> (listof Str)
(define (find-student/name name class-roster)
  (cond
    [(empty? class-roster) empty]
    [else (append
           (find-student/name name (rnode-left class-roster))
           (cond
             [(string=? name (student-name (rnode-student class-roster)))
              (cons (rnode-student class-roster) empty) ]
             [else empty])
           (find-student/name name (rnode-right class-roster)))]))
             
                                                                    
                  

    
;;tests 
(check-expect (find-student/name "John" sample-roster-2) (list john2 john1))
(check-expect (find-student/name "Jenny" sample-roster) (list jenny))
                                                                                                     


;;
;; =============================================================================
;; Q1 d) 
;; =============================================================================
;;

;;(add-students  new-student new-roster) adds new students to a roster or changes
;;  their name to their preferred name
;;Examples
(check-expect (add-students (list (list 20488192 "John")
                                  (list 8675309 "Jen"))
                            sample-roster)
              sample-roster-2)


;;add-students: (listof (list StudentID Str)) Roster -> Roster
(define (add-students new-student new-roster)
  (cond
    [(empty? new-student) new-roster]
    [else (add-students (rest new-student) (add-students-one (first new-student) new-roster ))]))

;;tests
(check-expect (add-students (list (list 20488192 "John")
                                  (list 8675309 "Jen")
                                  (list 01675309 "Bob"))
                            sample-roster)
              sample-roster-3)


;;(add-students-one new-student class-roster) either adds the one new student
;;  to the roster or changes an existing student name to their prefered name

;;add-students-one: (list StudentID Str) Roster -> Roster
(define (add-students-one new-student class-roster)
  (cond
    [(empty? class-roster) (make-rnode (make-student (first new-student) (second new-student) false) empty empty) ]
    [(= (first new-student) (student-id (rnode-student class-roster)))
     (make-rnode (make-student (student-id (rnode-student class-roster))
                               (second new-student) (student-grade (rnode-student class-roster)))
                 (rnode-left class-roster) (rnode-right class-roster))]
    [(< (first new-student) (student-id (rnode-student class-roster)))
     (make-rnode (rnode-student class-roster) (add-students-one new-student
                                                                (rnode-left class-roster))
                 (rnode-right class-roster))] 
    [(> (first new-student) (student-id (rnode-student class-roster)))
     (make-rnode (rnode-student class-roster) (rnode-left class-roster)
                 (add-students-one  new-student (rnode-right class-roster)))]))







