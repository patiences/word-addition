;   CRYPTARITHMETIC PUZZLES SOLVER
; 
; ;;P0:
;         S   E   N   D                9   5   6   7
;         M   O   R   E                1   0   8   5
; + ___________________         + __________________
;     M   O   N   E   Y            1   0   6   5   2   
; 
; ;;P1:                       ;;Soln                    
; 
;                 O   N                        2   3
;                 O   N                        2   3
;                 O   N                        2   3
; + ___________________         + ___________________
;                 G   O                        9   2 
; 


(define-struct letter (l n))
;; Letter is (make-letter 1-String Natural[0,9])
;; interp. a letter and its assigned number 

(define-struct problem (s r))
;; Problem is (make-problem (listof (listof 1String)) 
;;                          (listof 1String))
;; interp. a cryptarithmetic puzzle 
;;         s is the summands (listof (listof 1String)
;;         r is the sum (listof 1String)

(define P0 (make-problem (list (list "S" "E" "N" "D")
                               (list "M" "O" "R" "E"))
                         (list "M" "O" "N" "E" "Y")))
(define P1 (make-problem 
            (list (list "O" "N")
                  (list "O" "N")
                  (list "O" "N")
                  (list "O" "N"))
            (list "G" "O")))

;; Dict is (listof Letter)
;; interp. a dictionary of all letters in the problem 
(define DICT0 (list (make-letter "S" 9)  ;solution to P0 
                    (make-letter "E" 5)
                    (make-letter "N" 6)
                    (make-letter "D" 7)
                    (make-letter "M" 1)
                    (make-letter "O" 0)
                    (make-letter "R" 8)
                    (make-letter "Y" 2)))

(define DICT1 (list (make-letter "O" 2)  ;solution to P1
                    (make-letter "N" 3)
                    (make-letter "G" 9))) 

(define DICTX (list (make-letter "S" 3)  ;an invalid solution to P0
                    (make-letter "E" 5)
                    (make-letter "N" 6)
                    (make-letter "D" 7)
                    (make-letter "M" 1)
                    (make-letter "O" 0)
                    (make-letter "R" 8)
                    (make-letter "Y" 2)))

;; Problem Dict -> Dict
;; given a problem and a dict, produce the soln dictionary or false if can't 
;; ASSUME: if dict is not empty, all letters already defined are valid
; (check-expect (solve P0) DICT0)
; (check-expect (solve 
;                (make-problem 
;                 (list (list "A" "B" "C" "D" "E") ;more than 10 distinct letters 
;                       (list "F" "G" "H" "I" "J")) 
;                 
;                 (list "J" "K" "L" "M" "N"))
;                empty) 
;               false) 
; 
; (check-expect (solve 
;                (make-problem 
;                 (list (list "A" "B") ;sum must have at least same amount of chars 
;                       (list "C" "D" "E"))
;                 (list "F" "G"))
;                empty)
;               false)
; 
; (check-expect (solve 
;                (make-problem 
;                 (list (list "A" "B") ;sum is too long 
;                       (list "C" "D"))
;                 
;                 (list "E" "F" "G" "H"))
;                empty)
;               false)


(check-expect (solve P1 empty) DICT1)

(check-expect (solve (make-problem (list (list "T" "O" "O")
                                         (list "T" "O" "O")
                                         (list "T" "O" "O")
                                         (list "T" "O" "O"))
                                   (list "G" "O" "O" "D"))
                     empty)
              (list (make-letter "T" 4) 
                    (make-letter "O" 9) 
                    (make-letter "G" 1) 
                    (make-letter "D" 6)))

(define (solve p0 d)
  (local [(define (solve d)
            (if (complete? d p0)
                (if (valid? p0 d)
                    d
                    false)
                (solve-lod (next-dicts d p0))))
          (define (solve-lod lod)
            (cond [(empty? lod) false]
                  [else
                   (local [(define try (solve (first lod)))]
                     (if (not (false? try))
                         try
                         (solve-lod (rest lod))))]))]
    (solve d)))

; Termination Argument 
; 
; Base case: a complete dictionary (all letters in problem are defined)
; 
; Reduction step: update the dictionary by defining the next undefined 
;                 letter
; 
; This will eventually reach the base case because there are only a finite
; number of letters to be defined. 
; 


;; Problem -> Boolean 
;; produce true if all letters have been assigned a number 
(check-expect (complete? empty P0) false)
(check-expect (complete? DICT0 P0) true)
(check-expect (complete? DICTX P0) true)

(define (complete? d p)
  (= (length d) 
     (length (unique-letters p))))

;; Problem -> (listof String)
;; produce the number of unique letters in a given problem 
(check-expect (unique-letters P1) (list "O" "N" "G")) 

(define (unique-letters p)
  (unique-only 
   (compile (problem-s p)       ;(listof (listof 1String)) 
            (problem-r p))))    ;(listof 1String)

;; (listof 1String) ->  (listof 1String)
;; filter out the duplicate letters 
(check-expect (unique-only (list "A" "A" "A"))
              (list "A"))
(check-expect (unique-only (list "A" "B" "C"))
              (list "A" "B" "C"))

(define (unique-only los)
  ;; acc : (listof X) ; a list of elements already seen
  (local [(define (unique-only los acc)
            (cond [(empty? los) empty]
                  [else
                   (if (member? (first los) acc)
                       (unique-only (rest los) acc)
                       (cons (first los) 
                             (unique-only (rest los) 
                                          (cons (first los) acc))))]))]
    (unique-only los empty)))

;; (listof (listof 1String)) (listof 1String) -> (listof 1String)
;; compile all the 1-char strings in the problem into a flat list 
(check-expect (compile empty empty) empty)
(check-expect (compile empty (list "A")) (list "A"))
(check-expect (compile (list (list "A")) empty) (list "A"))
(check-expect (compile (list (list "A" "B")
                             (list "C")
                             (list "C" "D"))
                       (list "E" "F" "G" "H"))
              (list "A" "B" "C" "C" "D" "E" "F" "G" "H"))

; LOLOS ->            empty            (listof (listof 1String))         
; 
; LOS
;  v
; 
; empty                                 (extract LOLOS) (*2*)
; 
; 
;                    LOS (*1*)
; 
; (listof 
;  1String)                             (append (extract LOLOS) LOS)  (*3*) 


(define (compile lolos los)
  (local [(define extract-summands (foldr (λ (s los) (append s los)) empty lolos))]
    (cond [(empty? lolos) los]           ;1
          [(empty? los) extract-summands];2
          [else                          ;3
           (append extract-summands
                   los)])))

;; Dict Problem -> Boolean 
;; produce true if the dictionary is valid for the given problem
;; VALID? only gets called with complete dictionaries (dict can't be empty)
(check-expect (valid? P0 DICT0) true)
(check-expect (valid? P0 DICTX) false)
(check-expect (valid? P1 
                      (list (make-letter "G" 2) 
                            (make-letter "N" 5) 
                            (make-letter "O" 0))) false)

(define (valid? p d)
  (and (= (foldr + 0 (convert-to-num-summands (problem-s p) d))
          (convert (problem-r p) d))   ;summands add up to the sum
       (unique-assignments? d)         ;no multiple assignments of numbers
       (= (length d)                   ;all unique letters have assignments
          (length (unique-letters p)))
       (first-letters-not-zero? p d))) ;leading digits are not defined to be 0


;; (listof 1String) Dict -> Natural
;; produce the numerical equivalent of given list 
;; ASSUME: each 1String exists in the dictionary 
;;         (listof 1String) is not empty, no "" equivalent for numbers 
(check-expect (convert (list "S" "E" "N" "D") DICT0) 9567)
(check-expect (convert (list "M" "O" "N" "E" "Y") DICT0) 10652)

(define (convert los d)
  (string->number
   (foldr (λ (s rlos) (string-append (number->string (convert-to-num s d))
                                     rlos)) "" los)))

;; 1String Dict -> Natural 
;; produce the natural number assigned to the given string in the dictionary
(check-expect (convert-to-num "S" DICT0) 9)

(define (convert-to-num s d)
  (cond [(empty? d) false]
        [else
         (if (string=? s (letter-l (first d)))
             (letter-n (first d))
             (convert-to-num s (rest d)))]))

;; (listof (listof String) -> (listof Natural)
;; produce a list of the numerical equivalents 
;;        (convert-to-num for operating on summands)
(check-expect (convert-to-num-summands (list (list "S" "E" "N" "D")
                                             (list "M" "O" "R" "E"))
                                       DICT0)
              (list 9567 1085))

(define (convert-to-num-summands lolos d)
  (foldr (λ (first rest) (cons (convert first d) rest)) empty lolos))

;; Dict -> Boolean
;; produce true if all assigned numbers in the dictionary are unique
(check-expect (unique-assignments? DICT0) true)
(check-expect (unique-assignments? DICTX) true)
(check-expect (unique-assignments? (list (make-letter "S" 1)
                                         (make-letter "E" 1)))
              false)

(define (unique-assignments? d)
  (no-repeats? (pull-assignments d)))


;; Dict -> (listof Natural)
;; produce a list of all the assignments 
(check-expect (pull-assignments DICT0) 
              (list 9 5 6 7 1 0 8 2))

(define (pull-assignments d)
  (map (λ (ltr) (letter-n ltr)) d))

;; (listof X) -> Boolean
;; produce true if every element in the list is unique 
(check-expect (no-repeats? (list 1 2 3)) true)
(check-expect (no-repeats? (list 1 1)) false)

(define (no-repeats? lox)
  ;; acc : (listof X) ; list of elements already seen 
  (local [(define (fn-for-lox lox acc)
            (cond [(empty? lox) true]
                  [else 
                   (if (member? (first lox) acc)
                       false
                       (fn-for-lox (rest lox) (cons (first lox) acc)))]))]
    (fn-for-lox lox empty))) 

;; Problem Dict -> Boolean 
;; produce true if 1st letters in each summand and the sum are not assigned 0 
(check-expect (first-letters-not-zero? P1 
                                       (list (make-letter "O" 0) 
                                             (make-letter "N" 5) 
                                             (make-letter "G" 2)))
              false)
(check-expect (first-letters-not-zero? P0 DICT0) true)

(define (first-letters-not-zero? p d)
  (andmap (λ (n) (not (= 0 n))) (pull-firsts p d)))

;; Problem Dict -> (listof Natural)
;; produce list of the nums assigned to the 1st letters of each summand and sum
(check-expect (pull-firsts P0 DICT0)
              (list 9 1 1))

(define (pull-firsts p d)
  (append (convert-to-lon (map (λ (los) (first los)) (problem-s p)) d)
          (list (convert-to-num (first (problem-r p)) d))))

;; (listof 1String) Dict -> (listof Natural)
;; produce a list of the assigned numbers to the letters in given list 
(check-expect (convert-to-lon (list "S" "E" "N" "D") DICT0)
              (list 9 5 6 7))

(define (convert-to-lon los d)
  (map (λ (s) (convert-to-num s d)) los)) 

;; Dict Problem -> (listof Dict) 
;; produce next dictionaries by assigning values to 1st unassigned strng in prob
;; ASSUME: there is at least one undefined letter 
(check-expect (next-dicts empty P0)
              (list (list (make-letter "S" 0))
                    (list (make-letter "S" 1))
                    (list (make-letter "S" 2))
                    (list (make-letter "S" 3))
                    (list (make-letter "S" 4))
                    (list (make-letter "S" 5))
                    (list (make-letter "S" 6))
                    (list (make-letter "S" 7))
                    (list (make-letter "S" 8))
                    (list (make-letter "S" 9))))
(check-expect (next-dicts (list (make-letter "S" 9))
                          P0)
              (list (list (make-letter "S" 9) (make-letter "E" 0))
                    (list (make-letter "S" 9) (make-letter "E" 1))
                    (list (make-letter "S" 9) (make-letter "E" 2))
                    (list (make-letter "S" 9) (make-letter "E" 3))
                    (list (make-letter "S" 9) (make-letter "E" 4))
                    (list (make-letter "S" 9) (make-letter "E" 5))
                    (list (make-letter "S" 9) (make-letter "E" 6))
                    (list (make-letter "S" 9) (make-letter "E" 7))
                    (list (make-letter "S" 9) (make-letter "E" 8))
                    (list (make-letter "S" 9) (make-letter "E" 9))))


(define (next-dicts d p) 
  (assign-letter (find-first-undefined p d) d))

;; Problem Dict -> 1String 
;; produce the first 1string that isn't defined in the dictionary
;; ASSUME: there is at least 1 letter that is not defined in the dict 
(check-expect (find-first-undefined P0 empty) "S")
(check-expect (find-first-undefined P0 (list (make-letter "S" 9)))
              "E") 

(define (find-first-undefined p d)
  (first (filter (λ (s) (not (assigned? d s))) (unique-letters p))))

;; Dictionary 1String -> Boolean
;; produce true if there is already a definition for the given string in dict
(check-expect (assigned? DICT0 "S") true)
(check-expect (assigned? DICT0 "A") false)

(define (assigned? d s)
  (ormap (λ (ltr) (string=? s (letter-l ltr))) d))

;; 1String Dict -> (listof Dict)
;; update the current dictionary with a new definition for the given string 

(check-expect (assign-letter "S" empty)
              (list (list (make-letter "S" 0))
                    (list (make-letter "S" 1))
                    (list (make-letter "S" 2))
                    (list (make-letter "S" 3))
                    (list (make-letter "S" 4))
                    (list (make-letter "S" 5))
                    (list (make-letter "S" 6))
                    (list (make-letter "S" 7))
                    (list (make-letter "S" 8))
                    (list (make-letter "S" 9))))
(check-expect (assign-letter "A" DICT0)
              (list (append DICT0 (list (make-letter "A" 0)))
                    (append DICT0 (list (make-letter "A" 1)))
                    (append DICT0 (list (make-letter "A" 2)))
                    (append DICT0 (list (make-letter "A" 3)))
                    (append DICT0 (list (make-letter "A" 4)))
                    (append DICT0 (list (make-letter "A" 5)))
                    (append DICT0 (list (make-letter "A" 6)))
                    (append DICT0 (list (make-letter "A" 7)))
                    (append DICT0 (list (make-letter "A" 8)))
                    (append DICT0 (list (make-letter "A" 9)))))


(define (assign-letter s d)
  (map (λ (n) (append d (list (make-letter s n)))) (build-list 10 identity)))
