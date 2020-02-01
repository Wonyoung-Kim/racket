#lang racket

;; The big-num data structure is essentially a list of 3 digit numbers.

;; Exporting methods
(provide big-add big-subtract big-multiply big-power-of pretty-print number->bignum string->bignum)

(define MAX_BLOCK 1000)

;; Addition of two big-nums
(define (big-add x y)
  (big-add1 x y 0)
  )

(define (big-add1 x y co)
  (cond
    ;; If both lists are empty, the return value is either 0 or the caryover value.
    [(and (= 0 (length x)) (= 0 (length y)))
      (if (= co 0) '() (list co))]
    [(= 0 (length x))  (big-add1 (list co) y 0)]
    [(= 0 (length y))  (big-add1 x (list co) 0)]
    [else
      (if (< MAX_BLOCK (+ (car x) (car y)))
          (cons (+ (- (+ (car x) (car y)) MAX_BLOCK) co) (big-add1 (cdr x) (cdr y) 1)) ; goes over 1000 so handle carryover
          (cons (+ (+ (car x) (car y)) co) (big-add1 (cdr x) (cdr y) 0)))])) ; under 1000

;; Subtraction of two big-nums
(define (big-subtract x y)
  (let ([lst (big-subtract1 x y 0)])
    (reverse (strip-leading-zeroes (reverse lst)))
  ))

(define (strip-leading-zeroes x)
  (cond
    [(= 0 (length x)) '(0)]
    [(= 0 (car x)) (strip-leading-zeroes (cdr x))]
    [else x]
    ))

;; NOTE: there are no negative numbers with this implementation,
;; so 3 - 4 should throw an error.
(define (big-subtract1 x y borrow)
  (cond [(and (= 0 (length x)) (= 0 (length y))) (if (= 0 borrow) '() (list borrow))] ; similar checks to big-addl
        [(= 0 (length x)) (big-subtract1 (list borrow) y 0)]
        [(= 0 (length y)) (big-subtract1 x (list borrow) 0)]
        [(< (length x) (length y)) (error "NEGATIVE")] ; negative logic 1
        [(= (length x) (length y))
         (if (< (list-ref x (- (length x) 1)) (list-ref y (- (length y) 1))) ; negative logic 2
             (error "NEGATIVE")
             (if (< (- (car x) (car y)) 0) ; else do recursive subtract
              (cons (- (+ (car x) MAX_BLOCK) (car y)) (big-subtract1 (cdr x) (cdr y) 1)) ; needs to borrow
              (cons (- (- (car x) (car y)) borrow) (big-subtract1 (cdr x) (cdr y) 0))))]
        [else
          (if (< (- (car x) (car y)) 0)  ; do recursive subtract
              (cons (- (+ (car x) MAX_BLOCK) (car y)) (big-subtract1 (cdr x) (cdr y) 1)) ; needs to borrow
              (cons (- (- (car x) (car y)) borrow) (big-subtract1 (cdr x) (cdr y) 0)))]))

;; Returns true if two big-nums are equal
(define (big-eq x y)
  (if (equal? x y)
      #t
      #f))

;; Decrements a big-num
(define (big-dec x)
  (big-subtract x '(1))
  )

;; Multiplies two big-nums
(define (big-multiply x y)
  (if (equal? #t (big-eq y '(2)))
      (big-add x x) ; end case is the last add
      (big-add x (big-multiply x (big-dec y)))))
  
  ;; Solve this function in terms of big-add.
  ;; (Note that there are more efficient solutions,
  ;; but they are not required for this assignment).

;; Raise x to the power of y
(define (big-power-of x y)
  (if (equal? #t (big-eq y '(2))) ; similar to big-mult
      (big-multiply x x)
      (big-multiply x (big-power-of x (big-dec y)))))
  
  ;; Solve this function in terms of big-multiply.

;; Dispaly a big-num in an easy to read format
(define (pretty-print x)
  (let ([lst (reverse x)])
    (string-append
     (number->string (car lst))
     (pretty-print1 (cdr lst))
     )))

(define (pretty-print1 x)
  (cond
    [(= 0 (length x))  ""]
    [else
     (string-append (pretty-print-block (car x)) (pretty-print1 (cdr x)))]
    ))

(define (pretty-print-block x)
  (string-append
   ","
   (cond
     [(< x 10) "00"]
     [(< x 100) "0"]
     [else ""])
   (number->string x)))

;; Convert a number to a bignum
(define (number->bignum n)
  (cond
    [(< n MAX_BLOCK) (list n)]
    [else
     (let ([block (modulo n MAX_BLOCK)]
           [rest (floor (/ n MAX_BLOCK))])
       (cons block (number->bignum rest)))]))

;; Convert a string to a bignum
(define (string->bignum s)
  (let ([n (string->number s)])
    (number->bignum n)))



;;added test cases
;(big-eq '(999 999) '(999 999))
;(big-eq '(999 999) '(999 998))