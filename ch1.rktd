#lang racket

(define (square x) (* x x))
(define (sum-squares x y) (+ (square x) (square y)))
(define (average x y)
  (/ (+ x y) 2))
(define (mean-square x y)
  (average (square x)
           (square y)))
(define (abs x)
  (cond ((< x 0) (-x))
        ((= x 0) (0))
        ((> x 0) (x))))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     .001))

(define (try guess x)
  (if (good-enough? guess x)
      guess
      (try (improve guess x) x)))

(define (sqrt x) 
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x))
       .001))
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  (try 1))
  
  




