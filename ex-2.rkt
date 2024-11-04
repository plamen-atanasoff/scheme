#lang racket

(define (accumulate op nv a b f next) (if (> a b) nv (op (f a) (accumulate op nv (next a) b f next))))

(define (fact n) (accumulate * 1 1 n (lambda (x) x) (lambda (x) (+ x 1))))

(define (pow x n) (accumulate * 1 1 n (lambda (a) x) (lambda (a) (+ a 1))))

(define (sum1 x n) (accumulate + 0 0 n (lambda (a) (/ (pow x a) (fact a))) (lambda (a) (+ a 1))))

(define (pred? x a b pred) (accumulate (lambda (a1 a2) (or a1 a2)) #f a b pred (lambda (c) (+ c 1))))

;------------------------------------------------------------------------------------------------------------

;ex_5
(define (my-recursive-pow x n) (if (= n 1) x (* x (my-recursive-pow x (- n 1)))))
(define (my-iterative-pow x n)
  (define (helper x n res)
    (if (= n 0) res (helper x (- n 1) (* res x))))
  (helper x n 1))

;ex_6
(define (prime? n)
  (define sqrt-n (floor (sqrt n)))
  (define (helper a)
    (if (> a sqrt-n)
        #t
        (if (= (remainder n a) 0)
            #f
            (helper (+ a 1)))))
  (if (<= n 1) #f (helper 2)))


;ex_7
(define (increasing-digits? n)
  (define is-one-digit? (= (quotient n 10) 0))
  (define second-last-digit (quotient (remainder n 100) 10))
  (define last-digit (remainder n 10))
  (define last-two-digits-are-asc? (<= second-last-digit last-digit))
  (if is-one-digit?
      #t
      (if last-two-digits-are-asc? (increasing-digits? (quotient n 10)) #f)))

;ex_8
(define (palindromes-between a b)
  (define (is-one-digit? x)
    (= (remove-last-digit x) 0))
  (define (last-digit x)
    (remainder x 10))
  (define (remove-last-digit x)
    (quotient x 10))
  (define (digits-count x)
    (if (is-one-digit? x) 1 (+ 1 (digits-count (remove-last-digit x)))))
  (define (first-digit x)
    (quotient x (expt 10 (- (digits-count x) 1))))
  (define (remove-first-digit x)
    (- x (* (first-digit x) (expt 10 (- (digits-count x) 1)))))
  (define (is-palindrome? x)
    (if (is-one-digit? x)
        #t
        (if (= (last-digit x) (first-digit x))
            (is-palindrome? (remove-first-digit (remove-last-digit x)))
            #f)))
  (define (helper x ctr)
    (if (> x b)
        ctr
        (helper (+ x 1) (if (is-palindrome? x) (+ ctr 1) ctr))))
  (helper a 0))

;ex_9
(define (has-given-digit? n d)
  (define is-one-digit?
    (= (quotient n 10) 0))
  (define last-digit
    (remainder n 10))
  (define (remove-last-digit x)
    (quotient x 10))
  (if is-one-digit?
      (= n d)
      (if (= d last-digit)
          #t
          (has-given-digit? (remove-last-digit n) d))))

(define (prime-generator from)
  (define next (+ from 1))
  (if (prime? next) next (prime-generator next)))

(define (prime-generator-modified from d)
  (define next (+ from 1))
  (if (and (prime? next) (has-given-digit? next d)) next (prime-generator-modified next d)))

(define (sum-first-special-numbers n d)
  (define (helper ctr sum from)
    (define next-special-prime (prime-generator-modified from d))
    (if (> ctr n) sum (helper (+ ctr 1) (+ sum next-special-prime) next-special-prime)))
  (helper 1 0 1))

;ex_10
(define (sqr n) (* n n))
(define (last-digit n) (remainder n 10))
(define (have-same-last-digits? a b) (= (last-digit a) (last-digit b)))
(define (remove-last-digit n) (quotient n 10))
(define (is-one-digit? n) (= (quotient n 10) 0))
(define (digits-count x) (if (is-one-digit? x) 1 (+ 1 (digits-count (remove-last-digit x)))))

(define (automorphic? n)
  (define (helper a b from to)
    (if (> from to)
        #t
        (if (have-same-last-digits? a b)
            (helper (remove-last-digit a) (remove-last-digit b) (+ from 1) to)
            #f)))
  (helper n (sqr n) 1 (digits-count n)))

;ex_11
(define (automorphic2? a b)
  (define (helper a b from to)
    (if (> from to)
        #t
        (if (have-same-last-digits? a b)
            (helper (remove-last-digit a) (remove-last-digit b) (+ from 1) to)
            #f)))
  (helper a b 1 (digits-count a)))

(define (subnumber? x y)
  (define (helper b from to)
    (if (> from to)
        #f
        (if (automorphic2? x b)
            #t
            (helper (remove-last-digit b) (+ from 1) to))))
  (helper y 1 (+ (- (digits-count y) (digits-count x)) 1)))

;ex_12
(define (second-last-digit n) (remainder (quotient n 10) 10))

(define (all-digits-equal? n)
  (if (is-one-digit? n)
      #t
      (if (= (last-digit n) (second-last-digit n))
          (all-digits-equal? (remove-last-digit n))
          #f)))

(define (sum n)
  (define (helper x res)
    (if (is-one-digit? x) (+ res x) (helper (remove-last-digit x) (+ res (last-digit x)))))
  (helper n 0))

(define (digital-root n)
  (if (all-digits-equal? n)
      (last-digit n)
      (digital-root (sum n))))

;ex_13
(define (get-first-position n d)
  (define (helper a ctr)
    (if (is-one-digit? a)
      (if (= a d)
          ctr
          -1)
      (if (= (last-digit a) d)
          ctr
          (helper (remove-last-digit a) (+ ctr 1)))))
  (helper n 1))

(define (remove-first-occurrence n d)
  (define num-string (number->string n))
  (define len (string-length num-string))
  (define pos (- len (get-first-position n d)))
  (string->number (string-append (substring num-string 0 pos) (substring num-string (+ pos 1) len))))

;ex_14 TODO

;ex_15 TODO




