#lang racket

(define (accumulate a b nv op next term)
  (define (helper current result)
    (if (> current b)
        result
        (helper (next current) (op result (term current)))))
  (helper a nv))

;ex_9
(define (argmin f a b)
  (define (helper cur m argm)
    (if (> cur b)
        argm
        (if (< (f cur) m)
            (helper (add1 cur) (f cur) cur)
            (helper (add1 cur) m argm))))
  (helper (add1 a) (f a) a))

(define (mod7 x) (remainder x 7))
(= (argmin mod7 45 50) 49)

;ex_10
(define (accumulate2 a b nv op next term)
  (define (helper current result)
    (if (<= current b)
        result
        (helper (next current) (op result (term current)))))
  (helper a nv))

(define (count-digits n)
  (if (= n 0)
      1
      (accumulate2 n 0 0 + (λ (x) (quotient x 10)) (λ (x) 1))))

(= (count-digits 12345) 5)
(= (count-digits 0) 1)

;ex_11
(define (count-pairs a b n)
  (define (in-interval? x) (and (>= x a) (<= x b)))
  (accumulate a b 0 + add1 (λ (x) (if (and (in-interval? (- n x)) (<= x (- n x))) 1 0))))

(= (count-pairs 1 10 14) 4)









