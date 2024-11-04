#lang racket

(define (my-gcd a b) (if (= a 0) b (my-gcd (remainder b a) a)))

(define (help-gosho money) (
   cond
    [(>= money 10) "Pizza"]
    [(>= money 5) "Doner"]
    [(>= money 3) "University cafeteria"]
    [else "Hungry :("]
  )
)

(define (help-students x y) (
   cond
    [(and (= x 0) (= y 0)) "Center"]
    [(= x 0) "Ordinate"]
    [(= y 0) "Abscissa"]
    [(> x 0) (if (> y 0) "1 Quadrant" "4 Quadrant")]
    [(< x 0) (if (> y 0) "2 Quadrant" "3 Quadrant")]
  )
)