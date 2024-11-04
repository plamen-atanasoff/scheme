#lang racket

;ex_4
(define (remove-first el l)
  (define not-el? (λ (x) (not (equal? x el))))
  (define right-sublist (dropf l not-el?))
  (append
   (takef l not-el?)
   (if (empty? right-sublist)
       '()
       (cdr right-sublist))))

(equal? (remove-first 1 '(1 1 1 2)) '(1 1 2))
(equal? (remove-first 1 '(2 5 6)) '(2 5 6))
(equal? (remove-first 1 '(1)) '())
(equal? (remove-first 1 '(2 1)) '(2))
(equal? (remove-first "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN" "RNN"))

(define (has-el? el l)
  (if (empty? l)
      #f
      (if (equal? (car l) el)
          #t
          (has-el? el (cdr l)))))

(define (remove-all el l)
  (define (helper xs)
    (if (has-el? el xs) (helper (remove-first el xs)) xs))
  (helper l))

(equal? (remove-all 1 '(1 1 1 2)) '(2))
(equal? (remove-all 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all 1 '(1)) '())
(equal? (remove-all 1 '(1 2 1 1)) '(2))
(equal? (remove-all "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))

;ex_5
(define (num-to-xs num)
  (define (helper cur)
    (if (= (quotient cur 10) 0)
        (cons cur '())
        (cons (remainder cur 10) (helper (quotient cur 10)))))
  (reverse (helper num)))

(equal? (num-to-xs 123) '(1 2 3))
(equal? (num-to-xs 123456789) '(1 2 3 4 5 6 7 8 9))

#|(define (xs-to-num l)
  (define (helper xs res)
    (if (empty? xs)
        res
        (helper (cdr xs) (+ (* res 10) (car xs)))))
  (helper l 0))|#

(define (xs-to-num l)
  (foldl (λ (a res) (+ (* res 10) a)) 0 l))

(= (xs-to-num '(1 2 3)) 123)
(= (xs-to-num '(1 2 3 4 5 6 7 8 9)) 123456789)

;ex_6
(define (set-union xs xy)
  (sort (remove-duplicates (append xs xy)) <))

(equal? (set-union '(1 3 5 7) '(5 7 13)) '(1 3 5 7 13))
(equal? (set-union '(5 7 13) '(1 3 5 7)) '(1 3 5 7 13))

;ex_7
(define (my-reverse-foldl l)
  (foldl cons '() l))

(equal? (my-reverse-foldl '(1 2 3 4 5)) '(5 4 3 2 1))

(not (list? (cons (cons (cons '() 1) 2) 3)))
(list? (cons 3 (cons 2 (cons 1 '()))))

;ex_8
(define (negative? n) (< n 0))

(define (kth-max-negative l)
  (λ (k)
    (list-ref (sort (remove-duplicates (filter negative? l)) >) (sub1 k))))

(= ((kth-max-negative '(-1)) 1) -1)
(= ((kth-max-negative '(-1 -5 -6 -6 -6 -6)) 2) -5)
(= ((kth-max-negative '(1 2 3 4 -5 6 7 -2 -1 0)) 2) -2)

;ex_9
(define (insert-at num pos l)
  (append (take l pos) (list num) (drop l pos)))

(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))

;ex_10
(define (concat xs xy)
  (foldr cons xy xs))

(equal? (concat '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

;ex_11
(define (factor? a n)
  (= (remainder n a) 0))

#|(define (prime? n)
  (define b (floor (sqrt n)))
  (define (helper a res)
    (if (> a b) res (helper (add1 a) (or res (factor? a n)))))
  (not (helper 2 #f)))|#

(define (factorise n)
  (define (helper a b res)
    (cond
      [(= b 1) res]
      [(factor? a b) (helper a (/ b a) (append res (list a)))]
      [else (helper (add1 a) b res)]))
    #|(if (= b 1)
        res
        (if (and (prime? a) (factor? a b))
            (helper a (/ b a) (append res (list a)))
            (helper (add1 a) b res))))|#
  (helper 2 n '()))

(equal? (factorise 2) '(2))
(equal? (factorise 6) '(2 3))
(equal? (factorise 13) '(13))
(equal? (factorise 123) '(3 41))
(equal? (factorise 152) '(2 2 2 19))
(equal? (factorise 12356498) '(2 7 11 19 41 103))

;ex_12
(define (longest-ascending-sub l)
  (define (helper xs cur res)
    (if (empty? xs)
        (if (> (length cur) (length res))
            cur
            res)
        (if (>= (first xs) (last cur))
            (helper (cdr xs) (append cur (list (first xs))) res)
            (if (> (length cur) (length res))
                (helper (cdr xs) (list (first xs)) cur)
                (helper (cdr xs) (list (first xs)) res)))))
  (if (empty? l)
      '()
      (helper (cdr l) (list (first l)) '())))

(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))

;ex_13
(define (get-sublist start-idx end-idx xs)
  (take (drop xs start-idx) (add1 (- end-idx start-idx))))

(equal? (get-sublist 2 6 '(1 2 2 3 1 5 6 7 7)) '(2 3 1 5 6))

;ex_14
(define (starts-with? sublist list)
  (define (helper xs xy)
    (if (empty? xs)
        #t
        (if (= (first xs) (first xy))
            (helper (cdr xs) (cdr xy))
            #f)))
  (helper sublist list))

(define (count-occurrences sublist list)
  (define (helper xs res)
    (if (> (length sublist) (length xs))
        res
        (if (starts-with? sublist xs)
            (helper (cdr xs) (add1 res))
            (helper (cdr xs) res))))
  (helper list 0))

(= (count-occurrences '(1 5) '(1 5 2 3 1 5 6 7 7 1 5)) 3)
(= (count-occurrences '(5 5) '(5 5 5 3 1 5 6 7 5 5 5)) 4)
(= (count-occurrences '(6 6) '(2 2)) 0)

;ex_15
(define (ordered? l)
  (λ (pred?)
    (define (helper xs)
      (if (or (empty? xs) (= (length xs) 1))
          #t
          (if (pred? (first xs) (second xs))
              (helper (cdr xs))
              #f)))
    (helper l)))

(equal? ((ordered? '(1 2 3 5)) (λ (x y) (< x y))) #t)
(equal? ((ordered? '(1 8 29 92)) (λ (x y) (= y (+ (* x 3) 5)))) #t)
(equal? ((ordered? '(1 8 3 14)) (λ (x y) (= y (+ (* x 3) 5)))) #f)

;ex_16
(define (satisfies? num preds)
  (define (helper xs)
    (if (empty? xs)
        #t
        (if ((first xs) num)
            (helper (cdr xs))
            #f)))
  (helper preds))

(define (where nums preds)
  (define (helper xs res)
    (if (empty? xs)
        res
        (if (satisfies? (first xs) preds)
            (helper (cdr xs) (append res (list (first xs))))
            (helper (cdr xs) res))))
  (helper nums '()))

(equal? (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10))
(equal? (where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) '())
(equal? (where '() (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '())
(equal? (where '(1 2 3 4 5 6 7 8 9 10 11 13 15) (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '(5 7 9 11 13 15))

(define (where1 xs ps)
  (define (helper current-element)
    (foldl (λ (x y) (and x y)) #t (map (λ (pred?) (pred? current-element)) ps))
    )

  (filter helper xs)
  )

(equal? (where1 '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10))
(equal? (where1 '(3 4 5 7) (list even? (lambda (x) (> x 5)))) '())
(equal? (where1 '() (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '())
(equal? (where1 '(1 2 3 4 5 6 7 8 9 10 11 13 15) (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '(5 7 9 11 13 15))

;ex_17
(define (get-pairs num l)
  (define (helper xs res)
    (if (empty? xs)
        res
        (helper (cdr xs) (append res (list (cons num (first xs)))))))
  (helper l '()))

(define (my-cartesian-product xs xy)
  (define (helper xz res)
    (if (empty? xz)
        res
        (helper (cdr xz) (append res (get-pairs (first xz) xy)))))
  (helper xs '()))

(equal? (my-cartesian-product '(1 2) '(3 4)) '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))
(equal? (my-cartesian-product '(1 2 3 4 5) '(6 7 8)) '((1 . 6) (1 . 7) (1 . 8) (2 . 6) (2 . 7) (2 . 8) (3 . 6) (3 . 7) (3 . 8) (4 . 6) (4 . 7) (4 . 8) (5 . 6) (5 . 7) (5 . 8)))

(define (my-cartesian-product1 xs ys)
  (define (helper x)
    (map (λ (y) (cons x y)) ys)
    )
  (foldl (λ (x acc) (append acc (helper x))) '() xs)
  )

(equal? (my-cartesian-product1 '(1 2) '(3 4)) '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))
(equal? (my-cartesian-product1 '(1 2 3 4 5) '(6 7 8)) '((1 . 6) (1 . 7) (1 . 8) (2 . 6) (2 . 7) (2 . 8) (3 . 6) (3 . 7) (3 . 8) (4 . 6) (4 . 7) (4 . 8) (5 . 6) (5 . 7) (5 . 8)))

;ex_18
(define (my-flatten l)
  (define (helper xs res)
    (if (empty? xs)
        res
        (if (list? (first xs))
            (helper (cdr xs) (append res (helper (first xs) '())))
            (helper (cdr xs) (append res (list (first xs)))))))
  (helper l '()))

(equal? (my-flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12)))))) '(1 2 3 4 5 6 7 8 9 10 11 12))

(define (my-flatten1 xss)
  (cond
    [(empty? xss) xss]
    [(list? (car xss)) (append (my-flatten1 (car xss)) (my-flatten1 (cdr xss)))]
    [else (cons (car xss) (my-flatten1 (cdr xss)))]
    )
  )

(equal? (my-flatten1 '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12)))))) '(1 2 3 4 5 6 7 8 9 10 11 12))

;ex_19
(define (shuffle l)
  (define b (/ (length l) 2))
  (define (helper xs xy res)
    (if (= (length xs) 0)
        res
        (helper (cdr xs) (cdr xy) (append res (list (first xs) (first xy))))))
  (helper (take l b) (drop l b) '()))

(equal? (shuffle '(2 5 1 3 4 7)) '(2 3 5 4 1 7))
(equal? (shuffle '(1 2 3 4 4 3 2 1)) '(1 4 2 3 3 2 4 1))
(equal? (shuffle '(1 1 2 2)) '(1 2 1 2))

;ex_20
(define (tabulate f)
  (λ (a b)
    (define (helper cur res)
      (if (> cur b)
          res
          (helper (add1 cur) (append res (list (cons cur (f cur)))))))
    (helper a '())))

(equal? ((tabulate (λ (x) (* x x))) 1 5) '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))

(define (tabulate1 f)
  (λ (a b) (map (λ (x) (cons x (f x))) (range a (add1 b))))
  )

(equal? ((tabulate1 (λ (x) (* x x))) 1 5) '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))

;ex_21
(define (deep-delete l)
  (define (helper xs level res)
    (if (empty? xs)
        res
        (if (list? (first xs))
            (helper (cdr xs) level (append res (list (helper (first xs) (add1 level) '()))))
            (if (< (first xs) level)
                (helper (cdr xs) level res)
                (helper (cdr xs) level (append res (list (first xs))))))))
  (helper l 1 '()))

(equal? (deep-delete '(1 (2 (2 4) 1) 0 (3 (1)))) '(1 (2 (4)) (3 ())))

;--------------------------------------------------------------------------------------------------------------





