(define (isOneDigit? n) (if (= (quotient n 10) 0) #t #f))
(define (firstDigit n) (if (isOneDigit? n) n (firstDigit (quotient n 10))))
(define (lastDigit n) (remainder n 10))
(define (numWithoutLastDigit n) (quotient n 10))
(define (countDigits n) (if (isOneDigit? n) 1 (+ 1 (countDigits (numWithoutLastDigit n)))))
(define (numWithoutFirstDigit n) (- n (* (firstDigit n) (expt 10 (- (countDigits n) 1)))))
(define (numWithoutFirstAndLast n) (numWithoutFirstDigit (numWithoutLastDigit n)))
(define (palindrome? n) (if (isOneDigit? n) #t (if (= (firstDigit n) (lastDigit n)) (palindrome? (numWithoutFirstAndLast n)) #f)))

(define (fib n num1 num2) (if (= n 1) num2 (if (= n 2) num2 (fib (- n 1) (+ (- num1 num1) num2) (+ num1 num2)))))

(define (succ n) (+ n 1))
(define (pred n cur) (if (= n (succ cur)) cur (pred n (succ cur))))
