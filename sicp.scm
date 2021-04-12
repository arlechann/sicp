(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination
                           kinds-of-coins))
                       kinds-of-coins)))))
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (cc amount 5))

(define (expt b n)
  (define (iter b counter product)
    (if (= counter 0)
        product
        (iter b
              (- counter 1)
              (* b product))))
  (iter b n 1))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (define (iter b counter a)
    (cond ((= counter 0) a)
          ((even? counter) (iter (square b) (/ counter 2) a))
          (else (iter b (- counter 1) (* a b)))))
  (iter b n 1))

