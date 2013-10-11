;; Ex 1.11
(defun f (n)
  (if (< n 3)
      n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(defun f2 (n)
  (defun f2-iter (a b c now)
    (if (= now n)
        a
      (f2-iter (+ a (* 2 b) (* 3 c))
               a
               b
               (+ now 1))))
  (if (< n 3)
      n
    (f2-iter 2 1 0 2)))

(equal (mapcar 'f '(1 2 3 4 5 6 7 8 9 10))
       (mapcar 'f2 '(1 2 3 4 5 6 7 8 9 10)))

;; Ex 1.12

(defun pascal (i j)
  (cond ((or (<= i 0) (<= j 0)) 0)
        ((and (= i 1) (= j 1)) 1)
        (t (+ (pascal (- i 1) (- j 1)) (pascal (- i 1) j)))))

(pascal 6 6)

;; Ex 1.13

;; mathematical problem, ignored, :p.


;; Ex 1.14
;; coins: 50,25,10,5,1
cc(11, 5)
|
cc(11, 4), cc(-39, 5)
|
cc(11, 3), cc(-14, 4) 0
|
cc(11, 2), cc(1, 3) 0
|
cc(11, 1), cc(6, 2), cc(1, 2), cc(0, 3)
|
cc(11, 0), cc(10, 1), cc(6, 1), cc(1, 2), cc(1, 1), cc(-4, 2), 1
|
0, cc(10, 0), cc(9, 1), cc(6, 0), cc(5, 1)....

;; Ex 1.15
(defun cube (x) (* x x x))

(defun p (x) (- (* 3 x) (* 4 (cube x))))

(defun sine (angle)
  (if (not (> (abs angle) 0.1))
      angle
    (p (sine (/ angle 3.0)))))

;; FIXME: remove the global var, unadvice the advice
(defvar *count* 0)

(defmacro counter (func &rest args)
  `(progn
     (defadvice ,func (before foobar) 'activate
       (setq *count* (+ *count* 1))
       (message "[DEBUG] count %d" *count*))
     (setq *count* 0)
     (,func ,@args)
     *count*))

(ad-deactivate 'sine)
(ad-activate 'sine)
(ad-disable-advice 'sine 'before 'foo)
(ad-disable-advice 'sine 'before 'foobarnew2)
(ad-get-enabled-advices 'sine 'before)
(macroexpand '(counter sine 12.15))
(counter sine 12.15)

(mapcar (lambda (x) (counter sine x)) '(1 3 9 27 81 243))

;; a. 6
;; b. space: log3(a) number of steps: log3(a)


;; Ex 1.16

(defun fast-exp (b n)
  ;; a * b ^ n is invariant
  (defun fast-exp-iter (a b n)
    (cond
     ((= n 0) a)
     ((evenp n) (fast-exp-iter a (* b b) (/ n 2)))
     (t (fast-exp-iter (* a b) b (- n 1)))))
  (fast-exp-iter 1 b n))

(fast-exp 2 5)

;; Ex 1.17
;; lazy to do...

;; Ex 1.18

(defun fast-mul (a b)
  ;; rem + a * b is invariant
  (defun fast-mul-iter (rem a b)
    (cond
     ((= b 0) rem)
     ((evenp b) (fast-mul-iter rem (* 2 a) (/ b 2)))
     (t (fast-mul-iter (+ rem a) a (- b 1)))))
  (fast-mul-iter 0 a b))

(fast-mul 6 7)


;; Ex 1.19
p' = p^2 + q^2
q' = q^2 + 2pq

;; Ex 1.20

;; ref: http://wiki.drewhess.com/wiki/SICP_exercise_1.20
normal order: 18
applicative order: 4

;; Ex 1.21

(defun smallest-divisor (n)
  (find-divisor n 2))
(defun find-divisor (n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (t (find-divisor n (+ test-divisor 1)))))
(defun divides? (a b)
  (= (% b a) 0))

(mapcar 'smallest-divisor '(199 1999 19999))

;; Ex 1.22

(float-time)

(defun prime? (n)
  (= n (smallest-divisor n)))

(defun prime-test (n)
  (start-prime-test n (runtime)))

(defun start-prime-test (n time)
  (if (prime? n)
      (- (runtime) time)))

(defun search-for-primes (n count)
  (defun search-for-primes-iter (n count iter acc)
    (if (= iter count) (/ acc count)
      (progn
        (let ((result (prime-test n)))
          (if result
              (search-for-primes-iter (+ n 2) count (+ iter 1) (+ acc result))
            (search-for-primes-iter (+ n 2) count iter acc))))))
  (search-for-primes-iter n count 0 0))

;; NEEDED, as no tail recursion optimize for ELisp..
(setq max-lisp-eval-depth 1000000)
(setq max-specpdl-size 1000000)
(search-for-primes 1000001 3)
(search-for-primes 100001 3)
(search-for-primes 10001 3)
(search-for-primes 1001 3)

(mapcar (lambda (n)
          (search-for-primes n 3)) '(1001 10001 100001 1000001))

;; results
(2.471605936686198e-05 7.843971252441406e-05 0.00025208791097005207 0.0007874965667724609)

;; Ex 1.23

(defun next (n)
  (if (= n 2)
      3
    (+ n 2)))

(defun find-divisor (n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (t (find-divisor n (next test-divisor)))))
;; results
(1.0569890340169271e-05 3.147125244140625e-05 9.870529174804688e-05 0.0003094673156738281)


;; Ex 1.24

(defun even? (n)
  (= (% n 2) 0))

(defun remainder (a b)
  (% a b))

(defun square (n)
  (* n n))

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (t
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(defun fermat-test (n)
  (defun try-it (a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(defun fast-prime? (n times)
  (cond ((= times 0) t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (t nil)))

(defun fast-prime-time-helper (n times time)
  (if (fast-prime? n times)
      (- (runtime) time)))

(defun fast-prime-time (n times)
  (fast-prime-time-helper n times (runtime)))

(fast-prime? 24 10)

(defun search-for-primes (n count)
  (defun search-for-primes-iter (n count iter acc)
    (if (= iter count) (/ acc count)
      (progn
        (let ((result (fast-prime-time n 20)))
          (if result
              (search-for-primes-iter (+ n 2) count (+ iter 1) (+ acc result))
            (search-for-primes-iter (+ n 2) count iter acc))))))
  (search-for-primes-iter n count 0 0))

(mapcar (lambda (n)
          (search-for-primes n 3)) '(1001 10001 100001 1000001))

; f(1000 ^ 2) ~= 2 * f(1000)

;; Ex 1.25

;; base^exp grow rapidly

;; Ex 1.26

;; fast-prime? is now O(n).

;; Ex 1.27
;; Carmichael numbers 561, 1105, 1729, 2465, 2821, and 6601

(defun test-n (n)
  (defun test-n-iter (n i)
    (cond
     ((= i n) t)
     ((= (expmod i n n) i) (test-n-iter n (+ i 1)))
     (t nil)))
  (test-n-iter n 1))

(mapcar 'test-n '(561 1105 1729 2465 2821 6601))

(defun foo (n)
  (+ n 1))

(defun bar (foo)
  (foo 10))

