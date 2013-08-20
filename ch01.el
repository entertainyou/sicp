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


