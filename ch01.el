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




