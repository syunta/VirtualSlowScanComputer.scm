(define nil '())

(define (full-adder p q r)
  (receive (a b) (half-adder p q)
    (receive (c d) (half-adder a r)
      (values c (or b d)))))

(define (half-adder p q)
  (values (xor p q) (and p q)))

(define (xor p q)
  (or (and (not p) q) (and p (not q))))

(define (uint->int uint)
  (let iter ((uint uint) (n (- (length uint) 1)))
    (cond ((< n 0) 0)
          ((car uint) (+ (expt 2 n) (iter (cdr uint) (- n 1))))
          (else (iter (cdr uint) (- n 1))))))

(define (int->uint int n)
  (let iter ((int int) (n n) (uint nil))
    (if (= n 0)
      uint
      (iter (quotient int 2) (- n 1) (cons (odd? int) uint)))))

(define (uint-and xs ys)
  (map (lambda (x y) (and x y)) xs ys))

(define (uint-or xs ys)
  (map (lambda (x y) (or x y)) xs ys))

(define (uint-xor xs ys)
  (map (lambda (x y) (xor x y)) xs ys))

(define (uint-not xs)
  (map (lambda (x) (not x)) xs))

(define (uint-add xs ys)
  (cond ((or (null? xs) (null? ys))
         (values nil #f))
        (else
          (receive (seq r) (uint-add (cdr xs) (cdr ys))
          (receive (s c) (full-adder (car xs) (car ys) r)
                   (values (cons s seq) c))))))

(define (uint-inc xs)
  (cond ((null? xs) (values nil #t))
        (else
          (receive (seq r) (uint-inc (cdr xs))
          (receive (s c) (full-adder (car xs) #f r)
                   (values (cons s seq) c))))))

(define (uint-sub xs ys)
  (receive (complement+1 crry) (uint-inc (uint-not ys))
    (receive (result crry) (uint-add xs complement+1)
             (values result (not crry)))))

(define (uint-srl xs)
  (define (shift xs result)
    (if (null? (cdr xs))
      (values (reverse result) (car xs))
      (shift (cdr xs) (cons (car xs) result))))
  (receive (shifted lsb) (shift xs nil)
           (values (cons #f shifted) lsb)))

(define (uint-sll xs)
  (values (append (cdr xs) (list #f))
          (car xs)))
