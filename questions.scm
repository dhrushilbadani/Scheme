; Some utility functions that you may find useful.
(define (apply-to-all proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (apply-to-all proc (cdr items)))))

(define (keep-if predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (keep-if predicate (cdr sequence))))
        (else (keep-if predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

;(define (first-of-all pairs)
;(cond
  ;((null? pairs) ())
  ;(else (cons (caar pairs) (first-of-all (cdr pairs))))
  ;)
;)

;(define (second-of-all pairs)
;(cond
  ;((null? pairs) ())
  ;(else (cons (cadar pairs) (second-of-all (cdr pairs))))
  ;)
;)

; Problem 18
;; Turns a list of pairs into a pair of lists
(define (zip pairs)
  (cond
  ((null? pairs) (list (list) (list)))
  (else (list (apply-to-all car pairs) (apply-to-all cadr pairs)))
  )
  )

(zip '())
; expect (() ())
(zip '((1 2)))
; expect ((1) (2))
(zip '((1 2) (3 4) (5 6)))
; expect ((1 3 5) (2 4 6))

; Problem 19

;; A list of all ways to partition TOTAL, where  each partition must
;; be at most MAX-VALUE and there are at most MAX-PIECES partitions.
(define (list-partitions total max-pieces max-value)
  (define (helper curr-partition pieces value curr-total ) ; Helper function helps store the partition uptil now
    (cond 
      ((< curr-total value) (helper curr-partition pieces curr-total curr-total))
      ((< curr-total pieces) (helper curr-partition curr-total value curr-total))
      ((= curr-total 0) (list curr-partition))
      ((or (<= value 0) (<= pieces 0) (< curr-total 0)) () )
      (else  (append (helper (append curr-partition (list value)) (- pieces 1) value (- curr-total value))
                     (helper curr-partition pieces (- value 1) curr-total )))
      )
  )
  (helper () max-pieces max-value total )
)
;(define (list-partitions total max-pieces max-value)
  ;(define (list-partitions_helper total max-pieces max-value partition)
    ;(cond ((= total 0)
      ;(list partition))
        ;((< total 0)())
        ;((<= max-value 0)())
          ;((<= max-pieces 0)())
            ;(else
    ;(append (list-partitions_helper (- total max-value) (- max-pieces 1) max-value (append partition(list max-value)))
      ;(list-partitions_helper total max-pieces (- max-value 1)partition)))))
    ;(list-partitions_helper total max-pieces max-value ()))

(list-partitions 5 2 4)
; expects a permutation of ((4 1) (3 2))
(list-partitions 7 3 5)
; expects a permutation of ((5 2) (5 1 1) (4 3) (4 2 1) (3 3 1) (3 2 2))


; Problem 20
;; Returns a function that takes in an expression and checks if it is the special
;; form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
         expr
         )
        ((quoted? expr)
         expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           (cons form (cons params (apply-to-all analyze body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           (cons (cons 'lambda (cons (car (zip values)) (apply-to-all analyze body))) (apply-to-all analyze (cadr (zip values))))
           ))
        (else
         (apply-to-all analyze expr)
         )))

(analyze 1)
; expect 1
(analyze 'a)
; expect a
(analyze '(+ 1 2))
; expect (+ 1 2)

;; Quoted expressions remain the same
(analyze '(quote (let ((a 1) (b 2)) (+ a b))))
; expect (quote (let ((a 1) (b 2)) (+ a b)))

;; Lambda parameters not affected, but body affected
(analyze '(lambda (let a b) (+ let a b)))
; expect (lambda (let a b) (+ let a b))
(analyze '(lambda (x) a (let ((a x)) a)))
; expect (lambda (x) a ((lambda (a) a) x))

(analyze '(let ((a 1)
                (b 2))
            (+ a b)))
; expect ((lambda (a b) (+ a b)) 1 2)
(analyze '(let ((a (let ((a 2)) a))
                (b 2))
            (+ a b)))
; expect ((lambda (a b) (+ a b)) ((lambda (a) a) 2) 2)
(analyze '(let ((a 1))
            (let ((b a))
              b)))
; expect ((lambda (a) ((lambda (b) b) a)) 1)


;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  'YOUR-CODE-HERE
  nil)




