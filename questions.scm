

; Some utility functions that you may find useful.
(define (apply-to-all proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (apply-to-all proc (cdr items)))))

(define (cons-all first rests)
  (apply-to-all (lambda (rest) (cons first rest)) rests))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Problem 18
;; Turns a list of pairs into a pair of lists
(define (zip pairs)
    (if (null? pairs) 
          '(()())
          (cons (cons (car (car pairs)) (car (zip (cdr pairs)))) (cons (cons (car (cdr (car pairs))) (car (cdr (zip (cdr pairs))))) '()))
    )
)

(zip '((1 2) (3 4) (5 6)))
; expect ((1 3 5) (2 4 6))
(zip '((1 2)))
; expect ((1) (2))
(zip '())
; expect (() ())

; Problem 19

;; List all ways to partition TOTAL without using consecutive numbers.
(define (list-partitions total) 
  (filter no-consecutives (helper total total))
  ; (helper total total)
)

(define (filter f s)
  (if (null? s) s
    (let ((rest (filter f (cdr s))))
      (if (f (car s)) (cons (car s) rest) rest))))

(define (no-consecutives s)
    (cond ((null? (cdr s)) #t)
          ((= (car s) (+ (cadr s) 1)) #f)
          (else (no-consecutives (cdr s)))
    )
)

(define (helper amount parts)
    (cond ((= amount 0) (list nil))
          ((= parts 0) nil)
          ((< amount 0) nil)
          (else (begin
                    (define using_parts (helper (- amount parts) parts))
                    (define with_parts (cons-all parts using_parts))
                    (define without_parts (helper amount (- parts 1)))
                    (append with_parts without_parts)
                )
          )
    )
)

; For these two tests, any permutation of the right answer will be accepted.
(list-partitions 5)
; expect ((5) (4 1) (3 1 1) (1 1 1 1 1))
(list-partitions 7)
; expect ((7) (6 1) (5 2) (5 1 1) (4 1 1 1) (3 3 1) (3 1 1 1 1) (1 1 1 1 1 1 1))

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
         ((lambda (x) x) expr)
        )
        
        ((quoted? expr)
         ((lambda (x) x) expr)
        )
        
        ((or (lambda? expr)
             (define? expr))
          (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
               (cons form (cons params (apply-to-all analyze body)))
          )
        )
        
        ((let? expr)
          (let ((values (cadr expr))
               (body    (cddr expr)))
            (let
              ((params (car (zip values)))
              (bindings (cdr (zip (apply-to-all analyze values)))))
              (cons (cons 'lambda (cons params (apply-to-all analyze body))) (car bindings))
            )
          )
        )
        
        (else
         (apply-to-all analyze expr)
        )
  )
)

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
(analyze '(+ 1 (let ((a 1)) a)))
; expect (+ 1 ((lambda (a) a) 1))


;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  'YOUR-CODE-HERE
  nil)

