#lang sicp
(define (make-point x y)(cons x y))
(define (point-x p)(car p))
(define (point-y p)(cdr p))
(define (pretty-print p)
  (list 'x: (point-x p) 'y: (point-y p)))
(define (set-x! Xchange)
  (set! point-x Xchange))
(define (set-y! Ychange)
  (set! point-y Ychange))

;testing
(make-point 4 7)

;2 Defining operational clone 
;-----------------------------
(define (clone point_z x y)
  ;check if its a pair and then clone car and cdr
  (if(pair? point_z)
     (cons (clone (car point_z))
     (clone(cdr point_z)))
      point_z))

;-------------
;testing
(clone 1 18 20)



;3 Defining a point operational distance
----------------------------------------
;Defining square operation
(define (sqr value) (* value value))
(define (dif_z pt1 pt2)
  (make-point (- (point-x pt1) (point-x pt2))
         (- (point-y pt1) (point-y pt2))))

;Defining a point operation distance
(define (distance point1 point2)
  (let ((diff (dif_z point1 point2)))
    (sqrt (+ (sqr (point-x diff))
             (sqr (point-y diff))))))

;Testing
(distance (make-point 2 4) (make-point 8 7))
 
(define (make-point-2D x y)
  (let ((self (cons x y)))
    (define (point-x)
      (car self))
    (define (point-y)
      (cdr self))
    ;Defining point setter operations
    (define (set-x! v)
      (set-car! self v)
      )
    (define (set-y! y_value)
      (set-cdr! self y_value)
      )
    (define (clone) (make-point-2D x y))
    
    ;Calculation definitions for distance between points
    (define (distance point1 point2)
      (sqrt (+(+(-(expt (point1 'point-x) 2)
          (* 2(point1 'point-x) (point2 'point-x)))
            (expt (point2 'point-x) 2))
      (+(-(expt (point1 'point-y) 2)
          (* 2(point1 'point-y)
            (point2 'point-y)))
             (expt (point2 'point-y) 2)))))
    
    ;Translation definitions
    (define (translate point_1 dx dy)
      (point_1 'set-x! (- (point_1 'point-x) dx))
      (point_1 'set-y! (- (point_1 'point-y) dy))
      )
    ;Checking equivalence
    (define (point=? p1 p2)
      (if (< (distance p1 p2) 1)
          #t
          #f))
  ;-------------------------------------------- 
    (define (pretty-print)
      (list 'x: self))
        (lambda (message . args)
         (case message
            ((point-x) (point-x))
            ((point-y) (point-y))
            ((set-x!) (apply set-x! args))
            ((set-y!) (apply set-y! args))
            ((clone) (clone))
            ((distance) (apply distance args))
            ((translate) (apply translate args))
            ((pretty-print) (pretty-print))
            ((point=?) (apply point=? args))
            (else (error "UKNOWN MESSAGE"))))
    ))
(define point1 (make-point-2D 4 6))

(display "Printing\n")
(point1 'pretty-print)

;print x and y
(display "x and y values\n")
(point1 'point-x)
(point1 'point-y)

