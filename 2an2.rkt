#lang sicp

; Building on the above implementation, define the following operations for a point. object
(define (make-point x y)(cons x y))
(define (point-x p)(car p))
(define (point-y p)(cdr p))
(define (pretty-print p)
  (list 'x: (point-x p) 'y: (point-y p)))

;----------"definitions below"-------------

;Define point setter operations set-x! and set-y! to change the value of x and y coordinates, respectively.
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (set-x! Xchange)
  (set! point-x Xchange))
(define (set-y! Ychange)
  (set! point-y Ychange))

;testing
(make-point 4 7)



;2 Define a point operation clone that takes as argument a point object and returns its copy. The cloned point object should be independent of the original one.
;---------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (clone point_z x y)
  ;check if its a pair and then clone car and cdr
  (if(pair? point_z)
     (cons (clone (car point_z))
     (clone(cdr point_z)))
      point_z))

;-------------
;testing
(clone 1 18 20)



;3 Define a point operation distance that takes as argument two point objects and returns the distance between the two points.
;---------------------------------------------------------------------------------------------------------------------------------------------------------------
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



;4  Define a point operation translate that takes as argument a point object, dx, dy and moves the x and y point coordinates by dx and dy, respectively.
;---------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (translate point_1 dx dy)
  (point_1 'set-x! (- (point_1 'point-x) dx))
  (point_1 'set-y! (- (point_1 'point-y) dy)))



;5 Define a point operation point=? that takes as argument two point objects and returns true if the two point objects are equal, false otherwise.
;---------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (point=? x y)
   (if (= x y)
       'true
       'false))
;testing
(point=? 14 6)


;6 Complete the above implementation of make-point-2D such that a point object created using make-point-2D responds to all the operations
;------------------------------------------------------------------------------------------------------------------------------------------------------------- 
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

;point setter operations
(display "setting x and y values\n")
(point1 'set-x! 4)
(point1 'set-y! 6)
(display "printing set points \n")
(point1 'pretty-print)

;clone operations
(display "clone results \n")
(define point2 (point1 'clone))

(display "X and Y setting \n")
(point1 'set-x! 7)
(point1 'set-y! 9)

;printing the point
(display "second printing \n")
(point1 'pretty-print)
(point2 'pretty-print)

;Distance between the points
(display "Distance between two points \n")
(point1 'distance point1 point2)

;translation for point
(display "Translation results for point1  5 8\n")
(point1 'translate point1 5 8)
(point1 'pretty-print)

;Comparing points equivalence
(display "Compare points to check Equivalence \n")
(point1 'point=? point1 point2)
(point1 'pretty-print)