(use gauche.hook)
(use srfi-1)
(use util.match)

(define (conn g0 g1)
  (lambda (i0)
    (receive (r0 i1) (g0 i0)
      (g1 (append r0 i1)))))

(define (->- . args)
  (reduce-right conn (values) args))

(define (pile g0 g1)
  (lambda (i0)
    (receive (r0 i1) (g0 i0)
      (receive (r1 i2) (g1 i1)
        (values (append r0 r1) i2)))))

(define (-^- . args)
  (reduce-right pile (values) args))

(define (NOPn n)
  (lambda (inputs)
    (split-at inputs n)))

(define NOP (NOPn 1))

(define skip NOP)

(define AND
  (lambda (inputs)
    (receive (args rest) (split-at inputs 2)
      (match args ((x y) (values (list (and x y)) rest))))))

(define OR
  (lambda (inputs)
    (receive (args rest) (split-at inputs 2)
      (match args ((x y) (values (list (or x y)) rest))))))

(define NOT
  (lambda (inputs)
    (receive (args rest) (split-at inputs 1)
      (values (map not args) rest))))

(define (split n)
  (lambda (inputs)
    (receive (args rest) (split-at inputs n)
      (values (append args args) rest))))

(define (cross n)
  (lambda (inputs)
    (receive (args rest) (split-at inputs n)
      (values (reverse args) rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (input i gate) (receive (r0 _) (gate i) r0))

(define t23 ;; 3入力のうち真が2つ以上ある
  (->- (-^- NOP (->- (split 2) (-^- AND OR)))
       (->- OR AND)))

#|
  x -----------------,   ,-----,
             ,-----, '-->+     |
  y --,----->+     |     | OR  +--,
      |      | AND +---->+     |  |   ,-----,
  z --}--,-->+     |     '-----'  '-->+     |
      |  |   '-----'                  | AND +---> t23
      |  |   ,-----,              ,-->+     |     
      '--}-->+     |              |   '-----'
         |   | OR  +--------------'
         '-->+     |
             '-----'
|#

(define test-cases '((#f #f #f)   ; #tの数 0   t23 #f
                     (#f #f #t)   ;        1       #f
                     (#f #t #f)   ;        1       #f
                     (#f #t #t)   ;        2       #t
                     (#t #f #f)   ;        1       #f
                     (#t #f #t)   ;        2       #t
                     (#t #t #f)   ;        2       #t
                     (#t #t #t))) ;        3       #t


#|
gosh> (map (cut input <> t23) test-cases)
((#f) (#f) (#f) (#t) (#f) (#t) (#t) (#t))
|#

(define half-adder
  (->- (split 2)
       (->- (-^- OR AND)
            (->- (-^- NOP (split 1))
                 (->- (-^- NOP NOT NOP)
                      (-^- AND NOP))))))

(define test-cases2 '((#f #f)(#f #t)(#t #f)(#t #t)))

(define full-adder
  (->- (->- half-adder (cross 2))
       (->- (-^- NOP half-adder)
            (->- (cross 2) (-^- NOP AND)))))
