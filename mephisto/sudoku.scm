;; Sudoku Solver with MEPHISTO constarint system

(use srfi-1)
(use srfi-43)

(add-load-path "/Users/toru/src/mephisto/mephisto/lib")

(use mephisto.constraint)

(define (make-cell)
  (let ((val (make-wire))			;the value of this cell
	(p1 (make-wire))			;number which cannot be the value
	(p2 (make-wire))
	(p3 (make-wire))
	(p4 (make-wire))
	(p5 (make-wire))
	(p6 (make-wire))
	(p7 (make-wire))
	(p8 (make-wire))
	(p9 (make-wire)))
    (attach-constraint! (p2 p3 p4 p5 p6 p7 p8 p9 => val) 1)
    (attach-constraint! (p1 p3 p4 p5 p6 p7 p8 p9 => val) 2)
    (attach-constraint! (p1 p2 p4 p5 p6 p7 p8 p9 => val) 3)
    (attach-constraint! (p1 p2 p3 p5 p6 p7 p8 p9 => val) 4)
    (attach-constraint! (p1 p2 p3 p4 p6 p7 p8 p9 => val) 5)
    (attach-constraint! (p1 p2 p3 p4 p5 p7 p8 p9 => val) 6)
    (attach-constraint! (p1 p2 p3 p4 p5 p6 p8 p9 => val) 7)
    (attach-constraint! (p1 p2 p3 p4 p5 p6 p7 p9 => val) 8)
    (attach-constraint! (p1 p2 p3 p4 p5 p6 p7 p8 => val) 9)

    (list val p1 p2 p3 p4 p5 p6 p7 p8 p9)))

(define (connect! cell val)
  (for-each (lambda (x)
	      (let1 p (ref cell x)
		(attach-constraint! (val => p) (eq? val x))))
	    (iota 9 1)))

(define (connect9! cells)
  (for-each
   (lambda (x)
     (for-each
      (lambda (y)
	(unless (eq? x y) (connect! x (car y)))
	) cells)
     )
   cells))

(define (make-board)
  (let ((board (make-vector 81)))
    (vector-map! (lambda (i x)
		   (let ((cell (make-cell))
			 (repo (make-wire)))
;; 		     (let1 cell-value (car cell)
;; 		       (attach-constraint! (cell-value => repo)
;; 					   (begin (print #`",i = ,cell-value")
;; 						  #f)))
		     cell)) board)

    (let loop ((n 0))
      (unless (= n 9)
	(let ((b (* n 9)))
	  (connect9! (map (lambda (i) (vector-ref board (+ b i))) (iota 9))))
	(loop (+ n 1))))

    (let loop ((n 0))
      (unless (= n 9)
	(let ((b n))
	  (connect9! (map (lambda (i) (vector-ref board (+ b (* i 9)))) (iota 9))))
	(loop (+ n 1))))

    (let loop ((n 0))
      (unless (= n 3)
	(let loop2 ((m 0))
	  (unless (= m 3)

	    (let1 b (+ (* n 27) (* m 3))

	      (connect9! (map (cut vector-ref board <>)
	       (let loop3 ((i 0) (p '()))
		(if (= i 3)
		    p
		    (loop3 (+ i 1)
			  (append
			   (let loop4 ((j 0) (q '()))
			     (if (= j 3)
				 q
				 (loop4 (+ j 1) (cons (+ b (* i 9) j) q))))
			   p))
		    ))
	       ))

	      )

	    (loop2 (+ m 1)))
	  )
	(loop (+ n 1))))

    board))

(define (assign-cell-value! board n v)
  (wire-set-value! (car (vector-ref board n)) v))

(define (get-cell-value board n)
  (wire-get-value (car (vector-ref board n))))

(define board (make-board))

(define b
  '#(#f #f #f  1  4 #f  6  9 #f
     #f #f  2 #f #f #f #f #f  4
     #f  1 #f #f #f  8  5  3  2

      8  6 #f #f #f  1 #f #f #f
      2  4  1 #f  9 #f  7  8  5
     #f #f #f  8 #f #f #f  6  9

      6  5  3  2 #f #f #f #f #f
      1 #f #f #f #f #f  2 #f #f
     #f  2  8 #f  6  5 #f #f #f
     ))

(vector-for-each
 (lambda (i x)
   (when x
     (assign-cell-value! board i x))
   ) b)

(vector-for-each
 (lambda (i x)
   (display (wire-get-value (car x)))
   (if (= (modulo i 9) 8)
       (newline)
       (display " "))
   )
 board)
