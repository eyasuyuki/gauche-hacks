;; Sudoku Solver with MEPHISTO constarint system

(use srfi-1)
(use srfi-43)
(use sxml.ssax)
(use sxml.serializer)
(use util.match)
(use gauche.net)

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
	(unless (eq? x y) (connect! x (car y))))
      cells))
   cells))

(define (send-state x)
  (print (srl:sxml->xml-noindent `(*TOP* ,x))))

(define (make-board)
  (let ((board (make-vector 81)))
    (vector-map! (lambda (i x)
		   (let ((cell (make-cell)))
		     (let loop ((nums (cdr cell))
				(n 1))
		       (if (null? nums)
			   'done
			   (let ((repo (make-wire))
				 (num (car nums)))
			     (attach-constraint!
			      (num => repo)
			      (begin
				(send-state `(eliminated (@ (index ,|i|)) ,n))
				#f))
			     (loop (cdr nums) (+ n 1))
			     )))
		     (let ((cell-value (car cell))
			   (prev-value #f)
			   (repo (make-wire)))
		       (attach-constraint!
			(cell-value => repo)
			(begin
			  (if (not prev-value)
			      (send-state `(identified (@ (index ,|i|)) ,cell-value))
			      (when (not (eq? prev-value cell-value))
				(send-state `(conflict (@ (index ,|i|))
						       (from ,prev-value)
						       (to ,cell-value)))))
			  (set! prev-value cell-value)
			  #f)))
		     cell)) board)

    ;; set constraint for each rows/columns
    (let loop ((n 0))
      (unless (= n 9)
	(let ((b (* n 9)))
	  (connect9! (map (lambda (i) (vector-ref board (+ b i))) (iota 9))))
	(let ((b n))
	  (connect9! (map (lambda (i) (vector-ref board (+ b (* i 9)))) (iota 9))))
	(loop (+ n 1))))

    ;; set constraint for each 3x3 boxes
    (let loop ((n 0))
      (unless (= n 3)
	(let loop2 ((m 0))
	  (unless (= m 3)
	    (let1 b (+ (* n 27) (* m 3))
	      (connect9!
	       (map (cut vector-ref board <>)
		    (let loop3 ((i 0) (p '()))
		      (if (= i 3)
			  p
			  (loop3 (+ i 1)
				 (append
				  (let loop4 ((j 0) (q '()))
				    (if (= j 3)
					q
					(loop4 (+ j 1) (cons (+ b (* i 9) j) q))))
				  p)))))))
	    (loop2 (+ m 1))))
	(loop (+ n 1))))

    board))

(define (assign-cell-value! board n v)
  (wire-set-value! (car (vector-ref board n)) v))

(define (get-cell-value board n)
  (wire-get-value (car (vector-ref board n))))

(define board (make-board))

(define problem
  ;; evil level from http://www.websudoku.com/?level=4
  ;; not solved yet...
;;   '#(#f  5 #f  #f  6  7  #f  2 #f
;;      #f #f #f  #f #f #f  #f #f  9
;;      #f  1  3  #f #f  4  #f #f  6

;;      #f #f #f  #f  5  1   2 #f #f
;;      #f #f #f  #f #f #f  #f #f #f
;;      #f #f  2   4  9 #f  #f #f #f

;;       8 #f #f   5 #f #f   3  7 #f
;;       9 #f #f  #f #f #f  #f #f #f
;;      #f  4 #f   7  8 #f  #f  9 #f
;;      )

  ;; easy level
;;   '#(#f #f #f  1  4 #f  6  9 #f
;;      #f #f  2 #f #f #f #f #f  4
;;      #f  1 #f #f #f  8  5  3  2

;;       8  6 #f #f #f  1 #f #f #f
;;       2  4  1 #f  9 #f  7  8  5
;;      #f #f #f  8 #f #f #f  6  9

;;       6  5  3  2 #f #f #f #f #f
;;       1 #f #f #f #f #f  2 #f #f
;;      #f  2  8 #f  6  5 #f #f #f
;;      )

  ;; medium level
  '#(#f  9 #f  #f #f  1  #f #f #f
     #f #f  1  #f #f  3   9  2  4
     #f #f  3  #f  9 #f   6  5 #f

      6 #f  9  #f  4 #f  #f #f #f
     #f #f #f  #f #f #f  #f #f #f
     #f #f #f  #f  1 #f   5 #f  8

     #f  2  8  #f  6 #f   7 #f #f
      5  3  7   9 #f #f   1 #f #f
     #f #f #f   8 #f #f  #f  9 #f
     )
  )

;; (vector-for-each
;;  (lambda (i x)
;;    (when x
;;      (assign-cell-value! board i x))
;;    ) problem)

(define (process-input xml)
  (match xml
	 (`(*TOP* (assign (@ (index ,i)) ,val))
	  (assign-cell-value! board (x->number i) (x->number val)))
	 )
  )

(call/cc
 (lambda (end)
   (let loop ((port (current-input-port)))
     (let ((input (ssax:xml->sxml port '())))
       (process-input input)
       (let skip-whitespece ()
	 (let1 c (peek-char port)
	   (if (eof-object? c)
	       (end 'done)
	       (when (char-whitespace? c)
		 (read-char port)
		 (skip-whitespece)))))
       (loop port)))))

;; (assign-cell-value! board 4 8) ;; make conflict

(vector-for-each
 (lambda (i x)
   (display (wire-get-value (car x)))
   (if (= (modulo i 9) 8)
       (newline)
       (display " "))
   )
 board)
