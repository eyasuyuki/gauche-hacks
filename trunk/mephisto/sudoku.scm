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
   cells)
  (positive-constraint! cells))


(define (positive-constraint! cells)
  (let loop-t ((t 8))
    (if (< t 0)
	'done
	(begin
	  (let loop-n ((n 8))
	    (if (< n 0)
		'done
		(let1 const (make-constraint
			     (val p1 p2 p3 p4 p5 p6 p7 p8)
			     ((p1 p2 p3 p4 p5 p6 p7 p8 => val) (+ t 1)))
		  (apply (const 'connect)
			 (let loop ((cells cells) (head #f) (part '()) (i n))
			   (if (null? cells)
			       (cons head part)
			       (if (zero? i)
				   (loop (cdr cells) (car (car cells))
					 part (- i 1))
				   (loop (cdr cells) head
					 (cons (ref (car cells) (+ t 1)) part) (- i 1)))))
			 )
		  (loop-n (- n 1)))))
	  (loop-t (- t 1))))))

(define (send-state x output-port)
  (send-message-raw `(*TOP* (sudoku-state ,x)) output-port))

(define (send-message-raw x output-port)
  (display #?=(srl:sxml->xml-noindent x) output-port)
  (display "\0" output-port))

(define (make-elim-reporter i cell output-port)
  (let loop ((nums (cdr cell))
	     (n 1))
    (if (null? nums)
	'done
	(let ((repo (make-wire))
	      (num (car nums)))
	  (attach-constraint!
	   (num => repo)
	   (begin
	     (send-state `(eliminated (@ (index ,|i|)) ,n)
			 output-port)
	     #f))
	  (loop (cdr nums) (+ n 1))
	  ))))

(define (make-ident-reporter i cell-value output-port)
  (let ((repo (make-wire))
	(prev-value #f))
    (attach-constraint!
     (cell-value => repo)
     (begin
       (if (not prev-value)
	   (send-state `(identified (@ (index ,|i|)) ,cell-value)
		       output-port)
	   (when (not (eq? prev-value cell-value))
	     (send-state `(conflict (@ (index ,|i|))
				    (from ,prev-value)
				    (to ,cell-value))
			 output-port)))
       (set! prev-value cell-value)
       #f))))

(define (make-board output-port)
  (let ((board (make-vector 81)))
    (vector-map! (lambda (i x)
		   (let ((cell (make-cell)))
		     (make-elim-reporter i cell output-port)
		     (make-ident-reporter i (car cell) output-port)
		     cell))
		 board)

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

#| ;; example input
<assign index="1">9</assign>
<assign index="5">1</assign>
<assign index="11">1</assign>
<assign index="14">3</assign>
<assign index="15">9</assign>
<assign index="16">2</assign>
<assign index="17">4</assign>
<assign index="20">3</assign>
<assign index="22">9</assign>
<assign index="24">6</assign>
<assign index="25">5</assign>
<assign index="27">6</assign>
<assign index="29">9</assign>
<assign index="31">4</assign>
<assign index="49">1</assign>
<assign index="51">5</assign>
<assign index="53">8</assign>
<assign index="55">2</assign>
<assign index="56">8</assign>
<assign index="58">6</assign>
<assign index="60">7</assign>
<assign index="63">5</assign>
<assign index="64">3</assign>
<assign index="65">7</assign>
<assign index="66">9</assign>
<assign index="69">1</assign>
<assign index="75">8</assign>
<assign index="79">9</assign>
|#

;; (vector-for-each
;;  (lambda (i x)
;;    (when x
;;      (print (srl:sxml->xml `(*TOP* (assign (@ (index ,i)) ,x)))))
;; ;;      (assign-cell-value! board i x))
;;    ) problem)

(define (process-input board xml cont output-port)
  (match xml
	 (`(*TOP* (assign (@ (index ,i)) ,val))
	  (assign-cell-value! board (x->number i) (x->number val)))
	 ('(*TOP* (result))
	  (send-result board output-port))
	 ('(*TOP* (done))
	  (cont 'done))
	 ('(*TOP* (policy-file-request))
	  ;; <cross-domain-policy>
	  ;; <allow-access-from domain="*" to-ports="*"/>
	  ;; </cross-domain-policy>
	  (begin
	    (send-message-raw '(*TOP* (cross-domain-policy
				       (allow-access-from (@ (domain "*")
							     (to-ports "*")))))
			      output-port)
	    (cont 'continue)))
	 ))

;; stealed from the tower of hanoi
(define (hanoi-listen-accept output-port)
  (let* ((listen-sock (make-server-socket 'inet 0)) ; Kernel assign the port
	 (port (sockaddr-port (socket-address listen-sock))))
    (print #`"Sudoku server started.  port# = ,port")
    (print #`"http://localhost/~toru/mephisto/sudoku_board.swf?port=,port")
    (let loop ((final #f))
      (let ((sock (socket-accept listen-sock)))
	(when final (socket-close listen-sock))
	(values sock (if final #f loop))))))

(define (sudoku-main board iport oport)
  (call/cc
   (lambda (end)
     (let loop ()
       (let ((input (ssax:xml->sxml iport '())))
	 (process-input board input end oport)
	 (let skip-whitespece ()
	   (let1 c (peek-char iport)
	     (if (eof-object? c)
		 (end 'done)
		 (when (or (char-whitespace? c)
			   (char=? c #\null))
		   (read-char iport)
		   (skip-whitespece)))))
	 (loop))))))

(define (print-result board)
  (vector-for-each
   (lambda (i x)
     (display (wire-get-value (car x)))
     (if (= (modulo i 9) 8)
	 (newline)
	 (display " "))
     )
   board))

(define (send-result board output-port)
  (unless (port-closed? output-port)
    (with-output-to-port output-port
      (lambda ()
	(print "<result>")
	(print-result board)
	(print "</result>")))))

(let-values (((sock accept)
	      (hanoi-listen-accept (current-output-port))))
  (let loop ((sock sock)
	     (accept accept))
    (let* ((input-port (socket-input-port sock))
	      (output-port (socket-output-port sock))
	      (board (make-board output-port))
	      )
      (set! (port-buffering output-port) :none)
      (port-buffering output-port)
      (if (eq? (sudoku-main board input-port output-port) 'continue)
	  (let-values (((sock accept) (accept #t)))
	    (loop sock accept))
	  (print-result board)))
      ))

;; (assign-cell-value! board 4 8) ;; make conflict
