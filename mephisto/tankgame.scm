;; Mini MEPHISTO
;; for Gauche Hacks book
;; written by Toru Hisai
;; $Id$


(use srfi-1)
(use srfi-27)

(use gl)
(use gl.glut)
(use gl.math3d)
(use math.const)
(use util.stream)
(use gauche.uvector)

(use gauche.time)

(define (mephisto-init!)
  (gl-shade-model GL_FLAT)

  (gl-light GL_LIGHT0 GL_POSITION '#f32(-100.0 100.0 0.0 0.0))
  (gl-light GL_LIGHT0 GL_DIFFUSE '#f32(1.0 1.0 1.0 1.0))
  (gl-light GL_LIGHT0 GL_AMBIENT '#f32(0.3 0.3 0.3 1.0))

  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)

  (gl-enable GL_DEPTH_TEST)
  (gl-enable GL_NORMALIZE)
  )

(define (clear-screen)
  (gl-clear-color 0.3 0.3 1 1)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  )

(define (gen-enemies)
  (map (lambda (n)
	 (let ((x (* (random-real) 100))
	       (y 0)
	       (z (* (random-real) 100)))
	   (list n
		 (point4f x y z)	; position
		 (vector4f-normalize (vector4f (- (random-real) 0.5)
					       0
					       (- (random-real) 0.5))) ; velocity
		 )))
       (iota 50)))

(define enemy-position cadr)
(define enemy-velocity caddr)

;; http://www.red3d.com/cwr/boids/
;; http://itpro.nikkeibp.co.jp/article/COLUMN/20061024/251679/?ST=develop&P=2
(define (move-enemies)
  (for-each
   (lambda (e)
     (unless (null? e)
       (let1 vel (vector4f 0 0 0)
	 (vector4f-add! vel
			(vector4f-scale (calculate-separation e *enemies*) 0.2))
	 (vector4f-add! vel
			(vector4f-scale (calculate-alignment e *enemies*) 0.2))
	 (vector4f-add! vel
			(vector4f-scale (calculate-cohesion e *enemies*) 0.1))
	 (when *need-avoid*
	   (vector4f-add! vel
			  (vector4f-scale (calculate-avoidance e *position*) 0.02)))
	 ;; 	  Alignment Cohesion Avoidance
	 (vector4f-add! (enemy-velocity e) vel)
	 (vector4f-normalize! (enemy-velocity e))
	 )))
   *enemies*))

(define (near? e1 e2)
  (if (or (null? e1) (null? e2))
      #f
      (let1 d (point4f-sub (enemy-position e1)
			   (enemy-position e2))
	(and (> 70 (vector4f-dot d d))
	     (> (vector4f-dot d (enemy-velocity e1)) -0.7)
	     ))))

(define (calculate-separation self enemies)
  (let ((vel (vector4f 0 0 0)))
    (for-each
     (lambda (e)
       (unless (eq? self e)
	 (when (near? e self)
	   (vector4f-add! vel (vector4f-normalize
			       (let1 d (point4f-sub (enemy-position self)
						    (enemy-position e))
				 (vector4f-scale d (/. 1 (vector4f-dot d d)))))))))
     enemies)
    vel))

(define (calculate-avoidance self pos)
  (let1 d (point4f-sub (enemy-position self) pos)
    (if (< (vector4f-dot d d) 200) d `#,(vector4f 0 0 0)))
)

(define (calculate-alignment self enemies)
  (let ((vel (vector4f 0 0 0))
	(n 0))
    (for-each
     (lambda (e)
       (unless (eq? self e)
	 (when (near? e self)
	   (vector4f-add! vel (enemy-velocity e))
	   (inc! n))))
     enemies)
    (if (> n 0)
	(vector4f-sub (vector4f-scale vel (/. 1 n)) (enemy-velocity self))
	(vector4f 0 0 0))))

(define (calculate-cohesion self enemies)
  (let ((vel (f32vector 0 0 0 0))
	(n 0))
    (for-each
     (lambda (e)
       (unless (eq? self e)
	 (when (near? e self)
	   (f32vector-add! vel (point4f->f32vector (enemy-position e)))
	   (inc! n))))
     enemies)
    (if (> n 0)
	(point4f-sub (f32vector->point4f (f32vector-mul! vel (/. 1 n)))
		     (enemy-position self))
	(vector4f 0 0 0))))

(define *enemies* (gen-enemies))

(define (update-enemy render-element-list)
  (move-enemies)
  (for-each (lambda (e)
	      (append! render-element-list
		       (list (lambda ()
			       (unless (null? e)
				 (gl-push-matrix)
				 (let ((p (cadr e))
				       (v (caddr e)))
				   (point4f-add! p v)
				   (if (> (point4f-ref p 0) 100)
				       (point4f-set! p 0 0)
				       (when (< (point4f-ref p 0) 0)
					 (point4f-set! p 0 100)))
				   (if (> (point4f-ref p 2) 100)
				       (point4f-set! p 2 0)
				       (when (< (point4f-ref p 2) 0)
					 (point4f-set! p 2 100)))
				   (let ((x (point4f-ref p 0))
					 (y (point4f-ref p 1))
					 (z (point4f-ref p 2)))
				     (gl-translate x y z)))
				 (gl-scale 0.3 0.2 0.5)
				 (gl-translate 0 0.5 0)
				 (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE
					      (f32vector 0 0 1 1))
				 (glut-solid-cube 4)
				 (gl-pop-matrix))))))
	    *enemies*)
  #t)

(define *position* (point4f 3 0 3))
(define *rotation* (vector4f-normalize (vector4f 1 0 1)))
(define-constant *up-vec* (vector4f 0 1 0))
(define-constant *rot-angle* (make-quatf (vector4f 0 1 0) (/ pi 50)))
(define-constant *neg-rot-angle* (make-quatf (vector4f 0 -1 0) (/ pi 50)))
(define-constant *rot-matrix* (quatf->matrix4f *rot-angle*))
(define-constant *neg-rot-matrix* (quatf->matrix4f *neg-rot-angle*))

(define (vector4f-scale v s)
  (f32vector->vector4f (f32vector-mul (vector4f->f32vector v) s)))

(define (square x) (* x x))

(define (collide? enemy cannonball-pos)
  (let1 ep (cadr enemy)
    (let ((ex (point4f-ref ep 0))
	  (ez (point4f-ref ep 2))
	  (cx (point4f-ref cannonball-pos 0))
	  (cz (point4f-ref cannonball-pos 2)))
      (< (+ (square (- ex cx)) (square (- ez cz))) 10))
    ))

(define (check-collision cannonball-pos)
  (let loop ((enems *enemies*))
    (if (null? enems)
	#f				; no collision
	(let1 e (car enems)
	  (if (and (not (null? e)) (collide? e cannonball-pos))
	      (begin
		(set-car! enems '())
		#t)
	      (loop (cdr enems)))))))

(define (update-cannonball self render-element-list)
  (set! *need-avoid* #f)
  (let ((param (hash-table-get self 'param))
	(initpos (hash-table-get self 'initpos))
	(rotation (hash-table-get self 'rotation))
	(color (hash-table-get self 'color))
	(size (hash-table-get self 'size))
	(height (hash-table-get self 'height)))
    (if (< param 1)
	(let ((pos (point4f-add initpos
				(vector4f-scale rotation
						(* 15 param)))))
	  (set! *need-avoid* #t)
	  (point4f-set! pos 1
			(+ (ref pos 1)
			   (* (* 2 height)
			      (+ 1 (* -4 (square (- param 0.5)))))))
	  (if (check-collision pos)
	      #f
	      (begin
		(append!
		 render-element-list
		 (list (lambda ()
			 (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE color)
			 (gl-push-matrix)
			 (gl-translate (ref pos 0) (ref pos 1) (ref pos 2))
			 (glut-solid-cube (* size 0.3))
			 (gl-pop-matrix)
			 ))
		 )
		(hash-table-put! self 'param (+ param 0.1))
		#t)
	      ))
	#f
	)))

(define (gen-color x)
  (let ((h (* x 3)))
    (cond ((> h 2)
	   (f32vector 0 (- h 2) (- 3 h) 1))
	  ((> h 1)
	   (f32vector (- 2 h) 0 (- h 1) 1))
	  (else				; (> h 0)
	   (f32vector h (- 1 h) 0 1))
	   )))

(define cannonball-create
  (let1 col 0
    (lambda (pos rot height)
      (set! col (+ col 0.05))
      (if (> col 1) (set! col (- col 1)))
      (let ((h (make-hash-table)))
	(hash-table-put! h 'color (gen-color col))
	(hash-table-put! h 'size (+ (random-real) 0.5))
	(hash-table-put! h 'param 0)
	(hash-table-put! h 'height height)
	(hash-table-put! h 'initpos pos)
	(hash-table-put! h 'rotation rot)
	(hash-table-put! h 'update (lambda (elems)
				     (update-cannonball h elems)
				     ))

	h)
      )))

(define (get-mouse-angle)
  (make-quatf #f32(0 1 0) (* (- *mouse-x*) (/ pi 12))))

(define *need-avoid* #f)

(define (fire!)
  (let1 quat (get-mouse-angle)
    (append! *elements*
	     (list (cannonball-create
		    (point4f-add *position* (vector4f-scale *rotation* (random-real)))
		    (quatf-transform quat *rotation*)
		    (- 1 (* *mouse-y* 1))))))
  )

(define *key-active* #f)
(define *mouse-active* #f)

(define *velocity* 0)
(define *left-button* #f)
(define *right-button* #f)

(define (mouse-func btn stat x y)
  (cond ((eq? btn GLUT_LEFT_BUTTON)
	 (set! *left-button* (eq? stat GLUT_DOWN)))
	((eq? btn GLUT_RIGHT_BUTTON)
	 (set! *right-button* (eq? stat GLUT_DOWN)))
	 ))

(define (keyboard key x y)
  (set! *key-active* #f)
  (case key
    ((27) (exit 0))
    ((32) (fire!))
    ((99)				; c
     (compact! *elements*))
;;     ((97)				; a
;;      (set! *rotation* (* *rot-matrix* *rotation*)))
;;     ((100)				; d
;;      (set! *rotation* (* *neg-rot-matrix* *rotation*)))
    ((115)				; s
     (set! *position* (point4f-sub *position* (vector4f-scale *rotation* 0.5))))
    ((119)				; w
     (point4f-add! *position* *rotation*))))

(define (turn-left!)
  (set! *rotation* (* *rot-matrix* *rotation*)))
(define (turn-right!)
  (set! *rotation* (* *neg-rot-matrix* *rotation*)))

(define *bird-view* #t)

(define (special-keyboard key x y)
;;   #?=GLUT_KEY_LEFT
;;   #?=GLUT_KEY_RIGHT
;;   #?=GLUT_KEY_UP
;;   #?=GLUT_KEY_DOWN

  (case key
    ((100 GLUT_KEY_LEFT)
     (turn-left!))
    ((102 GLUT_KEY_RIGHT)
     (turn-right!))
    ((103 GLUT_KEY_DOWN)
     (set! *position* (point4f-sub *position* (vector4f-scale *rotation* 0.5))))
    ((101 GLUT_KEY_UP)
     (set! *bird-view* #?=(not *bird-view*)))))
;;      (point4f-add! *position* *rotation*))))

(define *mouse-x* 0)
(define *mouse-y* 0)
(define *window-width* 640)
(define *window-height* 480)

(define passive-motion
  (let ((w (/ *window-width* 2.0))
	(h (/ *window-height* 2.0))
	(alpha 1))
    (lambda (x y)
      (when *mouse-active*
	(set! *mouse-active* #f)
	(set! *mouse-x* (/ (- x w) w))
	(set! *mouse-y* (/ (- y h) h))
	))))

(define (mephisto-main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  (glut-init-window-size *window-width* *window-height*)
  (glut-init-window-position 0 0)
  (glut-create-window "TANK!")
  (mephisto-init!)

  (glut-motion-func passive-motion)
  (glut-passive-motion-func passive-motion)
  (glut-display-func render)
  (glut-idle-func render)
  (glut-keyboard-func keyboard)
  (glut-special-func special-keyboard)
  (glut-mouse-func mouse-func)
  (glut-main-loop)
  0)

(define (main args)
  (mephisto-main args))

(define (update-self render-element-list)
  (when *left-button* (fire!))
  (set! *velocity* ((if *right-button* - +) *velocity* 0.01))
  (if (> *velocity* 1)
      (set! *velocity* 1)
      (if (< *velocity* 0)
	  (set! *velocity* 0)))
  (set! *rotation* (quatf-transform (get-mouse-angle) *rotation*))

  (point4f-add! *position* (vector4f-scale *rotation* *velocity*))
  (if (> (point4f-ref *position* 0) 100)
      (point4f-set! *position* 0 0)
      (when (< (point4f-ref *position* 0) 0)
	(point4f-set! *position* 0 100)))
  (if (> (point4f-ref *position* 2) 100)
      (point4f-set! *position* 2 0)
      (when (< (point4f-ref *position* 2) 0)
	(point4f-set! *position* 2 100)))

  (append! render-element-list
	   (list (lambda ()
		   (gl-matrix-mode GL_PROJECTION)
		   (gl-load-identity)
		   (if *bird-view*
		       (glu-perspective 60 4/3 50 200)
		       (glu-perspective 60 4/3 0.1 20))

		   (gl-matrix-mode GL_MODELVIEW)
		   (gl-load-identity)
		   (if *bird-view*
		       (glu-look-at 50 100 50 50 0 50 0 0 1)
		       (glu-look-at (ref *position* 0)
				    (+ (ref *position* 1) 0.3)
				    (ref *position* 2)
				    (+ (ref *position* 0) (* (ref *rotation* 0) 30))
				    (+ (ref *position* 1) (* (ref *rotation* 1) 30))
				    (+ (ref *position* 2) (* (ref *rotation* 2) 30))
				    0.0 1.0 0.0))

		   (when *bird-view*
		     (gl-push-matrix)
		     (gl-translate (ref *position* 0)
				   (ref *position* 1)
				   (ref *position* 2))
		     (let1 ang (* (/ (acos (vector4f-dot *rotation* `#,(vector4f 0 0 1)))
				     pi)
				  180)
		       (gl-rotate (if (> (ref *rotation* 0) 0) ang (- ang)) 0 1 0)
		       )
		     (gl-scale 1 1 2)
		     (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE
				  (f32vector 1 0 0 1))
		     (glut-solid-cube 1)
		     (gl-pop-matrix)))))
  #t)

(define (update-ground render-element-list)
  (append! render-element-list
	   (list (lambda ()
		   (let loop-x ((x 0))
		     (if (> x 100)
			 #f
			 (begin
			   (let loop-z ((z 0))
			     (if (> z 100)
				 #f
				 (begin
				   (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE
						(if (odd? (/ (+ x z) 10))
						    (f32vector 0 1 0 0.5)
						    (f32vector 0 0.5 0 0.5)))
				   (gl-normal 0 1 0)
				   (gl-begin GL_POLYGON)
				   (gl-vertex x 0 z)
				   (gl-vertex (+ x 10) 0 z)
				   (gl-vertex (+ x 10) 0 (+ z 10))
				   (gl-vertex x 0 (+ z 10))
				   (gl-end)
				   (loop-z (+ z 10)))))
			   (loop-x (+ x 10)))
			 )))
		 )))

(define *time-counter* (make <real-time-counter>))

(define self (make-hash-table))
(hash-table-put! self 'update update-self)

(define ground (make-hash-table))
(hash-table-put! ground 'update update-ground)

; (define cannonball (make-hash-table))
; (hash-table-put! cannonball 'update update-cannonball)

(define enemy (make-hash-table))
(hash-table-put! enemy 'update update-enemy)

(define *elements* (list 'elements ground self enemy))
(define *render-elements* (list 'a))

(define (compact! elems)
;   (print elems)
  (let loop ((elems elems))
    (unless (null? (cdr elems))
	    (if (cadr elems)
		(loop (cdr elems))
		(begin (set-cdr! elems (cddr elems))
		       (loop elems)
		       ))))
;   (print elems)
  )

(define render
    (lambda ()
      (clear-screen)

      (let loop ((elems (cdr *elements*)))
	(unless (null? elems)
	  (let1 e (car elems)
	    (when e
	      (unless ((hash-table-get e 'update) *render-elements*)
		      (set-car! elems #f)))
	    (loop (cdr elems)))))
      (for-each (cut <>) (cdr *render-elements*))
      (set! *render-elements* (list 'a))

      (glut-swap-buffers)
      (set! *key-active* #t)
      (set! *mouse-active* #t)

      (time-counter-stop! *time-counter*)
      ;; wait for 0.04 seconds
      (let ((time (- 0.04 (time-counter-value *time-counter*))))
	(when (> time 0)
	      (sys-nanosleep (* time 1000 1000 1000)))
	(time-counter-reset! *time-counter*)
	(time-counter-start! *time-counter*))
      ))
