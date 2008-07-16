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
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  )

(define (gen-enemies)
  (map (lambda (n)
	 (let ((x (* (random-real) 100))
	       (y 0)
	       (z (* (random-real) 100)))
	   (list n x y z)))
       (iota 10)))

(define *enemies* (gen-enemies))

(define (update-enemy render-element-list)
  (for-each (lambda (e)
	      (append! render-element-list
		       (list (lambda ()
			       (unless (null? e)
				 (gl-push-matrix)
				 (let ((x (cadr e))
				       (y (caddr e))
				       (z (cadddr e)))
				   (gl-translate x y z))
				 (gl-scale 0.3 0.2 0.5)
;; 				 (gl-rotate 90 0 1 0)
				 (gl-translate 0 0.5 0)
				 (gl-material GL_FRONT GL_DIFFUSE (f32vector 1 1 1 1))
				 (gl-color 1 1 1 1)
;; 				 (glut-solid-teapot 4)
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
  (let ((ex (cadr enemy))
	(ez (cadddr enemy))
	(cx (ref cannonball-pos 0))
	(cz (ref cannonball-pos 2)))
    (< (+ (square (- ex cx)) (square (- ez cz))) 10)
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
  (let ((*cannonball-param* (hash-table-get self 'param))
	(*cannonball-initpos* (hash-table-get self 'initpos))
	(*cannonball-rotation* (hash-table-get self 'rotation)))
    (if (< *cannonball-param* 1)
	(let ((pos (point4f-add *cannonball-initpos*
				(vector4f-scale *cannonball-rotation*
						(* 50 *cannonball-param*)))))
	  (point4f-set! pos 1
			(+ (ref pos 1)
			   (* 2 (+ 1
				   (* -4 (square (- *cannonball-param* 0.5)))))))
	  (if (check-collision pos)
	      #f
	      (begin
		(append!
		 render-element-list
		 (list (lambda ()
			 (gl-push-matrix)
			 (gl-translate (ref pos 0) (ref pos 1) (ref pos 2))
			 (glut-solid-cube 0.1)
			 (gl-pop-matrix)
			 ))
		 )
		(hash-table-put! self 'param (+ *cannonball-param* 0.06))
		#t)
	      ))
	#f
	)))

; (define *cannonball-param* #f)
; (define *cannonball-initpos* #f)
; (define *cannonball-rotation* #f)

(define (cannonball-create pos rot)
  (let ((h (make-hash-table)))
    (hash-table-put! h 'param 0)
    (hash-table-put! h 'initpos pos)
    (hash-table-put! h 'rotation rot)
    (hash-table-put! h 'update (lambda (elems)
				 (update-cannonball h elems)
				 ))

    h))

(define (get-mouse-angle)
  (make-quatf #f32(0 1 0) (* (- *mouse-x*) (/ pi 12))))

(define (fire!)
;   (set! *cannonball-param* 0)
;   (set! *cannonball-initpos* (point4f-add *position* *rotation*))
;   (set! *cannonball-rotation* *rotation*)

  (let1 quat (get-mouse-angle)
    (append! *elements*
	     (list (cannonball-create
		    (point4f-add *position* (vector4f-scale *rotation* (random-real)))
		    (quatf-transform quat *rotation*)))))
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
     (point4f-add! *position* *rotation*))))

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
  (if (> *velocity* 0.5)
      (set! *velocity* 1)
      (if (< *velocity* 0)
	  (set! *velocity* 0)))
  (set! *rotation* (quatf-transform (get-mouse-angle) *rotation*))

  (point4f-add! *position* (vector4f-scale *rotation* *velocity*))
  (append! render-element-list
	   (list (lambda ()
		   (gl-matrix-mode GL_PROJECTION)
		   (gl-load-identity)
		   (glu-perspective 60 4/3 1 100)

		   (gl-matrix-mode GL_MODELVIEW)
		   (gl-load-identity)
		   (glu-look-at (ref *position* 0)
				(+ (ref *position* 1) 1)
				(ref *position* 2)
				(+ (ref *position* 0) (ref *rotation* 0))
				(+ (ref *position* 1) (+ (ref *rotation* 1) 1))
				(+ (ref *position* 2) (ref *rotation* 2))
				0.0 1.0 0.0))))
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
				   (gl-material GL_FRONT GL_DIFFUSE
						(if (odd? (/ (+ x z) 10))
						    (f32vector 0 1 0 1)
						    (f32vector 0 0.5 0 1)))
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
