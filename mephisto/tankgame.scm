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
;;   (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)

  (gl-light GL_LIGHT0 GL_POSITION '#f32(-100.0 0.0 0.0 0.0))
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

(define (draw-enemy)
  (for-each (lambda (e)
	      (unless (null? e)
		(gl-push-matrix)
		(let ((x (cadr e))
		      (y (caddr e))
		      (z (cadddr e)))
		  (gl-translate x (+ y 0.5) z))
		(gl-scale 0.5 1 0.5)
		(gl-material GL_FRONT GL_DIFFUSE (f32vector 1 1 1 1))
		(gl-color 1 1 1 1)
		(glut-solid-cube 4)
		(gl-pop-matrix)))
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

(define (draw-cannonball)
  (when *cannonball-param*
    (if (< *cannonball-param* 1)
	(let ((pos (point4f-add *cannonball-initpos*
				(vector4f-scale *cannonball-rotation*
						(* 20 *cannonball-param*)))))
	  (point4f-set! pos 1
			(+ (ref pos 1)
			   (* 2 (+ 1
				   (* -4 (square (- *cannonball-param* 0.5)))))))
	  (if (check-collision pos)
	      #f
	      (begin
		(gl-push-matrix)
		(gl-translate (ref pos 0) (ref pos 1) (ref pos 2))
		(glut-solid-cube 0.2)
		(gl-pop-matrix)
		(set! *cannonball-param* (+ *cannonball-param* 0.06))
		)
	      ))
	(set! *cannonball-param* #f)
	))
  #t)

(define *cannonball-param* #f)
(define *cannonball-initpos* #f)
(define *cannonball-rotation* #f)

(define (fire!)
  (set! *cannonball-param* 0)
  (set! *cannonball-initpos* (point4f-add *position* *rotation*))
  (set! *cannonball-rotation* *rotation*))

(define (keyboard key x y)
  (case key
    ((27) (exit 0))
    ((32) (fire!))
    ((97)				; a
     (set! *rotation* (* *rot-matrix* *rotation*)))
    ((100)				; d
     (set! *rotation* (* *neg-rot-matrix* *rotation*)))
    ((115)				; s
     (set! *position* (point4f-sub *position* (vector4f-scale *rotation* 0.5))))
    ((119)				; w
     (point4f-add! *position* *rotation*))))

(define *mouse-x* 0)
(define *mouse-y* 0)
(define *window-width* 640)
(define *window-height* 480)

(define passive-motion
  (let ((w (/ *window-width* 2))
	(h (/ *window-height* 2))
	(alpha 1))
    (lambda (x y)
      (set! *mouse-x* (/ (- x w) w))
      (set! *mouse-y* (/ (- y h) h))
      )))

(define (mephisto-main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  (glut-init-window-size *window-width* *window-height*)
  (glut-init-window-position 0 0)
  (glut-create-window "TANK!")
  (mephisto-init!)

  (glut-passive-motion-func passive-motion)
  (glut-display-func draw)
  (glut-idle-func draw)
  (glut-keyboard-func keyboard)

  (glut-main-loop)
  0)

(define (main args)
  (mephisto-main args))

(define (set-view)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-perspective 60 4/3 1 100)

  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  (glu-look-at (ref *position* 0) (ref *position* 1) (ref *position* 2)
	       (+ (ref *position* 0) (ref *rotation* 0))
	       (+ (ref *position* 1) (ref *rotation* 1))
	       (+ (ref *position* 2) (ref *rotation* 2))
	       0.0 1.0 0.0)
  #t)

(define *time-counter* (make <real-time-counter>))

(define *elements* (list set-view draw-cannonball draw-enemy))

(define draw
    (lambda ()
      (clear-screen)

      (let loop ((elems *elements*))
	(unless (null? elems)
	  (let1 e (car elems)
	    (when e
	      (unless (e) (set-car! elems #f)))
	    (loop (cdr elems)))))

      (glut-swap-buffers)

      (time-counter-stop! *time-counter*)
      ;; wait for 0.04 seconds
      (let ((time (- 0.04 (time-counter-value *time-counter*))))
	(when (> time 0)
	      (sys-nanosleep (* time 1000 1000 1000)))
	(time-counter-reset! *time-counter*)
	(time-counter-start! *time-counter*))
      ))