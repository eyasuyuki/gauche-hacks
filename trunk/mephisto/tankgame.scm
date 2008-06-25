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

(define (draw-cross-hair)
  (let ((x *mouse-x*)
	(y *mouse-y*))
    ))

(define (draw-gun)
  (gl-push-matrix)
  (gl-scale 0.2 0.2 1)
  (gl-rotate 15 0 1 0)
  (gl-translate 0 0 -15)
  (glut-solid-cube 1)
  (gl-pop-matrix)
  )

(define (gen-enemies)
  (map (lambda (n)
	 (let ((x (* (random-real) 100))
	       (y 0)
	       (z (* (random-real) 100)))
	   (list n x y z)))
       (iota 15)))

(define *enemies* (gen-enemies))

(define (draw-enemy)
  (for-each (lambda (e)
	      (gl-push-matrix)
	      (let ((x (cadr e))
		    (y (caddr e))
		    (z (cadddr e)))
		(gl-translate x (+ y 0.5) z))
	      (gl-scale 0.5 1 0.5)
	      (gl-material GL_FRONT GL_DIFFUSE (f32vector 1 1 1 1))
	      (gl-color 1 1 1 1)
	      (glut-solid-cube 4)
	      (gl-pop-matrix))
	    *enemies*))

(define (display-content)
  (draw-cross-hair)
  (draw-gun)
  (draw-enemy)
  )

(define *position* (point4f 3 0 3))
(define *rotation* (vector4f-normalize (vector4f 1 0 1)))
(define-constant *up-vec* (vector4f 0 1 0))
(define-constant *rot-angle* (make-quatf (vector4f 0 1 0) (/ pi 30)))
(define-constant *neg-rot-angle* (make-quatf (vector4f 0 -1 0) (/ pi 30)))
(define-constant *rot-matrix* (quatf->matrix4f *rot-angle*))
(define-constant *neg-rot-matrix* (quatf->matrix4f *neg-rot-angle*))

(define (keyboard key x y)
  (case key
    ((27) (exit 0))
    ((97)				; a
     (set! *rotation* (* *rot-matrix* *rotation*)))
    ((100)				; d
     (set! *rotation* (* *neg-rot-matrix* *rotation*)))
    ((115)				; s
     (set! *position* (point4f-sub *position* *rotation*)))
    ((119)				; w
     (point4f-add! *position* *rotation*))))

(define (make-display)
  (let1 strm make-anim-stream
    (lambda ()
      ((stream-car strm))
      (set! strm (stream-cdr strm))
      )
    ))

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
  (glut-create-window "MEPHISTO")
  (mephisto-init!)

  (glut-passive-motion-func passive-motion)
  (glut-display-func (make-display))
  (glut-idle-func (make-display))
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
  )

(define draw
    (lambda ()
      (clear-screen)
      (set-view)
      (display-content)
      (glut-swap-buffers)
      ))

(define make-anim-stream
  (stream-cons draw make-anim-stream))
