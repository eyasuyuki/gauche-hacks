;; Mini MEPHISTO
;; for Gauche Hacks book
;; written by Toru Hisai
;; $Id$


(use srfi-1)

(use gl)
(use gl.glut)
(use util.stream)
(use gauche.uvector)

(define (mephisto-init!)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)

  (gl-light GL_LIGHT0 GL_POSITION '#f32(0.0 10.0 20.0 0.0))
  (gl-light GL_LIGHT0 GL_DIFFUSE '#f32(1.0 1.0 1.0 1.0))
  (gl-light GL_LIGHT0 GL_AMBIENT '#f32(1.0 1.0 1.0 1.0))
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)

  (gl-enable GL_DEPTH_TEST)
  (gl-enable GL_NORMALIZE)

;   (gl-load-identity)
;   (glu-look-at 0.5 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0)
  )

(define strip
  (map (lambda (n)
	 (list (+ 5(* n 0.03))
	       (* n 0.2)
	       (* n 0.7)))
       (iota 1000)))

(define (display-content)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  (gl-color 0.0 1.0 0.0)

  (gl-material GL_FRONT GL_DIFFUSE (f32vector 0 1 1 1.0))
  (gl-material GL_FRONT GL_AMBIENT
	       (f32vector (/ 0 10) (/ 1 10) (/ 0 10) 1.0))

  (gl-begin GL_TRIANGLE_STRIP)
  (let loop ((n 100)
	     (s strip))
    (if (null? s)
	'done
	(let ((p (car s)))
	  (let ((r (car p))
		(theta (cadr p))
		(phi (caddr p)))
	    (let ((r-cos-phi (* r (cos phi))))
	      (let ((x (* r-cos-phi (cos theta)))
		    (y (* r-cos-phi (sin theta)))
		    (z (* r (sin theta))))
		(gl-normal x y z)
		(gl-vertex x y z)
		(if (< n 0)
		    (set-cdr! s '())
		    (loop (- n 1) (cdr s)))))))))
  (gl-end)

  (glut-swap-buffers)
  )

(define (keyboard key x y)
  (case key
    ((27) (exit 0))))

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
	(h (/ *window-height* 2)))
    (lambda (x y)
      (set! *mouse-x* (* (/ (- x w) w) 2))
      (set! *mouse-y* (* (/ (- y h) h) 2)))))

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

(define draw
  (let ((time 0)
	(ang-x 0)
	(ang-y 0)
	(ang-z 0))
    (lambda ()
      (gl-matrix-mode GL_PROJECTION)
      (gl-load-identity)
      (glu-perspective 60 4/3 1 100)

      (gl-matrix-mode GL_MODELVIEW)
      (gl-load-identity)
      (glu-look-at 0.0 0.0 -20.0 0.0 0.0 0.0 0.0 1.0 0.0)

      (gl-push-matrix)
;       (gl-rotate (* time 0.2) 0 (sin (* time 0.01)) (cos (* time 0.01)))
      (gl-rotate ang-x 1 0 0)
      (gl-rotate ang-y 0 1 0)
      (gl-rotate ang-z 0 0 1)
      (display-content)
      (gl-pop-matrix)

      (inc! time)
      (set! ang-x (- ang-x *mouse-y*))
      (set! ang-y (+ ang-y *mouse-x*))
      (set! ang-z (+ ang-z 0.1)))))

(define make-anim-stream
  (stream-cons draw make-anim-stream))
