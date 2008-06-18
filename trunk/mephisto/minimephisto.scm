;; Mini MEPHISTO
;; for Gauche Hacks book
;; written by Toru Hisai
;; $Id$


(use srfi-1)

(use gl)
(use gl.glut)
(use gl.math3d)
(use math.const)
(use util.stream)
(use gauche.uvector)

(define (mephisto-init!)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)

  (gl-light GL_LIGHT0 GL_POSITION '#f32(20.0 0.0 0.0 0.0))
  (gl-light GL_LIGHT0 GL_DIFFUSE '#f32(0.0 0.5 1.0 1.0))

  (gl-light GL_LIGHT1 GL_POSITION '#f32(-20.0 0.0 0.0 0.0))
  (gl-light GL_LIGHT1 GL_DIFFUSE '#f32(1.0 0.5 0.0 1.0))

  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_LIGHT1)

  (gl-enable GL_DEPTH_TEST)
  (gl-enable GL_NORMALIZE)

;   (gl-load-identity)
;   (glu-look-at 0.5 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0)
  )

; (define strip
;   (map (lambda (n)
; 	 (list (+ 5(* n 0.03))		;r
; 	       (* n 0.9)		;theta
; 	       (* n 0.7)))		;phi
;        (iota 1000)))

(define strip '())

(define (display-content)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  (gl-color 0.0 1.0 0.0)

  (gl-material GL_FRONT GL_DIFFUSE (f32vector 1 0.5 1 1))
;   (gl-material GL_FRONT GL_AMBIENT (f32vector 1 0 0 1))

  (gl-begin GL_TRIANGLE_STRIP)
  (gl-normal 0 0 -1)
  (gl-vertex 0 0 0)
  (let loop ((n 400)
	     (s strip)
	     (prev-vec (vector4f 0 0 0)))
    (if (null? s)
	'done
	(let ((p (car s)))
	  (let ((r (car p))
		(theta (cadr p))
		(phi (caddr p)))
	    (let ((r-cos-phi (* r (cos phi))))
	      (let ((x (* r-cos-phi (cos theta)))
		    (y (* r-cos-phi (sin theta)))
		    (z (* r (sin phi))))
		(let ((new-vec (vector4f x y z)))
		  (gl-normal (vector4f-cross (vector4f-sub new-vec prev-vec) new-vec))
		  (gl-vertex x y z)
		  (if (< n 0)
		      (set-cdr! s '())
		      (loop (- n 1) (cdr s) new-vec)))))))))
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
	(h (/ *window-height* 2))
	(alpha 1))
    (lambda (x y)
      (set! *mouse-x* (/ (- x w) w))
      (set! *mouse-y* (/ (- y h) h))

      (let ((r (* (+ 1 alpha) (* 7 (+ *mouse-x* *mouse-y*))))
	    (t (* pi *mouse-x*))
	    (p (* pi *mouse-y*)))
	(set! alpha (- 1 alpha))
	(set! strip (cons (list r t p) strip))))))

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
      (set! ang-x (- ang-x (/ *mouse-y* 3)))
;       (set! ang-y (+ ang-y (/ *mouse-x* 2)))
      (set! ang-y (+ ang-y 3))
;       (set! ang-z (+ ang-z 0.1))
      )))

(define make-anim-stream
  (stream-cons draw make-anim-stream))
