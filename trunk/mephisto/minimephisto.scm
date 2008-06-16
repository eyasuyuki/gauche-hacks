;; Mini MEPHISTO
;; for Gauche Hacks book
;; written by Toru Hisai
;; $Id$


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

(define (display-content m)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  (gl-color 0.0 1.0 0.0)

  (gl-material GL_FRONT GL_DIFFUSE (f32vector 0 1 1 1.0))
  (gl-material GL_FRONT GL_AMBIENT
	       (f32vector (/ 0 10) (/ 1 10) (/ 0 10) 1.0))

  (gl-push-matrix)
  (gl-rotate (* m 0.2) 0 (sin (* m 0.01)) (cos (* m 0.01)))
  (gl-scale 0.5 0.5 0.5)
  (gl-begin GL_TRIANGLE_STRIP)
  (let loop ((n 50))
    (let ((r (+ 5(* n 0.03)))
	  (theta (* n 0.3))
	  (phi (* n 0.5)))
      (let1 r-cos-phi (* r (cos phi))
	(let ((x (* r-cos-phi (cos theta)))
	      (y (* r-cos-phi (sin theta)))
	      (z (* r (sin theta))))
	  (gl-normal x y z)
	  (gl-vertex x y z))))
    (if (< n 0)
	'done
	(loop (- n 1)))
    )
  (gl-end)
  (gl-pop-matrix)

  (glut-swap-buffers)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-frustum -2.0 2.0 -1.5 1.5 1.5 30.0)
  (gl-matrix-mode GL_MODELVIEW)
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

(define (mephisto-main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  (glut-init-window-size 640 480)
  (glut-init-window-position 0 0)
  (glut-create-window "MEPHISTO")
  (mephisto-init!)

  (glut-display-func (make-display))
  (glut-idle-func (make-display))
  (glut-keyboard-func keyboard)

  (glut-main-loop)
  0)

(define (main args)
  (mephisto-main args))

(define draw
  (let1 time 0
    (lambda ()
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-perspective 60 4/3 1 100)

  (gl-matrix-mode GL_MODELVIEW)
      (gl-load-identity)
      (glu-look-at 0.0 0.0 10.0 0.0 0.0 0.0 0.0 1.0 0.0)
      (display-content time)
      (inc! time))))

(define make-anim-stream
  (stream-cons draw make-anim-stream))
