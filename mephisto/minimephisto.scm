(use gl)
(use gl.glut)

(define (mephisto-init!)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-ortho -1.0 1.0 -1.0 1.0 -1.0 1.0)

  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)

  (gl-light GL_LIGHT0 GL_POSITION '#f32(0.0 10.0 20.0 0.0))
  (gl-light GL_LIGHT0 GL_DIFFUSE '#f32(1.0 1.0 1.0 1.0))
  (gl-light GL_LIGHT0 GL_AMBIENT '#f32(1.0 1.0 1.0 1.0))
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)

  (gl-enable GL_DEPTH_TEST)
  (gl-enable GL_NORMALIZE)

  (gl-load-identity)
  (glu-look-at 0.5 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0)

;;   (glut-display-func display)
;;   (glut-keyboard-func keyboard)
  )

(define (display)
  (display-content)
  )

(define (display-content)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-clear GL_DEPTH_BUFFER_BIT)

  (gl-color '#f32(0 1.0 1.0))
  (gl-begin* GL_POLYGON
    (gl-vertex '#f32(0.25 0.25 0.0))
    (gl-vertex '#f32(0.75 0.25 0.0))
    (gl-vertex '#f32(0.75 0.75 0.0))
    (gl-vertex '#f32(0.25 0.75 0.0))
    )
;;   (gl-flush)

  (gl-color 0.0 1.0 0.0)

  (gl-begin GL_TRIANGLE_STRIP)
  (let loop ((n 50))
    (gl-vertex (* n 0.003) (* 1 (sin (* n 0.3))) (* 1 (cos (* n 0.3))))
    (if (< n 0)
	'done
	(loop (- n 1)))
    )
  (gl-end)

  (glut-solid-torus 0.2 0.5 10 10)
  (gl-flush)
  )

;; (define (reshape w h)
;;   (gl-viewport 0 0 w h)
;;   (gl-matrix-mode GL_PROJECTION)
;;   (gl-load-identity)
;;   (gl-frustum -2.0 2.0 -1.5 1.5 1.5 30.0)
;;   (gl-matrix-mode GL_MODELVIEW)
;;   )

(define (keyboard key x y)
  (case key
    ((27) (exit 0))))

(define (mephisto-main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_DEPTH GLUT_RGB))

  (glut-init-window-size 640 480)
  (glut-init-window-position 0 0)
  (glut-create-window "MEPHISTO")
  (mephisto-init!)

  (glut-display-func display)
  (glut-keyboard-func keyboard)

  (glut-main-loop)
  0)

(define (main args)
  (mephisto-main args))
