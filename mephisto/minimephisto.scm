(use gl)
(use gl.glut)

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

  (gl-load-identity)
  (glu-look-at 0.0 0.0 15.0 0.0 0.0 0.0 0.0 1.0 0.0)

  (glut-display-func display)
  (glut-keyboard-func keyboard)
  )

(define (display)
  (display-cont)
  )

(define (display-cont)
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

(define (mephisto-main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  (glut-init-window-size 320 240)
  (glut-init-window-position 0 0)
  (glut-create-window "MEPHISTO")
  (mephisto-init!)
  (glut-keyboard-func keyboard)

  (glut-main-loop)
  0)
