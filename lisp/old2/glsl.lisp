(asdf:oos 'asdf:load-op :cl-opengl)
(asdf:oos 'asdf:load-op :cl-glu)
(asdf:oos 'asdf:load-op :curry)
(load "glut")
(load "/Users/ed/bin/slime/swank-loader.lisp")

(defparameter *time* 0)
(defun time-in-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))


(defparameter fragment-shader nil)
(defparameter vertex-shader nil)
(defparameter mandelbrot-shader nil)

(setq mandelbrot-shader nil)

(defun draw ()
  (gl:clear-color 0 0 0 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 1 0 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  
  (unless mandelbrot-shader
    (setq vertex-shader (gl:create-shader :vertex-shader))
    (gl:shader-source vertex-shader
                      '("void main() { gl_TexCoord[0] = gl_MultiTexCoord0; gl_Position = ftransform(); }"))
    (gl:compile-shader vertex-shader)
    
    (setq fragment-shader (gl:create-shader :fragment-shader))
    (gl:shader-source fragment-shader
                      '("void main() { gl_FragColor = vec4(0, 1, 0, 1); }"))
	;;'("int max_iterations = 256;  float countMandelbrotEscapeIterations(vec2 c) {         int i;         float tmp;         vec2 z = vec2(0.0, 0.0);                  for(i = 0; (4.0 > z.s*z.s + z.t*z.t) && (i < max_iterations); i++)         {                 tmp = z.s;                 z = vec2(z.s*z.s - z.t*z.t + c.s, 2.0*tmp*z.t + c.t);         }                  return float(i); }  void main() {         vec2 z = vec2(0, 0);         vec2 c = gl_TexCoord[0].st;                  float color = countMandelbrotEscapeIterations(c) / float(max_iterations);                  gl_FragColor = vec4(color, color, color, 1); }"))
    (gl:compile-shader fragment-shader)
    
    (setq mandelbrot-shader (gl:create-program))
    (gl:attach-shader mandelbrot-shader vertex-shader)
    (gl:attach-shader mandelbrot-shader fragment-shader)
    (gl:link-program mandelbrot-shader))
  
  (gl:use-program mandelbrot-shader)
  (gl:color 1 0 0)
  (gl:begin :quads)
  (gl:tex-coord -1.5 -1.5) (gl:vertex 0 0)
  (gl:tex-coord 1.5 -1.5) (gl:vertex 1 0)
  (gl:tex-coord 1.5 1.5) (gl:vertex 1 1)
  (gl:tex-coord -1.5 1.5) (gl:vertex 0 1)
  (gl:end)
  
  (glut:swap-buffers))

(let ((last-time nil)
      (dt 0))
  (defun animate ()
    (let ((current-time (time-in-seconds)))
      (unless (eq last-time nil)
        (setq dt (- current-time last-time)))
      (setq last-time current-time))
    
    (setq *time* (+ *time* dt))
    
    (sb-sys:serve-all-events 0.01)
    (glut:post-redisplay)))

(defun initialize-glut ()
  (glut:init-posix-argv sb-ext:*posix-argv*)
  (glut:init-window-size 800 600)
  (glut:init-display-mode (logior glut:+double+ glut:+rgba+ glut:+depth+))
  (glut:create-window "GLUT AS FUCK")
  
  (cffi:defcallback display-callback :void ()
	(handler-case (draw)
	  (error (condition) (format t "Error in draw: ~a~%" condition))))
  (glut:display-func (cffi:callback display-callback))
  
  (cffi:defcallback animate-callback :void ()
	(handler-case (animate)
	  (error (condition) (format t "Error in animate: ~a~%" condition))))
  (glut:idle-func (cffi:callback animate-callback)))

(swank:create-server :port 4005 :dont-close t)
(initialize-glut)
(glut:main-loop)
