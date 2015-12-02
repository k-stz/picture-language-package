;; sbcl 2.6% mem

(defpackage :game
  ;; :use inherits all the exported symbols from the package given
  (:use :cl
	;; this one's important, as all the defclass lambda lists
	;; and the implicitly created generic functions appear in
	;; the example unqualified, as they're interned
	:sdl2.kit
	:kit.gl.shader
	:kit.math
	:opticl
	:opticl-utils ;; ~/opticl-stuff.lisp
	:game-objects
	:gl-utils)
  (:export
   #:main
   #:*game-window*))

(in-package :game)

;; with this you can call gl cmds in the repl while the sdl2kit window is running
;; affecting it:
;; repl> #m (gl:sampler-parameter *sampler* :texture-wrap-t :mirror-clamp-to-edge)
;; TODO: where to put reader-macro definitions. Surround by (evel-when xyz)? How does
;;       it behave on reloading/compiling the file
(set-dispatch-macro-character #\# #\m
			      #'(lambda (str char n)
				  (declare (ignore char n))
				  (list 'sdl2:in-main-thread () (read str t nil t))))

;; quite clunky, but with the right abstraction this can be done with even less typing
;; #m (progn
;;      (gl:bind-vertex-array *vao*)
;;      (%gl:buffer-sub-data :array-buffer (* 108 4) (* 72 4) *tex-coord*)) ;<- provide texture


;;; HOW TO USE:
;;;
;;; First, run this. It is SAFE to run repeatedly:
;;;
;   (sdl2.kit:start)

;;;
;;; Then, make a window.
;;;
;;;   (make-instance 'sdl2.kit.test:game-window)
;;;
;;; After you close a window, it will be collected at some point.

;;; You should NOT call any protocol functions on a window, except the
;;; following:
;;;
;;;   (render WINDOW)
;;;   (close-window WINDOW)
;;;
;;; These are the only functions guaranteed to be "safe" (including
;;; threadsafety and other expectations).

(defclass game-window (kit.sdl2:gl-window)
  ((start-time :initform (get-internal-real-time))
   (one-frame-time :initform (get-internal-real-time))
   ;; TODO: unify with framelimit usage, for now used for movement
   ;; in USING-KEYBOARD-STATE
   (last-frame-ticks :initform (sdl2:get-ticks))
   (frames :initform 0)
   (width :accessor window-width)
   (height :accessor window-height)
   (keystate-tracker :initform (make-instance 'keystate-tracker)
		     :reader keystate-tracker)))

(defvar *game-window*)

(defun main ()
  ;; TODO: interferes with other sdl2 using applications once executed!
  (sdl2.kit:start)
  (sdl2:in-main-thread ()
    ;; some drivers may, by default, choose a different core profile, this ensures the
    ;; right one is used:
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3))
  (setf *game-window* (make-instance 'game-window)))


;;Shader------------------------------------------------------------------------

;; the returned dictionary with the programs can be used like so:
;; (1) get the program directly (find-program <compiled-dictionary> <program-name>)
;; (2) or just use it directly (use-program <compiled-dictionary> <program-name>)
;;     also (use-program 0) works
(defun load-shaders ()
  (defdict shaders (:shader-path
		    (merge-pathnames
		     #p "shaders/" (asdf/system:system-source-directory :picture-language-package)))
    ;; instead of (:file <path>) you may directly provide the shader as a string containing the
    ;; source code
    (shader pixel-orthogonal-v :vertex-shader (:file "pixel-orthogonal.vert"))
    (shader 2d-rectangle-texture-f :fragment-shader (:file "2d-rectangle-texture.frag"))
    
    ;; here we compose the shaders into programs, in this case just one ":basic-projection"
    (program :pixel-orthogonal (:window-width :window-height :rectangle-texture)
	     (:vertex-shader pixel-orthogonal-v)
	     (:fragment-shader 2d-rectangle-texture-f)))
  ;; function may only run when a gl-context exists, as its documentation
  ;; mentions
  (compile-shader-dictionary 'shaders))

;; to be understood while reading the LOAD-SHADER function
;; example: (uniform :vec :<name-of-uniform> <new-value>)
(defgeneric uniform (type key value)
  (:method ((type (eql :vec)) key value)
    (uniformfv *programs-dict* key value))

  (:method ((type (eql :mat)) key value)
    ;; nice, transpose is NIL by default!
    (uniform-matrix *programs-dict* key 4 value NIL))

  (:method ((type (eql :int)) key value)
    (uniformi *programs-dict* key value)))


(defvar *programs-dict*)

(defun initialize-program ()
  (setf *programs-dict* (load-shaders)))

;;VAO setup.....................................................................

(defvar *vao* 0)
(defvar *vbo* 0)

(defun initialize-vao ()
  (let ((vao (first (gl:gen-vertex-arrays 1)))
	(vbo (first (gl:gen-buffers 1)))
	(ibo (first (gl:gen-buffers 1))))
    (gl:bind-vertex-array vao)
    ;;VBO
    (gl:bind-buffer :array-buffer vbo)
    ;;VBO - positions
    ;;to avoid magic numbers (* 24 4 3 2) the gl:gl-array (gl:alloc-gl-array) can be used,
    ;;which is a struct containing field with a pointer to the forein-memory and a field
    ;;with the size.  In this case ommited for the sake of a terse example.
    ;; Layout:
    ;; 8 number of vertices/colors
    ;; 3 components per vertex
    ;; 4 size of float
    ;; second (texture coordinate)
    ;; 8 vertices need to be associated with 2d-texture corner
    ;; 2 2d-texture needs two indices to read from it
    ;; 4 size of float
    (%gl:buffer-data :array-buffer (+ (* 108 4) (* 72 4)) *cube-positions* :static-draw)
    (%gl:enable-vertex-attrib-array 0)
    (%gl:vertex-attrib-pointer 0 3 :float :false 0 0)
    ;;VBO - texture coordinates
    ;; texture sub-data starts in vbo exactly after the position vertices hence
    (%gl:buffer-sub-data :array-buffer (* 108 4) (* 72 4) *tex-coord*)
    (%gl:enable-vertex-attrib-array 5)
    (%gl:vertex-attrib-pointer 5 2 :float :false 0 (* 108 4))

    ;;IBO
    (gl:bind-buffer :element-array-buffer ibo)
    ;; why (* 36 2)?
    ;; it takes 2 triangles to draw the side of cube, hence to draw a whole cube:
    ;; (* 2 6) => 12. Each triangle consists of 3 vertices, hence, (* 3 12) => 36
    ;; and the index buffer's indices first point to the vertices in the vbo,
    ;; supplied by *cube-positions*
    (%gl:buffer-data :element-array-buffer (* 2 36) *cube-indices* :static-draw)

    (gl:bind-vertex-array 0)
    (gl:bind-buffer :array-buffer 0)
    (setf *vbo* vbo)
    (setf *vao* vao)))

;;utils-------------------------------------------------------------------------

(defun framelimit (window &optional (fps 60))
  "Issues SDL2:DELAY's to get desired FPS."
  (with-slots (one-frame-time) window
    (let ((elapsed-time (- (get-internal-real-time) one-frame-time))
	  (time-per-frame (/ 1000.0 fps)))
      (when (< elapsed-time time-per-frame)
	(sdl2:delay (floor (- time-per-frame elapsed-time))))
      (setf one-frame-time (get-internal-real-time)))))


(defun display-fps (window)
  (with-slots (start-time frames) window
    (incf frames)
    (let* ((current-time (get-internal-real-time))
	   (seconds (/ (- current-time start-time) internal-time-units-per-second)))
      (when (> seconds 5)
	(format t "FPS: ~A~%" (float (/ frames seconds)))
	(setf frames 0)
	(setf start-time (get-internal-real-time))))))


;;init code---------------------------------------------------------------------

(defvar *tex-unit* 0)

(defun rectangle-program-pixel-transfer (game-window)
  ;; here we pass the window width and height to the shader, so it has
  ;; all the data needed to translate the pixel rectangle properly
  (use-program *programs-dict* :pixel-orthogonal)
  (uniform :int :window-width (window-width game-window))
  (uniform :int :window-height (window-height game-window))
  (use-program *programs-dict* 0))


(defmethod initialize-instance :after ((w game-window) &key &allow-other-keys)
  (multiple-value-bind (width height) (window-size w)
    (setf (window-width w) width
	  (window-height w) height))

  
  ;; GL setup can go here; your GL context is automatically active,
  ;; and this is done in the main thread.

  ;; if you (setf (idle-render window) t) it'll call RENDER as fast as
  ;; possible when not processing other events - suitable for games
  (setf (idle-render w) t)
  (gl:clear-color 0 0 0.5 1)
  (gl:clear :color-buffer-bit)
  (gl:viewport 0 0 (window-width w) (window-height w))

  ;; with culling
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)

  ;; with depth-buffer
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (%gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  
  (initialize-program)
  ;(initialize-vao)


  ;; EXPERIMENTS
  (game-objects::initialize-rectangle-vao)
  (game-objects::create-rectangle-texture)
  (rectangle-program-pixel-transfer w)
  ;; texture
  (use-program *programs-dict* :pixel-orthogonal)
  (uniform :int :rectangle-texture game-objects::*tex-unit*) ; = glUniform1i(<location>, <texture-image-unit>);
  (use-program *programs-dict* 0)
  
  ;; /EXPERIMETNS

  
  ;;texture
  ;; ;; here we associate the uniform sample with the texture image unit
  ;; (use-program *programs-dict* :basic-projection)
  ;; (uniform :int :test-texture *tex-unit*) ; = glUniform1i(<location>, <texture-image-unit>);
  ;; (use-program *programs-dict* 0)

)



;;texture data------------------------------------------------------------------


;; (defvar *2d-texture-data*
;;   (cffi:foreign-alloc :unsigned-char :initial-contents (loop for i upto 255 collect i)))



;; ;;(defparameter *123-texture-data*)

;; (defvar *sampler* 0)
;; (defvar *texture* 0)



;;Events------------------------------------------------------------------------


(defmethod close-window ((window game-window))
  (format t "Bye!~%")
  ;; To _actually_ destroy the GL context and close the window,
  ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
  ;; prompt the user!
  (call-next-method))


(defun init-nyo-rectangle ()
  (let ((nyo-rectangle ;(game-objects:make-rectangle 100.0 100.0 64.0 96.0)
	 (game-objects:make-rectangle-c (vec3 100.0 100.0 0.0)
					(vec3 32.0 48.0 0.0))))
    (print (bounding-volume nyo-rectangle))
    (game-objects:add-rectangle-as :nyo nyo-rectangle)
    ;; NEXT-TODO:
    ;; Problem with this: some BV transformation and creation function use the rendering coordinates
    ;; :x1 :x2 :y1 :y2. Either create, (CH?), point set as part of the representation stored in
    ;; :bounding-volume slot of Rectangle or derive point set from rectangle/radius etc.
    ;; (set-radius (bounding-volume nyo-rectangle) (vec3 20.0 42.0 0.0))
    (game-objects:set-animation :nyo :walk :down 0 :nyo)))

(defparameter *nyo-rectangle* (init-nyo-rectangle))

(defmethod keyboard-event ((window game-window) state ts repeat-p keysym)

  ;; makes the keyboard state global in the window object, so we can access
  ;; it whenever we want in the rendering loop
  (keystate-update (keystate-tracker window) state repeat-p keysym)
  
  (let ((scancode (sdl2:scancode keysym)))
    (when (eq :scancode-d scancode)
      (game-objects:move :nyo (vec2 5.0 0.0)))

    (when (eq :scancode-c scancode)
      (game-objects::clr-seq-hash game-objects::*dynamic-rectangles*))
    (when (eq :scancode-escape scancode)
      (close-window window))
    ;; for tests
    (when (eq :scancode-t scancode)
      (collision-test))))

;;Rendering----------------------------------------------------------------------

(defun draw-rectangles ()
  (game-objects::update-rectangle-vao)
  (game-objects::update-rectangle-texture)

  (gl:bind-vertex-array game-objects::*vao*)
  (use-program *programs-dict* :pixel-orthogonal)


  ;; TODO: apply draw range
  (game-objects::draw-rectangles)
  ;; very strange, why when I put this here it works, but not before the DRAW-* call??


  (gl:bind-vertex-array 0)
;  (gl:bind-buffer :array-buffer 0)
  (use-program *programs-dict* 0))

(defmethod render ((window game-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW  (!!)
  ;; after RENDER.
  (gl:clear :color-buffer :depth-buffer-bit)
  
;  (using-keyboard-state window)

  ;; (collision-test) ;; test if this is bottleneck UPDATE: rendering seems to hog CPU time

  (draw-rectangles)

  (display-fps window)
  (framelimit window 60))



