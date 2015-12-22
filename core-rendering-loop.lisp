;; sbcl 2.6% mem

(defpackage :pic-lang-core
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
   #:*pic-lang-window*))

(in-package :pic-lang-core)

;; with this you can call gl cmds in the repl while the sdl2kit window is running
;; affecting it:
;; repl> #m (gl:sampler-parameter *sampler* :texture-wrap-t :mirror-clamp-to-edge)
;; TODO: where to put reader-macro definitions. Surround by (evel-when xyz)? How does
;;       it behave on reloading/compiling the file
(set-dispatch-macro-character #\# #\m
			      #'(lambda (str char n)
				  (declare (ignore char n))
				  (list 'sdl2:in-main-thread () (read str t nil t))))



;;; HOW TO USE:
;;;
;;; First, run this. It is SAFE to run repeatedly:
;;;
;   (sdl2.kit:start)

;;;
;;; Then, make a window.
;;;
;;;   (make-instance 'sdl2.kit.test:pic-lang-window)
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

(defclass pic-lang-window (kit.sdl2:gl-window)
  ((start-time :initform (get-internal-real-time))
   (one-frame-time :initform (get-internal-real-time))
   ;; TODO: unify with framelimit usage, for now used for movement
   ;; in USING-KEYBOARD-STATE
   (last-frame-ticks :initform (sdl2:get-ticks))
   (frames :initform 0)
   (width :accessor window-width)
   (height :accessor window-height)))

(defvar *pic-lang-window*)

(defun main ()
  ;; TODO: interferes with other sdl2 using applications once executed!
  (sdl2.kit:start)
  (sdl2:in-main-thread ()
    ;; some drivers may, by default, choose a different core profile, this ensures the
    ;; right one is used:
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3))
  (setf *pic-lang-window* (make-instance 'pic-lang-window)))


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
    ;; for drawing lines
    (shader pixel-orthogonal-lines-v :vertex-shader (:file "pixel-orthogonal-lines.vert"))
    (shader pixel-orthogonal-lines-f :vertex-shader (:file "pixel-orthogonal-lines.frag"))
    
    ;; here we compose the shaders into programs, in this case just one ":basic-projection"
    (program :pixel-orthogonal (:window-width :window-height :rectangle-texture)
	     (:vertex-shader pixel-orthogonal-v)
	     (:fragment-shader 2d-rectangle-texture-f))
    (program :pixel-lines (:window-width :window-height)
	     (:vertex-shader pixel-orthogonal-lines-v)
	     (:fragment-shader pixel-orthogonal-lines-f)))
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


(defun rectangle-program-pixel-transfer (pic-lang-window)
  ;; here we pass the window width and height to the shader, so it has
  ;; all the data needed to translate the pixel rectangle properly
  (use-program *programs-dict* :pixel-orthogonal)
  (uniform :int :window-width (window-width pic-lang-window))
  (uniform :int :window-height (window-height pic-lang-window))
  (use-program *programs-dict* 0)
  (use-program *programs-dict* :pixel-lines)
  (uniform :int :window-width (window-width pic-lang-window))
  (uniform :int :window-height (window-height pic-lang-window))
  (use-program *programs-dict* 0))


(defmethod initialize-instance :after ((w pic-lang-window) &key &allow-other-keys)
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

  ;; EXPERIMENTS
  (game-objects::initialize-rectangle-vao)
  (game-objects::create-rectangle-texture)
  (rectangle-program-pixel-transfer w)


  ;; drawing lines TODO refactor once this works!
  (game-objects::initialize-lines-vao)

  
  ;; texture
  (use-program *programs-dict* :pixel-orthogonal)
  (uniform :int :rectangle-texture game-objects::*tex-unit*) ; = glUniform1i(<location>, <texture-image-unit>);
  (use-program *programs-dict* 0)
  
  ;; /EXPERIMETNS
)



;;Events------------------------------------------------------------------------


(defmethod close-window ((window pic-lang-window))
  (format t "Bye!~%")
  ;; To _actually_ destroy the GL context and close the window,
  ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
  ;; prompt the user!
  (call-next-method))


(defun init-nyo-rectangle ()
  (let ((nyo-rectangle ;(game-objects:make-rectangle 100.0 100.0 64.0 96.0)
	 (game-objects:make-rectangle-c (vec3 100.0 100.0 0.0)
					(vec3 32.0 48.0 0.0))))
    (game-objects:add-rectangle-as :nyo nyo-rectangle)
    ;; NEXT-TODO:
    ;; Problem with this: some BV transformation and creation function use the rendering coordinates
    ;; :x1 :x2 :y1 :y2. Either create, (CH?), point set as part of the representation stored in
    ;; :bounding-volume slot of Rectangle or derive point set from rectangle/radius etc.
    ;; (set-radius (bounding-volume nyo-rectangle) (vec3 20.0 42.0 0.0))
    (game-objects:set-animation :nyo :walk :down 0 :nyo)
    ))

(defparameter *nyo-rectangle* (init-nyo-rectangle))

(defmethod keyboard-event ((window pic-lang-window) state ts repeat-p keysym)

  ;; makes the keyboard state global in the window object, so we can access
  ;; it whenever we want in the rendering loop
;  (keystate-update (keystate-tracker window) state repeat-p keysym)
  
  (let ((scancode (sdl2:scancode keysym)))
    (when (eq :scancode-d scancode)
      (game-objects:move :nyo (vec2 5.0 0.0)))
    (when (eq :scancode-a scancode)
      (game-objects:move :nyo (vec2 -5.0 0.0)))

    (when (eq :scancode-c scancode)
      (game-objects::clr-seq-hash game-objects::*dynamic-rectangles*))
    (when (eq :scancode-n scancode)
      (init-nyo-rectangle))
    
    (when (eq :scancode-escape scancode)
      (close-window window))
    ;; for tests
    (when (eq :scancode-t scancode)
      (print "test"))))

;;Rendering----------------------------------------------------------------------

(defun draw-rectangles ()
  (game-objects::update-rectangle-vao)
  (game-objects::update-rectangle-texture)

  (gl:bind-vertex-array game-objects::*vao*)
  (use-program *programs-dict* :pixel-orthogonal)


  ;; TODO: apply draw range
  (game-objects::draw-rectangles)

  (gl:bind-vertex-array 0)

  (use-program *programs-dict* 0))

(defun draw-lines ()
  (game-objects::update-lines-vao)
  
  (gl:bind-vertex-array game-objects::*lines-vao*)
  (use-program *programs-dict* :pixel-lines)

  (game-objects::draw-lines)

  (gl:bind-vertex-array 0)
  (use-program *programs-dict* 0))

(defmethod render ((window pic-lang-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW  (!!)
  ;; after RENDER.
  (gl:clear :color-buffer :depth-buffer-bit)
  
  (draw-rectangles)
  (draw-lines)

  (display-fps window)
  (framelimit window 60))



