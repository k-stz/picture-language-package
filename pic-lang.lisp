(defpackage :pic-lang
  (:use :cl
	:kit.math
	:game-objects
	:gl-utils)
  (:export
   #:main))


(in-package :pic-lang)

(defparameter *frame-rectangle* (make-rectangle-c (vec3  300.0 300.0 0.0)))


(defun window-middle-point (pic-lang-window)
  (with-slots (pic-lang-core::width pic-lang-core::height) pic-lang-core-window
    (vec3 (float (floor (/ pic-lang-core::width 2.0))) 
	  (float (floor (/ pic-lang-core::height 2.0)))
	  0.0)))

;; experiments -----------------------------------------------------------------



;; move upstream once this works!
;; (defun draw-lines ()
;;   (init-lines-vao) ;; TODO: move upsteam once this works!

;;   )


;; -----------------------------------------------------------------------------

(defun main ()
  (add-rectangle-as :frame *frame-rectangle*)
  (pic-lang-core:main))


(defclass frame ()
  ((x1 :initarg :x1 :type vec3)
   (x2 :initarg :x2 :type vec3)
   (y1 :initarg :y1 :type vec3)
   (y2 :initarg :y2 :type vec3)))

