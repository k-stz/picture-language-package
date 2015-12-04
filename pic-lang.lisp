(defpackage :pic-lang
  (:use :cl
	:kit.math
	:game-objects
	:gl-utils))


(in-package :pic-lang)

(defparameter *frame-rectangle* (make-rectangle-c (vec3  300.0 300.0 0.0)))


(defun window-middle-point (game-window)
  (with-slots (width height) game-window
    (vec3 (float (floor (/ width 2.0))) (float (floor (/ height 2.0)))
	  0.0))))

;; If we did import the :game package this would overwrite the definition of MAIN in game.lisp!!!
(defun main ()
  (add-rectangle-as :frame *frame-rectangle*)
  (game:main)

  (game-objects::move-to :frame (window-middle-point game:*game-window*)))
