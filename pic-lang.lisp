(defpackage :pic-lang
  (:use :cl
	:kit.math
	:game-objects
	:gl-utils))


(in-package :pic-lang)

(defparameter *frame-rectangle* (make-rectangle-c (vec3  300.0 300.0 0.0)))


(defun window-middle-point (game-window)
  (with-slots (game::width game::height) game-window
    (vec3 (float (floor (/ game::width 2.0))) 
	  (float (floor (/ game::height 2.0)))
	  0.0)))

;; experiments -----------------------------------------------------------------



;; move upstream once this works!
;; (defun draw-lines ()
;;   (init-lines-vao) ;; TODO: move upsteam once this works!

;;   )


;; -----------------------------------------------------------------------------

;; If we did import the :game package this would overwrite the definition of MAIN in game.lisp!!!
(defun main ()
  (add-rectangle-as :frame *frame-rectangle*)
  (game:main)
  (draw-lines))


(defclass frame ()
  ((x1 :initarg :x1 :type vec3)
   (x2 :initarg :x2 :type vec3)
   (y1 :initarg :y1 :type vec3)
   (y2 :initarg :y2 :type vec3)))

