;; This command is package indepedent. (debug 3) will show you local variables
;; of a function and even it's lexically scoped ones (if they use a *special-variable*)
;; This pertains functions involved in signalled error
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defsystem #:picture-language-package
  :description ""
  :version ""
  ;; system-name != package-name   i.e. :sdl2kit != sdl2.kit !
  :depends-on (:cl-opengl  ;; (ql:quickload "cl-opengl")
	       :sdl2kit    ;; (ql:quickload "sdl2kit")
	       :glkit      ;; (ql:quickload "glkit")
	       :opticl     ;; (ql:quickload "opticl")
	       :texatl     ;; https://github.com/rpav/texatl + https://github.com/rpav/laconic
	       )
  :author "k-stz"
  :license "MIT"
  :serial t
  :components ((:file "tex")
	       (:file "math")
	       (:file "gl-utils")
	       (:file "opticl-utils")
	       (:file "game-objects")
	       (:file "core-rendering-loop")
	       (:file "pic-lang")))
