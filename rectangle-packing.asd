;;;; rectangle-packing.asd

(asdf:defsystem #:rectangle-packing
  :serial t
  :description "Code to pack rectangles into a bigger rectangle.  Useful for texture packing for OpenGL."
  :author "Willem Rein Oudshoorn <woudshoo@xs4all.nl>"
  :version "1.0.0"
  :license "LLGPL, but I am flexible, ask me if you want something else."
  :components ((:file "package")
               (:file "rectangle-packing")))

