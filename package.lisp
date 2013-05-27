;;;; package.lisp

(defpackage #:rectangle-packing
  (:use #:cl)
  (:export
   :pack-rectangles-tree
   :pack-rectangles
   :write-html
   :tree-utilized-size
   :rectangle-tree-to-rectangle-list))

