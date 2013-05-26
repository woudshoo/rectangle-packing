;;;; rectangle-packing.lisp

(in-package #:rectangle-packing)

;;; "rectangle-packing" goes here. Hacks and glory await!

(defun pack-rectangles-tree (rectangles &key (size (list 1000 1000)))
  "Takes a list of rectangles, where each rectangle
is specified as (width height . rest).

Returns a tree containing the pack information.  This can be used to add additional rectangles
later.
"

  (let ((root (make-instance 'node :content (make-instance 'target-rectangle)))
	(*size* size))

    (loop :for rectangle :in rectangles :do
       (insert-rectangle root rectangle))
    
    root))

(defun rectangle-tree-to-rectangle-list (root)
  (let (result)
    (walk-tree-pre-order root (lambda (content) 
				(when (placed-rectangle-p content)
				  (push (list* (lv content 0) (lv content 1) :0 (rectangle content)) result))))
    result))

(defun pack-rectangles (rectangles &key (size (list 1000 1000)))
  "Takes a list of rectangles, where each rectangle
is specified as (width height . rest).

Returns a list of (x y orientation . rectangle)
Where rectangle is one of the argument rectangles
and orientation is either :0 or :90 (when it is rotated)."

  (rectangle-tree-to-rectangle-list (pack-rectangles-tree rectangles :size size)))


(defun tree-utilized-size (node)
  (let ((max-0 0)
	(max-1 0))
    (walk-tree-pre-order node
			 (lambda (content)
			   (when (placed-rectangle-p content)
			     (setf max-0 (max max-0 (hv content 0)))
			     (setf max-1 (max max-1 (hv content 1))))))
    (list max-0 max-1)))

(defparameter *size* (list 0 0))

(defclass node ()
  ((content :accessor content :initform nil :initarg :content)
   (left-child :accessor left-child :initform nil)
   (right-child :accessor right-child :initform nil)))

(defgeneric walk-tree-pre-order (node function)
  (:method (node function) nil)
  (:method ((node node) function)
    (funcall function (content node))
    (walk-tree-pre-order (left-child node) function)
    (walk-tree-pre-order (right-child node) function)))

(defclass decision ()
  ((decision-var :type (integer 0 1) 
		 :initform 0 
		 :initarg :decision-var)
   (low :initform 0 :initarg :low)
   (decision :initform 0 :initarg :decision)
   (high :initform nil :initarg :high)))

(defclass target-rectangle ()
  ((split-var :type (integer 0 1) :accessor split-var :initarg :split-var :initform 0)
   (low :initarg :low :accessor low :initform (list 0 0))
   (high :initarg :high :accessor high :initform (list nil nil))
   (rectangle :initarg :rectangle :accessor rectangle :initform nil)))

(defgeneric dimension (object var)
  (:method ((rect target-rectangle) var) (- (or (nth var (high rect)) (nth var *size*))
					    (nth var (low rect))))
  (:method ((rect list) var) (nth var rect)))

(defgeneric hv (object var)
  (:method (object var) (nth var (high object)))
  (:method ((object list) var) (nth var object)))

(defgeneric lv (object var)
  (:method (object var) (nth var (low object)))
  (:method ((object list) var) (declare (ignore var)) 0))

(defgeneric empty-target-p (node)
  (:method ((node decision)) nil)
  (:method ((node target-rectangle)) (not (rectangle node))))

(defgeneric placed-rectangle-p (node-content)
  (:method (node-content) nil)
  (:method ((node-content target-rectangle)) (rectangle node-content)))

(defgeneric rectangle-fits (node rectangle)
  (:method ((node target-rectangle) rectangle)
    (and (>= (dimension node 0) (dimension rectangle 0))
	 (>= (dimension node 1) (dimension rectangle 1)))))

(defmethod insert-rectangle ((node node) rectangle)
  "Inserts the rectangle specified as (width height . rest)
in the tree, and if necessary expand the tree.

Special variables are *size* as (width height) of the total rectangle in which 
the rectangles are packed"
  (or (and (empty-target-p (content node))
	   (rectangle-fits (content node) rectangle)
	   
	   (split-node node rectangle))
      (and (left-child node) (insert-rectangle (left-child node) rectangle))
      (and (right-child node) (insert-rectangle (right-child node) rectangle))))

(defun translate (rect point)
  (mapcar #'+ rect point))

(defun split-node-once (node rectangle)
  "Replaces the target node node
with a decision node with two target node children."
  
  (let* ((content (content node))
	 (var (split-var content))
	 (split-point (translate rectangle (low content)))
	 (new-content (make-instance 'decision
				     :high (hv content var)
				     :low (lv content var)
				     :decision (nth var split-point)))
	 (left-child (make-instance 'target-rectangle :split-var (- 1 var)
				    :low (low content)
				    :high (if (= 0 var) 
					      (list (hv split-point var) (hv content (- 1 var)))
					      (list (hv content (- 1 var)) (hv split-point var)))))
	 (right-child (make-instance 'target-rectangle :split-var (- 1 var)
				     :low (if (= 0 var)
					      (list (hv split-point var) (lv content (- 1 var)))
					      (list (lv content (- 1 var)) (hv split-point var)))
				     :high (high content))))
    (setf (content node) new-content)
    (setf (left-child node) (make-instance 'node :content left-child))
    (setf (right-child node) (make-instance 'node :content right-child))))

(defun split-node (node rectangle)
  "Node needs to be a target-rectangle and rectangle
needs to fit inside this rectangle.  

This function will change:

   node ... (target-rectangle)

==>
      node ... (decision)
       /      \
      /     node ... (target-rectangle)
     /
   node ... (decision)
  /  \ 
 /   node ... (target-rectangle)
/
node ... (target-rectangle = rectangle) 
"

  (split-node-once node rectangle)
  (split-node-once (left-child node) rectangle)
  (setf (rectangle (content (left-child (left-child node)))) rectangle))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-html (node file-name)
  "Writes a packing tree to an html file so the packing can be previewd.
The red rectangles are placed, the blue is empty space."
  (with-open-file (s file-name :direction :output :if-exists :supersede)
    (let ((*size* (tree-utilized-size node)))
      (format s "<html><body>
<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">
 ")
      (write-svg-element s node)
      (format s "</svg></body></html"))))


(defgeneric write-svg-element (stream element)
  (:method ((s stream) (node node))
    (when (left-child node) (write-svg-element s (left-child node)))
    (when (right-child node) (write-svg-element s (right-child node)))
    (write-svg-element s (content node)))
  (:method ((s stream) (rect target-rectangle))
    (format s 
	    "<rect rx=\"10\" ry=\"10\" 
       x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\" 
       style=\"fill:~A;stroke:black;opacity=0.1\" />
<!-- low = ~A high = ~A   (rect: ~A)-->~%~%"
	    (lv rect 0) (lv rect 1) (dimension rect 0) (dimension rect 1)
	    (if (rectangle rect) "red" "blue")
	    (low rect) (high rect)
	    (rectangle rect)))
  (:method ((s stream) (rect decision))
    nil))
