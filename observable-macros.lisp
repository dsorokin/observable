;;;; OBSERVABLE -- events and observable objects from F# for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2011-2013
;;;;
;;;; Licence:
;;;;
;;;;  Permission is hereby granted, free of charge, to any person
;;;;  obtaining a copy of this software and associated documentation files
;;;;  (the "Software"), to deal in the Software without restriction,
;;;;  including without limitation the rights to use, copy, modify, merge,
;;;;  publish, distribute, sublicense, and/or sell copies of the Software,
;;;;  and to permit persons to whom the Software is furnished to do so,
;;;;  subject to the following conditions:
;;;;
;;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :observable)

(defmacro deftrigger (name qualifier ((sender type) &rest args) &body specs)
  "Define the event trigger. Example:
(deftrigger trigger-node-rect-changed :after ((node rect-node))
  (:slot 'rect-changed)
  (:documentation ...)
  (:form ...))  ; optional
"
  `(progn
     (defgeneric ,name (,sender ,@args)
       ,@(let ((doc (assoc :documentation specs)))
           (if doc (list doc))))
     (defmethod ,name ((,sender ,type) ,@args)
       ,@(if args `((declare (ignorable ,@args))))
       ,@(let ((form (assoc :form specs)))
           (cdr form)))
     (defmethod ,name ,qualifier ((,sender ,type) ,@args)
       (trigger-event 
        (slot-value ,sender 
                    ,(cadr (assoc :slot specs)))
        ,sender
        ,@args))))

(defmacro defevent (name ((sender type)) &body specs)
  "Define the event. Example:
(defevent node-rect-changed ((node rect-node))
  (:slot 'rect-changed)
  (:documentation ...))  ; optional
"
  `(progn
     (defgeneric ,name (,sender)
       ,@(let ((doc (assoc :documentation specs)))
           (if doc (list doc))))
     (defmethod ,name ((,sender ,type))
       (publish-event
        (slot-value ,sender ,(cadr (assoc :slot specs)))))))

(defmacro defaccessor (accessor ((name type)) &body body)
  "Create an accessor for the property. Example: 
(defaccessor geometry-z ((node geometry-node))
  (:slot 'z)
  (:test #'eql)          ; optional with #'eql by default
  (:changing-trigger trigger-geometry-z-changing)  ; optional
  (:changed-trigger trigger-geometry-z-changed)    ; optional
  (:midform (lambda (v0 v) ...))                   ; optional
  (:preform (progn ...))   ; optional
  (:postform (progn ...))  ; optional
  (:documentation ...))    ; optional
"
  (flet ((prop (key &key (default-value nil default-value-p))
               (let ((pair (assoc key body)))
                 (if pair (cadr pair)
                   (if default-value-p default-value
                     (error "Assoc ~s is expected: ~s" key body))))))
    (let ((slot (prop :slot))
          (test (prop :test :default-value '#'eql))
          (changing-trigger (prop :changing-trigger :default-value nil))
          (changed-trigger (prop :changed-trigger :default-value nil))
          (midform (prop :midform :default-value nil))
          (preform (prop :preform :default-value nil))
          (postform (prop :postform :default-value nil))
          (doc (assoc :documentation body))
          (v (gensym))
          (v0 (gensym)))
    `(progn
       (defgeneric ,accessor (,name)
         ,@(if doc (list doc)))
       (defgeneric (setf ,accessor) (,v ,name)
         ,@(if doc (list doc)))
       (defmethod ,accessor ((,name ,type))
         (slot-value ,name ,slot))
       (defmethod (setf ,accessor) (,v (,name ,type))
         (unless (funcall ,test (slot-value ,name ,slot) ,v)
           ,@(if preform (list preform))
           ,@(if changing-trigger (list `(,changing-trigger ,name)))
           (let ((,v0 (slot-value ,name ,slot)))
             (declare (ignorable ,v0))
             (setf (slot-value ,name ,slot) ,v)
             ,@(if midform (list `(funcall ,midform ,v0 ,v))))
           ,@(if changed-trigger (list `(,changed-trigger ,name)))
           ,@(if postform (list postform)))
         (slot-value ,name ,slot))))))
