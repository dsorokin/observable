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

(defpackage :observable
  (:use :cl)
  (:export
   #:event-push
   #:event-push-handler
   #:event-delete-handler
   #:event-source
   #:make-event-source
   #:trigger-event
   #:publish-event
   #:dispose
   #:disposable
   #:with-disposable
   #:observable
   #:observable-subscribe
   #:event->observable
   #:observable-map
   #:observable-remove-if-not
   #:observable-merge
   #:deftrigger
   #:defevent
   #:defaccessor))

(in-package :observable)

(defstruct event-source
  "Reprensents the event source."
  (handlers nil))

(defstruct event
  "The event created with help of TRIGGER-EVENT."
  source)

(defun event-push (function event)
  "Add the function of one argument to handle the event messages."
  (event-push-handler (lambda (sender &rest args) 
                        (declare (ignore sender))
                        (apply function args))
		      event))

(defun event-push-handler (handler event)
  "Add the event handler which must be a function of
two arguments: a sender and message."
  (let ((source (event-source event)))
    (push handler (event-source-handlers source))))

(defun event-delete-handler (handler event)
  "Delete the event handler which must be a function of
two arguments: a sender and message."
  (let ((source (event-source event)))
    (setf (event-source-handlers source)
	  (delete handler (event-source-handlers source)))))

(defun trigger-event (event-source sender &rest args)
  "Trigger the event associated with the specified source and 
pass in the sender and message to every handler."
  (loop for handler in (event-source-handlers event-source)
        do (apply handler sender args)))

(defun publish-event (event-source)
  "Publish and return an event associated with the specified source."
  (make-event :source event-source))

(defgeneric dispose (disposable)
  (:documentation "Dispose the resources."))

(defmacro with-disposable ((var expr) &body body)
  "Create and then dispose the resource after application of the body."
  `(let ((,var ,expr))
     (unwind-protect
         (progn ,@body)
       (dispose ,var))))

(defgeneric observable-subscribe (function observable)
  (:documentation "Subscribe the function to the observable object and 
return a disposable one. The function must be of one argument."))

(defclass disposable () 
  ()
  (:documentation "An object which resources should be disposed."))

(defclass observable () 
  ()
  (:documentation "An observable object."))

(defclass standard-disposable (disposable)
  ((dispose
    :initarg :dispose
    :initform (error "Must supply a dispose function.")))
  (:documentation "The standard version of the disposable object."))

(defclass standard-observable (observable)
  ((subscribe
    :initarg :subscribe
    :initform (error "Must supply a subscribe function.")))
  (:documentation "The standard version of the observable object."))

(defmethod dispose ((disposable standard-disposable))
  (with-slots (dispose) disposable
    (funcall dispose)))

(defmethod observable-subscribe (function (observable standard-observable))
    (with-slots (subscribe) observable
      (funcall subscribe function)))

(defun event->observable (event)
  "Convert the event to an observable object."
  (flet ((subscribe (function)
	   (let ((handler (lambda (sender &rest args)
			    (declare (ignore sender))
			    (apply function args))))
             (event-push-handler handler event)
             (flet ((dispose () 
                      (event-delete-handler handler event)))
               (make-instance 'standard-disposable :dispose #'dispose)))))
    (make-instance 'standard-observable :subscribe #'subscribe)))

(defun observable-map (function observable)
  "Apply the function to the arguments. It returns
a disposable object."
  (flet ((subscribe (function2)
           (observable-subscribe 
            (lambda (&rest args) 
              (funcall function2 (apply function args)))
	    observable)))
    (make-instance 'standard-observable :subscribe #'subscribe)))

(defun observable-remove-if-not (predicate observable)
  "Filter the messages. It returns a disposable object."
  (flet ((subscribe (function)
           (observable-subscribe
            (lambda (&rest args)
              (when (apply predicate args)
                (apply function args)))
	    observable)))
    (make-instance 'standard-observable :subscribe #'subscribe)))

(defun observable-merge (&rest observables)
  "Merge the messages from the specified observable objects. 
It returns a disposable object."
  (flet ((subscribe (function)
	   (let ((disposables
		  (loop for observable in observables
		     collect (observable-subscribe function observable))))
	     (flet ((dispose ()
		      (loop for disposable in disposables
			   do (dispose disposable))))
	       (make-instance 'standard-disposable :dispose #'dispose)))))
    (make-instance 'standard-observable :subscribe #'subscribe)))
