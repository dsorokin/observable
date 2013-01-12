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
  (:documentation ...))  ; optional
"
  `(progn
     (defgeneric ,name (,sender ,@args)
       ,@(let ((doc (assoc :documentation specs)))
           (if doc (list doc))))
     (defmethod ,name ((,sender ,type) ,@args)
       ,@(if args `((declare (ignore ,@args))))
       (values))
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
       (slot-value ,sender ,(cadr (assoc :slot specs))))))
