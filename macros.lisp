
(in-package :web-utils)


;; Useful utility macro to avoid repetetive code...
(defmacro dhtml (&body content)
  `(lambda ()
     (html ,@content)))




(defmacro with-ids (ids &body content)
  `(let ,(mapcar (lambda (id)
                   `(,id (js-next-window-id)))
          ids)
     ,@content))


(defmacro defun/m-s (name arg-list &body body)
  "Like defun but defines a memoizing function"
  (dolist (a '(&rest &body &key))
    (when (member a arg-list)
      (error "I haven't implemented memoized ~A parameters yet." a)))


  (let ((memo (gensym))
        (memoized-value (gensym "value"))
        (memoized-value-found (gensym "found")))
    `(defun ,name ,arg-list
       (unless (session-datum :memo)
         (setf (session-datum :memo) (make-hash-table)))
       (unless (gethash ',name (session-datum :memo))
         (setf (gethash ',name (session-datum :memo))
               (make-hash-table :test 'equal)))
       ;; then we have to check for memoized answer...
       (let ((,memo (gethash ',name (session-datum :memo))))
         (multiple-value-bind (,memoized-value ,memoized-value-found)
             (gethash (list ,@arg-list)
                      ,memo)
           (if ,memoized-value-found
               ,memoized-value
               (setf (gethash (list ,@arg-list)
                              ,memo)
                     (block ,name
                       ,@body))))))))
