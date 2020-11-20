
(in-package :web-utils)

;; Also, units could be a pain, so...
(defun px (n)
  (s "~Apx" (if (numberp n)
                (floor n)
                n)))

;; (with-html-output (*standard-output*) (html (:p :style (css (list :font-weight "bold" :background-color "green" :padding (px 10))))))






;; !!! This is used to specialise colors to make CSS descriptions
;; That should probably be redone with the presentation system
;; ALSO, I don't know if this has ever been used, but it might have
(defmethod css-value-description ((self t))
  (s "~A" self))

;; I'm getting fed up of interpolating stuff for CSS...
(defun css-string (options)
  (apply #'concatenate (cons 'string (loop for (label value) on options by #'cddr
                                        unless (consp value) ; ignore lists in style lists - we can use these for other things...
                                        collect (s "~A: ~A; " (string-downcase (symbol-name label))
                                                   (css-value-description value))))))



(defun css (&rest options)
  ;; as a convenicence (not really that useful but still) you can either pass a list of CSS options or multiple parameters
  (if (and (first options)
           (listp (first options)))
      (apply #'css (first options))
      (html (:attribute (:print (css-string options))))))

;; (with-html-output (*standard-output*) (html (:p :style (css (list :font-weight "bold" :background-color "green")))))
;; (with-html-output (*standard-output*) (html (:p :style (css :font-weight "bold" :background-color "green"))))




;; then we can use colors directly in css. Handy. Doesn't matter what format they're in either.
;; once I've got ADTs loading properly on startup (which might require fixing certain issues first) I'll put this into the ajax.lisp

;; I guess I could do with an hsva color type too. Although it would be trivial enough to just make a function to set the alpha of a colour



;; to allow some flexibility in what can be used as query parameters - not just strings, but hierarchical lists - I implement this function here...
;; The effect of this is to flatten the list and then concatenate the items with __ between them.
;; By flattening the list here we can just carelessly use (list ...) to accumulate hierarchical names
(defun query-parameter-name (name)
  (regex-replace-all "\\s"
                     (if (listp name)
                         (string-list (mapcar (lambda (x)
                                                (if (listp x)
                                                    (query-parameter-name x)
                                                    (s "~A" x))) name)
                                      "__")
                         (query-parameter-name (list name)))
                     "___"))

;; (query-parameter-name '((a b c) "one" "two" "three"))
;; (query-parameter-name :foo)

;; This is useful for outputting bits of HTML where we want to suppress nil attributes. It's a shame the HTML generator library doesn't do that itself, although this won't be a lot slower than doing that, since by the time you have to do things like that you lose a lot of ability to optimize HTML output anyway - we have to defer things until runtime.
;; This could be optimized slightly...
(defun <> (tag-name &rest content)
  (let ((tag (string-downcase (symbol-name tag-name))))
    (flet ((out (string)
             (write-sequence string com.gigamonkeys.html::*html-output*)))
      ;; (write-sequence "<" com.gigamonkeys.html::*html-output*)
      (when tag-name
        (out "<")
        (out tag))

      ;; first look for attributes...
      (labels ((output-attribute (name value)
                 (awhen (and tag-name value)
                   (out " ")
                   (out (string-downcase (symbol-name name)))
                   (out "='")
                   (out (com.gigamonkeys.html::escape (princ-to-string it) "<>&\"'"))
                   (out "'")))
               (f (content)
                 (cond ((keywordp (first content))
                        (output-attribute (first content) (cond ((and (member (first content) '(:name :id))
                                                                      (second content))
                                                                 ;; !!! This understands our id generation from hierarchical names
                                                                 (query-parameter-name (second content)))
                                                                ((eq (first content) :style)
                                                                 (if (or (stringp (second content))
                                                                         (not (second content)))
                                                                     (second content)
                                                                     (css-string (second content))))
                                                                (t (second content))))
                        (f (cddr content)))

                       ((and (not content) tag-name)
                        (out "/>"))
                       
                       ;; a list in the content whilst we're still writing attributes is assumed to be a list of attributes
                       ;; this sort of thing could be particularly useful for outputting event handlers from one function - rather than having to apply <> to lists with (apply ...)
                       ((listp (first content))
                        (loop for (name value) on (first content) by #'cddr
                             do (output-attribute name value))
                        (f (cdr content)))
                       
                       (t
                        (when tag-name (out ">"))
                        (mapcar (@ if (functionp ?1)
                                      (funcall ?1)
                                      (html ?1))
                                content)
                        (when tag-name
                          (out "</")
                          (out tag)
                          (out ">"))))))
        (f content))
      (out (string #\Newline)))))

;; (<> :element :attribute "Value" (some-more-attributes) "Content" (dhtml (:p "More content")))






;; This is a bit more sophisticated - a window. I should make it possible to gray out the background, but I don't know how to do that yet.
;; It's easy to make this look nice in Firefox and Safari. IE is, as ever, harder
;; This needs to be made better
(defparameter *js-window-next-id* 0)

(defun js-current-window-id ()
  (s "win_~A" (abs *js-window-next-id*)))


(defun js-next-window-id ()
  (setf *js-window-next-id*
        (1+ (abs *js-window-next-id*)))
  ;; I can do this to stop them getting too big...
  (when (> *js-window-next-id* 100000)
    (decf *js-window-next-id*
          100000))
  (js-current-window-id))


;; This can be used directly in an attribute without the annoying (:print) - it spits out HTML, properly escaped
(defun js (form)
  (html (:attribute (:print (parenscript:ps1* form)))))

(defun js-string (form)
  (parenscript:ps1* form))


(defun output-js-array (items)
  (html "[")
  (loop for (x . rest) on items
     do
       (if (listp x)
           (output-js-array x)
           (html (:noescape (:print (first (parenscript::parenscript-print x))))))
     when rest
     do (html ", "))
  (html "]"))

;; (output-js-array '("one" "two" "three" 4 5 6))
;; (output-js-array '(("one" "two" "three") "two" "three" 4 5 6))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These should probably be deprecated now - we can just rely on CSS transitions, which should generally work better
;; I could redo these to use those, but I don't know how much I've used this. It works well though.


;; Animated transitions are helpful affordances for letting the user
;; see what has happened, in addition to looking nice and fancy. I'm
;; using glow here, since it's easy, cross platform and we have
;; it. I'm wrapping things to simplify and in case I decide to change
;; how it works.

;; Safari does have built in animation ability, but Glow is simple and
;; works everywhere.

(defun js-animate-duration (n)
  `(if (= event.shift-key 1) 2 ,n))

;; Useful JS function to fade an element out
;; very simple to use animation function
;; NOTE - now I'm just showing off. Slow motion using the shift key!
(defun js-animate (element-id &key (duration (js-animate-duration 0.2)) on-complete (transition 'glow.anim.fade-out))
  `((lambda (event)
      (,transition ,(if (scan "^\\." element-id)
                        element-id
                        (s "#~A" element-id)) ,duration ,@(when on-complete
                                                           `(:on-complete (lambda ()
                                                                            ,on-complete)))))
    event))

;; (js-animate "foo" :on-complete '(alert "done"))
;; (js-animate "foo")


(defparameter *test-session* nil)

(defmethod session-datum (name)
  (if *test-session*
      (gethash name *test-session*)
      (session:get-session-datum http-response::*session* name)))

(defmethod (setf session-datum) (obj (name t))
  (session:set-session-datum http-response::*session* name obj))

(defun clear-session (&optional (session (session:get-session http-response::*session*)))
  (awhen session 
    (loop for k in (hash-keys (cdr it))
       unless (member k '(:session-id :site))
       do (remhash k (cdr it)))))


;; quick slurping
;; returns nil if the file does not exist



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better query url like things. The idea is to be able to write things like:-

;; (page-call page-name :param value :param value...)

(defun href-fn (name &rest params)
  (let ((name (if (symbolp name)
                  (concatenate 'string "/" (string-downcase (symbol-name name)))
                  name)))
    (if params
        (format nil "~A?~A" name
                (with-output-to-string (s)
                  (loop for (param value . rest) on params by #'cddr
                       when value
                     do (format s "~A=~A" (if (symbolp param)
                                              (string-downcase (symbol-name param))
                                              param)
                                (net.aserve::encode-form-urlencoded value))
                     when (and rest value)
                     do (format s "&"))))
        name)))

;; (href-fn 'foo :a 12 :query "Hello")

;; this is just to make the syntax slightly more convenient
(defmacro href (name &rest params)
  `(href-fn ',name ,@(loop for (this . rest) on params
                          collect this
                          when (and (or (not rest)
                                        (keywordp (first rest)))
                                    (keywordp this))
                          collect (intern (symbol-name this)))))


;; return an identifier for an object. For any writable object the identifier will always be the same
;; This is a good way of generating identifiers for things as long as we can somehow name the thing. It will be ideal for a (subject predicate) pair for example
;; It doesn't require storing any data in lookup tables or anything either. They are guaranteed to be valid XML/HTML/SGML IDs - nothing dodgy here.
;; *** NB - the identifiers will only be unique if the objects are unique.
(defun js-identifier (object)
  (if (symbolp object)
      (symbol-name object) ; don't bother hashing these - they'll be fine as IDs
      (concatenate 'string "o_"
                   (ironclad:byte-array-to-hex-string
                    (ironclad:digest-sequence :sha1
                                              (trivial-utf-8:string-to-utf-8-bytes (write-to-string object)))))))

;; (js-identifier '("one" "two"))
;; (js-identifier (list (abel "generated-identifier/2424") (rdf "type")))
;; (js-identifier :foo)

;; and it's friend...
(defun js-id (object)
  (html (:attribute (:print (js-identifier object)))))


(defun one-click-link (href content &key
                                      (load-message nil))
  "Outputs an <A> element which only responds to the first click, and immediately changes its content to a loading message and spinner.
Repeated clicking on it will do nothing.
"
  (html (:a :href href
            :onclick (:print (if load-message
                                 (ps:ps* `(progn (oc this)
                                                 (setf this.inner-text ,load-message)))
                                 "oc(this)"))
            
            (cond ((stringp content)
                   (html content))
                  ((functionp content)
                   (funcall content))
                  (t (error "invalid link content ~A" content))))))
