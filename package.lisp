

(in-package :cl-user)

(defpackage :web-utils
  (:use :cl :vip-utils :net.aserve :com.gigamonkeys.html :anaphors :cl-ppcre)
  (:export
   ;; quite a few things...
   #:css-value-description
   #:css-string
   #:query-parameter-name
   #:<>
   #:px
   #:css

   #:dhtml

   ;; It's a shame to have to export the parameter, but it's useful to bind it sometimes
   #:*js-window-next-id*
   #:js-next-window-id
   #:js-current-window-id
   #:with-ids
   #:js
   #:js-string
   #:output-js-array ; efficiently!

   #:js-animate-duration
   #:js-animate

   #:session-datum
   #:clear-session

   #:href-fn
   #:href

   #:js-identifier
   #:js-id

   #:defun/m-s

   #:one-click-link
   ))
