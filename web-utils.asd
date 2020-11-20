

(asdf:defsystem :web-utils
  :description "Various useful web utilities"
  :author "VIP"
  :serial t
  :depends-on ("vip-utils" "aserve" "html" "session"
                           "ironclad"
                           "http-response"
                           "anaphors" "parenscript"
                           "cl-ppcre")
  
  :components ((:file "package")
               (:file "macros")
               (:file "functions")
               )
  )
