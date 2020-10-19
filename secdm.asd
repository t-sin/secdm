(defsystem :secdm
  :description "SECD machine toy"
  :version "0.2.0"
  :author "Shinichi Tanaka <shinichi.tanaka45@gmail.com>"
  :components ((:file "vm")
               (:file "compile")
               (:file "secdm" :depends-on ("vm" "compile"))))
