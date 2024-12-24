(defsystem "cl-action/wvk"
  :author "Jacob Olson"
  :version "0.9.0"
  :license "MIT"
  :description "A collection of virtual-key parameters for use with cl-action on Windows"
  :components ((:module "windows-virtual-keys"
                :components ((:file "windows-virtual-keys")))))

(defsystem "cl-action"
  :author "Jacob Olson"
  :version "0.6.0"
  :license "MIT"
  :description "A library for the automation of mouse and keyboard actions, written in Common Lisp"
  :depends-on ("cl-action/wvk" "cffi")
  :components ((:file "cl-action")))
