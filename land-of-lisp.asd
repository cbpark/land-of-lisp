(in-package :cl-user)
(defpackage land-of-lisp-asd
  (:use :cl :asdf))
(in-package :land-of-lisp-asd)

(defsystem land-of-lisp
  :version "0.1"
  :author "Chan Beom Park"
  :license "BSD"
  :description "Codes from Land of Lisp"
  :long-description ""
  :depends-on ()
  :components ((:module "chapter02"
                        :components
                        ((:file "guess-my-number")))
               (:module "chapter05"
                        :components
                        ((:file "wizards-adventure-game")))
               (:module "chapter08"
                        :components
                        ((:file "graph-util")
                         (:file "grand-theft-wumpus" :depends-on ("graph-util"))))
               (:module "chapter09"
                        :components
                        ((:file "orc-battle-game")))
               (:module "chapter10"
                        :components
                        ((:file "simulated-evolution")))
               (:module "chapter15"
                        :components
                        ((:file "dice-of-doom-v1")))))
