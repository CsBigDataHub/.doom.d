;; -*- no-byte-compile: t; -*-
;;; private/mydired/packages.el

(package! dired-du
  :pin "3d17477e6a885d2a6c2bfc5ffdde7cd6f92e5cef"
  :recipe (:host github :repo "calancha/dired-du")
  )

(package! dired-hacks-utils
  :pin "7c0ef09d57a80068a11edc74c3568e5ead5cc15a"
  :recipe (:host github :repo "Fuco1/dired-hacks"))
