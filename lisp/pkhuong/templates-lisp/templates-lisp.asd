;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem templates-lisp
  :name "templates-lisp"
  :version "0.1.0"
  :maintainer "Paul-Virak Khuong"
  :author "Paul-Virak Khuong"
  :licence "BSD sans advertising clause (see file COPYING for details)"
  :description "Small lisp in C++ templates"
  :long-description "Compiles a sexpy representation of the lambda calculus to C++ templates"
  :serial t
  :components ((:file "packages")
	       (:file "TL-types")
	       (:file "TL-Front")
	       (:file "TL-Back")))