#|
 This file is a part of aoc-2022
 (c) 2022 Janne Pakarinen (gingeralesy@gmail.com)
 Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:cl-user)
(asdf:defsystem aoc-2022
  :version "0.0.0"
  :license "zlib"
  :author "Janne Pakarinen <gingeralesy@gmail.com>"
  :maintainer "Janne Pakarinen <gingeralesy@gmail.com>"
  :description "Advent of Code 2022"
  :serial T
  :components ((:file "package")
               (:file "input")
               (:file "day1")
               (:file "day2"))
  :depends-on (:cl-ppcre))
