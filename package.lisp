;;;; package.lisp

(defpackage #:bayes
  (:use #:cl #:let-over-lambda #:iterate)
  (:export :sampler
   :nested-sampling
   :z
   :posterior-samples
   :lrps
   :avg
   :find-index))
