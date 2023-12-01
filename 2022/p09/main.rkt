#lang racket/base

(require racket/file)

(define run
  (lambda ()
    (print (file->lines "in"))))

(run)
