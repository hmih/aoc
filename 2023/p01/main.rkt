#!/usr/bin/env racket
#lang typed/racket/base
(require racket/file)
(require racket/string)
(require racket/match)
(require racket/list)

(: words (Listof String))
(define words '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(: digits (Listof String))
(define digits '("1" "2" "3" "4" "5" "6" "7" "8" "9"))

(: extract (-> String (Listof String)))
(define (extract s)
  (define has-word (filter (lambda ([w : String]) (string-prefix? s w)) words))
  (define has-digit (filter (lambda ([d : String]) (string-prefix? s d)) digits))
  (assert (<= (length has-word) 1))
  (assert (<= (length has-digit) 1))
  (define symbol
    (match (list has-word has-digit)
      ['(() ()) "z"]
      [(list (list w) '()) w]
      [(list '() (list d)) d]
      [_ (error "err: " (list has-word has-digit) "on: " s)]))
  (define truncated (substring s (string-length symbol)))
  (define parsed
    (if (member symbol words)
        (number->string (+ 1 (assert (index-of words symbol))))
        (if (member symbol digits) symbol #f)))
  (append 
    (if parsed (list parsed) '()) 
    (if (non-empty-string? truncated) (extract truncated) '())))

(: run (-> Void))
(define (run)
  (define lines (file->lines "in"))
  (define nums (map extract lines))
  (define res
    (for/fold ([acc : Integer 0])
              ([ns nums])
      (define combined (string-append (first ns) (last ns)))
      (define num (assert (string->number combined) exact-integer?))
      (+ acc num)))
  (println res)
  (println "done!"))

(run)
