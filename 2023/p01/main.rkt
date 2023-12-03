#!/usr/bin/env racket
#lang typed/racket/base
(require racket/file)
(require racket/list)
(require racket/string)

(: words (Listof String))
(define words '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(: only-nums (-> String (Listof Char)))
(define (only-nums xs)
  (let* ([letters (string->list xs)]
         [digits (filter char-numeric? letters)]
         [worded : (Listof String) (filter (lambda ([w : String]) (string-contains? xs w)) words)]
         [words : (Listof Char) (map (lambda w (index-of words w)) worded)])
    (append digits words)))

(: concat (-> (Listof Char) Number))
(define (concat xs) 
  (let* ([fst (first xs)]
         [snd (last xs)]
         [both (string fst snd)]
         [parsed (string->number both 10)]
         [res (if (number? parsed) parsed (error "Wtf breh"))])
    res))

(: run Void)
(define run
  (let* ([lines (file->lines "in")]
        [digits (map only-nums lines)]
        [parsed (map concat digits)]
        [res (foldl + 0 parsed)])
    (println res)))

run
