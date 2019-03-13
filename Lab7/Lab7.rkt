#lang racket

;#1
(define member?
  (lambda (x L)
    (if (null? L)
        (not #t)
        (begin
          (if (= x (car L))
              (not #f)
              (begin
                (member? x (cdr L))))))))

;2
(define subset?
  (lambda (L1 L2)
    (if (null? L1)
         (not #f)
         (begin
    (if (not(member? (car L1) L2))
        (not #t)
        (begin
          (subset? (cdr L1) L2)))))))

;3
(define set-equal?
  (lambda (L1 L2)
    (if (and (subset? L1 L2) (subset? L2 L1))
        (not #f)
        (begin
          (not #t)))))

;4

;Union
(define union
  (lambda (L1 L2)
    (if (null? L1)
        (car (list L2))
        (begin
          (if (member? (car L1) L2)
              (union (cdr L1) L2)
              (begin
                (union (cdr L1) (cons (car L1) L2))))))))

;Intersect
(define intersect
  (lambda (L1 L2)
    ;Base: if L1 end up empty we've gone through the list
    (if (null? L1)
        '()
        (begin
          ;If the first member of L1 is in L2, and also in L1 then its in the intersect
          (if (member? (car L1) L2)
              ;Start building a list with it
              (cons (car L1) (intersect (cdr L1) L2))
              (begin
                ;Otherwise ignore it and move on
                (intersect (cdr L1) L2)))))))


;difference
(define difference
  (lambda (L1 L2)
    (if (null? L1)
        L1
        (begin
    (if (null? L2)
        L1
        (begin
          (if (member (car L2) L1)
              (difference (remove (car L2) L1) (cdr L2))
              (begin
                (difference L1 (cdr L2))))))))))
; get-input-file, return an input port
(define (get-input-file)
    (display "Input file name?")
    (display #\newline)
    (open-input-file (read-line))
    )
; get-output-file, returns an output port
(define (get-output-file)
    (display "Output file name?")
    (display #\newline)
    (open-output-file (read-line) #:exists 'replace)
    )

(define (Symmetric-Difference)
  (define in (get-input-file))
  (define out (get-output-file))
  (define L1 (read in))
  (define L2 (read in))
  (close-input-port in)
  (display (difference (union L1 L2) (intersect L1 L2)) out)
  (displayln "Calculated... Sent to output file!")
  (close-output-port out))
  