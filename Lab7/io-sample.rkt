#lang racket
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
;copy the content of the specified input file to the specified output
(define (file-copy)
  (define in (get-input-file))
  (define out (get-output-file))
  (let f ((x (read-line in)))
    (cond ((eof-object? x)(close-input-port in))
         (else (display x out)
               (display #\newline out)
               (f (read-line in)))))
  (display "Copying is done.")
  (close-output-port out))

(file-copy)