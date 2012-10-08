#lang racket

; timestamp \t tie value
(define (parse-tie-row row)
  (map string->number
       (list-tail (regexp-match #rx"^(.+)\t(.+)$" row) 1)))

(define data
  (call-with-input-file "example_data.dat"
    (lambda (in) (map parse-tie-row (port->lines in)))))

(define (mtie-window data window-size current-mtie)
  (define-values (data-new peak-to-peak) 
    (find-peaks data window-size))
  ; check if this iteration is the last one
  (if (= (length data) window-size)
      (max current-mtie peak-to-peak)
      (mtie-window data-new window-size (max current-mtie peak-to-peak))))

; find peak-to-peak value of a window starting from the beginning of data
; return that and data cropped from left according to extreme fix logic
(define (find-peaks data window-size)
  (define window-data (take data window-size))
  (define xmin (argmin (lambda (x) x) window-data))
  (define xmax (argmax (lambda (x) x) window-data))
  
  ; crop data that has no use anymore from the beginning
  (define minlist (member xmin data))
  (define maxlist (member xmax data))
  ; return the larger dataset so that any points won't be missed
  (define retlist (if (> (length minlist) (length maxlist) )
                      minlist maxlist))
  
  (define ret-data  
    (cond 
      ; last iteration has to have window-size samples
      [(< (length retlist) window-size)
       (list-tail data (- (length data) window-size))]
      ; move forward by one if it would otherwise be the same
      ; i.e. if peak is was at the first point
      [(= (length retlist) (length data))
       (rest retlist)]
      [else retlist]))
  
  (values ret-data (- xmax xmin)))

(define (print-mtie mtie-data sampling-interval)
  (map (lambda (mtie-pair) 
         (display (* (list-ref mtie-pair 0) sampling-interval))
         (display ";")
         (display (* (list-ref mtie-pair 1) 1e9))
         (display "\n"))
       mtie-data)
  1)

(define tie-data (map second data))
(define observation-window-sizes 
  (append
   (build-list 
    (inexact->exact (floor (/ (log (length tie-data)) (log 2))))
    (lambda (x) (expt 2 (+ x 1))))
   (list (length tie-data))))

(define mtie (map (lambda (window-size)
                    (list window-size (mtie-window tie-data window-size 0.0)))
                  observation-window-sizes))

(print-mtie mtie 0.033)
