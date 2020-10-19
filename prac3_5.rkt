#lang racket

(define (print_list list_t name)
  (cond [(= (length list_t) 0)
      (printf "~a - НЕ ЗАДАНЫ" name)]
      [else
         (printf "~a: (" name)
         (for-each (lambda (x)(printf "~a " x)) list_t)
         (printf ")\n")
       ]
  )
)

(define (generate_coordinates amount lower_bound upper_bound)
  (define list_temp null)
  (for ([i (in-range amount)])
    (set! list_temp (append list_temp (list (- (random upper_bound) (random lower_bound)))))
   )
  (cons list_temp ' ())
)

(define (distance coord1 coord2)
  (define cat (abs(-(list-ref coord2 1) (list-ref coord1 1))))
  (define cat2 (abs(-(car coord2) (car coord1))))
  (printf "ДИСТАНЦИЯ: ~a" (sqrt(+ (* cat cat) (* cat2 cat2) )))
)


(printf "НИКОЛАЕВ НИКИТА СЕРГЕЕВИЧ - ИКБО-13-17 - ПРАКТИКА 3 - ВАРИАНТ 5\n")
(define list1 (first (generate_coordinates 2 100 100)))
(print_list list1 "X1 Y1")
(define list2 (first (generate_coordinates 2 100 100)))
(print_list list2 "X2 Y2")
(distance list1 list2)