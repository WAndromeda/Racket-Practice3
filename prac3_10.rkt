#lang racket

(define (print_list list_t name)
  (cond [(= (length list_t) 0)
      (printf "~a - ПУСТ" name)]
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
    (cond [(= (random 2) 0)
            (set! list_temp (append list_temp (list (integer->char (+ 97 (random upper_bound))))))
          ]
          [else
           (set! list_temp (append list_temp (list (- (random upper_bound) (random lower_bound)))))
           ]
    )
  )
  (cons list_temp ' ())
)

(define (apply_rule list_t)
  (cond [(and
            (char? (car list_t))  
            (char? (list-ref list_t (-(length list_t) 1)))
          )
           (list (car list_t) (list-ref list_t (-(length list_t) 1)) )
         ]
        [else
          (append (list (car list_t)) (list-tail list_t 2))
         ]
  )
)


(printf "НИКОЛАЕВ НИКИТА СЕРГЕЕВИЧ - ИКБО-13-17 - ПРАКТИКА 3 - ВАРИАНТ 10\n")
(define list1 (first (generate_coordinates 10 1 26)))
(print_list list1 "ЛИСТ")
(apply_rule list1)
