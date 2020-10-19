#lang racket

(define (print_list list_t name)
  (cond [(= (length list_t) 0)
      (printf "~a - ПУСТ" name)]
      [else
         (printf "~a: " name)
         (for-each (lambda (x)(printf "~a " x)) list_t)
         (printf "\n")
       ]
  )
)

(define (generate_list amount lower_bound upper_bound)
  (define list_temp null)
  (for ([i (in-range amount)])
    (set! list_temp (append list_temp (list (- (random upper_bound) (random lower_bound)))))
   )
  (cons list_temp ' ())
)

(define (is_natural list1 list2)
  (cond [(>= (first list1) 0)
       (printf "ВОЗВРАЩАЕМ ВТОРОЙ СПИСОК\n")
       (first (cons list2 ' ()))]
      [else
        (printf "ВОЗВРАЩАЕМ СПИСОК, СОСТАВЛЕННЫЙ ИЗ ГОЛОВЫ ВТОРОГО И ХВОСТА ПЕРВОГО\n")
        (append (list (car list2)) (list (list-ref list1 (- (length list1) 1))))
       ]
)
)

(printf "НИКОЛАЕВ НИКИТА СЕРГЕЕВИЧ - ИКБО-13-17 - ПРАКТИКА 3 - ВАРИАНТ 1\n")
(define list1 (first (generate_list 10 10 12)))
(print_list list1 "ЛИСТ 1")
(define list2 (first (generate_list 10 10 12)))
(print_list list2 "ЛИСТ 2")
(is_natural list1 list2)