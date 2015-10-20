;Exercicio 1

; 1.1 Decimal->outra

(define decimal->outra
  (lambda (num)
    (cond ((= num 10) (display "a"))
          ((= num 11) (display "b"))
          ((= num 12) (display "c"))
          ((= num 13) (display "d"))
          ((= num 14) (display "e"))
          ((= num 15) (display "f"))
          ((= num 16) (display "g"))
          ((= num 17) (display "h"))
          ((= num 18) (display "i"))
          ((= num 19) (display "j"))
          ((>= num 20) (display "?"))
          (else (display num)))))

; 1.2

(define imprime-na-base
  (lambda (num base)
    (cond ((< num base) (display num))
          (else
           (imprime-na-base (quotient num base) base)
           (decimal->outra (remainder num base))))))

;Exercio 2

(define interest-rate
  (lambda (n)
    (cond ((< n 1)(display "erro"))
          ((= n 1)(display "0.02"))
          ((= n 2)(display "0.024"))
          ((= n 3)(display "0.032"))
          ((>= n 4)(display "0.038")))))

;Exercicio 3



