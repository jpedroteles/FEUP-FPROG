; Fundamentos da Programação
; Auto teste AT05

; -------------------------------------
; pergunta 1 
; -------------------------------------
(define imprime-na-base
  (lambda (num base)
    (cond ((< num base) (display num))
          (else
           (imprime-na-base (quotient num base) base)
           (display (remainder num base))))))

(define decimal->outra
  (lambda (num)
    (cond ((< num 10) num)
          ((= num 10) "a")
          ((= num 11) "b")
          ((= num 12) "c")
          ((= num 13) "d")
          ((= num 14) "e")
          ((= num 15) "f")
          ((= num 16) "g")
          ((= num 17) "h")
          ((= num 18) "i")
          ((= num 19) "j")
          (else "?"))))



(define imprime-ate-base-20
  (lambda (num base)
    (cond ((< num base) (display (decimal->outra num)))
          (else
           (imprime-ate-base-20 (quotient num base) base)
           (display (decimal->outra (remainder num base)))))))


; -------------------------------------
; pergunta 2
; -------------------------------------

 (define interest-rate
    (lambda (n) 
      (cond ((= n 1) 0.02)
            ((= n 2) 0.024)
            ((= n 3) 0.032)
            (else 0.038))))

; -------------------------------------
; pergunta 3
; -------------------------------------

  (define accumulate
    (lambda (amount n-years)
      (if (or (<= amount 0) (<= n-years 0))
          (display "invalid parameter(s)")
          (/ (round (* 100 (accumulate-aux amount n-years 1))) 100)))) 
  
  (define accumulate-aux
    (lambda (amount n-years year)
      (if (> year n-years)
          amount
          (accumulate-aux (* amount (+ 1 (interest-rate year))) n-years (add1 year)))))
