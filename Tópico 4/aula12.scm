; Complexidade temporal: O(n)
; Complexidade espacial: O(n)
(define codigo
  (lambda (numero)
    (letrec ((dms
              (lambda (n)
                (remainder n 10)))
             (d2ms
              (lambda (n)
                (dms (quotient n 10)))))
      (if (<= numero 10)
          0
          (+ (* (dms numero) (d2ms numero))
             (codigo (quotient numero 100)))))))
  
; Complexidade temporal: O(n)
; Complexidade espacial: O(1)
(define codigo-iter
  (lambda (numero)
    (letrec ((dms
              (lambda (n)
                (remainder n 10)))
             (d2ms
              (lambda (n)
                (dms (quotient n 10))))
             (aux
              (lambda (n soma)
                (if (<= n 10)
                    soma
                    (aux (quotient n 100)
                         (+ soma (* (dms n) (d2ms n))))))))
      (aux numero 0))))

; verifica se e multiplo de 3
(define mult3?
  (lambda (n)
    (case n
      ((3 6 9) #t)
      (else #f))))

; cria numero multiplo de 3
(define cria-numero-mult3
  (lambda (numero)
    (letrec ((dms
              (lambda (n)
                (remainder n 10))))
      (if (zero? numero)
          0
          (if (mult3? (dms numero))
              (+ (dms numero)
                 (* 10 (cria-numero-mult3 (quotient numero 10))))
              (cria-numero-mult3 (quotient numero 10)))))))
  