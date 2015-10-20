; resolucao exercicio 1

(define visu-fraccao
  (lambda (n d)
    (display "frac: "); representa a string frac: incluindo o espaço
    (display n)
    (display "/")
    (display d)))

; resolucao exercicio 2

(define reduz
  (lambda (n d)
    (visu-fraccao
    (/ n (gcd n d)) ;procedimento gcd para encontrar maior divisor comum entre n e d
    (/ d (gcd n d)))))

; resolucao exercicio 3


(define multiplicar-fraccoes
  (lambda (n1 d1 n2 d2)
    (reduz (* n1 n2) (* d1 d2))))

; resolucao exercicio 4

(define dividir-fraccoes
  (lambda (n1 d1 n2 d2)
    (multiplicar-fraccoes n1 d1 d2 n2)))