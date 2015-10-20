(require (lib "audio.scm" "user-feup"))
;(load "falante.scm")

; contagem decrescente
(define bomba
  (lambda (contagem)
    (if (negative? contagem)
        (display "Numero invalido.")
        (if (zero? contagem)
             (som "bomba1") ; caso base
             (begin
               (diz-numero contagem)
               (bomba (- contagem 1))))))) ; caso geral
; operação de redução: (- contagem 1)
    
; conta digitos de um numero
(define conta-digitos
  (lambda (numero)
    (if (zero? numero)
        0
        (add1 (conta-digitos (quotient numero 10))))))
               
; progressao aritmetica
(define pa
  (lambda (a b razao)
    (if (<= a b)
        (begin
          (display a)
          (display " ")
          (pa (+ a razao) b razao)))))