(define iva 0.23)

; calcula area de um triangulo
(define area-triangulo
  (lambda (base altura)
    (/ (* base altura) 2)))

; ajusta a temperatura 
(define ajusta-temperatura
  (lambda (temperatura)
    (if (> temperatura 20)
        (display "Ligar o ar condicionado"))))

; calculo da frequencia
(define frequencia
  (lambda (aulas faltas)
    (if (> faltas (ceiling (* 0.25 aulas)))
        (begin
          (display "Sem frequencia") ; se verdadeiro
          (newline)
          (display "Deu faltas a mais!!!"))
        (display "Tem frequencia") ; se falso
        )))

; ajusta a temperatura - melhorado
(define ajusta-temperatura-melhor
  (lambda (temperatura)
    (if (> temperatura 20)
        (display "Ligar o ar condicionado")
        (if (< temperatura 10)
            (display "Ligar aquecimento")
            (display "esta bom!!!")))))

