; progressao aritmetica
(define pa
  (lambda (a b razao)
    (if (<= a b)
        (begin
          (display a)
          (display " ")
          (pa (+ a razao) b razao)))))

           
; somatorio da progressao aritmetica
(define soma-pa
  (lambda (a b razao)
    (if (> a b)
        0
        (+ a (soma-pa (+ a razao) b razao)))))

(define soma-pa-melhorada
  (lambda (a b razao)
    (let ((n (add1 
              (/ (- b a) 
                 razao))))
      (/ (* n 
            (+ a b))
         2))))

; jogo 
(define jogo1-100
  (lambda (tentativas)
    (faz-jogada (add1 (random 100)) tentativas)))

(define faz-jogada
  (lambda (numero tentativas)
    ; verificar tentativas
    (if (< tentativas 1)
        (begin
          (newline)
          (display "Perdeu! Nao tem mais jogadas...")
          (newline)
          (display "O numero em que tinha pensado era o: ")
          (display numero))
        ; jogada
        (begin
          (display "Adivinhe o numero. Tem ")
          (display tentativas)
          (display " tentativas")
          (newline)
          (let ((jogada (read)))
            (if (= jogada numero)
                (display "Acertou. Parabens!!!")
                ; nao acertou
                (begin
                  (if (> jogada numero)
                      (display "Numero muito grande.")
                      (display "Numero muito pequeno."))
                  (faz-jogada numero (sub1 tentativas)))))))))
  