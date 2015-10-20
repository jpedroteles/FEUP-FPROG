; volume de um paralelipipedo
(define vol-paralelo
  (lambda (altura largura comprimento)
    (let* ((base (* comprimento largura))
          (perimetro (* 2 (+ comprimento largura)))
          (volume (* altura base)))
  
    volume)))

; classificacao
(define classificacao
  (lambda (nota)
    (if (> nota 16)
        (display "Muito bom")
        (if (> nota 13)
            (display "Bom")
            (if (>= nota 10)
                (display "Suficiente")
                (display "Insuficiente"))))))

(define classificacao2
  (lambda (nota)
    (cond
      ((or (> nota 20) (negative? nota) (not (integer? nota))) (display "Valor incorreto")
                                         (newline)
                                         (display "Tente novamente..."))
      ((> nota 16) (display "Muito bom"))
      ((> nota 13) (display "Bom"))
      ((>= nota 10) (display "Suficiente"))
      (else (display "Insuficiente")))))
      

