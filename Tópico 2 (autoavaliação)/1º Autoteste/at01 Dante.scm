
;1 Visualizar fracao
(define visu-fraccao
  (lambda (n d)
    (display "Frac: ")
    (display n)
    (display "/")
    (display d)
    )
  )


;2 Prodecimento reduz
(define reduz
  (lambda (n d)
    (define mdc (gcd n d))
    (display "MDC = ")
    (display mdc)
    (newline)
    (display "Fraccao reduzida: ")
    (display (/ n mdc))
    (display "/")
    (display (/ d mdc ))
    )
  )


; 3. Prodecimento multiplicar fraccao
(define multiplicar-fraccoes
  (lambda (n1 d1 n2 d2)
    (define numeradores (* n1 n2)) ;multiplica numeradores
    (define denominadores (* d1 d2)) ;multiplica denominadores
    
    (define mdc (gcd numeradores denominadores)) ;MDC das dois numeros
    (display "MDC dos numeros: ")
    (display mdc)
    (newline)
    (display "Fraccao reduzida: ")
    (display (/ numeradores mdc))
    (display "/")
    (display (/ denominadores mdc ))
    )
  )


;4 Prodecimento dividir fracao
(define dividir-fraccoes
  (lambda (n1 d1 n2 d2)
    (define numerador (* n1 d2)) ;multiplica numeradores
    (define denominador (* d1 n2)) ;multiplica denominadores
    
    (define mdc (gcd numerador denominador)) ;MDC das dois numeros
    (display "MDC dos numeros: = ")
    (display mdc)
    (newline)
    (display "Fraccao reduzida: ")
    (display (/ numerador mdc))
    (display "/")
    (display (/ denominador mdc ))
    )
  )