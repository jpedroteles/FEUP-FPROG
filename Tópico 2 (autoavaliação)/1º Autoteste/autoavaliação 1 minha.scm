; 1-Fracções sem redução

(define visu-fraccao
  (lambda (n d)
    (begin (display "frac: ") ;aparece frac
           (display n) ; pega no primeiro parametro e "mostra"
           (display "/")
           (display d))))

; 2-Fracções reduzidas

(define reduz
  (lambda (n d)
      (define mdc (gcd n d)) ;procura o maximo divisor comum
      (display "frac: ")
      (display (/ n mdc)) ;divide o numerador pelo mdc e "mostra o resultado
      (display "/")
      (display (/ d mdc))))

;3-Multiplicar Fracções

(define produto
  (lambda (n1 d1 n2 d2)
    (define numerador (* n1 n2))
    (define denominador (* d1 d2))
      (define mdc (gcd numerador denominador))
      (display "frac: ")
      (display (/ numerador mdc))
      (display "/")
      (display (/ denominador mdc))))


;4-Dividir Fracções

(define quociente
  (lambda (n1 d1 n2 d2)
    (define numerador (* n1 d2))
    (define denominador (* n2 d1))
      (define mdc (gcd numerador denominador))
      (display "frac: ")
      (display (/ numerador mdc))
      (display "/")
      (display (/ denominador mdc))))