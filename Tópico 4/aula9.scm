;fatorial de um numero

; algoritmo que gera processos recursivos
(define fatorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (fatorial (sub1 n))))))

; algoritmo que gera processos iterativos
(define fatorial-iter
  (lambda (n produto)
    (if (zero? n)
        produto
        (fatorial-iter (sub1 n) (* n produto)))))

(define fatorial-melhorado
  (lambda (n)
    (fatorial-iter n 1)))

; algoritmo que gera processos iterativos - nova versão

(define fatorial-melhorado-novo
  (lambda (n)
    (letrec ((fatorial-aux
              (lambda (n produto)
                (if (zero? n)
                    produto
                    (fatorial-aux (sub1 n) (* n produto))))))
    (fatorial-aux n 1))))

; conta digitos de um numero
(define conta-digitos
  (lambda (numero)
    (if (zero? numero)
        0
        (add1 (conta-digitos (quotient numero 10))))))

; conta digitos de um numero - melhorado
(define conta-digitos-iter
  (lambda (numero contador)
    (if (zero? numero)
        contador
        (conta-digitos-iter (quotient numero 10) (add1 contador)))))

; conta digitos de um numero - melhorado e com 1 parametro
(define conta-digitos-melhorado
  (lambda (numero)
    (letrec ((aux
              (lambda (n c)
                (if (zero? n)
                    c
                    (aux (quotient n 10) (add1 c))))))
      (aux numero 0))))

; conta digitos de um numero - melhorado e com 1 parametro e o zero!
(define conta-digitos-melhorado2
  (lambda (numero)
    (letrec ((aux
              (lambda (n c)
                (if (zero? n)
                    c
                    (aux (quotient n 10) (add1 c))))))
      (if (zero? numero)
          1
          (aux numero 0)))))