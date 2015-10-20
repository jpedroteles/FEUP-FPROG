;Nome do procedimento: adivinhar           
;Parâmetro: n-perg
;           Inteiro que define o número de perguntas que o programa vai fazer.
;Objectivo: Lança o programa que, através de um certo número de perguntas e
;           respectivas respostas, irá calcular e visualizar o número pensado
;           pelo utilizador.
;
;Nome do procedimento: frase-inicial
;Parâmetro: n-per
;           Inteiro que define o número de perguntas que irão ser feitas 
;           ao utilizador.
;Objectivo: Visualiza a frase inicial "Pense num numero entre ... "
;
;Nome do procedimento: perguntar-todas
;Parâmetro: n-per
;           Inteiro que define o número de perguntas que irão ser feitas 
;           ao utilizador.
;Objectivo: Faz um número de perguntas definidas pelo seu parâmetro n-per
;           e devolve uma lista com as respostas dadas pelo utilizador.
;


; corpo principal
;


(define adivinhar
  (lambda (n-perg)
    (frase-inicial n-perg)
    (let ((num-adivinhado (perguntas n-perg)))
      (newline)
      (display "O numero pensado foi: ")      
      (display num-adivinhado)))) 

(define frase-inicial
  (lambda (n-per)
    (newline)
    (display "Pense num numero entre 0 e ")
    (display (sub1 (expt 2 n-per)))
    (display "!")
    (newline)))


(define perguntas
  (lambda (n-perguntas)
    (let ((num-max (sub1 (expt 2 n-perguntas))))
      (letrec ((ciclo 
                (lambda (n-p)
                  (if (> n-p n-perguntas)
                      0
                      (+ (* (pergunta num-max n-p)
                            (expt 2 (sub1 n-p)))
                         (ciclo (add1 n-p)))))))
        (ciclo 1)))))


(define pergunta
  (lambda (n-max n-p)
    (letrec ((ler-ate-s-ou-n
              (lambda ()
                (display "P")
                (display n-p)
                (display "- O numero pensado esta' no conjunto (1=sim, 0=nao)? ")
                (let ((num-lido (read)))
                  (if (or (equal? num-lido 1)
                          (equal? num-lido 0))
                      num-lido
                      (begin
                        (newline)
                        (ler-ate-s-ou-n)))))))
      (visu-numeros n-max n-p)  ; no final faz newline
      (ler-ate-s-ou-n))))


(define visu-numeros
  (lambda (n-max nperg)
    (letrec ((tem-bit-a-1
              (lambda (numero ordem-do-bit)
                (odd? (quotient numero (expt 2 ordem-do-bit)))))
             
             (ciclo 
              (lambda (numero)
                (cond ((> numero n-max)
                       (newline))
                      ((tem-bit-a-1 numero (sub1 nperg))
                       (display numero)
                       (display " ")
                       (ciclo (add1 numero)))
                      (else
                       (ciclo (add1 numero)))))))
      (newline)
      (ciclo 1))))


