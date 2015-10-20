; Pesquisa sequencial
(define pesquisa
  (lambda (v elem)
    (letrec ((aux
              (lambda (i)
                (if (>= i (vector-length v))
                    #f
                    (if (equal? elem (vector-ref v i))
                        i
                        (aux (add1 i)))))))
      (aux 0))))

; testes
(define v1 (vector 6 2 3 7 4 9 8 1))

; Pesquisa binária
(define pesquisa-bin
  (lambda (v elem)
    (letrec ((aux
              (lambda (ini fim)
                (if (< fim ini)
                    #f
                    (let ((meio (quotient (+ ini fim) 2)))
                      (cond 
                        ; encontramos
                        ((equal? (vector-ref v meio) elem) meio)
                        ; o valor esta direita
                        ((> elem (vector-ref v meio)) 
                         (aux (add1 meio) fim))
                        ; o valor esta esquerda
                        (else
                         (aux ini (sub1 meio)))))))))
      (aux 0 (sub1 (vector-length v))))))
                        
; testes
(define v1-ord (vector 1 2 3 4 6 7 8 9))

; Ordenacao por selecao
(define ordena!
  (lambda (v)
    (letrec ((minimo
              ; determina a posicao do minimo
              (lambda (i m im)
                (if (>= i (vector-length v))
                    im ; posicao do minimo
                    (if (< (vector-ref v i) m)
                        (minimo (add1 i) (vector-ref v i) i)
                        (minimo (add1 i) m im)))))
             (aux
              (lambda (i)
                (if (< i (sub1 (vector-length v)))
                    (let ((pos-min (minimo (add1 i) (vector-ref v i) i)))
                      (if (not (= pos-min i))
                          ; trocar os valores
                          (let ((temp (vector-ref v i)))
                            (vector-set! v i (vector-ref v pos-min))
                            (vector-set! v pos-min temp)))
                      ; chamada recursiva
                      (aux (add1 i)))))))
            (aux 0))))
                    