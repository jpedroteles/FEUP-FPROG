; PROGRAMA PARA PREENCHER BOLETINS DE TOTOLOTO
;
; F. Nunes Ferreira - fnf@fe.up.pt
; 4 de Novembro de 2000
;
; Modo de utilização
; Gerar um número desejado de apostas de totoloto. 
; Ex: Para gerar 10 apostas
; (preenche-totoloto 10)
;
(define preenche-totoloto
  (lambda (num-apostas)
    (let ((registo (faz-registo)))
      (poe-n-apostas-num-registo! registo num-apostas)
      (newline)
      (display "Apostas seleccionadas:")
      (newline)
      (visu-registo (cdr registo))
      (display "----------------------")
      (newline)
      (newline))))

; ------------------
; ------------------
; cria um registo de apostas vazio, uma lista mutável com
; a cabeça "registo-de-aposta"
;
(define faz-registo
  (lambda ()
    (list 'registo-de-apostas)))
;
; ------------------
; ------------------
; Preenche um registo com n apostas
; geradas aleatoriamente
;
(define poe-n-apostas-num-registo!
  (lambda (reg num-apostas)
    (letrec ((aux
              (lambda (n-ap)
                (cond ((zero? n-ap)
                       'ok)
                      (else
                       (regista-aposta! reg
                                        (preenche-aposta!))
                       (aux (sub1 n-ap)))))))
      (aux num-apostas))))

;
; coloca uma aposta num registo
;
(define regista-aposta!
  (lambda (reg-apostas aposta)
    (append! reg-apostas (list aposta))))
;
; preenche, aleatoriamente, uma aposta
;
(define preenche-aposta!
  (lambda ()
    (let ((ap (faz-aposta)))
      (letrec ((aux
                (lambda (n-ap)
                  (if (zero? n-ap)
                      ap
                      (let ((mais-um (numero-para-a-aposta!
                                      ap
                                      (roleta-1-n 49))))
                        (if mais-um 
                            (aux (sub1 n-ap))
                            (aux n-ap)))))))
        (aux 6)))))
;
; cria uma aposta de totoloto vazia, ou seja,
; uma lista com 6 zeros.
;
(define faz-aposta
  (lambda ()
    (list 0 0 0 0 0 0)))
;
; coloca mais um número numa aposta.
; Devolve #t, se a operação tiver tido sucesso.
; Devolve #f, se a operação não se realizar, pelo 
;     facto de o número que se pretende registar
;     já fazer parte da aposta
;
(define numero-para-a-aposta!
  (lambda (aposta numero)
    (letrec ((aux
              (lambda (ap)
                (cond ((= (car ap) numero)  #f); o número já existe
                      ((zero? (car ap)) ; ainda não preenchido
                       (set-car! ap numero)
                       #t) ; colocou o número na aposta
                      (else
                       (aux (cdr ap)))))))
      (aux aposta))))
;
; devolve um número aleatório entre
; 1 e limite
;
(define roleta-1-n
  (lambda (limite)
    (add1 (random limite))))

; ------------------
; ------------------
(define visu-registo
  (lambda (registo)
    (cond ((not (null? registo))
           (visu-em-colunas-de-2 (ordena-aposta (car registo)))
           (newline)
           (visu-registo (cdr registo))))))

(define visu-em-colunas-de-2
  (lambda (aposta)
    (if (not (null? aposta))
        (begin 
          (if (< (car aposta) 10)
              (display " "))
          (display (car aposta))
          (display "  ")
          (visu-em-colunas-de-2 (cdr aposta))))))

(define ordena-aposta
  (lambda (aposta)
    (cria-lista-ord aposta)))

(define poe-um-ordenado
  (lambda (novo lista-ord)
    (cond ((null? lista-ord) (list novo))
          ((< novo (car lista-ord))
           (cons novo lista-ord))
          (else
           (cons (car lista-ord)
                 (poe-um-ordenado novo (cdr lista-ord)))))))

(define cria-lista-ord
  (lambda (lista)
    (letrec ((aux
              (lambda (lis lis-ord)
                (if (null? lis)
                    lis-ord
                    (aux (cdr lis)
                         (poe-um-ordenado (car lis)
                                          lis-ord))))))
      (aux lista '()))))

; ------------------
; ------------------








