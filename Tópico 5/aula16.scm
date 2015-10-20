;;;;;;;;;;;;;;;;;;;
; Abstração Conta ;
;;;;;;;;;;;;;;;;;;;

; Construtores
(define cria-conta
  (lambda (numero num-cliente nome-cliente)
    (list numero 
          (cons num-cliente nome-cliente)
          0)))

; credito na conta
(define credito
  (lambda (conta data desc valor)
    (append
     ; lista com os dados
     (list (car conta) 
           (cadr conta)
           (+ valor (caddr conta)))
     ; novo movimento
     (list (list data desc valor))
     ;movimentos anteriores
     (cdddr conta))))
          
           
; debito na conta
(define debito
  (lambda (conta data desc valor)
    (append
     ; lista com os dados
     (list (car conta) 
           (cadr conta)
           (- (caddr conta) valor))
     ; novo movimento
     (list (list data desc (- valor)))
     ;movimentos anteriores
     (cdddr conta))))

; Seletores
(define num-conta 
  (lambda (conta)
    (car conta)))

(define num-cliente
  (lambda (conta)
    (caadr conta)))

(define nome-cliente
  (lambda (conta)
    (cdadr conta)))

(define saldo
  (lambda (conta)
    (list-ref conta 2)))

(define vis-saldo
  (lambda (conta)
    (display "Conta: ")
    (display (num-conta conta))
    (newline)
    (display "Cliente: ")
    (display (nome-cliente conta))
    (display " (")
    (display (num-cliente conta))
    (display ")")
    (newline)
    (display "Saldo: ")
    (display (saldo conta))
    (newline)))

; movimentos da conta
(define vis-movimentos
  (lambda (conta num)
    (vis-saldo conta)
    (display "Movimentos:")
    (newline)
    (vis-n-mov (cdddr conta) (saldo conta) num)))

(define vis-n-mov
  (lambda (lista saldo num)
    (if (and (not (null? lista))
             (positive? num))
        (begin
          (display saldo)
          (display ": ")
          (display (list-ref (car lista) 0))
          (display " ")
          (display (list-ref (car lista) 1))
          (display " ")
          (vis-mov (list-ref (car lista) 2))
          (newline)
          (vis-n-mov (cdr lista) 
                     (- saldo (list-ref (car lista) 2))
                     (sub1 num))))))
(define vis-mov
  (lambda (valor)
    (if (negative? valor)
        (begin (display " (d) ")(display (- valor)))
        (begin (display " (c) ")(display valor)))))
    

; Testes
(define conta1 (cria-conta 11111 12345 "Antonio"))
(define conta2 (credito conta1 "2013-11-19" "deposito" 1000))
(define conta3 (credito conta2 "2013-11-19" "euromilhoes" 15e6))
(define conta4 (debito conta3 "2013-11-20" "Ferrari" 200000))
