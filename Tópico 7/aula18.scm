; Procedimentos como objetos de 1a classe

; 1 - ligam-se a identificadores
(define dobro (lambda (x) (* 2 x)))

; 2 - sao argumentos de outros procedimentos
;(map dobro (list 1 2 3 4))

; 3 - sao elementos de estruturas de dados
(define operadores (list + - * / dobro))
;((list-ref operadores 4) 3)

; 4 - sao devolvidos como resultado de outros procedimentos
(define faz-multiplo
  (lambda (mult)
    (lambda (x)
      (* mult x))))

(define triplo (faz-multiplo 3))
(define m23 (faz-multiplo 23))


; procedimentos com numero variavel de argumentos
(define media
  (lambda lista
    (if (null? lista)
        (display "nao tem valores...")
        (/ (apply + lista) (length lista)))))

; Conta corrente
(define saldo 1000)

(define compra!
  (lambda (valor)
    (if (>= saldo valor)
        (set! saldo (- saldo valor))
        (display "saldo insuficiente... :-("))))

(define venda!
  (lambda (valor)
    (set! saldo (+ valor saldo))))

; Abstracao Naves

; construtor
(define cria-nave
  (lambda (lin col ori)
    (cons (cons lin col) ori)))

; seletores
(define lin-nave
  (lambda (nave)
    (caar nave)))

(define col-nave
  (lambda (nave)
    (cdar nave)))

(define ori-nave
  (lambda (nave)
    (cdr nave)))

; modificadores

(define frente! 
  (lambda (nave dist)
    (case (ori-nave nave)
      ((n) (set-car! (car nave) (- (lin-nave nave) dist)))
      ((s) (set-car! (car nave) (+ (lin-nave nave) dist)))
      ((e) (set-cdr! (car nave) (+ (col-nave nave) dist)))
      ((o) (set-cdr! (car nave) (- (col-nave nave) dist))))))
 
(define roda-esq! 
  (lambda (nave)
    (case (ori-nave nave)
      ((n) (set-cdr! nave 'o))
      ((s) (set-cdr! nave 'e))
      ((e) (set-cdr! nave 'n))
      ((o) (set-cdr! nave 's)))))

(define roda-dir!
  (lambda (nave)
    (case (ori-nave nave)
      ((n) (set-cdr! nave 'e))
      ((s) (set-cdr! nave 'o))
      ((e) (set-cdr! nave 's))
      ((o) (set-cdr! nave 'n)))))
  
; testes
(define apolo11 (cria-nave 2 5 'n))