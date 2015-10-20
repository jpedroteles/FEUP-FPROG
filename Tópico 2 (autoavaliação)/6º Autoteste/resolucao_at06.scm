; ==============================================================
; FUNDAMENTOS DA PROGRAMACAO - MIEIC - 2009/10
; ==============================================================
; Auto teste 06
;
; RESOLUCAO
;

;;;;;;;;;;;;;;;;;;;;;;
; Abstracção Parcela ;
;;;;;;;;;;;;;;;;;;;;;;

; 1 - construtor de producao
(define cria-producao
  (lambda (ano litros)
    (cons ano litros)))

; 2 - selectores de producao

; ano de producao
(define ano-producao
  (lambda (prod)
    (car prod)))

; quantidade de producao
(define quantidade-producao
  (lambda (prod)
    (cdr prod)))

;;;;;;;;;;;;;;;;;;;;;;
; Abstracção Parcela ;
;;;;;;;;;;;;;;;;;;;;;;

; 3- Construtor de parcela
(define cria-parcela
  (lambda (geocodigo nif nome)
    (list geocodigo
          (cons nif nome)
          (list ))))

; 4- adiciona produção à parcela
(define adic-prod-parcela   
  (lambda (parcela producao)
    (list (car parcela)
          (cadr parcela)
          (cons producao (caddr parcela)))))

; 5- Selector prod-ano-parcela
(define prod-ano-parcela 
  (lambda (parcela ano)
    (pesquisa-ano (caddr parcela) ano)))

(define pesquisa-ano
  (lambda (lista ano)
    (cond
      ((null? lista) 0)
      ((equal? (caar lista) ano) (cdar lista))
      (else (pesquisa-ano (cdr lista) ano)))))

; 6- Media aritmetica
(define media-prod-parcela 
  (lambda (parcela)
    (media (caddr parcela) 0 0)))

(define media
  (lambda (lista soma cont)
    (if (null? lista)
        (if (positive? cont)
            (/ soma cont)
            0)
        (media (cdr lista) (+ soma (cdar lista)) (add1 cont)))))

; 7- Maximo da producao
(define ano-max-prod-parcela 
  (lambda (parcela)
    (max-aux (caddr parcela) -1 0)))

(define max-aux
  (lambda (lista maximo ano)
    (cond 
      ((null? lista) ano)
      ((> (cdar lista) maximo) (max-aux (cdr lista) (cdar lista) (caar lista)))
      (else (max-aux (cdr lista) maximo ano)))))
;