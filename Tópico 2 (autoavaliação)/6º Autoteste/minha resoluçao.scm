;;;;;;;;;;;;;;;;;;;;
;Abstracao Producao;
;;;;;;;;;;;;;;;;;;;;

;alinea 1

(define cria-producao
  (lambda (ano litros)
    (cons ano litros)))

;alinea 2

(define ano-producao
  (lambda (prod)
    (car prod)))

(define quantidade-producao
  (lambda (prod)
    (cdr prod)))

;;;;;;;;;;;;;;;;;;;
;Abstracao Parcela;
;;;;;;;;;;;;;;;;;;;

;alinea 3

(define cria-parcela
  (lambda (geocodigo nif nome)
    (list geocodigo (cons nif nome)
          (list) )))

;alinea 4

(define adic-prod-parcela
  (lambda (parcela producao)
    (list (car parcela)
          (cadr parcela)
          (cons producao (caddr parcela)
                 ))))
;alinea 5

(define pesquisa-ano
  (lambda (lista ano)
    (cond
      ((null? lista) 0)
      ((equal? (caar lista) ano) (cdar lista))
      (else (pesquisa-ano (cdr lista) ano)))))

(define prod-ano-parcela
  (lambda (parcela ano)
    (pesquisa-ano (caddr parcela) ano)))
     
;alinea 6

(define media
  (lambda (lista soma i)
    (if (null? lista)
        (if (positive? i)
            (/ soma i)
            0)
        (media (cdr lista) (+ soma (cdar lista)) (add1 i)))
    ))

(define media-prod-parcela
  (lambda (parcela)
    (media (caddr parcela) 0 0)))

;alinea 7

(define ano-max-prod-parcela
  (lambda (parcela)
    (max-aux (caddr parcela) -1 0)))

(define max-aux
  (lambda (lista maximo ano)
    (cond ((null? lista) ano)
          ((> (cdar lista) maximo) (max-aux (cdr lista) (cdar lista) (caar lista)))
          (else (max-aux (cdr lista) maximo ano)))))



; Testes
(define prod1 (cria-producao 2008 523))
(define parc1 (cria-parcela 111111222222 1234567890 "Antonio Silva"))
(define parc2 (adic-prod-parcela parc1 (cria-producao 2008 255)))
(define parc3 (adic-prod-parcela parc2 (cria-producao 2007 455)))