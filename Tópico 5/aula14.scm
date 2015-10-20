(define fila1 (list 29 19 18 20))
(define fila2 (list 21 22 23 24))
;(car fila1)
;(cdr fila1)
;(car (cdr fila1))
;(cadr fila1)
;(caddr fila1)
;(cadddr fila1)
;(caddddr fila1)
;(car (cddddr fila1))
;(cddddr fila1)
;(null? fila1)
;(null? (cddddr fila1))
;(list? fila1)
;(list? 3)
;(list? (car fila1))
;(list? (cdr fila1))
;(list? 2 4)
;(pair? fila1)
;(cons 21 fila1)
;(cons 30 (cdr fila1))
;(cons (car fila1) (cons 30 (cdr fila1)))

; (append fila1 fila2)
;(append fila1 23)
;(append fila1 (list 23))

;(length fila1)

(define comprimento
  (lambda (lista)
    (if (null? lista)
        0
        (add1 (comprimento (cdr lista))))))

(define comprimento-iter
  (lambda (lista contador)
    (if (null? lista)
        contador
        (comprimento-iter (cdr lista) (add1 contador)))))

; dobro dos elementos de uma lista
(define dobro-da-lista
  (lambda (lista)
    (if (null? lista)
        (list)
        ;'()
        ;lista
        (cons (* 2 (car lista))
              (dobro-da-lista (cdr lista))))))

; elementos pares de uma lista
(define pares-da-lista
  (lambda (lista)
    (if (null? lista)
        (list)
        (if (even? (car lista))
            (cons (car lista) (pares-da-lista (cdr lista)))
            (pares-da-lista (cdr lista))))))


; pesquisa elemento numa lista
(define pesquisa
  (lambda (lista elem)
    (if (null? lista)
        #f
        (if (equal? elem (car lista))
            #t
            (pesquisa (cdr lista) elem)))))