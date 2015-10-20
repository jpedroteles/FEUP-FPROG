;(define hora-atual (cons 8 57))
;(define casal (cons 'Maria 'Joao))
;(define classificacao (cons "Jose Silva" 18))

; Abstracao Casal
(define cria-casal
  (lambda (mulher marido)
    (cons  marido mulher)))

(define mulher
  (lambda (casal)
    (cdr casal)))

(define marido
  (lambda (casal)
    (car casal)))

(define vis-casal
  (lambda (casal)
    (display "Casal ")
    (display (mulher casal))
    (display " e ")
    (display (marido casal))))

;;;;;;;;;;;;;;;;;;;;;;;
; Abstracao casamento :
;;;;;;;;;;;;;;;;;;;;;;;

; construtor
(define cria-casamento
  (lambda (data mulher marido)
    (cons data 
          (cons mulher marido))))

; seletores
(define data-casamento
  (lambda (casamento)
    (car casamento)))

(define mulher-casamento
  (lambda (casamento)
;    (car (cdr casamento))))
    (cadr casamento)))

(define marido-casamento
  (lambda (casamento)
;    (cdr (cdr casamento))))
    (cddr casamento)))

; testes
(define casamento1 (cria-casamento "2013-11-11" 'Ana 'Alfredo))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Abstracao casamento 2 ;
;;;;;;;;;;;;;;;;;;;;;;;;;

; construtores
(define cria-casamento2
  (lambda (data mulher marido)
    (cons data 
          (cria-casal mulher marido))))

(define cria-casamento2-do-casal
  (lambda (data casal)
    (cons data casal)))

; seletores
(define data-casamento2
  (lambda (casamento)
    (car casamento)))

(define mulher-casamento2
  (lambda (casamento)
    (mulher (cdr casamento))))

(define marido-casamento2
  (lambda (casamento)
    (marido (cdr casamento))))

; testes
(define casamento1 (cria-casamento "2013-11-11" 'Ana 'Alfredo))

(define casamento2 (cria-casamento2 "2013-11-11" 'Ana 'Alfredo))

(define convidados (cria-casal 'Maria 'Joao))
(define casamento3 (cria-casamento2-do-casal "2001-01-01" convidados))

