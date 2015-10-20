;;;;;;;;;;;;;;;;;;;;;;;;;
; Abstracao Restaurante ;
;;;;;;;;;;;;;;;;;;;;;;;;;

; Abstracao Menu
(define cria-menu
  (lambda (n)
    (make-vector n (cons '? 0))))

(define adiciona-prato!
  (lambda (menu prato preco numero)
    (vector-set! menu (sub1 numero) (cons prato preco))))
    
(define nome-prato
  (lambda (menu numero)
    (car (vector-ref menu (sub1 numero)))))

(define preco-prato
  (lambda (menu numero)
    (cdr (vector-ref menu (sub1 numero)))))

; Abstracao Restaurante 
(define cria-restaurante 
  (lambda (n)
    (cons (make-vector n)
          (make-vector n '()))))

(define poe-mesa! 
  (lambda (rest lotacao numero)
    (vector-set! (car rest) (sub1 numero) lotacao)
    (vector-set! (cdr rest) (sub1 numero) (list 0))))

(define lotacao
  (lambda (rest)
    (apply + (vector->list (car rest)))))

(define conta-mesa
  (lambda (rest nmesa)
    (car (vector-ref (cdr rest) (sub1 nmesa)))))
    
(define adiciona-pedido!
  (lambda (rest menu nmesa nprato quantidade)
    (let ((lista (vector-ref (cdr rest) (sub1 nmesa))))
      (append! lista (list (cons nprato quantidade)))
      (set-car! lista (+ (car lista)
                         (* quantidade (preco-prato menu nprato)))))))
  

; testes
(define menu-terca (cria-menu 10))
(adiciona-prato! menu-terca 'sopa 2 1)
(adiciona-prato! menu-terca 'tripas 5 2)
(adiciona-prato! menu-terca 'pescada 6 3)
;menu-terca

(define mieic-gourmet (cria-restaurante 15))
(poe-mesa! mieic-gourmet 2 1)
(poe-mesa! mieic-gourmet 2 2)
(poe-mesa! mieic-gourmet 2 3)
(poe-mesa! mieic-gourmet 4 4)
(poe-mesa! mieic-gourmet 4 5)
(poe-mesa! mieic-gourmet 8 6)
(poe-mesa! mieic-gourmet 4 7)
(poe-mesa! mieic-gourmet 4 8)
(poe-mesa! mieic-gourmet 4 9)
(poe-mesa! mieic-gourmet 4 10)
(poe-mesa! mieic-gourmet 10 11)
(poe-mesa! mieic-gourmet 4 12)
(poe-mesa! mieic-gourmet 4 13)
(poe-mesa! mieic-gourmet 4 14)
(poe-mesa! mieic-gourmet 6 15)

(adiciona-pedido! mieic-gourmet menu-terca 1 1 2)
(adiciona-pedido! mieic-gourmet menu-terca 1 2 1)
(adiciona-pedido! mieic-gourmet menu-terca 1 3 1)