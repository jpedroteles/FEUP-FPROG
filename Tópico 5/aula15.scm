;(define naipes (list 'ouros 'paus 'espadas 'copas))
(define naipes '(ouros paus espadas copas))
(define cartas '(2 3 4 5 6 7 8 9 10 as rei dama valete))

; determina uma arta aleatoriamente
(define da-carta
  (lambda ()
    (cons (list-ref cartas (random (length cartas)))
          (list-ref naipes (random (length naipes))))))

; retorna o elemento i da lista
(define lista-ref
  (lambda (lista i)
    (cond
      ((null? lista) #f)
      ((negative? i) #f)
      ((zero? i) (car lista))
      (else
       (lista-ref (cdr lista) (sub1 i))))))
    
;;;;;;;;;;;;;;;;;;;;;;;
; Abstracao estudante ;
;;;;;;;;;;;;;;;;;;;;;;;

; Construtores
(define cria-estudante
  (lambda (numero nome)
    (list numero nome)))

(define adiciona-uc
  (lambda (estudante uc nota)
    (append estudante (list (cons uc nota)))))

; Seletores
(define nome-estudante
  (lambda (estudante)
    (cadr estudante)))

(define numero-estudante
  (lambda (estudante)
    (car estudante)))

(define num-uc-estudante
  (lambda (estudante)
    (length (cddr estudante))))

(define classificacao-estudante
  (lambda (estudante uc) 
        (if (null? estudante)
            ; nao encontrou
            #f
            (if (pair? (car estudante))
                ; lista de UCs
                (if (equal? (caar estudante) uc)
                    ; encontrou
                    (cdar estudante)
                    ; nao encotrou
                    (classificacao-estudante (cdr estudante) uc))
                ; inicio da lista -> lista de ucs
                (classificacao-estudante (cddr estudante) uc)))))
  
; media das UCs
(define media-estudante
  (lambda (estudante)
    (if (positive? (num-uc-estudante estudante))
        (/ (somatorio (cddr estudante))
           (num-uc-estudante estudante))
        (display "Nao tem UC concluidas"))))

(define somatorio
  (lambda (lista)
    (if (null? lista)
        0
        (+ (cdar lista) (somatorio (cdr lista))))))
    
    

; testes
(define antonio (cria-estudante 11111 "Antonio"))
(define antonio (adiciona-uc antonio "FPRO" 18))
(define antonio (adiciona-uc antonio "AMAT" 17))
(define antonio (adiciona-uc antonio "MDIS" 20))
(define antonio (adiciona-uc antonio "AOCO" 11))