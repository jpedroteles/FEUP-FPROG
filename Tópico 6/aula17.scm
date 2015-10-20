(define remove-impar
  (lambda (lista)
    (cond 
      ((null? lista) lista)
      ((odd? (car lista)) (remove-impar (cdr lista)))
      (else
       (cons (car lista) (remove-impar (cdr lista)))))))

(define remove-1o-impar
  (lambda (lista)
    (cond 
      ((null? lista) lista)
      ((odd? (car lista)) (cdr lista))
      (else
       (cons (car lista) (remove-1o-impar (cdr lista)))))))

; testes
(define l1 (list 1 2 3 4 5 6))

(define quadrado
  (lambda (x)
    (* x x)))

;(map odd? l1)
;(map number? '(2 era 7 "rer"))
;
;(map quadrado l1)
;(map (lambda (x)(* x x)) l1)
;(map (lambda (x)(expt x 5)) l1)

(define vis-numero 
  (lambda (n)
    (display "Numero: ")
    (display n)
    (newline)))

;(apply + l1)
;(+ 1 2 3 4 5 6)

(define media
  (lambda (lista)
    (if (not (null? lista))
        (/ (apply + lista)
           (length lista)))))

(define soma-quadrados
  (lambda lista
    (if (null? lista)
        0
        (+ (quadrado (car lista))
           (apply soma-quadrados (cdr lista))))))

(define soma-quadrados2
  (lambda lista
    (apply + (map quadrado lista))))

(define teste
  (lambda lista
    (for-each vis-numero lista)))


(define logaritmo
  (lambda (numero base)
    (/ (log numero)
       (log base))))

; faz procedimentos para calculo do logaritmo de uma determinada base
(define faz-logaritmo
  (lambda (base)
    (lambda (numero)
      (/ (log numero)
         (log base)))))

(define log2 (faz-logaritmo 2))
;(define log2
;  (lambda (numero)
;    (/ (log numero)
;       (log 2))))

(define log3 (faz-logaritmo 3))
(define log21 (faz-logaritmo 21))

(define operadores (list + - * /))


    