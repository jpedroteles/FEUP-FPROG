; classificacao de 0 a 5
(define classificacao0a5
  (lambda (nota)
    (case (inexact->exact (round nota))
      ((5) (display "Muito bom"))
      ((4) (display "Bom"))
      ((3) (display "Suficiente"))
      ((0 1 2) (display "Insuficiente"))
      (else (display "Valor incorreto")))))

(define classificador
  (lambda ()
    (let ((nota (read)))
      (classificacao0a5 nota))))

; calculo de volumes

; volumes de solidos
(define vol-prisma-tri
  (lambda (base altura comprimento)
    (* comprimento (/ (* base altura) 2))))

(define vol-paralelo
  (lambda (base altura comprimento)
    (* comprimento base altura)))

(define vol-piramide
  (lambda (base altura)
    (exact->inexact (* base base altura 1/3))))

; caculo do volumes da nave
(define vol-nave
  (lambda (larg comp alt tel)
    (+ (vol-paralelo larg alt comp)
       (vol-prisma-tri larg tel comp))))
      
; caculo do volumes da torre
(define vol-torre
  (lambda (base altura telhado)
    (+ (vol-paralelo base base altura)
       (vol-piramide base telhado))))
      
; volumes finais
(define vol-taipas
  (lambda ()
    (+ (vol-nave 20 20 20 5)
       (vol-torre 7.5 30 7))))

(define vol-ildefonso
  (lambda ()
    (+ (vol-nave 15 20 20 7)
       (* 2 (vol-torre 7.5 27 7)))))