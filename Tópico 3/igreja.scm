(define vol-prisma-tri
  (lambda (base altura comprimento)
    (* comprimento (/ (* base altura) 2))))
(define vol-paralelo
  (lambda (base altura comprimento)
    (* comprimento base altura)))
(define vol-piramide
  (lambda (base altura)
    (exact -> inexact (* base base altura 1/3))))
(define vol-nave
  (lambda (largura comprimento altura telhado)
    (+ (vol-paralelo largura altura comprimento)
       (vol-prisma-tri largura telhado comprimento))))
(define vol-igreja
  (lambda (largura comprimento altura telhado base)
    (+ (* (read) (vol-nave largura comprimento altura)
       (* (read) (vol-torre largura base telhado))))))
