; ==============================================================
; FUNDAMENTOS DA PROGRAMACAO - MIEIC
; ==============================================================
; Auto teste 02
;
; SOLUÇÃO
;
; ---------------- classificações --------------
;
(define visu-classifica
  (lambda (cla)
    (display "classificacao = ")
    (display cla)
    (newline)))

(define calcula-classifica
  (lambda (pp1 pp2 pp3 pp4 ad pe)
    (let ((app (/ (- (+ pp1 pp2 pp3 pp4) (min pp1 pp2 pp3 pp4)) 3)))
      (+ (* app 0.55)
         (* ad 0.05)
         (* pe 0.40)))))
  
(define classificacao
  (lambda (pp1 pp2 pp3 pp4 ad pe)
    (let ((app (/ (- (+ pp1 pp2 pp3 pp4) (min pp1 pp2 pp3 pp4)) 3)))
      (cond ((< app 8)
             (display "classificacao = sf")
             (newline))
            ((<  pe 8)
             (display "classificacao = pe")
             (newline))
            (else
             (visu-classifica 
              (calcula-classifica pp1 pp2 pp3 pp4 ad pe)))))))
;
; ----------------
; TRIANGULOS
; ----------------
(define comprimento
  (lambda (x1 y1 x2 y2)
    (let ((x1-menos-x2 (- x1 x2))
          (y1-menos-y2 (- y1 y2)))
      (sqrt (+ (* x1-menos-x2 x1-menos-x2)
               (* y1-menos-y2 y1-menos-y2))))))
;
(define lado-mais-comprido
  (lambda (x1 y1 x2 y2 x3 y3)
    (let ((lado1 (comprimento x1 y1 x2 y2))
          (lado2 (comprimento x2 y2 x3 y3))
          (lado3 (comprimento x3 y3 x1 y1)))
      (cond ((and (> lado1 lado2)
                  (> lado1 lado3))
             1)
            ((and (> lado2 lado1)
                  (> lado2 lado3))
             2)
            ((and (> lado3 lado1)
                  (> lado3 lado2))
             3)
            (else 0)))))

