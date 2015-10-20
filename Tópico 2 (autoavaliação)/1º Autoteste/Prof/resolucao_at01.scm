; ==============================================================
; FUNDAMENTOS DA PROGRAMACAO - MIEIC 
; ==============================================================
; Auto teste 01
;
; RESOLUCAO
;
; ----------------
; FRACCOES
; ----------------
(define visu-fraccao
  (lambda (num denum)
    (display "frac: ")
    (display num)
    (display "/")
    (display denum)))

(define reduz
  (lambda (num denum)
    (visu-fraccao (/ num (gcd num denum))
                  (/ denum (gcd num denum)))))

(define multiplicar-fraccoes
  (lambda (n1 d1 n2 d2)
    (reduz (* n1 n2)
           (* d1 d2))))

(define dividir-fraccoes
  (lambda (n1 d1 n2 d2)
    (multiplicar-fraccoes n1 d1 d2 n2)))


; ----------------------------------------------
