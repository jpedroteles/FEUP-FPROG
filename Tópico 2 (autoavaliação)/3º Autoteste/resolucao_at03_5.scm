; Fundamentos da Programação
; Auto teste AT03

; -------------------------------------
; pergunta 1  - fruta e muita fruta ! -
; -------------------------------------
;
(define fruta
  (lambda (laranjas peras)
    (cond ((and (= laranjas 0)
                (= peras 0))
           0)
          ;
          ((= laranjas peras)
           3)
          ;
          ((> laranjas peras)
           1)
          ;
          (else 
           2))))
;
(define fruta-com-negativos
  (lambda (laranjas peras)
    (if (or (< laranjas 0)
            (< peras 0))
        ;
        -1
        ;
        (fruta laranjas peras))))
;
(define muita-fruta
  (lambda (laranjas peras mangas bananas figos)
    (if (or (negative? laranjas)
            (negative? peras)
            (negative? mangas)
            (negative? bananas)
            (negative? figos))
        -1
        (let ((lar+peras (+ laranjas peras))
              (m+b+figos (+ mangas bananas figos)))
          (cond ((> lar+peras m+b+figos)
                 (if (< figos bananas)
                     1
                     3))
                (else
                 (if (< figos bananas)
                     2
                     4)))))))

; -------------------------------
; pergunta 2  - falsa numeração -
; -------------------------------
;
(define falso->verdadeiro
  (lambda (num-falso)
    (if (< num-falso 10)
        (digito-f-verd num-falso)
        (+ (digito-f-verd (remainder num-falso 10)) 
           (* 10 (falso->verdadeiro (quotient num-falso 10)))))))
;
(define digito-f-verd
  (lambda (n-f)
    (cond ((= n-f 2) 1)
          ((= n-f 4) 2)
          ((= n-f 6) 3)
          ((= n-f 8) 4)
          ((= n-f 1) 5)
          ((= n-f 3) 6)
          ((= n-f 5) 7)
          ((= n-f 7) 8)
          (else n-f))))


; DAQUI PARA A FRENTE A PROVA NÃO SERÁ AVALIADA! 
;
(define verdadeiro->falso
  (lambda (num-verd)
    (if (< num-verd 10)
        (digito-v-falso num-verd)
        (+ (digito-v-falso (remainder num-verd 10)) 
           (* 10 (verdadeiro->falso (quotient num-verd 10)))))))
;
(define digito-v-falso
  (lambda (n-v)
    (cond ((= n-v 1) 2)
          ((= n-v 2) 4)
          ((= n-v 3) 6)
          ((= n-v 4) 8)
          ((= n-v 5) 1)
          ((= n-v 6) 3)
          ((= n-v 7) 5)
          ((= n-v 8) 7)
          (else n-v))))
;
(define soma-falso
  (lambda (n-falso1 n-falso2)
    (verdadeiro->falso
     (+ (falso->verdadeiro n-falso1)
        (falso->verdadeiro n-falso2)))))
