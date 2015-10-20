; Fundamentos da Programação
; Auto teste AT05

; -------------------------------------
; pergunta 1 
; -------------------------------------
;
(define raccons
  (lambda (num denom)
    (cons num denom)))

(define racnum
  (lambda (rac)
    (car rac)))

(define racdenom
  (lambda (rac)
    (cdr rac)))

(define racvisu
  (lambda (rac)
    (display (racnum rac))
    (display "/")
    (display (racdenom rac))))

(define racreduz
  (lambda (rac)
    (let ((gcd-num-denom (gcd (racnum rac)
                              (racdenom rac))))
    (raccons (/ (racnum rac)
                gcd-num-denom)
             (/ (racdenom rac)
                gcd-num-denom)))))

; -------------------------------------
; pergunta 2
; -------------------------------------

(define da-carta
  (lambda ()
    (let ((faces '(as duque tres quatro cinco seis sete oito nove dez
                      dama valete rei))
          (naipes '(copas ouros paus espadas)))
      ;
      (list (list-ref faces (random 13))
            (list-ref naipes (random 4))))))
       