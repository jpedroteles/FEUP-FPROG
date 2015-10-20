; ==============================================================
; FUNDAMENTOS DA PROGRAMACAO - MIEIC
; ==============================================================
; Auto-teste 07
;
; resolução
; 
; problema 1 ---------------------
;
(define funcao-reta
  (lambda (p1 p2)
    (if (= (car p1) (car p2))
        (display "reta vertical")
        (let* ((m (/ (- (cdr p2) (cdr p1))
                     (- (car p2) (car p1))))
               (b (- (cdr p1)
                     (* m (car p1)))))
          (lambda (x)
            (+ (* m x) b))))))


(define cima?
  (lambda (reta1 reta2 x)
    (> (reta1 x)
       (reta2 x))))


; problema 2 ---------------------
;

; p 2.1
(define cria-jogo
  (lambda (clube1 golos1 clube2 golos2)
    (cons (cons clube1 clube2) (cons golos1 golos2))))

; p 2.2
(define vencedor-jogo
  (lambda (jogo)
    (cond ((> (cadr jogo) (cddr jogo))
           (caar jogo))
          ((< (cadr jogo) (cddr jogo))
           (cdar jogo))
          (else
           'empate))))

; p 2.3
(define cria-pontuacao
  (lambda (clubes)
    (if (null? clubes)
        ()
        (cons (cons (car clubes) 0) (cria-pontuacao (cdr clubes))))))
; p 2.4
(define atualiza-pontuacao ; manter a ordem dos clubes
  (lambda (pontuacao clube pontos) ; admitir argumentos válidos
    (if (null? pontuacao)
        ()
        (if (equal? (caar pontuacao) clube)
            (cons (cons (caar pontuacao) (+ (cdar pontuacao) pontos)) (cdr pontuacao))
            (cons (car pontuacao) (atualiza-pontuacao (cdr pontuacao) clube pontos))))))

; p 2.5
(define primeiros
  (lambda (pontuacao)
    (if (null? pontuacao)
        'pontuacao_invalida
        (letrec ((primeiros-aux
                  (lambda (pontuacao maxlist maxateagora)
                    (if (null? pontuacao)
                        maxlist
                        (cond ((> (cdar pontuacao) maxateagora)
                               (primeiros-aux (cdr pontuacao) (cons (list (caar pontuacao))(cdar pontuacao)) (cdar pontuacao)))
                              ((= (cdar pontuacao) maxateagora)
                               (primeiros-aux (cdr pontuacao) (cons (append (car maxlist) (list (caar pontuacao))) (cdar pontuacao)) maxateagora))
                              (else
                               (primeiros-aux (cdr pontuacao) maxlist maxateagora)))))))
          (primeiros-aux pontuacao () -1)))))

