;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                SCHEME PACMAN - Versao 1.0                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                                                          ;
; Scheme Pacman foi criado por: Luis Filipe Guimaraes Teofilo                                                              ;
;                               Tiago Pinto Fernandes                                                                      ;
;                                                                                                                          ;
; Data: 7-3-2006                                                                                                           ;
;                                                                                                                          ;
;                                     Faculdade de Engenharia da Universidade do Porto                                     ;
;                                   Licenciatura em Engenharia Informatica e Computacao                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                                                          ;
; Para jogar:                                                                                                              ;
; Basta clicar em Run ou F5 para iniciar o jogo.                                                                           ;
;                                                                                                                          ;
; Teclas:                                                                                                                  ;
; "W" ou Seta Cima para mover o pacman para cima;                                                                          ;
; "S" ou Seta Baixo para mover o pacman para baixo;                                                                        ;
; "A" ou Seta Esquerda para mover o pacman para a esquerda;                                                                ;
; "D" ou Seta Direita para mover o pacman para a direita;                                                                  ;
; "X" fecha a janela de jogo.                                                                                              ;
;                                                                                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Headers necessarios                                                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (lib "swgr.scm" "user-feup"))
(require (lib "varios.scm" "user-feup"))
(require (lib "audio.scm" "user-feup"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variaveis globais                                                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define eat? 0)
(define centro-pac (list 265 115))
(define cima? #f)
(define baixo? #f)
(define direita? #t)
(define esquerda? #f)
(define mover-cima? #t)
(define mover-baixo? #t)
(define mover-direita? #t)
(define mover-esquerda? #t)
(define caminho '())
(define caminho2 '())
(define lista-bolas (list 0 0))             ;Nao compreendo porque tenho que inicializar a lista aqui para fazer o append!
(define bool-dir '())
(define test-drive? #f)                     ;Variavel responsavel pela criacao de waypoints
(define path-override? #f)
(define distancia-fant 0)
;(define nivel 1)                           ;Nivel corrente
(define ghostbuster? #f)                    ;Decide se o pacman pode ou nao comer fantasmas
(define fant1 (list 18 115 300))            ;Os fantasmas sao listas em que o primeiro elemento e a cor, o segundo o x e o terceiro o y
(define fant2 (list 8 315 315))
(define fant3 (list 20 150 210))
(define fant4 (list 6 185 310))
(define mais-proximo 0)
(define vf1 (list 5 0))                     ;Vectores de movimentacao dos fantasmas
(define vf2 (list -5 0))
(define vf3 (list 0 5))
(define vf4 (list 0 -5))
(define fechar? #f)                         ;Condicao de fecho da janela
(define tempo 0)
(define vidas 3)
(define vitoria #f)
(define pontuacao 0)
(define gameover? #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Ponto de Lancamento do Motor de Jogo                                                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Ponto de lancamento da aplicacao

(define pacman
  (lambda ()
    (main)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Funcao de temporizacao (adaptacao da funcao do Luis)                                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ate-mil
  (lambda (x)
    (if (not (= x 5000)) ;Daqui vem o time counter
        (ate-mil (add1 x))
        )))

(define 1-segundo
  (lambda ()
    (ate-mil 0)))

(define crono-aux
  (lambda ()
    (1-segundo)
    (set! tempo (+ tempo 1))))

(define cronometro
  (lambda ()
    (crono-aux)))

(define main
  (lambda ()
    (janela 475 600 "Pacman")
    (pinta (list (list 0 0) (list 0 600) (list 500 600) (list 500 0)))
    (desenha-nivel nivel-1)
    (inicio)
    (set! caminho (loop-analise-path nivel-1 1 '()))
    (desenhar-pacman 24)
    (desenhar-fantasma fant1 (car fant1))
    (desenhar-fantasma fant2 (car fant2))
    (desenhar-fantasma fant3 (car fant3))
    (desenhar-fantasma fant4 (car fant4))
    (bolas)   ;Cria uma lista com as posicoes das moedas que o pacman tem de comer
    (move '(260 50))
    (desenha-txt "Scheme Pacman foi criado por: ")
    (move '(260 30))
    (desenha-txt "Luis Filipe Guimaraes Teofilo ")
    (move '(260 10))
    (desenha-txt "Tiago Pinto Fernandes ")
    (append! lista-bolas (list (list -10 -10)))
    (msgloop)))

(define msgloop
  (lambda ()
    (letrec ((loop1 
              (lambda ()
                (cor 0)
                (output-stats)
                (if (or (not gameover?) (not vitoria))
                      (verificar-teclado))
                (if (equal? fechar? #f)
                    (loop1)
                    (fecha-janela)))))
      (loop1))))

(define verificar-teclado
  (lambda ()
            (case (tecla-pressionada #f)
              ((#\x) (fecha-janela))
              ((#\w) (if (not test-drive?)
                         (desenhar-pacman 0))
                     (mover-pacman (list (car centro-pac) (+ (cadr centro-pac) 5)))
                     (set! cima? #t)
                     (set! baixo? #f)
                     (set! direita? #f)
                     (set! esquerda? #f))
              ((#\s) (if (not test-drive?)
                         (desenhar-pacman 0))
                     (mover-pacman (list (car centro-pac) (- (cadr centro-pac) 5)))
                     (set! cima? #f)
                     (set! baixo? #t)
                     (set! direita? #f)
                     (set! esquerda? #f))
              ((#\a) (if (not test-drive?)
                         (desenhar-pacman 0))
                     (mover-pacman (list (- (car centro-pac) 5) (cadr centro-pac)))
                     (set! cima? #f)
                     (set! baixo? #f)
                     (set! direita? #f)
                     (set! esquerda? #t))
              ((#\d) (if (not test-drive?)
                         (desenhar-pacman 0))
                     (mover-pacman (list (+ (car centro-pac) 5) (cadr centro-pac)))
                     (set! cima? #f)
                     (set! baixo? #f)
                     (set! direita? #t)
                     (set! esquerda? #f))
              ((up) (desenhar-pacman 0)
                    (mover-pacman (list (car centro-pac) (+ (cadr centro-pac) 5)))
                    (set! cima? #t)
                    (set! baixo? #f)
                    (set! direita? #f)
                    (set! esquerda? #f))
              ((down) (desenhar-pacman 0)
                      (mover-pacman (list (car centro-pac) (- (cadr centro-pac) 5)))
                      (set! cima? #f)
                      (set! baixo? #t)
                      (set! direita? #f)
                      (set! esquerda? #f))
              ((left) (desenhar-pacman 0)
                      (mover-pacman (list (- (car centro-pac) 5) (cadr centro-pac)))
                      (set! cima? #f)
                      (set! baixo? #f)
                      (set! direita? #f)
                      (set! esquerda? #t))
              ((right) 
               (desenhar-pacman 0)
               (mover-pacman (list (+ (car centro-pac) 5) (cadr centro-pac)))
               (set! cima? #f)
               (set! baixo? #f)
               (set! direita? #t)
               (set! esquerda? #f))
              )
      (if (= tempo 15)
          (begin
            ;Coloca o valor da cor do fantasma mais proximo do pacman na variavel mais-proximo
            (fantasma-mais-proximo (list (distancia-fantasma-pacman fant1) (distancia-fantasma-pacman fant2) (distancia-fantasma-pacman fant3) (distancia-fantasma-pacman fant4)) (cadr (distancia-fantasma-pacman fant1)))
            (desenhar-fantasma fant1 0)
            (desenhar-fantasma fant2 0)
            (desenhar-fantasma fant3 0)
            (desenhar-fantasma fant4 0)
            (set! fant1 (car (IA fant1 vf1)))
            (cond ((> (cadr fant1) 485) (set! fant1 (list (car fant1) -10 (caddr fant1))))
                  ((< (cadr fant1) -10) (set! fant1 (list (car fant1) 485 (caddr fant1)))))
            (set! fant2 (car (IA fant2 vf2)))
            (cond ((> (cadr fant2) 485) (set! fant2 (list (car fant2) -10 (caddr fant2))))
                  ((< (cadr fant2) -10) (set! fant2 (list (car fant2) 485 (caddr fant2)))))
            (set! fant3 (car (IA fant3 vf3)))
            (cond ((> (cadr fant3) 485) (set! fant3 (list (car fant3) -10 (caddr fant3))))
                  ((< (cadr fant3) -10) (set! fant3 (list (car fant3) 485 (caddr fant3)))))
            (set! fant4 (car (IA fant4 vf4)))
            (cond ((> (cadr fant4) 485) (set! fant4 (list (car fant3) -10 (caddr fant4))))
                  ((< (cadr fant4) -10) (set! fant4 (list (car fant4) 485 (caddr fant4)))))
            (set! vf1 (cadr (IA fant1 vf1)))
            (set! vf2 (cadr (IA fant2 vf2)))
            (set! vf3 (cadr (IA fant3 vf3)))
            (set! vf4 (cadr (IA fant4 vf4)))
            (desenha-lista-bolas lista-bolas)
            (set! tempo 0))
          (cronometro))
      (colisoes-pac-fant fant1)
      (colisoes-pac-fant fant2)
      (colisoes-pac-fant fant3)
      (colisoes-pac-fant fant4)
      (desenhar-pacman 24)
      (vitoria?)
      (if vitoria
          (begin
            (final "VITORIA!!!!!!!!")))
      (if (= vidas 0)
          (begin
          (set! gameover? #t)
          (set! vitoria #t) ;Espero k isto resolva o problema de loop infinito
          (final "GAME OVER!!!!!!!!")))
      (desenhar-fantasma fant1 (car fant1))
      (desenhar-fantasma fant2 (car fant2))
      (desenhar-fantasma fant3 (car fant3))
      (desenhar-fantasma fant4 (car fant4))))

(define fantasma-mais-proximo
  (lambda (lista-distancias distancia-ant)
    (if (not (null? lista-distancias))
        (if (< (cadar lista-distancias) distancia-ant)
            (begin
              (set! mais-proximo (caar lista-distancias)) ;A cor identifica o fantasma
              (set! distancia-fant (cadar lista-distancias))
              (fantasma-mais-proximo (cdr lista-distancias) (cadar lista-distancias)))
            (fantasma-mais-proximo (cdr lista-distancias) distancia-ant)))))

(define mover-pacman
  (lambda (ponto-destino)   
    (come-bolas)
    (output-stats)
    (if test-drive?
        (if path-override?
            (begin
              (set! centro-pac ponto-destino)
              (set! caminho2 (append! caminho2 (list ponto-destino))))
            (begin
              (if (not (equal? (member ponto-destino caminho-temp) #f))
                  (begin
                    (set! centro-pac ponto-destino)
                    (set! caminho2 (append! caminho2 (list ponto-destino)))))))
        (begin
          (if (not (equal? (member ponto-destino caminho-temp) #f))
              (begin
                (cond ((> (car centro-pac) 485) (set! ponto-destino (list -10 (cadr centro-pac))))
                      ((< (car centro-pac) -10) (set! ponto-destino (list 485 (cadr centro-pac)))))
                (set! centro-pac ponto-destino)))))))

(define IA
  (lambda (fantasma vector-anterior)
    (letrec ((pode-mover?
              (lambda ()
                (let* ((vector-novo (gerar-vector))
                       (ponto-destino-novo (list (+ (cadr fantasma) (car vector-novo)) (+ (caddr fantasma) (cadr vector-novo)))))
                  (if (not (equal? (member ponto-destino-novo caminho-temp) #f))
                      (begin
                        (set! fantasma (list (car fantasma) (+ (cadr fantasma) (car vector-novo)) (+ (caddr fantasma) (cadr vector-novo))))
                        (list fantasma vector-novo))
                      (pode-mover?))))))
      (if (equal? (car fantasma) mais-proximo)
          (persegue-pacman fantasma vector-anterior)
          (begin
            (let ((ponto-destino (list (+ (cadr fantasma) (car vector-anterior)) (+ (caddr fantasma) (cadr vector-anterior)))))
              (if (not (equal? (member ponto-destino caminho-temp) #f))
                  (begin
                    (set! fantasma (list (car fantasma) (+ (cadr fantasma) (car vector-anterior)) (+ (caddr fantasma) (cadr vector-anterior))))
                    (list fantasma vector-anterior))
                  (pode-mover?))))))))

(define gerar-vector
  (lambda ()
    (let ((direccao-vector (+ 1 (random 4))))
      (cond ((= direccao-vector 1) (list 0 5))      ;Cima
            ((= direccao-vector 2) (list 0 -5))     ;Baixo
            ((= direccao-vector 3) (list 5 0))      ;Direita
            ((= direccao-vector 4) (list -5 0)))))) ;Esquerda

(define distancia-fantasma-pacman
  (lambda (fantasma)
    (list (car fantasma) (sqrt (+ (expt (- (cadr fantasma) (car centro-pac)) 2) (expt (- (caddr fantasma) (car centro-pac)) 2))))))

(define persegue-pacman
  (lambda (fantasma vector-anterior)
    (letrec ((pode-mover?
              (lambda ()
                (let* ((vector-novo (gerar-vector))
                       (ponto-destino-novo (list (+ (cadr fantasma) (car vector-novo)) (+ (caddr fantasma) (cadr vector-novo)))))
                  (if (not (equal? (member ponto-destino-novo caminho-temp) #f))
                      (begin
                        (set! fantasma (list (car fantasma) (+ (cadr fantasma) (car vector-novo)) (+ (caddr fantasma) (cadr vector-novo))))
                        (list fantasma vector-novo))
                      (pode-mover?))))))
      (if (not (equal? (member (list (+ (cadr fantasma) (car vector-anterior)) (+ (caddr fantasma) (cadr vector-anterior))) caminho-temp) #f))
          (list (list (car fantasma) (+ (cadr fantasma) (car vector-anterior)) (+ (caddr fantasma) (cadr vector-anterior))) vector-anterior)
          (begin ;Tem que ocorrer uma mudanca de sentido
            (cond ((and (> (cadr centro-pac) (caddr fantasma)) (not (equal? (member (list (cadr fantasma) (+ (caddr fantasma) 5)) caminho-temp) #f)))
                   (list (list (car fantasma) (cadr fantasma) (+ (caddr fantasma) 5)) (list 5 0)))
                  ((and (< (cadr centro-pac) (caddr fantasma)) (not (equal? (member (list (cadr fantasma) (- (caddr fantasma) 5)) caminho-temp) #f)))
                   (list (list (car fantasma) (cadr fantasma) (- (caddr fantasma) 5)) (list -5 0)))
                  ((and (> (car centro-pac) (cadr fantasma)) (not (equal? (member (list (+ (cadr fantasma) 5) (caddr fantasma)) caminho-temp) #f)))
                   (list (list (car fantasma) (+ (cadr fantasma) 5) (caddr fantasma)) (list 0 5)))
                  ((and (< (car centro-pac) (cadr fantasma)) (not (equal? (member (list (- (cadr fantasma) 5) (caddr fantasma)) caminho-temp) #f)))
                   (list (list (car fantasma) (- (cadr fantasma) 5) (caddr fantasma)) (list 0 -5)))
                  (else (pode-mover?))))))))

(define desenhar-pacman
  (lambda (cor-pac)
    (cor cor-pac)
    (pinta-oval (list (list (- (car centro-pac) 8) (+ (cadr centro-pac) 8)) 
                      (list (+ (car centro-pac) 8) (- (cadr centro-pac) 8))))
    (cor 0)
    (cond ((equal? direita? #t) (pinta (list centro-pac (list (+ (car centro-pac) 10) (+ (cadr centro-pac) (* (tan (degrees->radians 60)) 5)))
                                             (list (+ (car centro-pac) 10) (- (cadr centro-pac) (* (tan (degrees->radians 60)) 5))))))
          ((equal? esquerda? #t) (pinta (list centro-pac (list (- (car centro-pac) 10) (+ (cadr centro-pac) (* (tan (degrees->radians 60)) 5)))
                                              (list (- (car centro-pac) 10) (- (cadr centro-pac) (* (tan (degrees->radians 60)) 5))))))
          ((equal? baixo? #t) (pinta (list centro-pac (list (+ (car centro-pac) (* (tan (degrees->radians 60)) 5)) (- (cadr centro-pac) 10))
                                           (list (- (car centro-pac) (* (tan (degrees->radians 60)) 5)) (- (cadr centro-pac) 10)))))
          ((equal? cima? #t) (pinta (list centro-pac (list (+ (car centro-pac) (* (tan (degrees->radians 60)) 5)) (+ (cadr centro-pac) 10))
                                          (list (- (car centro-pac) (* (tan (degrees->radians 60)) 5)) (+ (cadr centro-pac) 10))))))))

(define desenhar-fantasma
  (lambda (fantasma cor-fant)
    (cor cor-fant)
    ;Desenha a cabeca do fantasma
    (pinta-oval (list (list (- (cadr fantasma) 7) (+ (caddr fantasma) 7))
                      (list (+ (cadr fantasma) 6) (- (caddr fantasma) 6))))
    ;Desenha a parte inferior do fantasma
    (pinta (list (list (- (cadr fantasma) 8) (caddr fantasma)) (list (+ (cadr fantasma) 8) (caddr fantasma))
                 (list (+ (cadr fantasma) 8) (- (caddr fantasma) 8)) (list (- (cadr fantasma) 8) (- (caddr fantasma) 8))
                 (list (- (cadr fantasma) 8) (caddr fantasma))))))

(define colisoes-pac-fant
  (lambda (fantasma)
    (let ((distancia (sqrt (+ (expt (- (cadr fantasma) (car centro-pac)) 2) (expt (- (caddr fantasma) (cadr centro-pac)) 2)))))
      (if (< distancia 16) ;Para evitar incoerencias posso aumentar ate 34. Neste ponto, como medida de optimizacao,
          (begin           ;verifico apenas se se encontra a uma distancia passivel de colidir com a parte inferior do fantasma, ja que e a maior. Assim elimino a partida varios calculos desnecessarios
            (cond ((< (cadr centro-pac) (caddr fantasma)) (morre-pacman))
                  ((< distancia 15) (morre-pacman))))))))

(define bolas
  (lambda ()
    (letrec ((loop-y
              (lambda (y)
                (if (not (<= y 0))
                    (begin
                      (loop-x 40 y)
                      (loop-y (- y 25))))))
             
             (loop-x
              (lambda (x y)
                (if (not (>= x 475))
                    (begin
                      (if (or (not (equal? (member (list x y) caminho-temp) #f))) ;Se o ponto existir na lista
                          (begin
                            (append! lista-bolas (list (list x y)))
                            (loop-x (+ x 25) y))
                          (loop-x (+ x 25) y)))))))
      
      (loop-y 560)
      (set! lista-bolas (cddr lista-bolas)) ;Para eliminar os 0 0 da inicializacao da lista
      (desenha-lista-bolas lista-bolas))))

(define desenha-lista-bolas
  (lambda (lista) ;Eu sei que esta variavel e global, mas da jeito passar como argumento, para nao perder dados nos cdr's
    (cor 24)
    (if (not (null? lista))
        (begin
          (pinta-oval (list (list (- (caar lista) 3) (+ (cadar lista) 3)) (list (+ (caar lista) 3) (- (cadar lista) 3))))
          (desenha-lista-bolas (cdr lista))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Funcoes de desenho dos niveis - Por Luis Teofilo                                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; BLOCOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;As medidas de cada bloco são de 25 por 25
;Nome dos blocos:
;0  blk        n/a       n/a            n/a          n/a                   n/a
;1  lve-d      linha     vertical       exterior     direita               simples
;2  lve-e      linha     vertical       exterior     esquerda              simples
;3  lhe-s      linha     horizontal     exterior     superior              simples
;4  lhe-i      linha     horizontal     exterior     inferior              simples
;5  ce-se      canto     n/a            exterior     superior esquerdo     simples
;6  ce-sd      canto     n/a            exterior     superior direito      simples
;7  ce-ie      canto     n/a            exterior     inferior esquerdo     simples
;8  ce-id      canto     n/a            exterior     inferior direito      simples
;9  lhe-sa     linha     horizontal     exterior     superior              apagada
;10 lhe-ia     linha     horizontal     exterior     inferior              apagada
;11 lve-da     linha     vertical       exterior     direita               apagada
;12 lve-ea     linha     vertical       exterior     esquerda              apagada
;13 lvi        linha     vertical       interior     meio                  simples
;14 lvi-da     linha     vertical       interior     direita               apagada
;15 lvi-ea     linha     vertical       interior     esquerda              apagada
;16 lhi        linha     horizontal     interior     meio                  simples
;17 lhi-ia     linha     horizontal     interior     inferior              apagada
;18 lhi-sa     linha     horizontal     interior     superior              apagada
;19 ci-s       curva     n/a            interior     superior              simples
;20 ci-i       curva     n/a            interior     inferiro              simples
;21 ci-d       curva     n/a            interior     direita               simples
;22 ci-e       curva     n/a            interior     esquerda              simples
;23 ci-se      canto     n/a            interior     superior esquerdo     simples
;24 ci-sd      canto     n/a            interior     superior direito      simples
;25 ci-id      canto     n/a            interior     inferior direito      simples
;26 ci-ie      canto     n/a            interior     inferior esquerdo     simples

(define desenha-bloco
  (lambda (codigo x y)
    (cond
      ((= codigo 1)(lve-d x y))
      ((= codigo 2)(lve-e x y))
      ((= codigo 3)(lhe-s x y))
      ((= codigo 4)(lhe-i x y))
      ((= codigo 5)(ce-se x y))
      ((= codigo 6)(ce-sd x y))
      ((= codigo 7)(ce-ie x y))
      ((= codigo 8)(ce-id x y))
      ((= codigo 9)(lhe-sa x y))
      ((= codigo 10)(lhe-ia x y))
      ((= codigo 11)(lve-da x y))
      ((= codigo 12)(lve-ea x y))
      ((= codigo 13)(lvi x y))
      ((= codigo 14)(lvi-da x y))
      ((= codigo 15)(lvi-ea x y))
      ((= codigo 16)(lhi x y))
      ((= codigo 17)(lhi-ia x y))
      ((= codigo 18)(lhi-sa x y))
      ((= codigo 19)(ci-s x y))
      ((= codigo 20)(ci-i x y))
      ((= codigo 21)(ci-d x y))
      ((= codigo 22)(ci-e x y))
      ((= codigo 23)(ci-se x y))
      ((= codigo 24)(ci-sd x y))
      ((= codigo 25)(ci-id x y))
      ((= codigo 26)(ci-ie x y))
      ((= codigo 27)(l-d x y))
      ((= codigo 28)(l-e x y))
      ((= codigo 29)(lb-e x y))
      ((= codigo 30)(lb-d x y))
      ((= codigo 31)(porta x y))
      )))

(define lve-d
  (lambda (x y)
    (cor 2)
    (desenha (list (list (+ 18 x) y) (list (+ 18 x) (+ 25 y))))
    (desenha (list (list (+ 19 x) y) (list (+ 19 x) (+ 25 y))))
    (desenha (list (list (+ 24 x) y) (list (+ 24 x) (+ 25 y))))
    (desenha (list (list (+ 25 x) y) (list (+ 25 x) (+ 25 y))))
    ))

(define lve-e
  (lambda (x y)
    (cor 2)
    (desenha (list (list (+ 0 x) y) (list (+ 0 x) (+ 25 y))))
    (desenha (list (list (+ 1 x) y) (list (+ 1 x) (+ 25 y))))
    (desenha (list (list (+ 6 x) y) (list (+ 6 x) (+ 25 y))))
    (desenha (list (list (+ 7 x) y) (list (+ 7 x) (+ 25 y))))
    ))

(define lhe-s
  (lambda (x y)
    (cor 2)
    (desenha (list (list x (+ 25 y)) (list (+ 25 x) (+ 25 y))))
    (desenha (list (list x (+ 24 y)) (list (+ 25 x) (+ 24 y))))
    (desenha (list (list x (+ 19 y)) (list (+ 25 x) (+ 19 y))))
    (desenha (list (list x (+ 18 y)) (list (+ 25 x) (+ 18 y)))) 
    ))

(define lhe-i
  (lambda (x y)
    (cor 2)
    (desenha (list (list x (+ 0 y)) (list (+ 25 x) (+ 0 y))))
    (desenha (list (list x (+ 1 y)) (list (+ 25 x) (+ 1 y))))
    (desenha (list (list x (+ 6 y)) (list (+ 25 x) (+ 6 y))))
    (desenha (list (list x (+ 7 y)) (list (+ 25 x) (+ 7 y)))) 
    ))

(define ce-se
  (lambda (x y)
    (cor 2)
    (desenha (list (list (+ 24 x) (+ 25 y)) (list (+ 25 x) (+ 25 y))))
    (desenha (list (list (+ 24 x) (+ 24 y)) (list (+ 25 x) (+ 24 y))))
    (desenha (list (list (+ 19 x) (+ 25 y)) (list (+ 25 x) (+ 19 y))))
    (desenha (list (list (+ 18 x) (+ 25 y)) (list (+ 25 x) (+ 18 y))))
    ))

(define ce-sd
  (lambda (x y)
    (cor 2)
    (desenha (list (list (+ 1 x) (+ 25 y)) (list (+ 0 x) (+ 25 y))))
    (desenha (list (list (+ 1 x) (+ 24 y)) (list (+ 0 x) (+ 24 y))))
    (desenha (list (list (+ 7 x) (+ 25 y)) (list (+ 0 x) (+ 19 y))))
    (desenha (list (list (+ 6 x) (+ 25 y)) (list (+ 0 x) (+ 18 y))))
    ))

(define ce-ie
  (lambda (x y)
    (cor 2)
    (desenha (list (list (+ 1 x) (+ 0 y)) (list (+ 0 x) (+ 0 y))))
    (desenha (list (list (+ 0 x) (+ 1 y)) (list (+ 1 x) (+ 1 y))))
    (desenha (list (list (+ 0 x) (+ 5 y)) (list (+ 5 x) (+ 0 y))))
    (desenha (list (list (+ 0 x) (+ 6 y)) (list (+ 6 x) (+ 0 y))))  
    ))

(define ce-id
  (lambda (x y)
    (cor 2)
    (desenha (list (list (+ 24 x) (+ 0 y)) (list (+ 25 x) (+ 0 y))))
    (desenha (list (list (+ 24 x) (+ 1 y)) (list (+ 25 x) (+ 1 y))))
    (desenha (list (list (+ 19 x) (+ 0 y)) (list (+ 25 x) (+ 7 y))))
    (desenha (list (list (+ 18 x) (+ 0 y)) (list (+ 25 x) (+ 6 y))))
    ))

(define lhe-sa
  (lambda (x y)
    (lhe-s x y)
    (cor 0)
    (desenha (list (list (+ 2 x) (+ 25 y)) (list (+ 23 x) (+ 25 y))))
    (desenha (list (list (+ 2 x) (+ 24 y)) (list (+ 23 x) (+ 24 y))))
    ))

(define lhe-ia
  (lambda (x y)
    (lhe-i x y)
    (cor 0)
    (desenha (list (list (+ 2 x) (+ 0 y)) (list (+ 23 x) (+ 0 y))))
    (desenha (list (list (+ 2 x) (+ 1 y)) (list (+ 23 x) (+ 1 y))))
    ))

(define lve-da
  (lambda (x y)
    (lve-d x y)
    (cor 0)
    (desenha (list (list (+ 24 x) (+ 2 y)) (list (+ 24 x) (+ 23 y))))
    (desenha (list (list (+ 25 x) (+ 2 y)) (list (+ 25 x) (+ 23 y))))
    ))

(define lve-ea
  (lambda (x y)
    (lve-e x y)
    (cor 0)
    (desenha (list (list (+ 0 x) (+ 2 y)) (list (+ 0 x) (+ 23 y))))
    (desenha (list (list (+ 1 x) (+ 2 y)) (list (+ 1 x) (+ 23 y))))
    ))

(define lvi
  (lambda (x y)
    (cor 2)
    (desenha (list (list x y) (list x (+ 25 y))))
    (desenha (list (list (+ 1 x) y) (list (+ 1 x) (+ 25 y))))
    (desenha (list (list (+ 24 x) y) (list (+ 24 x) (+ 25 y))))
    (desenha (list (list (+ 25 x) y) (list (+ 25 x) (+ 25 y))))
    ))

(define lvi-da
  (lambda (x y)
    (lvi x y)
    (cor 0)
    (desenha (list (list (+ 24 x) (+ 2 y)) (list (+ 24 x) (+ 23 y))))
    (desenha (list (list (+ 25 x) (+ 2 y)) (list (+ 25 x) (+ 23 y))))
    ))

(define lvi-ea
  (lambda (x y)
    (lvi x y)
    (cor 0)
    (desenha (list (list (+ 0 x) (+ 2 y)) (list (+ 0 x) (+ 23 y))))
    (desenha (list (list (+ 1 x) (+ 2 y)) (list (+ 1 x) (+ 23 y))))
    ))

(define lhi
  (lambda (x y)
    (cor 2)
    (desenha (list (list x y) (list (+ 25 x) y)))
    (desenha (list (list x (+ 1 y)) (list (+ 25 x) (+ 1 y))))
    (desenha (list (list x (+ 24 y)) (list (+ 25 x) (+ 24 y))))
    (desenha (list (list x (+ 25 y)) (list (+ 25 x) (+ 25 y))))
    ))

(define lhi-ia
  (lambda (x y)
    (lhi x y)
    (cor 0)
    (desenha (list (list (+ 2 x) y) (list (+ 23 x) y)))
    (desenha (list (list (+ 2 x) (+ 1 y)) (list (+ 23 x) (+ 1 y))))
    ))

(define lhi-sa
  (lambda (x y)
    (lhi x y)
    (cor 0)
    (desenha (list (list (+ 2 x) (+ 24 y)) (list (+ 23 x) (+ 24 y))))
    (desenha (list (list (+ 2 x) (+ 25 y)) (list (+ 23 x) (+ 25 y))))
    ))

(define ci-s
  (lambda (x y)
    (lvi x y)
    (cor 2)
    (desenha (list (list x (+ 25 y)) (list (+ 25 x) (+ 25 y))))
    (desenha (list (list x (+ 24 y)) (list (+ 25 x) (+ 24 y))))
    ))

(define ci-i
  (lambda (x y)
    (lvi x y)
    (cor 2)
    (desenha (list (list x (+ 0 y)) (list (+ 25 x) (+ 0 y))))
    (desenha (list (list x (+ 1 y)) (list (+ 25 x) (+ 1 y))))
    ))

(define ci-d
  (lambda (x y)
    (lhi x y)
    (cor 2)
    (desenha (list (list (+ 25 x) y) (list (+ 25 x) (+ 25 y))))
    (desenha (list (list (+ 24 x) y) (list (+ 24 x) (+ 25 y))))
    ))

(define ci-e
  (lambda (x y)
    (lhi x y)
    (cor 2)
    (desenha (list (list (+ 0 x) y) (list (+ 0 x) (+ 25 y))))
    (desenha (list (list (+ 1 x) y) (list (+ 1 x) (+ 25 y))))
    ))

(define ci-se
  (lambda (x y)
    (lvi-da x y)
    (cor 2)
    (desenha (list (list (+ 0 x) (+ 25 y)) (list (+ 25 x) (+ 25 y))))
    (desenha (list (list (+ 0 x) (+ 24 y)) (list (+ 25 x) (+ 24 y))))
    ))

(define ci-sd
  (lambda (x y)
    (lvi-ea x y)
    (cor 2)
    (desenha (list (list (+ 0 x) (+ 25 y)) (list (+ 25 x) (+ 25 y))))
    (desenha (list (list (+ 0 x) (+ 24 y)) (list (+ 25 x) (+ 24 y))))
    ))

(define ci-id
  (lambda (x y)
    (lvi-ea x y)
    (cor 2)
    (desenha (list (list (+ 0 x) (+ 0 y)) (list (+ 25 x) (+ 0 y))))
    (desenha (list (list (+ 0 x) (+ 1 y)) (list (+ 25 x) (+ 1 y))))
    ))

(define ci-ie
  (lambda (x y)
    (lvi-da x y)
    (cor 2)
    (desenha (list (list (+ 0 x) (+ 0 y)) (list (+ 25 x) (+ 0 y))))
    (desenha (list (list (+ 0 x) (+ 1 y)) (list (+ 25 x) (+ 1 y))))
    ))

(define l-d
  (lambda (x y)
    (cor 2)
    (desenha (list (list (+ 0 x) (+ 25 y)) (list (+ 25 x) (+ 25 y))))
    (desenha (list (list (+ 0 x) (+ 24 y)) (list (+ 25 x) (+ 24 y))))
    (desenha (list (list (+ 25 x) (+ 0 y)) (list (+ 25 x) (+ 25 y))))
    (desenha (list (list (+ 24 x) (+ 0 y)) (list (+ 24 x) (+ 25 y))))
    
    (desenha (list (list (+ 0 x) (+ 19 y)) (list (+ 19 x) (+ 19 y))))
    (desenha (list (list (+ 0 x) (+ 18 y)) (list (+ 19 x) (+ 18 y))))
    (desenha (list (list (+ 19 x) (+ 0 y)) (list (+ 19 x) (+ 19 y))))
    (desenha (list (list (+ 18 x) (+ 0 y)) (list (+ 18 x) (+ 19 y))))
    ))

(define l-e
  (lambda (x y)
    (cor 2)
    (desenha (list (list (+ 0 x) (+ 25 y)) (list (+ 25 x) (+ 25 y))))
    (desenha (list (list (+ 0 x) (+ 24 y)) (list (+ 25 x) (+ 24 y))))
    (desenha (list (list (+ 0 x) (+ 0 y)) (list (+ 0 x) (+ 25 y))))
    (desenha (list (list (+ 1 x) (+ 0 y)) (list (+ 1 x) (+ 25 y))))
    (desenha (list (list (+ 7 x) (+ 19 y)) (list (+ 25 x) (+ 19 y))))
    (desenha (list (list (+ 7 x) (+ 18 y)) (list (+ 25 x) (+ 18 y))))
    (desenha (list (list (+ 6 x) (+ 0 y)) (list (+ 6 x) (+ 19 y))))
    (desenha (list (list (+ 7 x) (+ 0 y)) (list (+ 7 x) (+ 19 y))))
    ))

(define lb-e
  (lambda (x y)
    (cor 2)
    (desenha (list (list (+ 0 x) (+ 0 y)) (list (+ 0 x) (+ 25 y))))
    (desenha (list (list (+ 1 x) (+ 0 y)) (list (+ 1 x) (+ 25 y))))
    (desenha (list (list (+ 6 x) (+ 6 y)) (list (+ 6 x) (+ 25 y))))
    (desenha (list (list (+ 7 x) (+ 7 y)) (list (+ 7 x) (+ 25 y))))
    (desenha (list (list (+ 6 x) (+ 6 y)) (list (+ 25 x) (+ 6 y))))
    (desenha (list (list (+ 6 x) (+ 7 y)) (list (+ 25 x) (+ 7 y))))
    (desenha (list (list (+ 0 x) (+ 0 y)) (list (+ 25 x) (+ 0 y))))
    (desenha (list (list (+ 0 x) (+ 1 y)) (list (+ 25 x) (+ 1 y))))
    ))

(define lb-d
  (lambda (x y)
    (cor 2)
    (desenha (list (list (+ 25 x) (+ 0 y)) (list (+ 25 x) (+ 25 y))))
    (desenha (list (list (+ 24 x) (+ 0 y)) (list (+ 24 x) (+ 25 y))))
    (desenha (list (list (+ 19 x) (+ 6 y)) (list (+ 19 x) (+ 25 y))))
    (desenha (list (list (+ 18 x) (+ 7 y)) (list (+ 18 x) (+ 25 y))))
    (desenha (list (list (+ 19 x) (+ 6 y)) (list (+ 0 x) (+ 6 y))))
    (desenha (list (list (+ 19 x) (+ 7 y)) (list (+ 0 x) (+ 7 y))))
    (desenha (list (list (+ 0 x) (+ 0 y)) (list (+ 25 x) (+ 0 y))))
    (desenha (list (list (+ 0 x) (+ 1 y)) (list (+ 25 x) (+ 1 y))))
    ))

(define porta
  (lambda (x y)
    (cor 24)
    (desenha (list (list (+ 1 x) (+ 20 y)) (list (+ 24 x) (+  20 y))))
    (desenha (list (list (+ 1 x) (+ 21 y)) (list (+ 24 x) (+  21 y))))
    (desenha (list (list (+ 1 x) (+ 22 y)) (list (+ 24 x) (+  22 y))))
    (desenha (list (list (+ 1 x) (+ 23 y)) (list (+ 24 x) (+  23 y))))
    
    (cor 2)
    (desenha (list (list (+ 24 x) (+ 20 y)) (list (+ 25 x) (+  20 y))))
    (desenha (list (list (+ 24 x) (+ 21 y)) (list (+ 25 x) (+  21 y))))
    (desenha (list (list (+ 24 x) (+ 22 y)) (list (+ 25 x) (+  22 y))))
    (desenha (list (list (+ 24 x) (+ 23 y)) (list (+ 25 x) (+  23 y))))
    (desenha (list (list (+ 24 x) (+ 24 y)) (list (+ 25 x) (+  24 y))))
    (desenha (list (list (+ 24 x) (+ 25 y)) (list (+ 25 x) (+  25 y))))
    (desenha (list (list (+ 24 x) (+ 19 y)) (list (+ 25 x) (+  19 y))))
    (desenha (list (list (+ 24 x) (+ 18 y)) (list (+ 25 x) (+  18 y))))
    (desenha (list (list (+ 0 x) (+ 20 y)) (list (+ 1 x) (+  20 y))))
    (desenha (list (list (+ 0 x) (+ 21 y)) (list (+ 1 x) (+  21 y))))
    (desenha (list (list (+ 0 x) (+ 22 y)) (list (+ 1 x) (+  22 y))))
    (desenha (list (list (+ 0 x) (+ 23 y)) (list (+ 1 x) (+  23 y))))
    (desenha (list (list (+ 0 x) (+ 24 y)) (list (+ 1 x) (+  24 y))))
    (desenha (list (list (+ 0 x) (+ 25 y)) (list (+ 1 x) (+  25 y))))
    (desenha (list (list (+ 0 x) (+ 18 y)) (list (+ 1 x) (+  18 y))))
    (desenha (list (list (+ 0 x) (+ 19 y)) (list (+ 1 x) (+  19 y))))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; FUNCAO QUE LE A LISTA E CONSTROI O NIVEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define desenha-linha-nivel
  (lambda (lista x y)
    (if (not (null? lista))
        (begin
          (desenha-bloco (car lista) x y)
          (desenha-linha-nivel (cdr lista) (+ 25 x) y)
          ))))

(define desenha-nivel
  (lambda (nivel)
    (letrec ((aux
              (lambda (nivel contador linha)
                (if (not (= contador 21))
                    (begin
                      (desenha-linha-nivel (list-ref nivel contador) 0 linha)
                      (aux nivel (add1 contador) (- linha 25)
                           ))))))
      (aux nivel 0 575))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  GLOBAL DO NIVEL ACTUAL E NIVEIS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nivel-1 (list
                 (list 08 04 04 04 04 04 04 04 04 10 04 04 04 04 04 04 04 04 07)
                 (list 01 00 00 00 00 00 00 00 00 13 00 00 00 00 00 00 00 00 02)
                 (list 01 00 22 21 00 22 16 21 00 20 00 22 16 21 00 22 21 00 02)
                 (list 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 02)
                 (list 01 00 22 21 00 19 00 22 16 17 16 21 00 19 00 22 21 00 02)
                 (list 01 00 00 00 00 13 00 00 00 13 00 00 00 13 00 00 00 00 02)
                 (list 05 03 03 27 00 14 16 21 00 20 00 22 16 15 00 28 03 03 06)
                 (list 00 00 00 01 00 13 00 00 00 00 00 00 00 13 00 02 00 00 00)
                 (list 04 04 04 30 00 20 00 28 03 31 03 27 00 20 00 29 04 04 04)
                 (list 00 00 00 00 00 00 00 02 00 00 00 01 00 00 00 00 00 00 00)
                 (list 03 03 03 27 00 19 00 29 04 04 04 30 00 19 00 28 03 03 03)
                 (list 00 00 00 01 00 13 00 00 00 00 00 00 00 13 00 02 00 00 00)
                 (list 08 04 04 30 00 20 00 22 16 17 16 21 00 20 00 29 04 04 07)
                 (list 01 00 00 00 00 00 00 00 00 13 00 00 00 00 00 00 00 00 02)
                 (list 01 00 22 24 00 22 16 21 00 20 00 22 16 21 00 23 21 00 02)
                 (list 01 00 00 13 00 00 00 00 00 00 00 00 00 00 00 13 00 00 02)
                 (list 11 21 00 20 00 19 00 22 16 17 16 21 00 19 00 20 00 22 12)
                 (list 01 00 00 00 00 13 00 00 00 13 00 00 00 13 00 00 00 00 02)
                 (list 01 00 22 16 16 18 16 21 00 20 00 22 16 18 16 16 21 00 02)
                 (list 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 02)
                 (list 05 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 06)
                 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  FUNCAO DO CAMINHO PARA DEFINIR COLISOES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define loop-analise-path
  (lambda (nivel count caminho-booleano)
    (if (= count (- (length nivel) 1))
        caminho-booleano
        (loop-analise-path nivel (+ count 1) (append caminho-booleano (list (linha-path nivel count)))))))

(define linha-path
  (lambda (nivel linha)
    (letrec ((aux
              (lambda (nivel linha-nivel contador nova-lista)
                (if (= contador 19)
                    nova-lista
                    (if (= (list-ref linha-nivel contador) 0)
                        (aux nivel linha-nivel (add1 contador) (append nova-lista (list (ways nivel linha contador))))
                        (aux nivel linha-nivel (add1 contador) (append nova-lista (list (list 'parede))))
                        )))))
      (aux nivel (list-ref nivel linha) 0 (list)))))


;O procedimento ways devolve uma lista de boleanos
;O primeiro booleano corresponde se pode mover para cima
;O segundo para baixo
;O terceiro para a direita
;O quarto para a esquerda

(define ways
  (lambda (nivel linha-nivel contador)
    (append (list (vai-cima? nivel linha-nivel contador))
            (list (vai-baixo? nivel linha-nivel contador))
            (list (vai-direita? nivel linha-nivel contador))
            (list (vai-esquerda? nivel linha-nivel contador))
            )))

(define vai-cima?
  (lambda (nivel linha-nivel contador)
    (if (= linha-nivel 0)
        #f
        (if (= (list-ref (list-ref nivel (- linha-nivel 1)) contador) 0)
            #t
            #f
            ))))

(define vai-baixo?
  (lambda (nivel linha-nivel contador)
    (if (= linha-nivel 20)
        #f
        (if (= (list-ref (list-ref nivel (+ linha-nivel 1)) contador) 0)
            #t
            #f
            ))))

(define vai-direita?
  (lambda (nivel linha-nivel contador)
    (if (= contador 18)
        #f
        (if (= (list-ref (list-ref nivel linha-nivel) (+ contador 1)) 0)
            #t
            #f
            ))))

(define vai-esquerda?
  (lambda (nivel linha-nivel contador)
    (if (= contador 0)
        #f
        (if (= (list-ref (list-ref nivel linha-nivel) (- contador 1)) 0)
            #t
            #f
            ))))

(define waypoints
  (lambda (nivel)
    (letrec ((aux
              (lambda (nivel contador nova-lista)
                (if (= contador 21)
                    nova-lista
                    (aux nivel (add1 contador) (append nova-lista (list (linha-path nivel contador))))))))
      (aux nivel 0 (list)))))

(define verificar-posicao-pacman
  (lambda (nivel-actual)
    (let ((lr-x (truncate (/ (car centro-pac) 25)))
          (lr-y (truncate (- 19 (/ (cadr centro-pac) 25)))))
      (posicao-pacman-lista nivel-actual lr-x lr-y))))

;Devolve a lista booleana correspondente a posicao do pacman
(define posicao-pacman-lista
  (lambda (nivel-actual lr-x lr-y)
    (list-ref (list-ref nivel-actual lr-y) lr-x)))

;(define caminho2 (list
;                 (list 265 160) (list 270 160) (list 275 160) (list 280 160) (list 285 160) (list 290 160)
;                 (list 295 160) (list 300 160) (list 305 160) (list 310 160) (list 315 160) (list 320 160)
;                 (list 325 160) (list 330 160) (list 325 160) (list 300 160) (list 300 160) (list 300 160)))

(define caminho-temp '((260 115) (255 115) (250 115) (245 115) (240 115) (235 115) (230 115) (225 115) (220 115) (215 115) (210 115) (205 115) (200 115) (195 115) (190 115) (185 115) (180 115) (175 115) (170 115) (165 115) (160 115) (155 115) (150 115) (145 115) (150 115) (155 115) (160 115) (165 115) (170 115) (175 115) (180 115) (185 115) (190 115) (195 115) (200 115) (205 115) (210 115) (215 115) (220 115) (225 115) (230 115) (235 115) (240 115) (245 115) (250 115) (255 115) (260 115) (265 115) (270 115) (275 115) (280 115) (285 115) (290 115) (295 115) (300 115) (305 115) (310 115) (315 115) (320 115) (325 115) (330 115) (335 115) (340 115) (345 115) (350 115) (355 115) (360 115) (365 115) (370 115) (375 115) (380 115) (385 115) (390 115) (395 115) (400 115) (405 115) (410 115) (415 115) (420 115) (425 115) (430 115) (435 115) (440 115) (440 120) (440 125) (440 130) (440 135) (440 140) (440 145) (440 150) (440 155) (440 160) (440 165) (435 165) (430 165) (425 165) (420 165) (415 165) (410 165) (405 165) (400 165) (395 165) (390 165) (385 165) (380 165) (375 165) (370 165) (365 165) (360 165) (360 170) (360 175) (360 180) (360 185) (360 190) (360 195) (360 200) (360 205) (360 210) (360 215) (360 220) (360 225) (360 230) (360 235) (360 240) (360 245) (360 250) (360 255) (360 260) (360 265) (360 270) (360 275) (360 280) (360 285) (360 290) (360 295) (360 300) (360 305) (360 310) (360 315) (360 320) (360 325) (360 330) (360 335) (360 340) (360 345) (360 350) (360 355) (360 360) (360 365) (360 370) (360 375) (360 380) (360 385) (360 390) (360 395) (360 400) (360 405) (360 410) (360 415) (360 420) (360 425) (360 430) (360 435) (360 440) (360 445) (360 450) (360 455) (360 460) (360 465) (360 470) (360 475) (360 480) (360 485) (360 490) (360 495) (360 500) (360 505) (360 510) (360 515) (360 520) (360 525) (360 530) (360 535) (360 540) (360 545) (360 550) (365 550) (365 545) (365 540) (365 535) (365 530) (365 525) (365 520) (365 515) (365 510) (365 505) (270 115) (275 115) (280 115) (285 115) (290 115) (295 115) (300 115) (305 115) (310 115) (315 115) (320 115) (325 115) (330 115) (335 115) (340 115) (345 115) (350 115) (355 115) (360 115) (365 115) (370 115) (375 115) (380 115) (385 115) (390 115) (395 115) (400 115) (405 115) (410 115) (415 115) (420 115) (425 115) (430 115) (435 115) (440 115) (440 120) (440 125) (440 130) (440 135) (440 140) (440 145) (440 150) (440 155) (440 160) (440 165) (435 165) (430 165) (425 165) (420 165) (415 165) (410 165) (405 165) (400 165) (395 165) (390 165) (385 165) (380 165) (375 165) (370 165) (365 165) (360 165) (360 170) (360 175) (360 180) (360 185) (360 190) (360 195) (360 200) (360 205) (360 210) (360 215) (360 220) (360 225) (360 230) (360 235) (360 240) (360 245) (360 250) (360 255) (365 255) (365 250) (365 245) (365 240) (365 235) (365 230) (365 225) (365 220) (365 215) (365 210) (365 205) (365 200) (365 195) (365 190) (365 185) (365 180) (365 175) (365 170) (365 165) (365 160) (370 160) (375 160) (380 160) (385 160) (390 160) (395 160) (400 160) (405 160) (410 160) (415 160) (420 160) (425 160) (430 160) (435 160) (435 155) (435 150) (435 145) (435 140) (435 135) (435 130) (435 125) (435 120) (435 115) (430 115) (425 115) (420 115) (415 115) (410 115) (405 115) (400 115) (395 115) (390 115) (385 115) (380 115) (375 115) (370 115) (365 115) (360 115) (355 115) (350 115) (345 115) (340 115) (335 115) (330 115) (325 115) (320 115) (315 115) (310 115) (305 115) (300 115) (295 115) (290 115) (285 115) (280 115) (275 115) (270 115) (265 115) (260 115) (255 115) (250 115) (245 115) (240 115) (235 115) (230 115) (225 115) (220 115) (215 115) (210 115) (205 115) (200 115) (195 115) (190 115) (185 115) (180 115) (175 115) (170 115) (165 115) (160 115) (155 115) (150 115) (145 115) (140 115) (135 115) (130 115) (125 115) (120 115) (115 115) (110 115) (105 115) (100 115) (95 115) (90 115) (85 115) (80 115) (75 115) (70 115) (65 115) (60 115) (55 115) (50 115) (45 115) (40 115) (35 115) (35 120) (35 125) (35 130) (35 135) (35 140) (35 145) (35 150) (35 155) (35 160) (35 165) (40 165) (45 165) (50 165) (55 165) (60 165) (65 165) (70 165) (75 165) (80 165) (85 165) (90 165) (95 165) (100 165) (105 165) (110 165) (115 165) (115 170) (115 175) (115 180) (115 185) (115 180) (115 175) (115 170) (115 165) (115 160) (110 160) (105 160) (100 160) (95 160) (90 160) (85 160) (80 160) (75 160) (70 160) (65 160) (60 160) (55 160) (50 160) (45 160) (40 160) (35 160) (35 155) (35 150) (35 145) (35 140) (35 135) (35 130) (35 125) (35 120) (35 115) (35 110) (40 110) (45 110) (50 110) (55 110) (60 110) (65 110) (70 110) (75 110) (80 110) (85 110) (90 110) (95 110) (100 110) (105 110) (110 110) (115 110) (120 110) (125 110) (130 110) (135 110) (140 110) (145 110) (150 110) (155 110) (160 110) (165 110) (170 110) (175 110) (180 110) (185 110) (190 110) (195 110) (200 110) (205 110) (210 110) (210 115) (210 120) (210 125) (210 130) (210 135) (210 140) (210 145) (210 150) (210 155) (210 160) (210 165) (205 165) (200 165) (195 165) (190 165) (185 165) (180 165) (175 165) (170 165) (165 165) (160 165) (160 170) (160 175) (160 180) (160 185) (160 190) (160 195) (160 200) (160 205) (160 210) (155 210) (155 215) (150 215) (145 215) (140 215) (135 215) (130 215) (125 215) (120 215) (115 215) (110 215) (110 210) (110 205) (110 200) (110 195) (110 190) (110 185) (110 180) (110 175) (110 170) (110 165) (110 160) (115 160) (115 165) (115 170) (110 170) (110 175) (110 180) (110 185) (110 190) (110 195) (110 200) (110 205) (110 210) (110 215) (110 220) (110 225) (110 230) (110 235) (110 240) (110 245) (110 250) (110 255) (110 260) (110 265) (110 270) (110 275) (110 280) (110 285) (110 290) (110 295) (110 300) (110 305) (110 310) (110 315) (110 320) (110 325) (110 330) (110 335) (110 340) (110 345) (110 350) (110 355) (110 360) (110 365) (110 370) (110 375) (110 380) (110 385) (110 390) (110 395) (110 400) (110 405) (110 410) (110 415) (110 420) (110 425) (110 430) (110 435) (110 440) (110 445) (110 450) (110 455) (110 460) (110 465) (110 470) (110 475) (110 480) (110 485) (110 490) (110 495) (110 500) (110 505) (110 510) (110 515) (110 520) (110 525) (110 530) (110 535) (110 540) (110 545) (110 550) (110 555) (110 560) (265 120) (265 125) (265 130) (265 135) (265 140) (265 145) (265 150) (265 155) (265 160) (265 165) (270 165) (275 165) (280 165) (285 165) (290 165) (295 165) (300 165) (305 165) (310 165) (315 165) (315 170) (315 175) (315 180) (315 185) (315 190) (315 195) (315 200) (315 205) (315 210) (315 215) (310 215) (305 215) (300 215) (295 215) (290 215) (285 215) (280 215) (275 215) (270 215) (265 215) (260 215) (255 215) (250 215) (245 215) (240 215) (235 215) (230 215) (225 215) (220 215) (215 215) (210 215) (210 220) (210 225) (210 230) (210 235) (210 240) (210 245) (210 250) (210 255) (210 260) (210 265) (205 265) (200 265) (195 265) (190 265) (185 265) (180 265) (175 265) (170 265) (165 265) (160 265) (155 265) (150 265) (145 265) (140 265) (135 265) (130 265) (125 265) (120 265) (115 265) (110 265) (110 260) (115 260) (115 265) (115 270) (115 275) (115 280) (115 285) (115 290) (115 295) (115 300) (115 305) (115 310) (115 315) (115 320) (115 325) (115 330) (115 335) (115 340) (115 345) (115 350) (115 355) (115 360) (115 365) (115 370) (115 375) (115 380) (115 385) (115 390) (115 395) (115 400) (115 405) (115 410) (115 415) (115 420) (115 425) (115 430) (115 435) (115 440) (115 445) (115 450) (115 455) (115 460) (115 465) (110 465) (105 465) (100 465) (95 465) (90 465) (85 465) (80 465) (75 465) (70 465) (65 465) (60 465) (55 465) (50 465) (45 465) (40 465) (40 470) (40 475) (40 480) (40 485) (40 490) (40 495) (40 500) (40 505) (40 510) (40 515) (40 520) (40 515) (40 510) (40 505) (40 500) (40 495) (40 490) (40 485) (40 480) (40 475) (40 470) (40 465) (45 465) (50 465) (55 465) (60 465) (65 465) (70 465) (75 465) (80 465) (85 465) (90 465) (95 465) (100 465) (105 465) (110 465) (115 465) (115 460) (115 455) (115 450) (115 445) (115 440) (115 435) (115 430) (115 425) (115 420) (115 415) (115 410) (115 405) (115 400) (115 395) (115 390) (115 385) (115 380) (115 375) (115 370) (115 365) (115 360) (115 355) (115 350) (115 345) (115 340) (115 335) (115 330) (115 325) (115 320) (115 315) (115 310) (115 305) (115 310) (115 315) (115 320) (115 325) (115 330) (115 335) (115 340) (115 345) (115 350) (115 355) (115 360) (115 365) (115 370) (115 375) (115 380) (115 385) (115 390) (115 395) (115 400) (115 405) (115 410) (115 415) (115 420) (115 425) (115 430) (115 435) (115 440) (115 445) (115 450) (115 455) (115 460) (115 465) (115 470) (115 475) (115 480) (115 485) (115 490) (115 495) (115 500) (115 505) (115 510) (115 515) (115 520) (115 525) (115 530) (115 535) (115 540) (115 545) (115 550) (115 555) (115 560) (110 560) (105 560) (100 560) (95 560) (90 560) (85 560) (80 560) (75 560) (70 560) (65 560) (60 560) (55 560) (50 560) (45 560) (40 560) (35 560) (35 555) (35 550) (35 545) (35 540) (35 535) (35 530) (35 525) (35 520) (35 515) (35 510) (35 505) (35 500) (35 495) (35 490) (35 485) (35 480) (35 475) (35 470) (35 465) (35 460) (40 460) (45 460) (50 460) (55 460) (60 460) (65 460) (70 460) (75 460) (80 460) (85 460) (90 460) (95 460) (100 460) (105 460) (110 460) (115 460) (115 455) (115 460) (115 465) (115 470) (115 475) (115 480) (115 485) (115 490) (115 495) (115 500) (115 505) (115 510) (120 510) (125 510) (130 510) (135 510) (140 510) (145 510) (150 510) (155 510) (160 510) (165 510) (170 510) (165 510) (165 505) (165 500) (165 495) (165 490) (165 485) (165 480) (165 475) (165 470) (165 465) (165 460) (160 460) (160 465) (160 470) (160 475) (160 480) (160 485) (160 490) (160 495) (160 500) (160 495) (160 490) (160 485) (160 480) (160 475) (160 470) (160 475) (160 480) (160 475) (160 480) (160 485) (160 490) (160 495) (160 500) (160 505) (160 510) (160 505) (160 500) (160 495) (160 490) (160 485) (160 480) (160 475) (160 470) (160 465) (160 460) (165 460) (170 460) (175 460) (180 460) (185 460) (190 460) (195 460) (200 460) (205 460) (210 460) (215 460) (215 455) (215 450) (215 445) (215 440) (215 435) (215 430) (215 425) (215 420) (215 415) (215 410) (220 410) (225 410) (230 410) (235 410) (240 410) (245 410) (250 410) (255 410) (260 410) (265 410) (270 410) (275 410) (280 410) (285 410) (290 410) (295 410) (300 410) (305 410) (310 410) (315 410) (315 405) (315 400) (315 395) (315 390) (315 385) (315 380) (315 375) (315 370) (315 365) (315 360) (315 355) (315 350) (315 345) (315 340) (315 335) (315 330) (315 325) (315 320) (315 315) (315 310) (315 305) (315 300) (315 295) (315 290) (315 285) (315 280) (315 275) (315 270) (315 265) (315 260) (310 260) (305 260) (300 260) (295 260) (290 260) (285 260) (280 260) (275 260) (270 260) (265 260) (260 260) (260 255) (260 250) (260 245) (260 240) (260 235) (260 230) (260 225) (260 220) (260 215) (260 210) (265 210) (270 210) (275 210) (280 210) (285 210) (290 210) (295 210) (300 210) (305 210) (310 210) (315 210) (315 205) (315 200) (315 195) (315 190) (315 185) (315 180) (315 175) (315 170) (315 165) (315 160) (310 160) (305 160) (300 160) (295 160) (290 160) (285 160) (280 160) (275 160) (270 160) (265 160) (270 115) (275 115) (280 115) (285 115) (290 115) (295 115) (300 115) (305 115) (310 115) (315 115) (320 115) (325 115) (330 115) (335 115) (340 115) (345 115) (350 115) (355 115) (360 115) (365 115) (370 115) (375 115) (380 115) (385 115) (390 115) (395 115) (400 115) (405 115) (410 115) (415 115) (420 115) (425 115) (430 115) (435 115) (440 115) (440 120) (440 125) (440 130) (440 135) (440 140) (440 145) (440 150) (440 155) (440 160) (440 165) (435 165) (430 165) (425 165) (420 165) (415 165) (410 165) (405 165) (400 165) (395 165) (390 165) (385 165) (380 165) (375 165) (370 165) (365 165) (360 165) (360 160) (365 160) (370 160) (375 160) (380 160) (385 160) (390 160) (395 160) (400 160) (405 160) (410 160) (415 160) (420 160) (425 160) (430 160) (435 160) (435 155) (435 150) (435 145) (435 140) (435 135) (435 130) (435 125) (435 120) (435 115) (435 120) (435 125) (435 130) (435 135) (435 140) (435 145) (435 150) (435 155) (435 160) (435 165) (430 165) (425 165) (420 165) (415 165) (415 170) (415 175) (415 180) (415 185) (415 190) (415 195) (415 200) (415 205) (415 210) (415 215) (410 215) (410 210) (410 205) (410 200) (410 195) (410 190) (410 185) (410 180) (410 175) (410 170) (410 165) (410 160) (410 165) (410 170) (410 175) (410 180) (410 185) (410 190) (410 195) (410 200) (410 205) (410 210) (415 210) (420 210) (425 210) (430 210) (435 210) (440 210) (440 215) (440 220) (440 225) (440 230) (440 235) (440 240) (440 245) (440 250) (440 255) (440 260) (440 265) (435 265) (435 260) (435 255) (435 250) (435 245) (435 240) (435 235) (435 230) (435 225) (435 220) (435 215) (430 215) (425 215) (420 215) (415 215) (410 215) (265 120) (265 125) (265 130) (265 135) (265 140) (265 145) (265 150) (265 155) (265 160) (265 165) (270 165) (275 165) (280 165) (285 165) (290 165) (295 165) (300 165) (305 165) (310 165) (315 165) (315 170) (315 175) (315 180) (315 185) (315 190) (315 195) (315 200) (315 205) (315 210) (315 215) (320 215) (325 215) (330 215) (335 215) (340 215) (345 215) (350 215) (355 215) (360 215) (360 220) (360 225) (360 230) (360 235) (360 240) (360 245) (360 250) (360 255) (360 260) (360 265) (365 265) (365 260) (365 265) (365 260) (365 265) (365 260) (365 255) (365 260) (370 260) (375 260) (380 260) (385 260) (390 260) (395 260) (400 260) (405 260) (410 260) (415 260) (420 260) (425 260) (430 260) (435 260) (440 260) (440 265) (435 265) (430 265) (425 265) (420 265) (415 265) (410 265) (405 265) (400 265) (395 265) (390 265) (385 265) (380 265) (375 265) (370 265) (365 265) (265 120) (265 125) (265 130) (265 135) (265 140) (265 145) (265 150) (265 155) (265 160) (265 165) (270 165) (275 165) (280 165) (285 165) (290 165) (295 165) (300 165) (305 165) (310 165) (315 165) (315 170) (315 175) (315 180) (315 185) (315 190) (315 195) (315 200) (315 205) (315 210) (315 215) (320 215) (325 215) (330 215) (335 215) (340 215) (345 215) (350 215) (355 215) (360 215) (365 215) (365 220) (365 225) (365 230) (365 235) (365 240) (365 245) (365 250) (365 255) (360 255) (360 260) (360 265) (360 260) (355 260) (350 260) (345 260) (340 260) (335 260) (330 260) (325 260) (320 260) (315 260) (310 260) (310 265) (315 265) (320 265) (325 265) (330 265) (335 265) (340 265) (345 265) (350 265) (355 265) (360 265) (265 120) (265 125) (265 130) (265 135) (265 140) (265 145) (265 150) (265 155) (265 160) (265 165) (270 165) (275 165) (280 165) (285 165) (290 165) (295 165) (300 165) (305 165) (310 165) (315 165) (315 170) (315 175) (315 180) (315 185) (315 190) (315 195) (315 200) (315 205) (315 210) (315 215) (320 215) (325 215) (330 215) (335 215) (340 215) (345 215) (350 215) (355 215) (360 215) (365 215) (365 220) (365 225) (365 230) (365 235) (365 240) (365 245) (365 250) (365 255) (360 255) (360 260) (360 265) (360 260) (355 260) (350 260) (345 260) (340 260) (335 260) (330 260) (325 260) (320 260) (315 260) (310 260) (310 265) (315 265) (320 265) (325 265) (330 265) (335 265) (340 265) (345 265) (350 265) (355 265) (360 265) (265 110) (260 110) (260 115) (265 120) (265 115) (265 110) (260 110) (260 115) (260 120) (260 125) (260 130) (260 135) (260 140) (260 145) (260 150) (260 155) (260 160) (260 165) (265 165) (265 160) (265 155) (265 150) (265 145) (265 140) (265 135) (265 130) (265 125) (265 120) (265 115) (265 110) (260 110) (255 110) (250 110) (245 110) (240 110) (235 110) (230 110) (225 110) (220 110) (215 110) (210 110) (210 115) (210 120) (210 125) (210 130) (210 135) (210 140) (210 145) (210 150) (210 155) (210 160) (210 165) (215 165) (215 160) (215 155) (215 150) (215 145) (215 140) (215 135) (215 130) (215 125) (215 120) (215 115) (215 110) (210 110) (205 110) (200 110) (195 110) (190 110) (185 110) (180 110) (175 110) (170 110) (165 110) (160 110) (155 110) (150 110) (145 110) (140 110) (135 110) (130 110) (125 110) (120 110) (115 110) (110 110) (105 110) (100 110) (95 110) (90 110) (85 110) (80 110) (75 110) (70 110) (65 110) (60 110) (55 110) (50 110) (45 110) (40 110) (35 110) (35 115) (35 120) (35 125) (35 130) (35 135) (35 140) (35 145) (35 150) (35 155) (35 160) (35 165) (40 165) (40 160) (40 155) (40 150) (40 145) (40 140) (40 135) (40 130) (40 125) (40 120) (40 115) (45 115) (50 115) (55 115) (60 115) (65 115) (70 115) (75 115) (80 115) (85 115) (90 115) (95 115) (100 115) (105 115) (110 115) (115 115) (120 115) (125 115) (130 115) (135 115) (140 115) (145 115) (150 115) (155 115) (160 115) (165 115) (170 115) (175 115) (180 115) (185 115) (190 115) (195 115) (200 115) (205 115) (210 115) (215 115) (220 115) (225 115) (230 115) (235 115) (240 115) (245 115) (250 115) (255 115) (260 115) (265 115) (270 115) (275 115) (280 115) (285 115) (290 115) (295 115) (300 115) (305 115) (310 115) (315 115) (320 115) (325 115) (330 115) (335 115) (340 115) (345 115) (350 115) (355 115) (360 115) (365 115) (370 115) (365 115) (360 115) (355 115) (350 115) (345 115) (340 115) (335 115) (330 115) (325 115) (320 115) (315 115) (310 115) (305 115) (300 115) (295 115) (290 115) (285 115) (280 115) (275 115) (270 115) (265 115) (265 120) (265 125) (265 130) (265 135) (265 140) (265 145) (265 150) (265 155) (265 150) (265 145) (265 140) (265 135) (265 130) (265 125) (265 120) (265 115) (265 110) (270 110) (265 110) (260 110) (260 115) (260 120) (260 125) (260 130) (260 135) (260 140) (260 145) (260 140) (260 135) (260 130) (260 125) (260 120) (260 115) (260 110) (265 110) (270 110) (275 110) (280 110) (285 110) (290 110) (295 110) (300 110) (305 110) (310 110) (315 110) (320 110) (325 110) (330 110) (335 110) (340 110) (345 110) (350 110) (355 110) (360 110) (365 110) (370 110) (375 110) (380 110) (385 110) (390 110) (395 110) (400 110) (405 110) (410 110) (415 110) (420 110) (425 110) (430 110) (435 110) (440 110) (440 115) (435 115) (430 115) (425 115) (420 115) (415 115) (410 115) (405 115) (400 115) (395 115) (390 115) (385 115) (380 115) (375 115) (370 115) (375 115) (380 115) (385 115) (390 115) (395 115) (400 115) (405 115) (410 115) (415 115) (420 115) (425 115) (430 115) (435 115) (435 120) (435 125) (435 130) (435 135) (435 140) (435 145) (435 150) (435 155) (435 160) (430 160) (425 160) (420 160) (415 160) (410 160) (405 160) (400 160) (395 160) (390 160) (385 160) (380 160) (375 160) (370 160) (365 160) (265 110) (270 110) (275 110) (280 110) (285 110) (290 110) (295 110) (300 110) (305 110) (310 110) (315 110) (320 110) (315 110) (310 110) (305 110) (300 110) (295 110) (290 110) (285 110) (280 110) (275 110) (270 110) (265 110) (260 110) (260 115) (260 120) (260 125) (260 130) (260 135) (260 140) (260 145) (265 145) (265 140) (265 135) (265 130) (265 125) (265 120) (265 115) (270 115) (275 115) (280 115) (285 115) (290 115) (295 115) (300 115) (305 115) (310 115) (315 115) (320 115) (325 115) (330 115) (335 115) (340 115) (345 115) (350 115) (355 115) (360 115) (365 115) (370 115) (375 115) (380 115) (385 115) (390 115) (395 115) (400 115) (405 115) (410 115) (415 115) (420 115) (425 115) (430 115) (435 115) (435 120) (435 125) (435 130) (435 135) (435 140) (435 145) (435 150) (435 155) (435 160) (430 160) (425 160) (420 160) (415 160) (410 160) (405 160) (400 160) (395 160) (390 160) (385 160) (380 160) (375 160) (370 160) (365 160) (360 160) (360 165) (360 170) (360 175) (360 180) (360 185) (360 190) (360 195) (360 200) (360 205) (360 210) (360 215) (360 220) (360 225) (360 230) (360 235) (360 240) (360 245) (360 250) (360 255) (360 260) (360 265) (360 270) (360 275) (360 280) (360 285) (360 290) (360 295) (360 300) (360 305) (360 310) (360 315) (360 320) (360 325) (360 330) (360 335) (360 340) (360 345) (360 350) (360 355) (360 360) (360 365) (360 370) (360 375) (360 380) (360 385) (360 390) (360 395) (360 400) (360 405) (360 410) (360 415) (360 420) (360 425) (360 430) (360 435) (360 440) (360 445) (360 450) (360 455) (360 460) (360 465) (360 470) (360 475) (360 480) (360 485) (360 490) (360 495) (360 500) (360 505) (360 510) (360 515) (360 520) (360 515) (355 515) (350 515) (345 515) (340 515) (335 515) (265 110) (260 110) (255 110) (250 110) (245 110) (240 110) (235 110) (230 110) (225 110) (220 110) (215 110) (210 110) (205 110) (200 110) (195 110) (190 110) (185 110) (180 110) (175 110) (170 110) (165 110) (160 110) (155 110) (150 110) (145 110) (140 110) (135 110) (130 110) (125 110) (120 110) (115 110) (110 110) (105 110) (100 110) (95 110) (90 110) (85 110) (80 110) (75 110) (70 110) (65 110) (60 110) (55 110) (50 110) (45 110) (40 110) (35 110) (35 115) (35 110) (40 110) (45 110) (50 110) (55 110) (60 110) (65 110) (70 110) (75 110) (80 110) (85 110) (90 110) (95 110) (100 110) (105 110) (110 110) (115 110) (120 110) (125 110) (130 110) (135 110) (140 110) (145 110) (150 110) (155 110) (160 110) (165 110) (170 110) (175 110) (180 110) (185 110) (190 110) (195 110) (200 110) (205 110) (210 110) (215 110) (210 110) (205 110) (200 110) (195 110) (190 110) (185 110) (180 110) (185 110) (190 110) (195 110) (200 110) (205 110) (210 110) (215 110) (210 110) (210 115) (210 120) (210 125) (210 130) (210 135) (210 140) (210 145) (210 150) (210 155) (215 155) (215 150) (215 145) (215 140) (215 135) (215 130) (215 125) (215 120) (210 120) (210 125) (210 130) (210 135) (210 140) (210 145) (210 150) (210 155) (210 160) (205 160) (200 160) (195 160) (190 160) (185 160) (180 160) (175 160) (170 160) (165 160) (160 160) (160 165) (160 170) (160 175) (160 180) (160 185) (160 190) (160 195) (160 200) (160 205) (160 210) (165 210) (165 205) (165 200) (165 195) (165 190) (165 185) (165 180) (165 175) (165 170) (165 165) (165 160) (170 160) (170 165) (175 165) (180 165) (185 165) (190 165) (195 165) (200 165) (205 165) (210 165) (215 165) (220 165) (215 165) (210 165) (205 165) (200 165) (195 165) (190 165) (185 165) (180 165) (175 165) (170 165) (165 165) (165 170) (165 175) (165 180) (165 185) (165 190) (165 195) (165 200) (165 205) (165 210) (160 210) (155 210) (150 210) (145 210) (140 210) (135 210) (130 210) (125 210) (120 210) (115 210) (115 205) (115 200) (115 195) (115 190) (115 185) (115 180) (115 175) (115 170) (115 165) (115 160) (110 160) (105 160) (100 160) (95 160) (90 160) (85 160) (80 160) (75 160) (70 160) (65 160) (60 160) (55 160) (50 160) (45 160) (40 160) (35 160) (35 165) (40 165) (45 165) (50 165) (55 165) (60 165) (65 165) (70 165) (75 165) (80 165) (85 165) (90 165) (95 165) (100 165) (105 165) (100 165) (95 165) (90 165) (85 165) (80 165) (75 165) (70 165) (65 165) (60 165) (65 165) (65 170) (265 120) (260 120) (255 120) (255 115) (250 115) (245 115) (240 115) (235 115) (230 115) (225 115) (220 115) (220 110) (215 110) (210 110) (205 110) (200 110) (195 110) (190 110) (185 110) (180 110) (175 110) (170 110) (165 110) (160 110) (155 110) (150 110) (145 110) (150 110) (155 110) (160 110) (165 110) (170 110) (175 110) (180 110) (185 110) (190 110) (195 110) (200 110) (205 110) (210 110) (215 110) (220 110) (225 110) (230 110) (235 110) (240 110) (245 110) (250 110) (255 110) (260 110) (265 110) (270 110) (275 110) (280 110) (285 110) (290 110) (295 110) (300 110) (305 110) (310 110) (315 110) (320 110) (325 110) (330 110) (335 110) (340 110) (345 110) (350 110) (355 110) (360 110) (365 110) (370 110) (375 110) (380 110) (385 110) (390 110) (395 110) (400 110) (405 110) (410 110) (415 110) (420 110) (425 110) (430 110) (435 110) (440 110) (440 115) (440 120) (440 125) (440 130) (440 135) (440 140) (440 145) (440 150) (440 155) (440 160) (435 160) (430 160) (425 160) (420 160) (415 160) (410 160) (405 160) (400 160) (395 160) (390 160) (385 160) (380 160) (375 160) (370 160) (365 160) (360 160) (360 165) (360 170) (360 175) (360 180) (360 185) (360 190) (360 195) (360 200) (360 205) (360 210) (360 215) (360 220) (360 225) (360 230) (360 235) (360 240) (360 245) (360 250) (360 255) (360 260) (360 265) (360 270) (360 275) (360 280) (360 285) (360 290) (360 295) (360 300) (360 305) (360 310) (360 315) (360 320) (360 325) (360 330) (360 335) (360 340) (360 345) (360 350) (360 355) (360 360) (360 365) (360 370) (360 375) (360 380) (360 385) (360 390) (360 395) (360 400) (360 405) (360 410) (360 415) (360 420) (360 425) (360 430) (360 435) (360 440) (360 445) (360 450) (360 455) (360 460) (360 465) (360 470) (360 475) (360 480) (360 485) (360 490) (360 495) (360 500) (360 505) (360 510) (355 510) (355 515) (355 510) (350 510) (345 510) (340 510) (335 510) (330 510) (325 510) (320 510) (315 510) (315 505) (315 500) (315 495) (315 490) (315 485) (315 480) (315 475) (315 470) (315 465) (315 460) (310 460) (305 460) (300 460) (295 460) (290 460) (285 460) (280 460) (275 460) (270 460) (265 460) (260 460) (260 455) (260 450) (260 445) (260 440) (260 435) (260 430) (260 425) (260 420) (260 415) (260 410) (255 410) (250 410) (245 410) (240 410) (235 410) (230 410) (225 410) (220 410) (215 410) (210 410) (205 410) (205 415) (210 415) (215 415) (215 420) (215 425) (215 430) (215 435) (215 440) (215 445) (215 450) (215 455) (215 460) (210 460) (205 460) (200 460) (195 460) (190 460) (185 460) (180 460) (175 460) (170 460) (165 460) (160 460) (160 465) (160 470) (160 475) (160 480) (160 485) (160 490) (160 495) (160 500) (160 505) (160 510) (160 515) (155 515) (160 515) (165 515) (170 515) (175 515) (180 515) (185 515) (190 515) (195 515) (200 515) (205 515) (210 515) (210 520) (210 525) (210 530) (210 535) (210 540) (210 545) (210 550) (210 555) (210 560) (210 565) (215 565) (215 560) (215 555) (215 550) (215 545) (215 540) (215 535) (215 530) (215 525) (215 520) (215 515) (265 120) (265 125) (265 130) (265 135) (265 140) (265 145) (265 150) (265 155) (265 160) (265 165) (270 165) (275 165) (280 165) (285 165) (290 165) (295 165) (300 165) (305 165) (310 165) (315 165) (315 170) (315 175) (315 180) (315 185) (315 190) (315 195) (315 200) (315 205) (315 210) (315 215) (315 210) (315 205) (315 210) (310 210) (305 210) (300 210) (295 210) (290 210) (285 210) (280 210) (275 210) (270 210) (265 210) (260 210) (255 210) (250 210) (245 210) (240 210) (235 210) (230 210) (225 210) (220 210) (215 210) (210 210) (205 210) (200 210) (195 210) (190 210) (185 210) (180 210) (175 210) (170 210) (165 210) (160 210) (155 210) (150 210) (145 210) (140 210) (135 210) (130 210) (125 210) (120 210) (115 210) (115 205) (115 200) (115 195) (115 190) (115 185) (115 180) (115 175) (115 170) (115 165) (110 165) (105 165) (100 165) (95 165) (90 165) (85 165) (80 165) (75 165) (70 165) (65 165) (60 165) (55 165) (50 165) (45 165) (40 165) (35 165) (35 160) (35 155) (35 150) (35 145) (35 140) (35 135) (35 130) (35 125) (35 120) (35 115) (35 110) (40 110) (45 110) (50 110) (55 110) (60 110) (65 110) (70 110) (75 110) (80 110) (85 110) (90 110) (95 110) (100 110) (105 110) (110 110) (115 110) (120 110) (125 110) (130 110) (135 110) (140 110) (145 110) (150 110) (155 110) (160 110) (165 110) (170 110) (175 110) (180 110) (185 110) (190 110) (195 110) (200 110) (205 110) (210 110) (215 110) (220 110) (225 110) (230 110) (235 110) (240 110) (245 110) (250 110) (255 110) (260 110) (265 110) (270 110) (275 110) (280 110) (285 110) (290 110) (295 110) (300 110) (305 110) (310 110) (315 110) (320 110) (325 110) (330 110) (335 110) (340 110) (345 110) (350 110) (355 110) (360 110) (365 110) (370 110) (375 110) (380 110) (385 110) (390 110) (395 110) (400 110) (405 110) (410 110) (415 110) (420 110) (425 110) (430 110) (435 110) (440 110) (440 115) (440 120) (440 125) (440 130) (440 135) (440 140) (440 145) (440 150) (440 155) (440 160) (435 160) (430 160) (425 160) (420 160) (415 160) (410 160) (405 160) (400 160) (395 160) (390 160) (385 160) (380 160) (375 160) (370 160) (365 160) (360 160) (360 165) (360 170) (360 175) (360 180) (360 185) (360 190) (360 195) (360 200) (360 205) (360 210) (360 215) (360 220) (360 225) (360 230) (360 235) (360 240) (360 245) (360 250) (360 255) (360 260) (360 265) (360 270) (360 275) (360 280) (360 285) (360 290) (360 295) (360 300) (360 305) (360 310) (360 315) (360 320) (360 325) (360 330) (360 335) (360 340) (360 345) (360 350) (360 355) (360 360) (360 365) (360 370) (360 375) (360 380) (360 385) (360 390) (360 395) (360 400) (360 405) (360 410) (360 415) (360 420) (360 425) (360 430) (360 435) (360 440) (360 445) (360 450) (360 455) (360 460) (360 465) (360 470) (360 475) (360 480) (360 485) (360 490) (360 495) (360 500) (360 505) (360 510) (360 515) (360 520) (360 525) (360 530) (360 535) (360 540) (360 545) (360 550) (365 550) (365 555) (365 550) (360 550) (360 545) (360 540) (360 535) (360 530) (360 525) (360 520) (360 515) (360 510) (360 515) (360 520) (360 525) (360 530) (360 535) (360 540) (360 545) (360 550) (360 555) (360 560) (360 565) (365 565) (370 565) (375 565) (380 565) (385 565) (390 565) (395 565) (400 565) (405 565) (410 565) (415 565) (410 565) (405 565) (405 560) (400 560) (395 560) (390 560) (385 560) (380 560) (375 560) (370 560) (365 560) (360 560) (365 560) (370 560) (375 560) (380 560) (385 560) (390 560) (395 560) (400 560) (405 560) (410 560) (415 560) (420 560) (425 560) (430 560) (430 565) (425 565) (430 565) (435 565) (440 565) (440 560) (440 555) (440 550) (440 545) (440 540) (440 535) (440 530) (440 525) (440 520) (440 515) (440 510) (440 505) (440 500) (440 495) (440 490) (440 485) (440 480) (440 475) (440 470) (440 465) (440 460) (435 460) (430 460) (425 460) (420 460) (415 460) (410 460) (405 460) (400 460) (395 460) (390 460) (385 460) (380 460) (375 460) (370 460) (365 460) (360 460) (360 455) (360 450) (360 445) (360 440) (360 435) (360 430) (360 425) (360 420) (360 415) (360 410) (360 405) (360 400) (360 395) (360 390) (360 385) (360 380) (360 375) (360 370) (360 365) (360 360) (355 360) (350 360) (345 360) (340 360) (335 360) (330 360) (325 360) (320 360) (315 360) (310 360) (310 355) (310 350) (310 345) (310 340) (310 335) (310 330) (310 325) (310 320) (310 315) (310 310) (310 305) (310 300) (310 295) (310 290) (310 285) (310 280) (310 275) (310 270) (310 265) (310 260) (305 260) (300 260) (295 260) (290 260) (285 260) (280 260) (275 260) (270 260) (265 260) (260 260) (260 255) (260 250) (260 245) (260 240) (260 235) (260 230) (260 225) (260 220) (260 215) (260 210) (265 210) (265 215) (265 220) (265 225) (265 230) (265 235) (265 240) (265 245) (265 250) (260 250) (260 255) (260 250) (260 245) (260 240) (260 235) (265 235) (265 240) (265 245) (265 250) (265 255) (265 260) (270 260) (265 260) (265 265) (260 265) (265 265) (270 265) (275 265) (280 265) (285 265) (290 265) (295 265) (300 265) (305 265) (310 265) (315 265) (320 265) (325 265) (330 265) (335 265) (340 265) (340 260) (335 260) (330 260) (325 260) (320 260) (315 260) (310 260) (305 260) (300 260) (295 260) (290 260) (285 260) (280 260) (275 260) (270 260) (270 255) (265 255) (265 250) (265 245) (265 240) (265 235) (265 230) (265 225) (265 220) (265 215) (270 215) (275 215) (280 215) (285 215) (290 215) (295 215) (300 215) (305 215) (310 215) (315 215) (320 215) (325 215) (330 215) (335 215) (340 215) (345 215) (350 215) (355 215) (360 215) (360 210) (355 210) (350 210) (345 210) (340 210) (335 210) (330 210) (325 210) (320 210) (315 210) (310 210) (305 210) (300 210) (295 210) (290 210) (285 210) (280 210) (275 210) (270 210) (265 210) (260 210) (255 210) (250 210) (245 210) (240 210) (235 210) (230 210) (225 210) (220 210) (215 210) (210 210) (205 210) (200 210) (195 210) (190 210) (185 210) (180 210) (175 210) (170 210) (165 210) (160 210) (155 210) (150 210) (145 210) (140 210) (135 210) (130 210) (125 210) (120 210) (115 210) (115 215) (115 220) (115 225) (115 230) (115 235) (115 240) (115 245) (115 250) (115 255) (115 260) (115 265) (115 270) (115 275) (115 280) (115 285) (115 290) (115 295) (115 300) (115 305) (115 310) (115 315) (115 320) (115 325) (115 330) (115 335) (115 340) (115 345) (115 350) (115 355) (115 360) (115 365) (115 370) (115 375) (115 380) (115 385) (115 390) (115 395) (115 400) (115 405) (115 410) (115 415) (115 420) (115 425) (115 430) (115 435) (115 440) (115 445) (115 450) (115 455) (115 460) (115 465) (115 470) (115 475) (115 480) (115 485) (115 490) (115 495) (115 500) (115 505) (115 510) (115 515) (115 520) (115 525) (115 530) (115 535) (115 540) (115 545) (115 550) (115 555) (115 560) (115 565) (110 565) (105 565) (100 565) (95 565) (90 565) (85 565) (80 565) (75 565) (70 565) (65 565) (60 565) (55 565) (50 565) (45 565) (40 565) (40 560) (40 555) (40 550) (40 545) (40 540) (40 535) (40 530) (40 525) (40 520) (40 515) (40 510) (40 505) (40 500) (40 495) (40 490) (40 485) (40 480) (40 475) (40 470) (40 465) (45 465) (50 465) (55 465) (60 465) (65 465) (70 465) (75 465) (80 465) (85 465) (90 465) (95 465) (100 465) (105 465) (110 465) (115 465) (115 470) (115 475) (115 480) (115 485) (115 490) (115 495) (115 500) (115 505) (115 510) (110 510) (105 510) (100 510) (95 510) (90 510) (85 510) (80 510) (75 510) (70 510) (65 510) (60 510) (55 510) (50 510) (55 510) (60 510) (65 510) (70 510) (75 510) (80 510) (85 510) (90 510) (95 510) (100 510) (105 510) (110 510) (115 510) (120 510) (125 510) (130 510) (135 510) (140 510) (145 510) (150 510) (155 510) (160 510) (165 510) (170 510) (175 510) (180 510) (185 510) (190 510) (195 510) (200 510) (205 510) (210 510) (210 515) (215 515) (215 520) (215 525) (215 530) (215 535) (215 540) (215 545) (215 550) (215 555) (215 560) (215 565) (210 565) (205 565) (200 565) (195 565) (190 565) (185 565) (180 565) (175 565) (170 565) (165 565) (160 565) (155 565) (150 565) (145 565) (140 565) (135 565) (130 565) (125 565) (120 565) (115 565) (110 565) (110 560) (115 560) (120 560) (125 560) (130 560) (135 560) (140 560) (145 560) (150 560) (155 560) (160 560) (165 560) (170 560) (175 560) (180 560) (185 560) (190 560) (195 560) (200 560) (205 560) (205 555) (210 555) (210 550) (210 545) (210 540) (210 535) (210 530) (210 525) (210 520) (210 515) (210 510) (205 510) (200 510) (195 510) (190 510) (185 510) (180 510) (175 510) (170 510) (165 510) (160 510) (155 510) (150 510) (145 510) (140 510) (135 510) (130 510) (125 510) (120 510) (115 510) (110 510) (115 510) (120 510) (125 510) (130 510) (135 510) (140 510) (145 510) (150 510) (155 510) (160 510) (160 505) (160 500) (160 495) (160 490) (160 485) (160 480) (160 475) (160 470) (160 465) (160 460) (165 460) (170 460) (175 460) (180 460) (185 460) (190 460) (195 460) (200 460) (205 460) (210 460) (215 460) (215 455) (215 450) (215 445) (215 440) (215 435) (215 430) (215 425) (215 420) (215 415) (215 410) (210 410) (205 410) (200 410) (195 410) (190 410) (185 410) (180 410) (180 415) (175 415) (170 415) (165 415) (170 415) (175 415) (180 415) (180 410) (175 410) (170 410) (165 410) (160 410) (160 415) (160 410) (160 405) (160 400) (160 395) (160 390) (160 385) (160 380) (160 375) (160 370) (160 365) (160 360) (160 355) (160 350) (160 345) (160 340) (160 335) (160 330) (160 325) (160 320) (160 315) (160 310) (160 305) (160 300) (160 295) (160 290) (160 285) (160 280) (160 275) (160 270) (160 265) (160 260) (155 260) (150 260) (145 260) (140 260) (135 260) (130 260) (125 260) (120 260) (115 260) (110 260) (105 260) (100 260) (95 260) (90 260) (85 260) (80 260) (75 260) (70 260) (65 260) (60 260) (55 260) (50 260) (45 260) (40 260) (35 260) (35 255) (35 250) (35 245) (35 240) (35 235) (35 230) (35 225) (35 220) (35 215) (35 210) (40 210) (45 210) (50 210) (55 210) (60 210) (60 205) (60 200) (60 195) (60 190) (60 185) (60 180) (60 175) (65 175) (65 180) (65 185) (65 190) (65 195) (65 200) (65 205) (65 210) (65 215) (60 215) (55 215) (50 215) (45 215) (40 215) (40 220) (40 225) (40 230) (40 235) (40 240) (40 245) (40 250) (40 255) (40 260) (40 265) (45 265) (50 265) (55 265) (60 265) (65 265) (70 265) (75 265) (80 265) (85 265) (90 265) (95 265) (100 265) (105 265) (110 265) (115 265) (120 265) (125 265) (120 265) (115 265) (110 265) (105 265) (100 265) (95 265) (90 265) (85 265) (80 265) (75 265) (70 265) (65 265) (60 265) (55 265) (50 265) (45 265) (40 265) (35 265) (40 265) (45 265) (50 265) (55 265) (60 265) (65 265) (70 265) (75 265) (80 265) (85 265) (90 265) (95 265) (100 265) (105 265) (110 265) (115 265) (120 265) (125 265) (130 265) (135 265) (140 265) (145 265) (150 265) (155 265) (160 265) (165 265) (170 265) (175 265) (180 265) (185 265) (190 265) (195 265) (200 265) (205 265) (210 265) (215 265) (215 260) (215 255) (215 260) (210 260) (205 260) (200 260) (195 260) (190 260) (185 260) (180 260) (175 260) (170 260) (165 260) (160 260) (155 260) (150 260) (145 260) (140 260) (135 260) (140 260) (145 260) (150 260) (155 260) (160 260) (165 260) (170 260) (175 260) (180 260) (185 260) (190 260) (195 260) (200 260) (205 260) (210 260) (215 260) (215 255) (215 250) (215 245) (215 240) (215 235) (215 230) (215 225) (215 220) (215 215) (215 220) (210 220) (210 225) (210 230) (210 235) (210 240) (210 245) (210 250) (210 255) (210 260) (205 260) (200 260) (195 260) (200 260) (205 260) (210 260) (215 260) (215 255) (215 250) (215 245) (215 240) (215 235) (215 230) (215 225) (215 220) (215 215) (215 210) (210 210) (205 210) (200 210) (195 210) (190 210) (185 210) (180 210) (175 210) (170 210) (165 210) (160 210) (155 210) (150 210) (145 210) (140 210) (135 210) (130 210) (125 210) (125 215) (130 215) (135 215) (140 215) (145 215) (150 215) (155 215) (160 215) (165 215) (170 215) (175 215) (180 215) (185 215) (190 215) (195 215) (200 215) (205 215) (210 215) (215 215) (220 215) (225 215) (230 215) (235 215) (240 215) (245 215) (250 215) (255 215) (260 215) (265 215) (270 215) (275 215) (280 215) (285 215) (290 215) (295 215) (300 215) (305 215) (310 215) (315 215) (320 215) (325 215) (330 215) (335 215) (340 215) (345 215) (350 215) (355 215) (360 215) (365 215) (365 220) (365 225) (365 230) (365 235) (365 240) (365 245) (365 250) (365 255) (365 260) (365 265) (365 270) (365 275) (365 280) (365 285) (365 290) (365 295) (365 300) (365 305) (365 310) (365 315) (365 320) (365 325) (365 330) (365 335) (365 340) (365 345) (365 350) (365 355) (365 360) (365 365) (365 370) (365 375) (365 380) (365 385) (365 390) (365 395) (365 400) (365 405) (365 410) (365 415) (365 420) (365 425) (365 430) (365 435) (365 440) (365 445) (365 450) (365 455) (365 460) (365 465) (365 470) (365 475) (365 480) (365 485) (365 490) (365 495) (365 500) (365 505) (365 510) (365 515) (370 515) (375 515) (380 515) (385 515) (390 515) (395 515) (400 515) (405 515) (410 515) (415 515) (420 515) (425 515) (430 515) (435 515) (440 515) (440 510) (435 510) (430 510) (425 510) (420 510) (415 510) (410 510) (405 510) (400 510) (395 510) (390 510) (385 510) (380 510) (375 510) (370 510) (365 510) (360 510) (355 510) (350 510) (345 510) (340 510) (335 510) (330 510) (325 510) (320 510) (315 510) (310 510) (305 510) (300 510) (295 510) (290 510) (285 510) (280 510) (275 510) (270 510) (265 510) (260 510) (255 510) (250 510) (245 510) (240 510) (235 510) (230 510) (225 510) (220 510) (215 510) (210 510) (205 510) (200 510) (195 510) (190 510) (185 510) (180 510) (175 510) (170 510) (165 510) (160 510) (155 510) (150 510) (145 510) (140 510) (135 510) (130 510) (125 510) (120 510) (115 510) (110 510) (105 510) (100 510) (95 510) (90 510) (85 510) (80 510) (75 510) (70 510) (65 510) (60 510) (55 510) (50 510) (45 510) (40 510) (40 505) (40 500) (40 495) (40 490) (40 495) (40 500) (40 505) (40 510) (40 515) (45 515) (50 515) (55 515) (60 515) (65 515) (70 515) (75 515) (80 515) (85 515) (90 515) (95 515) (100 515) (105 515) (110 515) (115 515) (120 515) (125 515) (130 515) (135 515) (140 515) (145 515) (150 515) (155 515) (160 515) (165 515) (170 515) (175 515) (180 515) (185 515) (190 515) (195 515) (200 515) (205 515) (210 515) (215 515) (220 515) (225 515) (230 515) (235 515) (240 515) (245 515) (250 515) (255 515) (260 515) (265 515) (270 515) (275 515) (280 515) (285 515) (290 515) (295 515) (300 515) (305 515) (310 515) (315 515) (320 515) (325 515) (330 515) (335 515) (340 515) (345 515) (350 515) (355 515) (360 515) (365 515) (370 515) (375 515) (380 515) (385 515) (390 515) (395 515) (400 515) (405 515) (410 515) (415 515) (410 515) (415 515) (420 515) (425 515) (430 515) (435 515) (440 515) (440 520) (440 525) (440 530) (440 535) (440 540) (435 540) (435 545) (435 550) (435 555) (435 550) (435 545) (435 540) (435 535) (435 530) (435 525) (435 520) (435 515) (435 520) (435 525) (435 530) (435 535) (435 540) (435 545) (435 550) (435 555) (435 560) (435 565) (430 565) (425 565) (420 565) (415 565) (410 565) (405 565) (400 565) (395 565) (390 565) (385 565) (380 565) (375 565) (370 565) (365 565) (360 565) (355 565) (350 565) (345 565) (345 560) (340 560) (335 560) (330 560) (325 560) (320 560) (315 560) (310 560) (305 560) (300 560) (295 560) (290 560) (285 560) (280 560) (275 560) (270 560) (265 560) (260 560) (260 565) (265 565) (270 565) (275 565) (280 565) (285 565) (290 565) (295 565) (300 565) (305 565) (310 565) (315 565) (320 565) (325 565) (330 565) (335 565) (340 565) (345 565) (350 565) (345 565) (340 565) (335 565) (330 565) (325 565) (320 565) (315 565) (310 565) (305 565) (300 565) (295 565) (290 565) (285 565) (280 565) (275 565) (270 565) (265 565) (265 560) (265 555) (265 550) (265 545) (265 540) (265 535) (265 530) (265 525) (265 520) (265 515) (265 510) (260 510) (255 510) (255 515) (260 515) (260 520) (260 525) (260 530) (260 535) (260 540) (260 545) (260 550) (260 555) (260 560) (260 565) (260 560) (260 555) (260 550) (260 545) (260 540) (260 535) (260 530) (260 525) (260 520) (260 515) (260 510) (265 510) (270 510) (275 510) (280 510) (285 510) (290 510) (295 510) (300 510) (305 510) (310 510) (315 510) (320 510) (325 510) (320 510) (315 510) (310 510) (310 505) (310 500) (310 495) (310 490) (310 485) (310 480) (310 475) (310 470) (310 465) (310 460) (315 460) (315 465) (315 470) (315 475) (315 480) (315 485) (315 490) (315 495) (315 500) (315 505) (315 510) (315 505) (315 500) (315 495) (315 490) (315 485) (315 480) (315 475) (315 470) (315 465) (315 460) (310 460) (305 460) (300 460) (295 460) (290 460) (285 460) (280 460) (275 460) (270 460) (265 460) (260 460) (260 465) (265 465) (270 465) (275 465) (280 465) (285 465) (290 465) (295 465) (300 465) (305 465) (310 465) (315 465) (315 460) (310 460) (305 460) (300 460) (295 460) (290 460) (285 460) (280 460) (275 460) (270 460) (265 460) (260 460) (260 455) (260 450) (260 445) (260 440) (260 435) (260 430) (260 425) (260 420) (260 415) (260 410) (255 410) (250 410) (245 410) (240 410) (235 410) (230 410) (225 410) (220 410) (215 410) (210 410) (205 410) (200 410) (195 410) (190 410) (185 410) (180 410) (175 410) (170 410) (165 410) (160 410) (160 415) (165 415) (170 415) (175 415) (180 415) (185 415) (190 415) (195 415) (200 415) (205 415) (210 415) (215 415) (220 415) (225 415) (230 415) (235 415) (240 415) (245 415) (250 415) (255 415) (260 415) (265 415) (260 415) (265 415) (265 420) (265 425) (265 430) (265 435) (265 440) (265 445) (265 450) (265 455) (265 460) (265 465) (265 460) (265 455) (265 450) (265 445) (265 440) (265 435) (265 430) (265 425) (265 420) (265 415) (270 415) (275 415) (280 415) (285 415) (290 415) (295 415) (300 415) (305 415) (310 415) (315 415) (315 410) (315 405) (315 400) (315 395) (315 390) (315 385) (315 380) (315 375) (315 370) (315 365) (315 360) (315 355) (315 350) (315 345) (315 340) (315 335) (315 330) (315 325) (315 320) (315 315) (315 310) (310 310) (305 310) (300 310) (295 310) (290 310) (285 310) (280 310) (275 310) (270 310) (265 310) (260 310) (255 310) (250 310) (245 310) (240 310) (235 310) (230 310) (225 310) (220 310) (215 310) (210 310) (205 310) (200 310) (195 310) (190 310) (185 310) (180 310) (175 310) (170 310) (165 310) (160 310) (160 315) (160 320) (160 325) (160 330) (160 335) (160 340) (160 345) (160 350) (160 355) (160 360) (160 365) (160 370) (160 375) (160 380) (160 385) (160 390) (160 395) (160 400) (165 400) (165 395) (165 390) (165 385) (165 380) (165 375) (165 370) (165 365) (165 360) (165 355) (165 350) (165 345) (165 340) (165 335) (165 330) (165 325) (165 320) (165 315) (170 315) (175 315) (180 315) (185 315) (190 315) (195 315) (200 315) (205 315) (210 315) (215 315) (220 315) (225 315) (230 315) (235 315) (240 315) (245 315) (250 315) (255 315) (260 315) (265 315) (270 315) (275 315) (280 315) (285 315) (290 315) (295 315) (300 315) (305 315) (310 315) (315 315) (315 320) (315 325) (315 330) (315 335) (315 340) (315 345) (315 350) (315 355) (315 360) (315 365) (315 370) (315 375) (315 380) (315 385) (315 390) (315 395) (315 400) (315 405) (315 410) (310 410) (305 410) (300 410) (295 410) (290 410) (285 410) (280 410) (275 410) (270 410) (265 410) (260 410) (255 410) (250 410) (245 410) (240 410) (245 410) (250 410) (255 410) (260 410) (265 410) (270 410) (275 410) (280 410) (285 410) (290 410) (295 410) (300 410) (305 410) (310 410) (305 410) (310 410) (310 405) (310 400) (310 395) (310 390) (310 385) (310 380) (310 375) (310 370) (310 365) (310 360) (310 355) (310 350) (310 345) (310 340) (310 335) (310 330) (310 325) (310 320) (310 315) (310 310) (310 305) (310 300) (310 295) (310 290) (310 285) (310 280) (310 275) (310 270) (310 265) (310 260) (315 260) (320 260) (315 260) (315 265) (315 270) (315 275) (315 280) (315 285) (315 290) (315 295) (315 300) (315 305) (315 310) (315 315) (315 320) (315 325) (315 330) (315 325) (315 320) (315 315) (315 310) (315 305) (315 300) (315 295) (315 290) (315 285) (315 280) (315 275) (315 280) (315 285) (315 290) (315 295) (315 300) (315 305) (315 310) (310 310) (305 310) (300 310) (295 310) (290 310) (285 310) (280 310) (275 310) (270 310) (265 310) (260 310) (255 310) (250 310) (245 310) (240 310) (235 310) (230 310) (225 310) (220 310) (215 310) (210 310) (205 310) (200 310) (195 310) (190 310) (185 310) (180 310) (175 310) (170 310) (165 310) (160 310) (160 305) (160 300) (160 295) (160 290) (160 285) (160 280) (160 275) (160 270) (160 265) (160 260) (165 260) (170 260) (175 260) (170 260) (165 260) (160 260) (155 260) (150 260) (145 260) (140 260) (135 260) (140 260) (145 260) (150 260) (155 260) (160 260) (165 260) (165 265) (160 265) (165 265) (165 270) (165 275) (165 280) (165 285) (165 290) (165 295) (165 300) (165 305) (165 310) (165 315) (165 320) (165 325) (165 330) (165 335) (165 340) (165 345) (165 350) (165 355) (165 360) (165 365) (160 365) (160 360) (155 360) (150 360) (145 360) (140 360) (135 360) (130 360) (125 360) (120 360) (115 360) (110 360) (105 360) (100 360) (95 360) (90 360) (85 360) (80 360) (75 360) (70 360) (65 360) (60 360) (55 360) (50 360) (45 360) (40 360) (35 360) (30 360) (25 360) (20 360) (15 360) (10 360) (5 360) (0 360) (-5 360) (-10 360) (-15 360) (-20 360) (-25 360) (-30 360) (-35 360) (-40 360) (-45 360) (-50 360) (-55 360) (-60 360) (-60 365) (-55 365) (-50 365) (-45 365) (-40 365) (-35 365) (-30 365) (-25 365) (-20 365) (-15 365) (-10 365) (-5 365) (0 365) (5 365) (10 365) (15 365) (20 365) (25 365) (30 365) (35 365) (40 365) (45 365) (50 365) (55 365) (60 365) (65 365) (70 365) (75 365) (80 365) (85 365) (90 365) (95 365) (100 365) (105 365) (110 365) (115 365) (120 365) (125 365) (130 365) (135 365) (140 365) (145 365) (150 365) (155 365) (160 365) (165 365) (165 370) (165 375) (165 380) (165 385) (165 390) (165 395) (165 400) (165 405) (165 410) (165 415) (170 415) (175 415) (180 415) (185 415) (190 415) (195 415) (200 415) (205 415) (210 415) (215 415) (220 415) (225 415) (230 415) (235 415) (240 415) (245 415) (250 415) (255 415) (260 415) (265 415) (270 415) (275 415) (280 415) (285 415) (290 415) (295 415) (300 415) (305 415) (310 415) (315 415) (315 410) (315 405) (315 400) (315 395) (315 390) (315 385) (315 380) (315 375) (315 370) (315 365) (315 360) (315 355) (315 350) (315 355) (315 360) (315 355) (315 360) (320 360) (325 360) (330 360) (335 360) (340 360) (345 360) (350 360) (355 360) (360 360) (365 360) (370 360) (375 360) (380 360) (385 360) (390 360) (395 360) (400 360) (405 360) (410 360) (415 360) (420 360) (425 360) (430 360) (435 360) (440 360) (445 360) (450 360) (455 360) (460 360) (465 360) (470 360) (475 360) (480 360) (485 360) (480 360) (475 360) (470 360) (465 360) (460 360) (455 360) (450 360) (445 360) (440 360) (435 360) (430 360) (425 360) (420 360) (415 360) (420 360) (425 360) (430 360) (435 360) (440 360) (445 360) (450 360) (455 360) (460 360) (465 360) (470 360) (475 360) (480 360) (485 360) (490 360) (495 360) (500 360) (505 360) (510 360) (515 360) (520 360) (525 360) (530 360) (535 360) (540 360) (545 360) (550 360) (550 365) (545 365) (540 365) (535 365) (530 365) (525 365) (520 365) (515 365) (510 365) (505 365) (500 365) (495 365) (490 365) (485 365) (480 365) (475 365) (470 365) (465 365) (460 365) (455 365) (450 365) (445 365) (440 365) (435 365) (430 365) (425 365) (420 365) (415 365) (410 365) (405 365) (400 365) (395 365) (390 365) (385 365) (380 365) (375 365) (370 365) (365 365) (360 365) (355 365) (350 365) (345 365) (340 365) (335 365) (330 365) (325 365) (320 365) (315 365) (310 365)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COLISAO COM FANTASMAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define desenha-cordenadas-polares
  (lambda (x y r o)
    (cor 0)
    (desenha (list
                         (list x y)
                         (list (+ x (* r (cos o))) (+ y (* r (sin o))))
                         ))))

(define graus-radianos
  (lambda (o)
    (/ (* o 3.14) 180)))

(define roda
  (lambda (oi of x y raio)
    (cor 24)
    (if (< oi of) 
        (begin
          (desenha-cordenadas-polares x y raio (graus-radianos oi)) ;(desenha-cordenadas-polares x y 12 (graus-radianos oi))
          (roda (+ 0.01 oi) of x y raio)
          )
        )))

(define morre-pacman
  (lambda ()
    (desenhar-pacman 24)
    (desenhar-fantasma fant1 0)
    (desenhar-fantasma fant2 0)
    (desenhar-fantasma fant3 0)
    (desenhar-fantasma fant4 0)
    (morte-sound)
    (cronometro)
    (cond
      ((equal? direita? #t)(roda 30 330 (car centro-pac) (cadr centro-pac) 12))
      ((equal? baixo? #t)(roda 300 600 (car centro-pac) (cadr centro-pac) 12))
      ((equal? esquerda? #t)(roda 210 510 (car centro-pac) (cadr centro-pac) 12))
      ((equal? cima? #t)(roda 120 420 (car centro-pac) (cadr centro-pac) 12)))
    (cronometro)
    (set! vidas (- vidas 1))
    (set! fant1 (list 18 115 300))
    (set! fant2 (list 8 315 315))
    (set! fant3 (list 20 150 210))
    (set! fant4 (list 6 185 310))
    (set! centro-pac (list 265 115))
    (desenha-nivel nivel-1)))

(define morte-sound
  (lambda ()
    (som "bomba1")))
    ;(play-sound "end.wav" #t)))

(define start
  (lambda ()
    (som "poing1")))
    ;(play-sound "start.wav" #t)))

(define eat
  (lambda ()
    #t))
    ;(som "anda1")))
    ;(play-sound "eat.wav" 1)))

(define inicio
  (lambda ()
    (start)
    (inicio-aux 0)))

(define inicio-aux
  (lambda (tempo)
    (if (not (= tempo 5000000))
        (inicio-aux (add1 tempo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; TIRA-BOLAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define retira-elemento-indice-x!
  (lambda (lista elemento)
    (set! pontuacao (+ pontuacao 100))
    (if (and (list? lista) (not (null? elemento)) )
        (if (>= (- elemento 1) (length lista))
            'impossivel
            (letrec ((aux
                      (lambda (lista elemento contador nova-lista)
                        (if (= elemento contador)
                            (append nova-lista (cdr lista))
                            (aux (cdr lista) elemento (add1 contador) (append nova-lista (list (car lista))))
                            ))))
              (set-car! lista (car (aux lista elemento 1 (list))))
              (set-cdr! lista (cdr (aux lista elemento 1 (list))))
              )))))

(define tira-bolas!
  (lambda (lista elemento)
    (cond ((not (equal? (member elemento lista) #f)) (retira-elemento-indice-x! (member elemento lista) 1))
          ((not (equal? (member (list (+ (car elemento) 5) (cadr elemento)) lista) #f)) (retira-elemento-indice-x! (member (list (+ (car elemento) 5) (cadr elemento)) lista) 1))
          ((not (equal? (member (list (- (car elemento) 5) (cadr elemento)) lista) #f)) (retira-elemento-indice-x! (member (list (- (car elemento) 5) (cadr elemento)) lista) 1))
          ((not (equal? (member (list (car elemento) (+ (cadr elemento) 5)) lista) #f)) (retira-elemento-indice-x! (member (list (car elemento) (+ (cadr elemento) 5)) lista) 1))
          ((not (equal? (member (list (car elemento) (- (cadr elemento) 5)) lista) #f)) (retira-elemento-indice-x! (member (list (car elemento) (- (cadr elemento) 5)) lista) 1)))))

(define come-bolas
  (lambda ()
    (if (equal? eat? 10)
        (begin 
          (eat)
          (set! eat? 0))
        (set! eat? (add1 eat?)))
        
    (tira-bolas! lista-bolas (list (car centro-pac) (cadr centro-pac)))
    (tira-bolas! lista-bolas (list (+ 5 (car centro-pac)) (+ 5 (cadr centro-pac))))
    (tira-bolas! lista-bolas (list (- 5 (car centro-pac)) (+ 5 (cadr centro-pac))))
    (tira-bolas! lista-bolas (list (+ 5 (car centro-pac)) (- 5 (cadr centro-pac))))
    (tira-bolas! lista-bolas (list (- 5 (car centro-pac)) (- 5 (cadr centro-pac))))
   
    ))

(define vitoria?
  (lambda ()
    (if (equal? (length lista-bolas) 1)
        (set! vitoria #t))))

(define output-stats
  (lambda ()
    (cor 0)
    (pinta '((0 0) (0 75) (200 75) (200 0) (0 0)))
    (cor 24)
    (move '(0 50))
    (desenha-txt "Pontuacao:")
    (move '(100 50))
    (desenha-txt (number->string pontuacao))
    (move '(0 25))
    (desenha-txt "Vidas:")
    (desenha-pacmans-vida vidas 50)
    ))

(define desenha-pacmans-vida
  (lambda (count x)
    (if (not (equal? 0 count))
        (begin
        (desenha-pacman-vida 24 (list x 30))
        (desenha-pacmans-vida (- count 1) (+ x 30))))))
         
(define desenha-pacman-vida
  (lambda (cor-pac ponto)
    (cor cor-pac)
    (pinta-oval (list (list (- (car ponto) 8) (+ (cadr ponto) 8)) 
                      (list (+ (car ponto) 8) (- (cadr ponto) 8))))
    (cor 0)
    (pinta (list ponto (list (+ (car ponto) 10) (+ (cadr ponto) (* (tan (degrees->radians 60)) 5)))
                                             (list (+ (car ponto) 10) (- (cadr ponto) (* (tan (degrees->radians 60)) 5)))))))
          
(define final
  (lambda (texto)
    (if (equal? texto "VITORIA!!!!!!!!")
        (cor 26)
        (cor 0))
    (roda 0 360 200 300 400)
    (desenhar-fantasma fant1 0)
    (desenhar-fantasma fant2 0)
    (desenhar-fantasma fant3 0)
    (desenhar-fantasma fant4 0)
    (if (equal? texto "VITORIA!!!!!!!!")
        (cor 6)
        (cor 18))
    (set! gameover? #t)
    (move (list 200 300))
    (desenha-txt texto)))

(pacman)