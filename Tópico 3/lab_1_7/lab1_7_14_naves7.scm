(require (lib "tabuleiro.scm" "user-feup"))

; -- prepara uma janela gráfica onde será visualizado um tabuleiro -------
(define largura-jan 390)  ; em pixels
(define altura-jan 240)   ; em pixels
;(define titulo-jan "experiencia com percursos simples")
(define titulo-jan "experiencia com percursos complexos")
;
(define jan-tab (janela largura-jan altura-jan titulo-jan)) 

; -- prepara um tabuleiro onde será visualizado um percurso --------------
(define lado-cel 15)    ; número de pixels do lado da célula quadrada
(define num-cel-x 24)   ; número de células em x
(define num-cel-y 15)   ; número de células em y
(define org-x 15)       ; coordenadas do canto inferior-esquerdo
(define org-y 235)      ; do tabuleiro
;
(define tab-teste (tabuleiro org-x org-y lado-cel num-cel-x  num-cel-y))

(require (lib "naves.scm" "user-feup"))

(define cor-rasto 18)        ; vermelho - cor do rasto da nave

(define x-nave 5)           	; entre 0 e 29 - coordenada x inicial da nave
(define y-nave 3)           	; entre 0 e 19 - coordenada y inicial da nave
(define ori-inicial 'n) ; pode ser 'n, 's, 'e, 'o - orientação inicial da nave
(define t-resposta 200)        ; tempo de resposta da nave, em ms

; ----------------
(define cor-perc 15)         ; verde amarelado

(celulas tab-teste 
         (list (cons 19 6)(cons 18 6)(cons 17 6)(cons 16 6) (cons 15 6)(cons 14 6) 
               (cons 13 6) (cons 12 6) (cons 11 6) (cons 11 5) 
               (cons 11 4) (cons 11 3)
               (cons 11 2) (cons 10 2) (cons 9 2)
               (cons 8 2) (cons 8 3)(cons 7 3) (cons 6 3)
               (cons 5 3) (cons 4 3)(cons 4 3) (cons 4 4)
               (cons 4 5) (cons 4 6)(cons 4 7)(cons 4 8)
               (cons 5 8) (cons 6 8)(cons 7 8)(cons 8 8)
               (cons 8 9) (cons 8 10)
               (cons 7 10)(cons 6 10) (cons 5 10)(cons 4 10)(cons 3 10)(cons 2 10)
               (cons 2 11)(cons 2 12)(cons 2 13)
               (cons 3 13)(cons 4 13) (cons 5 13)(cons 6 13)(cons 7 13)(cons 8 13)
               (cons 9 13)(cons 9 12) (cons 9 11)(cons 9 10)(cons 9 9)(cons 9 8)
               (cons 9 7)(cons 9 6) (cons 9 5)(cons 9 4)(cons 9 3)(cons 9 2)
               (cons 9 1)(cons 9 0) 
               (cons 10 0)(cons 11 0) (cons 12 0)(cons 13 0)
               (cons 13 1)(cons 13 2)(cons 13 3)(cons 13 4) (cons 13 5)(cons 13 6)
               (cons 13 7)(cons 13 8)(cons 13 9)(cons 13 10) (cons 13 11)(cons 13 12)
               (cons 13 13)
               (cons 14 13)(cons 15 13)(cons 16 13)(cons 17 13) (cons 18 13)(cons 19 13)
               (cons 20 13)(cons 21 13)(cons 22 13)
               (cons 10 6)(cons 10 7)(cons 11 7)(cons 12 7)
               (cons 10 5)(cons 12 5)
               (cons 14 0)(cons 14 1)(cons 12 1)
               (cons 8 0)(cons 8 1)
               (cons 7 0)(cons 7 1)(cons 7 2)
               (cons 3 2)(cons 4 2)(cons 5 2)
               (cons 4 4)(cons 4 5)
               (cons 3 8)(cons 3 9)
               (cons 4 9)(cons 3 9)(cons 2 9)(cons 1 10)(cons 10 6)(cons 0 10)
               (cons 1 14)(cons 2 14)(cons 3 14)(cons 0 14)
               (cons 11 1)
               (cons 8 14) (cons 9 14) (cons 10 14) (cons 11 14) (cons 12 14)
               (cons 13 14) (cons 14 14)) 
         'l 50 cor-perc)                 ; 'l- limpa com cor-perc

(celulas tab-teste 
         (list (cons 11 6)(cons 8 3)(cons 8 8)(cons 8 10) (cons 9 0)(cons 13 0)) 
         'l 300 14)    ; cor de virar à direita

(celulas tab-teste 
         (list (cons 11 2)(cons 8 2)(cons 4 3)(cons 4 8) (cons 2 10)(cons 2 13) 
               (cons 9 13)(cons 13 13))
         'l 300 22)    ; cor de virar à esquerda

(celula	tab-teste 22 13 'l  13) ; cor de chegada

; ---------------------
(define n (cria-nave tab-teste 19 6 'o cor-rasto t-resposta))
