; acesso à abstracção tabuleiro.plus
(require (lib "tabuleiro.scm" "user-feup"))

; -- prepara uma janela gráfica onde será visualizado um tabuleiro -------
(define largura-jan 390)  ; em pixels
(define altura-jan 240)   ; em pixels
(define titulo-jan "experiencia com percursos simples")
;
(define jan-tab (janela largura-jan altura-jan titulo-jan)) 

; -- prepara um tabuleiro onde será visualizado um percurso --------------
(define lado-cel 20)    ; número de pixels do lado da célula quadrada
(define num-cel-x 18)   ; número de células em x
(define num-cel-y 10)   ; número de células em y
(define org-x 15)       ; coordenadas do canto inferior-esquerdo
(define org-y 220)      ; do tabuleiro
;
(define tab-teste (tabuleiro org-x org-y lado-cel num-cel-x  num-cel-y))

; acesso à abstracção naves
(require (lib "naves.scm" "user-feup"))

; ----------------
(define cor-perc 15)         ; verde amarelado

; definição de um percurso
(celulas tab-teste 
         (list (cons 13 6) (cons 12 6) (cons 11 6) (cons 11 5) 
               (cons 11 4) (cons 11 3)
               (cons 11 2) (cons 10 2) (cons 9 2)
               (cons 9 3) (cons 8 3)(cons 7 3) (cons 6 3)
               (cons 5 3) (cons 4 3)(cons 4 3) (cons 4 4)
               (cons 4 5) (cons 4 6)) 
         'l 300 cor-perc)                 ; 'l- limpa com cor-perc

; criação de uma nave
(define n (cria-nave tab-teste 13 6 'o 18 200))
; ---------------------

(require (lib "audio.scm" "user-feup"))

(define segue-o-percurso
  (lambda (nave)
    (frente nave 2)
    (roda-dir nave 1)
    (frente nave 4)
    (roda-esq nave 1)
    (frente nave 2)
    (roda-esq nave 1)
    (frente nave 1)
    (roda-dir nave 1)
    (frente nave 5)
    (roda-esq nave 1)
    (frente nave 3)
    (som "ri3")))
