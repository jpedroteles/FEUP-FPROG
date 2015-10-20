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
               (cons 9 13)(cons 10 13) (cons 11 13)(cons 12 13)(cons 13 13)(cons 14 13)
               (cons 15 13)(cons 16 13) (cons 17 13)(cons 18 13)(cons 19 13)(cons 20 13)
               (cons 21 13)(cons 22 13) 
               ) 
         'l 300 cor-perc)                 ; 'l- limpa com cor-perc


(define n (cria-nave tab-teste 19 6 'o cor-rasto t-resposta))

; para produção de sons...
(require (lib "audio.scm" "user-feup"))
; define nova direcção para a nave em função do percurso
; atenção aos valores devolvidos por este procedimento...
(define nova-direccao
  (lambda (nave cor-percurso)
    (cond ((= (ve-frente nave) cor-percurso) 0)
          ((= (ve-dir nave) cor-percurso) 1)
          ((= (ve-tras nave) cor-percurso) 2)
          ((= (ve-esq nave) cor-percurso) 3)
          (else -1))))
;
(define segue-percurso-desconhecido
  (lambda (nave cor-percurso)
    (let ((direccao (nova-direccao nave cor-percurso)))
      (if (= direccao -1)
          (som "ri1")
          (begin
            (cond
              ((= direccao 1)(roda-dir nave 1))
              ((= direccao 2)(roda-dir nave 2))
              ((= direccao 3)(roda-esq nave 1)))
            (frente nave 1)
            (segue-percurso-desconhecido nave cor-percurso))))))

; para testar o procedimento segue-percurso-desconhecido
; (segue-percurso-desconhecido n cor-perc)
          
          