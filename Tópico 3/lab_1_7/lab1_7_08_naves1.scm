; para ter acesso à funcionalidade associada 
; à abstracção tabuleiro
(require (lib "tabuleiro.scm" "user-feup"))

; -- prepara uma janela gráfica onde será visualizado um tabuleiro -------
(define largura-jan 390)  ; largura da janela em pixels
(define altura-jan 240)   ; altura da janela em pixels
(define titulo-jan "experiencia com percursos simples")  ; titulo da janela

; cria uma janela com a designação jan-tab, usando o procedimento janela
(define jan-tab (janela largura-jan altura-jan titulo-jan))

; -- prepara um tabuleiro onde se deslocará a nave --------------
(define lado-cel 20)    ; número de pixels do lado de cada uma das células quadradas
(define num-cel-x 18)   ; número de células em x
(define num-cel-y 10)   ; número de células em y
(define org-x 15)       ; coordenadas, da janela gráfica, do ponto correspondente
(define org-y 220)      ; ao canto superior-esquerdo do tabuleiro

; cria um tabuleiro com a designação tab-teste, usando o procedimento tabuleiro
(define tab-teste (tabuleiro org-x org-y lado-cel num-cel-x  num-cel-y))

; para ter acesso à funcionalidade associada 
; à abstracção naves
(require (lib "naves.scm" "user-feup"))

; a preparação de naves com as quais irá continuar o teste desta abstracção.
(define cor-rasto 18)        ; vermelho - cor do rasto da nave
(define x-nave 5)           	; entre 0 e 17 - coordenada x inicial da nave
(define y-nave 3)           	; entre 0 e 9 - coordenada y inicial da nave
(define ori-inicial 'n) ; pode ser 'n, 's, 'e, 'o - orientação inicial da nave

(define t-resposta 200)        ; tempo de resposta da nave, em ms, ao rodar ou
                               ; ao avançar para a próxima célula

; cria uma nave com a designação nave-k, usando o procedimento cria-nave
(define nave-k (cria-nave tab-teste x-nave y-nave ori-inicial cor-rasto t-resposta))
