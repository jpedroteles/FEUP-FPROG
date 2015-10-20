; para começao um jogo
; > (jogo-damas)
;
; Jogo feito por:
;                Pedro Daniel Sousa
;        e-mail: ei05060@fe.up.pt
;
; Ano lectivo: 2005/2006
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Chamada de bibliotecas necessarias ao jogo

(require (lib "swgr.scm" "user-feup"))
(require (lib "tabuleiro.scm" "user-feup"))


; ============================================================
; Cria uma janela com um tabuleiro, ajustados
; nao necessita argumentos
;
(define jogo-damas
  (lambda ()
    (display "Lado do tabuleiro, de damas, em celulas: ")
    (let ((dim (read)))
      (display "Lado da celula em pixels: ")
      (let* ((escala (read))    ; pixels por cada celula
             (dim-tab-pixels (+ 20 (* dim escala))))    ; largura da janela
        ;
        (cond ((< dim 7) (newline)
                         (display "!!!Numero de celulas de lado insuficientes!!!"))
              (else (janela dim-tab-pixels dim-tab-pixels "Jogo de damas")
                    (let ((tab (tabuleiro 10 (- dim-tab-pixels 10) escala dim dim)))
                      ; no fim deste procedimento devolve a tabela
                      (pinta-xadrez tab 0)
                      (let ((lista-vermelhas (damas-vermelhas-iniciais tab 0 0 '()))
                            (lista-azuis (damas-azuis-iniciais tab 0 (- dim 3) '())))
                        (display tab)
                        (newline)
                        (newline)
                        (display "Comecam a jogar as azuis.")
                        (ciclo-azul tab lista-azuis lista-vermelhas)))))))))

; ============================================================
; Pinta uma determinada celula (sem cor predefenida)
; Argumentos: tabela
;             coluna ( x )
;             linha ( y )
;
(define pinta-celula
  (lambda (tab x y)
    (let* ((escala (caddr tab))
           (altura-total (+ 10 (* (cadddr tab) escala))))
      (pinta (list (list (+ 10 (* x escala)) (- altura-total (* y escala)))
                   (list (+ 10 (* (add1 x) escala)) (- altura-total (* y escala)))
                   (list (+ 10 (* (add1 x) escala)) (- altura-total (* (add1 y) escala)))
                   (list (+ 10 (* x escala)) (- altura-total (* (add1 y) escala))))))))

; ============================================================
; Pinta uma celula-sim celula-nao
; Argumentos: tabela
;             coluna ( x ) inicial
;             linha ( y ) inicial
;
(define pintar-sim-pintar-nao
  (lambda (tab x y)
    (if (<= x (sub1 (cel-x tab)))  ; se estiver dentro dos limites
        (begin (pinta-celula tab x y)
               (pintar-sim-pintar-nao tab (+ 2 x) y)))))

; ============================================================
; Pinta o tabuleiro em forma de xadrez atravez do procedimento
; (pintar-sim-pintar-nao)
; Argumentos: tabela
;
(define pinta-xadrez
  (lambda (tab linha)
    (cond ((> linha (sub1 (cel-y tab))))
          ((even? linha) (pintar-sim-pintar-nao tab 0 linha) ; comeca na primeira celula
                         (pinta-xadrez tab (add1 linha)))
          (else (pintar-sim-pintar-nao tab 1 linha) ; comeca na segunda celula
                (pinta-xadrez tab (add1 linha))))))

; ============================================================
; Desenha uma peca de damas
; numa celula com as coordenadas x e y
; Argumentos: tabela
;             coluna
;             linha
;
(define desenha-dama
  (lambda (tab coluna linha)
    (let ((orig-x (car tab))
          (orig-y (cadr tab))
          (lado (caddr tab)))
      (let* ((lado/5 (/ lado 5))
             (lado4/5 (* 4 lado/5))
             (vertice-x (+ orig-x (* coluna lado)))
             (vertice-y (- orig-y (* linha lado))))
        (pinta-oval (list  (list (+ vertice-x lado/5)
                                 (- vertice-y lado/5))
                           (list (+ vertice-x lado4/5)
                                 (- vertice-y lado4/5))))))))

; ============================================================
; Posicao das damas iniciais vermelhas
; Argumentos: tabela
;             coordenada x (inicial)
;             coordenada y (inicial)
;             lista (vazia)
;
(define damas-vermelhas-iniciais
  (lambda (tab x y lista-final)
    (let ((x-max (cadddr tab)))
      (cond ((> y 2) (cor 18)
                     (desenha-lista-damas tab lista-final)
                     lista-final)
            ((< x x-max) (damas-vermelhas-iniciais tab       ; tabela
                                                   (+ x 2)   ; duas colunas para a direita
                                                   y         ; na mesma linha
                                                   (append lista-final (list (cons x y)))))
            ((>= x x-max) (if (even? y)
                              (damas-vermelhas-iniciais tab 1 (add1 y) lista-final)
                              (damas-vermelhas-iniciais tab 0 (add1 y) lista-final)))))))

; ============================================================
; Posicao das damas iniciais azuis
; Argumentos: tabela
;             coordenada x (inicial)
;             coordenada y (inicial)
;             lista (vazia)
;
(define damas-azuis-iniciais
  (lambda (tab x y lista-final)
    (letrec ((azuis-iniciais   ; procedimento auxiliar
              (lambda (tab x y lista-final)
                (let ((x-max (cadddr tab))
                      (y-max (car (cddddr tab))))
                  (cond ; se o y ja tiver fora dos limites desenha as damas e devolve a lista
                    ((>= y y-max) (cor 2)
                                  (desenha-lista-damas tab lista-final)
                                  lista-final)
                    ; preencher linha de damas
                    ((< x x-max) (azuis-iniciais
                                  tab       ; tabela
                                  (+ x 2)   ; duas colunas para a direita
                                  y         ; na mesma linha
                                  (append lista-final (list (cons x y)))))
                    ; mudar de linha as damas
                    ((>= x x-max) (if
                                   (even? y)
                                   (azuis-iniciais tab 1 (add1 y) lista-final)
                                   (azuis-iniciais tab 0 (add1 y) lista-final))))))))
      ; definir a posicao da primeira dama
      (let ((x-ajustado (if (even? y) ; se a linha for par a primeira dama fica na
                            0         ; coluna 0
                            1)))      ; se nao fica na coluna 1
        (azuis-iniciais tab x-ajustado y lista-final)))))

; ============================================================
; Desenha as posicoes das damas atraves de uma lista de posicoes
; Argumentos: tabela
;             lista (das posicoes das damas)
;
(define desenha-lista-damas
  (lambda (tab lista)
    (if (not (null? lista))  ; condicao final
        (cond ((null? (car lista)) (desenha-lista-damas tab (cdr lista)))
              ;
              ((not (null? lista)) (desenha-dama tab (caar lista) (cdar lista))
                                   (desenha-lista-damas tab (cdr lista)))))))


; ============================================================
;
; ============================================================
;
; =============  Fim dos procedimentos iniciais ==============
;
; ============================================================
;
; ============================================================

;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;

; ============================================================
; Selecionador de uma celula atraves de um clique de rato
; no fim devolve a posicao deste
; Argumentos: tabela
;
(define seleccionar-com-rato
  (lambda (tab)
    (move (cursor))
    (let* ((coord-x (car (caneta)))
           (coord-y (cadr (caneta)))
           (escala (caddr tab))
           (altura-janela (+ 20 (* (cadddr tab) escala)))
           (coluna (quotient (- coord-x 10) escala))
           (linha (quotient (- altura-janela coord-y 10) escala)))
      (cons coluna linha))))

; ============================================================
; Apaga uma peca de damas
; numa celula com a indicacao da celula
; Argumentos: tabela
;             celula
;
(define apaga-dama
  (lambda (tab celula)
    (cor 0)       ; pinta um círculo "O" com a cor das celulas vermelhas
    (desenha-dama tab (car celula) (cdr celula))))

;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;

; ============================================================
; Ciclo de instrucoes
; Argumentos: tabela
;             lista-azuis
;             lista-vermelhas
;
(define ciclo-azul
  (lambda (tab lista-azuis lista-vermelhas)
    (cond ((alguma-jogada? tab lista-azuis lista-vermelhas 'azul)
           (newline)
           (let ((ver-azu (mover-azul tab lista-azuis lista-vermelhas)))
             (newline)
             (display "Jogam as vermelhas")
             (newline)
             (ciclo-vermelha tab (car ver-azu) (cdr ver-azu))))
          (else (newline)
                (newline)
                (display "!!!!Azuis Perderam!!!!")))))

(define ciclo-vermelha
  (lambda (tab lista-vermelhas lista-azuis)
    (cond ((alguma-jogada? tab lista-azuis lista-vermelhas 'vermelha)
           (let ((ver-azu (mover-vermelha tab lista-vermelhas lista-azuis)))
             (newline)
             (display "Jogam as azuis")
             (ciclo-azul tab (cdr ver-azu) (car ver-azu))))
          (else (newline)
                (newline)
                (display "!!!!Vermelhas Perderam!!!!")))))



; ============================================================
; Substitui um elemento ja existente por um novo elemento
; Argumentos: lista
;             velho elemento (a substituir)
;             novo elemento (o que vai surgir no lugar do velho elemento)
;
(define substitui-lista
  (lambda (lista velho novo)
    (letrec ((aux
              (lambda (lista novo velho fim)
                (cond ((null? lista) fim)
                      ((equal? (car lista) velho) (append fim (list novo) (cdr lista)))
                      (else (aux (cdr lista) novo velho (append fim (list (car lista)))))))))
      (aux lista novo velho '()))))

; ============================================================
; Apaga a celula seleccionada da lista
; Argumentos: lista
;             celula a apagar
;
(define apaga-lista
  (lambda (lista cel)
    (letrec ((aux
              (lambda (lista cel fim)
                (cond ((null? lista) fim)
                      ((equal? (car lista) cel) (append fim (cdr lista)))
                      (else (aux (cdr lista) cel (append fim (list (car lista)))))))))
      (aux lista cel '()))))


; ============================================================
;
; ============================================================
;
; ===============  Fim dos procedimentos base ================
;
; ============================================================
;
; ============================================================

;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;
;                    ;   ;   ;   ;   ;   ;

; ============================================================
;
; ============================================================
;
; ==================  Procedimentos #t / #f ==================
;
; ============================================================
;
; ============================================================

; Verifica se a celula seleccionada contem alguma dama
; Argumentos: tabela
;             lista
;
(define dama-na-celula?
  (lambda (celula lista)
    (if (member celula lista)
        #t
        #f)))

; ============================================================
; Verifica se a celula esta dentro dos limites
; Argumentos: tabela
;             celula (a verificar)
;
(define dentro-limites?
  (lambda (tab celula)
    (let ((col-max (sub1 (cadddr tab)))
          (col-min 0)
          (lin-max (sub1 (cadddr tab)))
          (lin-min 0)
          (coluna (car celula))
          (linha (cdr celula)))
      (and (<= coluna col-max)
           (>= coluna col-min)
           (<= linha lin-max)
           (>= linha lin-min)))))

; ============================================================
; Verifica se ha algum movimento possivel para qualquer dama
; Argumentos: tabela
;             azuis
;             vermelhas
;             cor das damas a verificar
(define alguma-jogada?
  (lambda (tab azuis vermelhas cor)
    (letrec ((aux-azul
              (lambda (lista)
                (cond ((null? lista) #f)
                      ((algum-movimento-azul? tab (car lista) azuis vermelhas) #t)
                      (else (aux-azul (cdr lista))))))
             (aux-vermelha
              (lambda (lista)
                (cond ((null? lista) #f)
                      ((algum-movimento-vermelha? tab (car lista) azuis vermelhas) #t)
                      (else (aux-vermelha (cdr lista)))))))
      (cond ((equal? cor 'azul) (aux-azul azuis))
            ((equal? cor 'vermelha) (aux-vermelha vermelhas))))))






; ============================================================
;
; ============================================================
;
; ===================  Procedimentos azuis ===================
;
; ============================================================
;
; ============================================================

; Verifica se a dama azul pode deslocar-se
; para a celula indicada
; Argumentos: tabela
;             celula-inicial (onde se encontra a dama a jogar)
;             lista das damas azuis
;             lista das damas vermelhas
;
(define azul-pode-jogar-normal?
  (lambda (tab celula azuis vermelhas)
    (let ((col (car celula))
          (lin (cdr celula)))
      (or   ; Movimento para direita e cima
       (if (dentro-limites? tab (cons (add1 col) (sub1 lin))) ; A celula esta vazia?:
           (or (not (dama-na-celula? (cons (add1 col) (sub1 lin)) (append azuis vermelhas))))
           #f)
       ;      Movimento para esquerda e cima
       (if (dentro-limites? tab (cons (sub1 col) (sub1 lin))) ; A celula esta vazia?:
           (or (not (dama-na-celula? (cons (sub1 col) (sub1 lin)) (append azuis vermelhas))))
           #f)))))

;============================================================
; Verifica se e' possivel "comer" alguma dama
; Argumentos: tabela
;             celula inicial
;             lista das posicoes das damas azuis
;             damas opositoras (lista das posicoes das damas vermelhas)
;
(define azul-pode-comer?
  (lambda (tab celula azuis vermelhas)
    (let ((x (car celula))
          (y (cdr celula)))
      (or  ; Se a celula: duas a direita e duas acima esta dentro dos limites
       (if (dentro-limites? tab (cons (+ x 2) (- y 2)))
           ; A celula: duas a direita e duas acima esta livre? (nao ocupada?)
           (if (not (dama-na-celula? (cons (+ x 2) (- y 2)) (append azuis vermelhas)))
               (if   ; A celul:a uma a direita e uma acima esta ocupada pelas vermelhas?
                (dama-na-celula? (cons (add1 x) (sub1 y)) vermelhas)
                #t
                #f)
               #f)
           #f)
       ;
       ; Se a celula: duas a esquerda e duas acima esta dentro dos limites
       (if (dentro-limites? tab (cons (- x 2) (- y 2)))
           ; A celula: duas a esquerda e duas acima esta livre? (nao ocupada?)
           (if (not (dama-na-celula? (cons (- x 2) (- y 2)) (append azuis vermelhas)))
               (if   ; A celula: uma a esquerda e uma acima esta ocupada pelas vermelhas?
                (dama-na-celula? (cons (sub1 x) (sub1 y)) vermelhas)
                #t
                #f)
               #f)
           #f)))))

;============================================================
; Mover dama azul
; Argumentos: tabela
;             lista das posicoes das damas azuis
;             damas opositoras (lista das posicoes das damas vermelhas)
(define mover-azul
  (lambda (tab azuis vermelhas)
    (let ((celula-partida (seleccionar-com-rato tab)))
      (cond   ;verifica se a celula seleccionada contem uma dama azul
        ((dama-na-celula? celula-partida azuis)
         (cond ( ; Verifica se ha algum movimento possivel
                (algum-movimento-azul? tab celula-partida azuis vermelhas)
                (apaga-dama tab celula-partida)
                (cor 2)
                (faz-movimento-azul tab celula-partida azuis vermelhas))
               ;
               (else (nenhum-movimento-possivel)
                     (mover-azul tab azuis vermelhas))))
        ; Caso a celula nao tenha nenhuma dama azul devolve erro e recomeca mover-azul
        (else (celula-sem-damas 'azul)
              (mover-azul tab azuis vermelhas))))))

;============================================================
; Verifica se ha algum movimento possivel para a dama azul
; Argumentos: tabela
;             dama azul
;             lista das posicoes das damas azuis
;             lista das posicoes das damas vermelhas
(define algum-movimento-azul?
  (lambda (tab partida azuis vermelhas)
    (or (azul-pode-comer? tab partida azuis vermelhas)
        (azul-pode-jogar-normal? tab partida azuis vermelhas))))

;============================================================
; Faz o movimento da dama azul
; Argumentos: tabela
;             dama azul (partida)
;             lista das posicoes das damas azuis
;             lista das posicoes das damas vermelhas
(define faz-movimento-azul
  (lambda (tab partida azuis vermelhas)
    (let ((destino (seleccionar-com-rato tab)))
      ; Verifica se a celula seleccionada corresponde a uma jogada normal
      (cond ((azul-joga-normal? tab partida destino (append azuis vermelhas))
             (cor 2)
             (desenha-dama tab (car destino)(cdr destino))
             (cons vermelhas
                   (substitui-lista azuis partida (cons (car destino)(cdr destino)))))
            ; Verifica se a celula seleccionada corresponde a uma jogada em que come adversario
            ((azul-come? tab partida destino azuis vermelhas)
             (apaga-dama tab (cons (if (< (- (car destino) (car partida)) 0)
                                       (sub1 (car partida))
                                       (add1 (car partida)))
                                   (sub1 (cdr partida))))
             (cor 2)
             (desenha-dama tab (car destino)(cdr destino))
             (cons (apaga-lista vermelhas (cons (if (< (- (car destino) (car partida)) 0)
                                                    (sub1 (car partida))
                                                    (add1 (car partida)))
                                                (sub1 (cdr partida))))
                   (substitui-lista azuis partida (cons (car destino)(cdr destino)))))
            ; Caso a celula seleccionada nao ter nenhum movimento valido
            (else (movimento-invalido)
                  (faz-movimento-azul tab partida azuis vermelhas))))))

;============================================================
;Verifica se a jogada da dama azul foi uma jogada normal
; Argumentos: tabela
;             dama azul (partida)
;             dama azul (chegada)
;             lista das posicoes de todas as damas
(define azul-joga-normal?
  (lambda (tab partida destino todas-damas)
    (and (not (dama-na-celula? destino todas-damas)) ; Destino esta livre
         (dentro-limites? tab destino) ; Destino esta dentro dos limites
         (= (abs (- (car destino) (car partida))) 1)
         (= (- (cdr partida) (cdr destino)) 1))))

;============================================================
;Verifica se a jogada da dama azul foi uma jogada em que "comeu" alguma
; Argumentos: tabela
;             dama azul (partida)
;             dama azul (chegada)
;             lista das posicoes de todas as damas
(define azul-come?
  (lambda (tab partida destino azuis vermelhas)
    (let ((col (car partida))
          (lin (cdr partida)))
      (or (and  ; Dama vermelha na celula a direita e em cima
           (dama-na-celula? (cons (add1 col) (sub1 lin)) vermelhas)
           ; Celula duas a direita e duas acima livre
           (not (dama-na-celula? (cons (+ col 2) (- lin 2)) (append azuis vermelhas)))
           (equal? destino (cons (+ col 2) (- lin 2))))
          ;
          (and  ; Dama vermelha na celula a esquerda e em cima
           (dama-na-celula? (cons (sub1 col) (sub1 lin)) vermelhas)
           ; Celula duas a esquerda e duas acima livre
           (not (dama-na-celula? (cons (- col 2) (- lin 2)) (append azuis vermelhas)))
           (equal? destino (cons (- col 2) (- lin 2))))))))


; ============================================================
;
; ============================================================
;
; =================  Procedimentos vermelhas =================
;
; ============================================================
;
; ============================================================

; Verifica se a dama vermelha pode deslocar-se
; para a celula indicada
; Argumentos: tabela
;             celula-inicial (onde se encontra a dama a jogar)
;             lista das damas vermelhas
;             lista das damas azuis
;
(define vermelha-pode-jogar-normal?
  (lambda (tab celula vermelhas azuis)
    (let ((col (car celula))
          (lin (cdr celula)))
      (or   ; Movimento para direita e baixo
       (if (dentro-limites? tab (cons (add1 col) (add1 lin))) ; A celula esta vazia?:
           (or (not (dama-na-celula? (cons (add1 col) (add1 lin)) (append vermelhas azuis))))
           #f)
       ;      Movimento para esquerda e baixo
       (if (dentro-limites? tab (cons (sub1 col) (add1 lin))) ; A celula esta vazia?:
           (or (not (dama-na-celula? (cons (sub1 col) (add1 lin)) (append vermelhas azuis))))
           #f)))))

;============================================================
; Verifica se e' possivel "comer" alguma dama
; Argumentos: tabela
;             celula inicial
;             lista das posicoes das damas vermelhas
;             damas opositoras (lista das posicoes das damas azuis)
;
(define vermelha-pode-comer?
  (lambda (tab celula vermelhas azuis)
    (let ((x (car celula))
          (y (cdr celula)))
      (or  ; Se a celula: duas a direita e duas abaixo esta dentro dos limites
       (if (dentro-limites? tab (cons (+ x 2) (+ y 2)))
           ; A celula: duas a direita e duas abaixo esta livre? (nao ocupada?)
           (if (not (dama-na-celula? (cons (+ x 2) (+ y 2)) (append vermelhas azuis)))
               (if   ; A celul:a uma a direita e uma abaixo esta ocupada pelas azuis?
                (dama-na-celula? (cons (add1 x) (add1 y)) azuis)
                #t
                #f)
               #f)
           #f)
       ;
       ; Se a celula: duas a esquerda e duas abaixo esta dentro dos limites
       (if (dentro-limites? tab (cons (- x 2) (+ y 2)))
           ; A celula: duas a esquerda e duas abaixo esta livre? (nao ocupada?)
           (if (not (dama-na-celula? (cons (- x 2) (+ y 2)) (append vermelhas azuis)))
               (if   ; A celula: uma a esquerda e uma abaixo esta ocupada pelas azuis?
                (dama-na-celula? (cons (sub1 x) (add1 y)) azuis)
                #t
                #f)
               #f)
           #f)))))

;============================================================
; Mover dama vermelha
; Argumentos: tabela
;             lista das posicoes das damas vermelhas
;             damas opositoras (lista das posicoes das damas azuis)
(define mover-vermelha
  (lambda (tab vermelhas azuis)
    (let ((celula-partida (seleccionar-com-rato tab)))
      (cond   ;verifica se a celula seleccionada contem uma dama vermelha
        ((dama-na-celula? celula-partida vermelhas)
         (cond ( ; Verifica se ha algum movimento possivel
                (algum-movimento-vermelha? tab celula-partida vermelhas azuis)
                (apaga-dama tab celula-partida)
                (cor 18)
                (faz-movimento-vermelha tab celula-partida vermelhas azuis))
               ;
               (else (nenhum-movimento-possivel)
                     (mover-vermelha tab vermelhas azuis))))
        ; Caso a celula nao tenha nenhuma dama vermelha devolve erro e recomeca mover-vermelha
        (else (celula-sem-damas 'vermelha)
              (mover-vermelha tab vermelhas azuis))))))

;============================================================
; Verifica se ha algum movimento possivel para a dama vermelha
; Argumentos: tabela
;             dama vermelha
;             lista das posicoes das damas vermelhas
;             lista das posicoes das damas azuis
(define algum-movimento-vermelha?
  (lambda (tab partida vermelhas azuis)
    (or (vermelha-pode-comer? tab partida vermelhas azuis)
        (vermelha-pode-jogar-normal? tab partida vermelhas azuis))))

;============================================================
; Faz o movimento da dama vermelha
; Argumentos: tabela
;             dama vermlha (partida)
;             lista das posicoes das damas vermelhas
;             lista das posicoes das damas azuis
(define faz-movimento-vermelha
  (lambda (tab partida vermelhas azuis)
    (let ((destino (seleccionar-com-rato tab)))
      ; Verifica se a celula seleccionada corresponde a uma jogada normal
      (cond ((vermelha-joga-normal? tab partida destino (append vermelhas azuis))
             (cor 18)
             (desenha-dama tab (car destino)(cdr destino))
             (cons (substitui-lista vermelhas partida (cons (car destino)(cdr destino)))
                   azuis))
            ; Verifica se a celula seleccionada corresponde a uma jogada em que come adversario
            ((vermelha-come? tab partida destino vermelhas azuis)
             (apaga-dama tab (cons (if (< (- (car destino) (car partida)) 0)
                                       (sub1 (car partida))
                                       (add1 (car partida)))
                                   (add1 (cdr partida))))
             (cor 18)
             (desenha-dama tab (car destino)(cdr destino))
             (cons (substitui-lista vermelhas partida (cons (car destino)(cdr destino)))
                   (apaga-lista azuis (cons (if (< (- (car destino) (car partida)) 0)
                                                (sub1 (car partida))
                                                (add1 (car partida)))
                                            (add1 (cdr partida))))))
            ; Caso a celula seleccionada nao ter nenhum movimento valido
            (else (movimento-invalido)
                  (faz-movimento-vermelha tab partida vermelhas azuis))))))

;============================================================
;Verifica se a jogada da dama vermelha foi uma jogada normal
; Argumentos: tabela
;             dama vermelha (partida)
;             dama vermelha (chegada)
;             lista das posicoes de todas as damas
(define vermelha-joga-normal?
  (lambda (tab partida destino todas-damas)
    (and (not (dama-na-celula? destino todas-damas)) ; Destino esta livre
         (dentro-limites? tab destino) ; Destino esta dentro dos limites
         (= (abs (- (car destino) (car partida))) 1)
         (= (- (cdr destino) (cdr partida)) 1))))

;============================================================
;Verifica se a jogada da dama vermelha foi uma jogada em que "comeu" alguma
; Argumentos: tabela
;             dama vermelha (partida)
;             dama vermelha (chegada)
;             lista das posicoes de todas as damas
(define vermelha-come?
  (lambda (tab partida destino vermelhas azuis)
    (let ((col (car partida))
          (lin (cdr partida)))
      (or (and  ; Dama azul na celula a direita e em baixo
           (dama-na-celula? (cons (add1 col) (add1 lin)) azuis)
           ; Celula duas a direita e duas abaixo livre
           (not (dama-na-celula? (cons (+ col 2) (+ lin 2)) (append vermelhas azuis)))
           (equal? destino (cons (+ col 2) (+ lin 2))))
          ;
          (and  ; Dama azul na celula a esquerda e em baixo
           (dama-na-celula? (cons (sub1 col) (add1 lin)) azuis)
           ; Celula duas a esquerda e duas abaixo livre
           (not (dama-na-celula? (cons (- col 2) (+ lin 2)) (append vermelhas azuis)))
           (equal? destino (cons (- col 2) (+ lin 2))))))))

; ============================================================
;
; ============================================================
;
; ====================  Mensagens de erro ====================
;
; ============================================================
;
; ============================================================

; Nao necessita de argumentos
;
(define movimento-invalido
  (lambda ()
    (newline)
    (display "O movimento seleccionado nao e' valido.")
    (newline)
    (display "PECA TOCADA TEM DE SER JOGADA.")
    (newline)
    (display "Por favor seleccione uma celula de destino valida.")
    (newline)))

; Argumentos: cor (a apresentar no final de cada frase)
;
(define celula-sem-damas
  (lambda (cor)
    (newline)
    (display "A celula seleccionada nao contem nenhuma dama ")
    (display cor)
    (display ".")
    (newline)
    (display "Por favor seleccione uma celula que contenha uma dama ")
    (display cor)
    (display ".")
    (newline)))

(define nenhum-movimento-possivel
  (lambda ()
    (newline)
    (display "A dama seleccionada nao permite nenhum movimento legal")
    (newline)
    (display "Por favor seleccione uma dama que possa ser jogada")
    (newline)))

(jogo-damas)