; LABIRINTO
; Baseado no codigo caminho2.scm do professor Fernando Nunes Ferreira 
; fnf@fe.up.pt
; Modificado por Luis Santos 
; ei00008@fe.up.pt
;
; correr o programa -> (caminho)
;
; Opções do Menu
; 1-> Definir obstaculos um a um
; 2-> Definir uma nova posicao de partida
; 3-> Colocar obstaculos de forma aleatoria pelo tabuleiro a excepção do ponto de partida 
;     e do ponto de chegada
; 4-> Definir uma nova posicao final
; 5->
; 6->
; 7-> Achar o caminho (versao do Luis Santos). Com este metodo o programa calcula a distancia 
;     de cada celula ao ponto de chegada dando o valor #f (falso) as celulas que foram 
;     obstaculos de forma a serem sempre evitadas. Depois vai-se movimentando para a celula 
;     adjacente (cima, baixo, esquerda, direita) que contenha uma distancia menor do que actual. 
; 8-> Achar caminho (versao prof. FNF)
; 9-> Limpa a trajectoria
; 10-> Sai do programa

(require (lib "tabuleiro.scm" "user-feup"))
(require (lib "audio.scm" "user-feup"))

(define som_fim
  (lambda ()
    (som "ri3")))

(define som
  (lambda ()
    (som "anda1")))

(define caminho
  (lambda ()
    (display "Lado do tabuleiro em celulas : ")
    (let ((dim (read)))
      (display "Lado da celula em pixels: ")
      (let* ((lado-cel (read))  
             (dim-tab-pixels (+ 20 (* dim lado-cel))))
        (janela dim-tab-pixels dim-tab-pixels "caminho-grafico")
        (let ((tab (tabuleiro 10     
                              (- dim-tab-pixels 10) 
                              lado-cel 
                              dim dim)))
          (menu tab 
                (list '()            ; lista inicial de obst?culos
                      '()            ; trajectória inicial
                      (cons 0 0)     ; ponto de partida inicial
                      (cons (sub1 dim) (sub1 dim)))))))))  ; objectivo
(define obstaculos_novos '())

(define menu
  (lambda (tab jogo)
    (let ((obstaculos (car jogo))
          (trajectoria (cadr jogo))
          (pnt-partida (caddr jogo))
          (objectivo (cadddr jogo))
          (dimensao (cadddr tab)))
      
      (if (null? trajectoria)
          (mostra-jogo tab jogo))
      (newline)
      (newline)
      (display "1-Definir obstaculo manualmente")(newline)
      (display "2-Obstaculos aleatorios")(newline)      
      (display "3-Definir posicao partida")(newline)
      (display "4-Definir posicao final")(newline)
      (display "5-            ")(newline)
      (display "6-            ")(newline)
      (display "7-Andar (metodo Luis Santos)")(newline)
      (display "8-Andar (metodo Prof. FNF)")(newline)
      (display "9-Limpar trajectoria")(newline)
      (newline)
      (display "10- fim")
      (newline)
      (display "=>")
      
      (let ((opcao (read)))
        (cond ((= 1 opcao)      ; ------> define novo obstáculo
               (let ((obs (novo-obstaculo tab jogo)))
                 (menu tab (list obs '() pnt-partida objectivo))))
              ((= 2 opcao) ;---------> coloca obstaculos aleatoriamente
               (display "numero de obstaculos? ")
               (let* ( (numero (read))
                       (numero_maximo (- (* dimensao dimensao) (+ 2 dimensao dimensao)))
                       )
                 (if (>= numero numero_maximo )
                     (begin (display "Erro! o numero maximo de obstaculos ? ")(display numero_maximo)
                            (set! obstaculos_novos '())) ; <- Para limpar os obstaculos que foram postos anteiromente
                     (begin (set! obstaculos_novos (obstaculo-random numero '() pnt-partida objectivo dimensao ))
                            (celulas tab obstaculos_novos 'x 10 0))
                     )
                 (menu tab (list obstaculos_novos '() pnt-partida objectivo))
                 ))
              ((= 3 opcao)      ; ------> define posição de partida
               (let ((lin-col (posicao-partida tab jogo)))
                 (menu tab (list obstaculos '() lin-col objectivo))))
              ((= 4 opcao)posicao-fim
               (let ((lin-col (posicao-partida tab jogo)))
                 (menu tab (list obstaculos '() pnt-partida lin-col))))
              
              ((= 7 opcao) ;------> andar utilizando o meu metodo
               (newline)
               (display "---> ATENÇÃO! <---")(newline)
               (display "Por favor espere um pouco.")(newline)
               (let ( (trajectoria (procura (populate (sub1 dimensao) (sub1 dimensao) 0 0 '() obstaculos objectivo) pnt-partida  objectivo '() (sub1 dimensao) obstaculos tab) )
                      )
                 (som_fim)
                 (menu tab (list obstaculos trajectoria pnt-partida objectivo))
                 )
               )
              ((= 8 opcao)      ; ------> ordem para partir
               (let ((caminho (caminho-lista tab jogo)))
                 (menu tab (list obstaculos caminho pnt-partida objectivo))))
              ((= 9 opcao) ; -------> limpar trajectória
               (menu tab (list obstaculos '() pnt-partida objectivo)))
              ((= 10 opcao)
               'acabou)
              (else (menu tab jogo)))))))

(define caminho-lista       ; produz uma lista com o caminho percorrido
  (lambda (tab jogo)          
    (let* ((obstaculos (car jogo))
           (partida (caddr jogo))
           (obj (cadddr jogo)))
      (if (membro? partida obstaculos)
          '()
          (andar partida tab obj obstaculos)))))

(define andar
  (lambda (partida tab obj obstaculos)
    (let ((dim (cadddr tab)))
      (celulas tab (list partida) 'p 500 0) ; só uma célula: o pnt corrente
      (cons partida
            (cond
              ((equal? partida obj) '())  
              ((avanca-hor? partida dim obstaculos)
               (andar (cons (add1 (car partida)) (cdr partida)) 
                      tab obj obstaculos))
              ((avanca-ver? partida dim obstaculos)
               (andar (cons (car partida) (add1 (cdr partida)))
                      tab obj obstaculos))
              (else '()))))))
 
(define avanca-hor?
  (lambda (partida dim obstaculos)
    (and
     (not (= (car partida) (sub1 dim)))    ; ---> não está na coluna da direita
     (not (membro? (cons (add1 (car partida)) (cdr partida))
                   obstaculos))))) ;----> e na horizontal não encontra obstáculo

(define avanca-ver?
  (lambda (partida dim obstaculos)
    (and (< (cdr partida) (sub1 dim))       ; -----> não está na última linha
         (not (membro? (cons (car partida) (add1 (cdr partida)))
                       obstaculos)))))  ; --> na vertical não encontra obstáculo

(define membro?
  (lambda (elem lista)
    (member elem lista)))

(define mostra-jogo 
  (lambda (tab jogo)
    (let ((dim (cadddr tab))
          (obstaculos (car jogo))
          (trajectoria (cadr jogo))
          (p-part (caddr jogo))
          (obj (cadddr jogo)))
      (limpa-tab tab 26)
      (celula tab (car p-part) (cdr p-part) 'p 0) ; pnt partida
      (celula tab (car obj) (cdr obj) 'c 0)       ; pnt chegada
      (celulas tab obstaculos 'x 0 0)
      (celulas tab trajectoria 'p 100000 0))))

; pede linha-coluna
; comuta situação de obstáculo em linha-coluna
; e devolve numa lista de obstáculos
(define novo-obstaculo  
  (lambda (tab jogo)
    (let ((obs (car jogo))
          (col-linh (pede-col-linh tab)))
      (letrec ((actualiza-obs
                (lambda (obst)
                  (cond ((null? obst)
                         (list col-linh))
                        ((equal? (car obst) col-linh)
                         (cdr obst))
                        (else (cons (car obst)
                                    (actualiza-obs (cdr obst))))))))
        (actualiza-obs obs)))))

;por Luis Santos
;coloca obstaculos de forma totalmente aleatoria pelo tabulerio
(define obstaculo-random
  (lambda (numero obstaculos ponto_inicial ponto_final dimensao)
    (if (zero? numero)
        obstaculos
        (letrec ( (x (random dimensao))
                  (y (random dimensao))
                  (x-y (cons x y))
                  
                  (verifica-se-repetido 
                   (lambda (novo lista)
                     (if (or (null? (member lista (list novo))) (equal? (member lista (list novo)) #f))
                         #f
                         #t
                         )
                     ))
                  (valido?
                   (lambda ()
                     (cond ((or (and (= x (car ponto_final)) (= y (cdr ponto_final)))
                                (and (= x (car ponto_inicial)) (= y (cdr ponto_inicial)))
                                )
                            #f)
                           (else #t))))
                  )
          ;(display "faltam: ")
          ;(display numero)
          ;(newline)
          ;(display "    novo:")(display x-y)(newline)
          ;(display "valido?")(display (valido?))(newline)
          ;(display "repetido? ")(display (verifica-se-repetido x-y obstaculos))(newline)
          ;(display "-----")(newline)
          (if (verifica-se-repetido x-y obstaculos)
              (obstaculo-random numero obstaculos ponto_inicial ponto_final dimensao)
              (begin (if (valido?)
                         (obstaculo-random (sub1 numero) (append obstaculos (list x-y)) ponto_inicial ponto_final dimensao)
                         (obstaculo-random  numero obstaculos ponto_inicial ponto_final dimensao))))))))

;
; pede coluna-linha
; devolve novo ponto de partida
;
(define posicao-partida 
  (lambda (tab jogo)
    (let ((obs (car jogo))
          (col-linh (pede-col-linh tab)))
      (if (member col-linh obs)
          (posicao-partida tab jogo)
          col-linh))))

(define posicao-fim 
  (lambda (tab jogo)
    (let ((obs (car jogo))
          (col-linh (pede-col-linh tab)))
      (if (member col-linh obs)
          (posicao-partida tab jogo)
          col-linh))))

;
; --------------------------
; procedimentos auxiliares
;

(define pede-col-linh  
  (lambda (tab)
    (let ((dim-x (cel-x tab))
          (dim-y (cel-y tab)))
      (display "Coluna (de 0 a ")
      (display (sub1 dim-x))
      (display "): ")
      (let ((col (read)))
        (if (or (negative? col)
                (>= col dim-x))
            (pede-col-linh tab)
            (begin
              (display "Linha (de 0 a ")
              (display (sub1 dim-y))
              (display "): ")
              (let ((linh (read)))
                (if (or (negative? linh)
                        (>= linh dim-y))
                    (pede-col-linh tab)
                    (cons col linh)))))))))

;------------ Por Luis Santos
(define quadrado
  (lambda (numero)
    (* numero numero)))

(define distancia
  (lambda (x1 y1 x2 y2)
    (sqrt (+ (quadrado (- x2 x1)) (quadrado (- y2 y1))))))

(define nao_fora?
  (lambda (celx cely dim )
    (cond ( (< celx 0) #f)
          ( (< cely 0) #f)
          ( (> celx dim) #f)
          ( (> cely dim) #f)
          (else #t))))

(define valor?
  (lambda (celx cely lista)
    (let ((val (member (cons celx cely) lista)))
      (cadr val))))

;devolve uma lista um as distancias da posicao (nx,ny) ate a posicao (dimx,dimy)
; se o celula for um obstaculo entao da-lhe o valor de #f 
;a aprtir daki pode-se estabelecer um caminho unindo os pontos ke tiverem menor distancia!!!!!!!!!

(define populate
  (lambda (dimx dimy nx ny lista_final lista_obstaculos ponto-final)
    (if (and (= dimx nx) (= dimy ny))
        (append (append lista_final (list (cons nx ny)))(list (distancia nx ny dimx dimy)))
        
        (let ( (prox_ny (if (= ny dimy)
                            0
                            (add1 ny)))
               (prox_nx (if (= ny dimy)
                            (add1 nx)
                            nx))
               (x-final (car ponto-final))
               (y-final (cdr ponto-final))
               )
          (populate dimx dimy
                    prox_nx prox_ny
                    (append 
                     (append lista_final (list (cons nx ny)))
                     (if (member (cons nx ny) lista_obstaculos)
                         (list #f)      ;---> prenchimento com um #f pois e um obstaculo
                         (list (distancia nx ny x-final y-final))))
                    lista_obstaculos ponto-final)))))

(define troca-valor
  (lambda (celula1 celula2 lista)
    (letrec (
             (aux (lambda (elem_subs listai lista_final)
                    (if (null? listai)
                        lista_final
                        (aux  elem_subs (cddr listai) (if (equal? celula1 (car listai))
                                                          (append (append lista_final (list (car listai))) (list elem_subs))
                                                          (append (append lista_final (list (car listai))) (list (cadr listai))))))))
             )
      (aux (valor? (car celula2) (cdr celula2) lista) lista '()))))

(define fora_ou_obstaculo?
  (lambda (x y dimensao lista_obstaculos)
    (if (nao_fora? x y dimensao)
        (if (not  (equal? #f (member (cons x y) lista_obstaculos)))
            #t))))

(define obstaculo?
  (lambda (ponto lista_obstaculos)
    (if (equal? #f (member ponto lista_obstaculos))
        #f
        #t)))

(define mostrar_grafico
  (lambda (tab lista delay posicao)
    (letrec ( (espera
               (lambda (tempo)
                 (if (= tempo 0)
                     #t
                     (espera (sub1 tempo))))))
      (espera delay)
      (celula tab (car posicao) (cdr posicao) '+ 0)
      (celula tab (car posicao) (cdr posicao) 'x 0)
      (celula tab (car posicao) (cdr posicao) 'p 0)
      (espera (* 6 delay))
      (celula tab (car lista) (cdr lista) 'l 26)
      (celula tab (car lista) (cdr lista) 'p 0))))

(define procura
  (lambda (lista_val posicao posicao_final trajecto dim obstaculos tab)
    (if (not (null? trajecto))
        (begin 
          (mostrar_grafico tab (car (reverse trajecto)) 20000 posicao)
          (som)))
    (celula tab (car posicao) (cdr posicao) 'p 0)
    
    (if (equal? posicao posicao_final)
        (begin ;(display "fim! a trajectoria e:")(display trajecto)
          trajecto)
        (begin 
          (letrec ( (celula_x (car posicao))
                    (celula_y (cdr posicao))
                    (celula_x+1 (add1 celula_x))
                    (celula_y+1 (add1 celula_y))
                    (celula_x-1 (sub1 celula_x))
                    (celula_y-1 (sub1 celula_y))
                    
                    (celula_direita (cons celula_x celula_y+1))
                    (celula_esquerda (cons celula_x celula_y-1))
                    (celula_cima (cons celula_x-1 celula_y))
                    (celula_baixo (cons celula_x+1 celula_y))
                    
                    (valor_actual (valor? celula_x celula_y lista_val))
                    
                    (nao_fora_direita (nao_fora? celula_x celula_y+1 dim))
                    (nao_fora_esquerda (nao_fora? celula_x celula_y-1 dim))
                    (nao_fora_cima (nao_fora? celula_x-1 celula_y dim))
                    (nao_fora_baixo (nao_fora? celula_x+1 celula_y dim))
                    
                    (Nobstaculo_direita (not (obstaculo? celula_direita obstaculos)))
                    (Nobstaculo_esquerda (not (obstaculo? celula_esquerda obstaculos)))
                    (Nobstaculo_cima (not (obstaculo? celula_cima obstaculos)))
                    (Nobstaculo_baixo (not (obstaculo? celula_baixo obstaculos)))
                    
                    (valido_direita? (fora_ou_obstaculo? celula_x celula_y+1 dim obstaculos))
                    (valido_esquerda? (fora_ou_obstaculo? celula_x celula_y-1 dim obstaculos))
                    (valido_cima? (fora_ou_obstaculo? celula_x-1 celula_y dim obstaculos))
                    (valido_baixo? (fora_ou_obstaculo? celula_x+1 celula_y dim obstaculos))
                    
                    (falso #f)
                    
                    (testa_direita
                     (lambda ()
                       (if nao_fora_direita
                           (if (equal? falso (valor? celula_x celula_y+1 lista_val))
                               #f
                               (if (> valor_actual (valor? celula_x celula_y+1 lista_val))
                                   #t
                                   #f))
                           #f)))
                    
                    (testa_esquerda
                     (lambda ()
                       (if nao_fora_esquerda
                           (if (equal? falso (valor? celula_x celula_y-1 lista_val))
                               #f
                               (if (> valor_actual (valor? celula_x celula_y-1 lista_val))
                                   #t
                                   #f))
                           #f)))
                    
                    (testa_cima
                     (lambda ()
                       (if nao_fora_cima
                           (if (equal? falso (valor? celula_x-1 celula_y lista_val))
                               #f
                               (if (> valor_actual (valor? celula_x-1 celula_y lista_val))
                                   #t
                                   #f))
                           #f)))
                    
                    (testa_baixo
                     (lambda ()
                       (if nao_fora_baixo
                           (if (equal? falso (valor? celula_x+1 celula_y lista_val))
                               #f
                               (if (> valor_actual (valor? celula_x+1 celula_y lista_val))
                                   #t
                                   #f))
                           #f)))
                    
                    (movimentos-auxliares
                     ;nao consegue mexer-se !
                     (lambda ()
                       (let (
                             (novo_trajecto (if (null? trajecto)
                                                (begin (display "CAMINHO NAO ENCONTRADO! VERIFIQUE SE HA' UM CAMINHO POSSIVEL.")
                                                       trajecto)
                                                trajecto)) 
                             )
                         ;(newline)
                         (cond ( (null? trajecto)
                                 (display "CAMINHO NAO ENCONTRADO! VERIFIQUE SE HA' UM CAMINHO POSSIVEL.")
                                 '())
                               ( (and nao_fora_esquerda Nobstaculo_esquerda)
                                 ;(display "movimento de recurso!! andar para a ESQUERDA!")
                                 (procura (troca-valor posicao celula_esquerda lista_val) celula_esquerda posicao_final (append trajecto (list celula_esquerda)) dim obstaculos tab))
                               ( (and nao_fora_cima Nobstaculo_cima)
                                 ;(display "movimento de recurso!! andar para CIMA!")
                                 (procura (troca-valor posicao celula_cima lista_val) celula_cima posicao_final (append trajecto (list celula_cima)) dim obstaculos tab))
                               ( (and nao_fora_baixo Nobstaculo_baixo)
                                 ;(display "movimento de recurso!! andar para BAIXO!")
                                 (procura (troca-valor posicao celula_baixo lista_val) celula_baixo posicao_final (append trajecto (list celula_baixo)) dim obstaculos tab))
                               ( (and nao_fora_direita Nobstaculo_direita)
                                 ;(display "movimento de recurso!! andar para A DIREITA!")
                                 (procura (troca-valor posicao celula_direita lista_val) celula_direita posicao_final (append trajecto (list celula_direita)) dim obstaculos tab))
                               (else (display "CAMINHO NAO ENCONTRADO! VERIFIQUE SE HA' UM CAMINHO POSSIVEL.")
                                     '() ))))))
            ;(newline)(display "<><><><><><><><><><>")(newline)(display "posicao: (")(display (car posicao))(display " , ")
            ;(display (cdr posicao))(display " )")(newline)
            ;(display "valor actual: ") (display valor_actual)(newline)
            ;(display "direita: ")(display (testa_direita))(newline)
            ;(display "esquerda: ")(display (testa_esquerda))(newline)
            ;(display "cima: ")(display (testa_cima))(newline)
            ;(display "baixo: ")(display (testa_baixo))(newline)
            ;(newline)
            (if (or (fora_ou_obstaculo? celula_x+1 celula_y dim obstaculos) 
                    (fora_ou_obstaculo? celula_x-1 celula_y dim obstaculos)
                    (fora_ou_obstaculo? celula_x celula_y+1 dim obstaculos)
                    (fora_ou_obstaculo? celula_x celula_y-1 dim obstaculos))
                (cond 
                  ( (testa_direita)
                    (procura lista_val celula_direita posicao_final (append trajecto (list celula_direita)) dim obstaculos tab))
                  ( (testa_esquerda)
                    (procura lista_val celula_esquerda posicao_final (append trajecto (list celula_esquerda)) dim obstaculos tab))
                  ( (testa_cima)
                    (procura lista_val celula_cima posicao_final (append trajecto (list celula_cima)) dim obstaculos tab))
                  ( (testa_baixo)
                    (procura lista_val celula_baixo posicao_final (append trajecto (list celula_baixo)) dim obstaculos tab))
                  
                  (else 
                   (movimentos-auxliares)))
                (movimentos-auxliares)))))))


