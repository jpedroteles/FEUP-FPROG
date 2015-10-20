; constante de tempo para, na animação, se poder observar 
; com alguma "calma"

(define repouso 500) 

; inclusão da biblioteca gráfica

(require (lib "swgr.scm" "user-feup"))

; inclusão da biblioteca com vários procedimentos
; 
(require (lib "varios.scm" "user-feup"))
;-----------------------------------------------------------------
;                                                                -
; (torre-de-hanoi 4)                                       -
; solução sob a forma de uma lista de movimentos, para 4 anéis   -
; mas incluindo a dimensão dos anéis                             -
;                                                                -
; (torre-de-hanoi-1 4)                                           -
; solução sob a forma de uma lista de movimentos, para 4 anéis   -
;                                                                -
; (hanoi-anim n-aneis largura-janela altura-janela)              -
; (hanoi-anim 4 400 400)                                         -
; solução sob a forma gráfica, para 4 anéis                      -
;-----------------------------------------------------------------


; -------- torre-de-hanoi ------------
; --- solução: lista de movimentos  ---
(define torre-de-hanoi-1
  (lambda (n-aneis)
    (mover-1 n-aneis 'e 'd 'c)))

(define mover-1
  (lambda (num orig dest aux)
    (cond ((zero? num) '())
          ((= num 1)
           (list (list orig dest)))
          (else
            (append
              (mover-1 (sub1 num) orig aux dest)
              (list (list orig dest))
              (mover-1 (sub1 num) aux dest orig))))))

; -------- torre-de-hanoi ------------
; --- solução: lista de movimentos, incluindo a dimensão dos anéis

(define torre-de-hanoi
  (lambda (n-aneis)
    (mover n-aneis 'e 'd 'c)))

(define mover
  (lambda (num orig dest aux)
    (cond ((zero? num) '())
          ((= num 1)
           (list (list num orig dest)))
          (else
            (append
              (mover (sub1 num) orig aux dest)
              (list (list num orig dest))
              (mover (sub1 num) aux dest orig))))))
;
; --------- torre-de-hanoi animada ------------
(define hanoi-anim
  (lambda (n-aneis larg-janela alt-janela)
    (let ((larg-base (quotient larg-janela 3))
          (alt-base (quotient alt-janela (* 10 n-aneis))))
      (let ((torre-e (faz-torre (list larg-base alt-base n-aneis)
                                (list (+ (* .2 larg-janela) 10)
                                      (* .5 alt-janela))))
            
            (torre-d (faz-torre (list larg-base alt-base n-aneis)
                                (list (- (* .8 larg-janela) 10)
                                      (* .5 alt-janela))))
            
            (torre-c (faz-torre (list larg-base alt-base n-aneis)
                                (list (* .5 larg-janela)
                                      10)))
            (lista-movimentos (torre-de-hanoi n-aneis)))
        (inicializa-torre! torre-e n-aneis)
        (janela larg-janela alt-janela "hanoi-anim")
        (display "qualquer tecla para avancar...")
        (read)
        (visu-torre torre-e)
        (visu-torre torre-d)
        (visu-torre torre-c)
        ;
        (visu-movimentos torre-e torre-d torre-c
                         lista-movimentos)))))

(define visu-movimentos
  (lambda (t-e t-d t-c lis-movimentos)
    (if (null? lis-movimentos)
        'ok
        (let ((anel-dim-e-cor (caar lis-movimentos))
              (simb-tor-ori (cadar lis-movimentos))
              (simb-tor-dest (caddar lis-movimentos))
              ;
              (identifica-torre
                (lambda (simb-de-torre)
                  (cond ((equal? simb-de-torre 'e)
                         t-e)
                        ((equal? simb-de-torre 'd)
                         t-d)
                        (else t-c)))))
          (let ((tor-ori (identifica-torre simb-tor-ori))
                (tor-dest (identifica-torre simb-tor-dest)))
            (actualiza-torres tor-ori tor-dest)
            
            (visualiza-seta simb-tor-ori tor-ori
                            simb-tor-dest tor-dest
                            2)
            (espera repouso)
            (visu-torre-limpa tor-ori)
            (visu-torre tor-ori)
            (visu-torre tor-dest)
            (espera (* 3 repouso))
            (visu-movimentos t-e t-d t-c (cdr lis-movimentos)))))))
;
; ------- abstracção torre-de-hanoi
(define faz-torre
  (lambda (lis-lar-alt-nmaxaneis lis-x-y)
    (list lis-lar-alt-nmaxaneis lis-x-y)))

(define inicializa-torre!
  (lambda (torre n-aneis)
    (letrec ((ciclo
               (lambda (dim-e-cor)
                 (if (not (zero? dim-e-cor))
                     (begin
                       (poe-anel! dim-e-cor torre)
                       (ciclo (sub1 dim-e-cor)))))))
      (ciclo n-aneis))))

(define poe-anel!
  (lambda (dim-e-cor torre)
    (letrec ((vai-ate-ao-fim
               (lambda (torre-em-actual)
                 (if (null? (cdr torre-em-actual))
                     (set-cdr! torre-em-actual
                               (list dim-e-cor))
                     (vai-ate-ao-fim (cdr torre-em-actual))))))
      (vai-ate-ao-fim torre))))

(define tira-anel!
  (lambda (torre)
    (letrec ((vai-ate-ao-fim
               (lambda (torre-em-actual)
                 (if (null? (cddr torre-em-actual))
                     (let ((anel-a-sair (cadr torre-em-actual)))
                       (set-cdr! torre-em-actual '())
                       anel-a-sair)
                     (vai-ate-ao-fim (cdr torre-em-actual))))))
      (vai-ate-ao-fim torre))))


(define visu-torre
  (lambda (torre)
    (let ((larg-base (caar torre))
          (alt-base (cadar torre))
          (ori-base (cadr torre)))
      (pinta-base-da-torre ori-base larg-base alt-base)
      (pinta-eixo-da-torre ori-base larg-base alt-base)
      (pinta-aneis-da-torre torre))))

(define pinta-base-da-torre
  (lambda (ori-base larg-base alt-base)
    (cor 0)
    (pinta-rectangulo ori-base larg-base alt-base)))

(define pinta-eixo-da-torre
  (lambda (ori-base larg-base alt-base)
    (cor 0)
    (pinta-rectangulo (list (car ori-base)
                            (+ (cadr ori-base)
                               alt-base))
                      alt-base
                      (* .7 larg-base))))

(define pinta-rectangulo
  (lambda (ori larg alt)
    (let ((larg/2 (/ larg 2)))
      (move ori)
      (pinta-rel (list (list 0 0)
                       (list larg/2 0)
                       (list 0 alt)
                       (list (- larg) 0)
                       (list 0 (- alt))
                       (list larg/2 0))))))

(define pinta-aneis-da-torre
  (lambda (torre)
    (let ((larg-base.8 (* .8 (caar torre)))
          (alt-base.8 (* 1 (cadar torre)))
          (alt-base1.5 (* 1.8 (cadar torre)))
          (n-max-aneis (caddar torre))
          (ori-base (cadr torre))
          (aneis-da-torre (cddr torre)))
      (letrec ((pinta-aneis
                 (lambda (lista-aneis origem)
                   (if (not (null? lista-aneis))
                       (begin
                         (cor (remainder (car lista-aneis) 26))
                         (pinta-rectangulo
                           origem
                           (/ (* larg-base.8 (car lista-aneis))
                              n-max-aneis)
                           alt-base.8)
                         (pinta-aneis (cdr lista-aneis)
                                      (list (car origem)
                                            (+ (cadr origem)
                                               alt-base1.5))))))))
        (pinta-aneis aneis-da-torre
                     (list (car ori-base)
                           (+ (cadr ori-base)
                              alt-base1.5)))))))

(define visu-torre-limpa
  (lambda (torre)
    (let ((larg-base (caar torre))
          (alt-base (cadar torre))
          (ori-base (cadr torre)))
      (let ((larg-base/2 (/ larg-base 2)))
        (move (list (+ (car ori-base)
                       larg-base/2)
                    (+ (cadr ori-base)
                       (* 1.2 alt-base))))
        (cor 26)
        (pinta-rel (list (list 0 0)
                         (list 0 (* .7 larg-base))
                         (list (- larg-base) 0)
                         (list 0
                               (- (* .7 larg-base)))))
        (pinta-eixo-da-torre ori-base larg-base alt-base)))))

(define actualiza-torres
  (lambda (torre-ori torre-dest)
    (let ((anel-a-mover (tira-anel! torre-ori)))
      (poe-anel! anel-a-mover torre-dest))))

;
; ----- visualização das setas ---------
;
(define visualiza-seta
  (lambda (s-t-ori t-ori s-t-dest t-dest n-piscas)
    (let ((a 30)
          (desloca-x (* .5 (caar t-ori)))
          (desloca-y (* .7 (caar t-ori))))
      (let ((a.5 (* .5 a))
            (a1.5 (* 1.5 a)))
        (let ((n-pis (if (even? n-piscas)
                         n-piscas
                         (add1 n-piscas)))
              (ini-seta (cadr t-ori))
              (seta (list (list 0 0)
                          (list 0 a)
                          (list a (- a.5))
                          (list 0 a)
                          (list a (- a1.5))
                          (list (- a) (- a1.5))
                          (list 0 a)
                          (list (- a) (- a.5)))))
          (cor 'temp)
          (cond
            ; seta esquerda-direita
            ((and (equal? s-t-ori 'e)
                  (equal? s-t-dest 'd))
             (visu-seta (list (+ (car ini-seta)
                                 desloca-x)
                              (+ (cadr ini-seta)
                                 desloca-y))
                        seta
                        0
                        n-pis))
            ; seta direita-esquerda
            ((and (equal? s-t-ori 'd)
                  (equal? s-t-dest 'e))
             (visu-seta (list (- (car ini-seta)
                                 desloca-x)
                              (+ (cadr ini-seta)
                                 desloca-y))
                        seta
                        180
                        n-pis))
            ; seta esquerda-centro
            ((and (equal? s-t-ori 'e)
                  (equal? s-t-dest 'c))
             (visu-seta (list (car ini-seta)
                              (- (cadr ini-seta) 20))
                        seta
                        90
                        n-pis))
            ; seta centro-esquerda
            ((and (equal? s-t-ori 'c)
                  (equal? s-t-dest 'e))
             (visu-seta (list (- (car ini-seta)
                                 (* 2 desloca-x))
                              (+ (cadr ini-seta) 20))
                        seta
                        -90
                        n-pis))
            ;seta direita-centro
            ((and (equal? s-t-ori 'd)
                  (equal? s-t-dest 'c))
             (visu-seta (list (car ini-seta)
                              (- (cadr ini-seta) 20))
                        seta
                        90
                        n-pis))
            (else
              ; seta centro-direita
              (visu-seta (list (+ (car ini-seta)
                                  (* 2 desloca-x))
                               (+ (cadr ini-seta) 20))
                         seta
                         -90
                         n-pis))))))))

(define visu-seta
  (lambda (ori seta ang n-piscas)
    (let ((seta-rodada (rodar seta ang)))
      (letrec ((piscar
                 (lambda (n-pis)
                   (if (not (zero? n-pis))
                       (begin
                         (move ori)
                         (pinta-rel seta-rodada)
                         (espera repouso)
                         (piscar (sub1 n-pis)))))))
        (piscar n-piscas)))))


(define rodar
  (lambda (lis-pontos ang)
    (let ((cos-a (cos (degrees->radians ang)))
          (sin-a (sin (degrees->radians ang))))
      (letrec ((aux
                 (lambda (lis)
                   (if (null? lis)
                       '()
                       (let ((x (caar lis))
                             (y (cadar lis)))
                         (cons (list (+ (* x cos-a) (* y sin-a))
                                     (- (* y cos-a) (* x sin-a)))
                               (aux (cdr lis))))))))
        (aux lis-pontos)))))

(define espera
  (lambda (n-voltas)
    (if (zero? n-voltas)
        #f
        (espera (sub1 n-voltas)))))







