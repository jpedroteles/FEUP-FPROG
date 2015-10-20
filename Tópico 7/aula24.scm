; lista de nomes a adivinhar
(define nomes (list "Ana" "Antonio" "Joao" "Miguel" "Jose"))
(define cidades (list "Porto" "Braga" "Aveiro" "Lisboa" "Evora" "Faro"))

;;;;;;;;;;;;;;;;;;;;;
; Jogo do Enforcado ;
;;;;;;;;;;;;;;;;;;;;;

(define jogo-enforcado
  (lambda (dicionario tentativas)
    (let* ((palavra-chave (list-ref dicionario (random (length dicionario))))
          (palavra-escondida (make-string (string-length palavra-chave) #\*))
          (letras-erradas (list 'erradas)))
    (display ";;;;;;;;;;;;;;;;;;;;;")(newline)
    (display "; Jogo do Enforcado ;")(newline)
    (display ";;;;;;;;;;;;;;;;;;;;;")(newline)
    (newline)
      (faz-jogada palavra-chave palavra-escondida letras-erradas tentativas))))
  
; faz jogada
(define faz-jogada
  (lambda (palavra-chave palavra-escondida letras-erradas tentativas)
    (cond
      ; verifica se venceu
      ((string-ci=? palavra-chave palavra-escondida)
       (display "Parabens! Venceu o jogo")(newline)
       (display "Palavra escondida: ")
       (display palavra-escondida))
      
      ; verifica se perdeu
      ((> (length letras-erradas) (add1 tentativas))
       (display "Perdeu! Acabaram as tentativas.")(newline)
       (display "Palavra escondida: ")
       (display palavra-chave))
      
      ; volta a jogar
      (else
       ; Mostra palavra escondida
       (display "Palavra a adivinhar: ")
       (display palavra-escondida)
       (newline)
       (display "Letras erradas: ")
       (display (cdr letras-erradas))
       (newline)
       
       ; le letra
       (display "Indique uma letra: ")
       (let ((letra (string-ref (read-line) 0)))
         ; desvenda a palavra escondida
         (if (desvenda! letra palavra-chave palavra-escondida)
             (faz-jogada palavra-chave palavra-escondida letras-erradas tentativas)
             (faz-jogada palavra-chave palavra-escondida (append letras-erradas (list letra)) tentativas))))
)))

; desvenda a palavra escondida
(define desvenda! 
  (lambda (letra palavra-chave palavra-escondida)
    (letrec ((aux
              (lambda (i encontrou)
                (if (>= i (string-length palavra-chave))
                    encontrou
                    ; verifica letra
                    (if (char-ci=? letra (string-ref palavra-chave i))
                        ; encontrou a letra
                        (begin 
                          (string-set! palavra-escondida i
                                       (string-ref palavra-chave i))
                          (aux (add1 i) #t)) 
                        ; não encontrou
                        (aux (add1 i) encontrou))))))
      (aux 0 #f))))