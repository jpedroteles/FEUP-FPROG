;Auto Avaliacao em 03 Outubro 2013
;FPRO - Prof. António Coelho
;Aluno: Dante Marinho


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Visualizar fracao ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define visu-fraccao
  (lambda (n d)
    (display "Frac: ")
    (display n)
    (display "/")
    (display d)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Prodecimento fracao reduzida ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define reduz
  (lambda (n d)
    (define mdc (gcd n d))  ;MDC dos dois numeros
    ;(display ".::Fração Reduzida::.")
    (newline)
    (display "MDC = ")
    (display mdc)
    (newline)  
    ;Agora chama a funcao visu-fraccao:
    (visu-fraccao (/ n mdc)
                  (/ d mdc))
 ) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Prodecimento multiplicar fraccao ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define multiplicar-fraccoes
  (lambda (n1 d1 n2 d2)
    (define numeradores (* n1 n2)) ;multiplica numeradores
    (define denominadores (* d1 d2)) ;multiplica denominadores
    ;Agora chama a funcao reduz:
    (reduz numeradores denominadores) ;chama a função "reduz"    
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Prodecimento dividir fracao ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dividir-fraccoes
  (lambda (n1 d1 n2 d2)
    ;Agora chama a funcao multiplicar-fraccoes:
    (multiplicar-fraccoes n1 d1 d2 n2)    
    )
  )