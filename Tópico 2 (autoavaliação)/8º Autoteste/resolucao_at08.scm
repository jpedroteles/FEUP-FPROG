; ==============================================================
; FUNDAMENTOS DA PROGRAMACAO - MIEIC
; ==============================================================
; Auto teste 08
;
; RESOLUCAO
;
; -------------------------------------
; pergunta 1
; -------------------------------------
;
; 1.1
(define conta-letras
  (lambda (cadeia caracter)
    (letrec ((aux
              (lambda (i soma)
                (if (>= i (string-length cadeia))
                    soma
                    (if (char-ci=? caracter (string-ref cadeia i))
                        (aux (add1 i) (add1 soma))
                        (aux (add1 i) soma))))))
      (aux 0 0))))
; 1.2.
(define conta-vogais 
  (lambda (vec)
    (letrec ((vogais (make-vector 5 0))
             (aux
              (lambda (i)
                (if (>= i (vector-length vec))
                    vogais ; retorna o resultado final
                    (begin ; actualiza os resultados das vogais
                      ; a
                      (vector-set! vogais 0 (+ (vector-ref vogais 0)
                                               (conta-letras (vector-ref vec i) #\a)))
                      ; e
                      (vector-set! vogais 1 (+ (vector-ref vogais 1)
                                               (conta-letras (vector-ref vec i) #\e)))
                      ; i
                      (vector-set! vogais 2 (+ (vector-ref vogais 2)
                                               (conta-letras (vector-ref vec i) #\i)))
                      ; o
                      (vector-set! vogais 3 (+ (vector-ref vogais 3)
                                               (conta-letras (vector-ref vec i) #\o)))
                      ; u
                      (vector-set! vogais 4 (+ (vector-ref vogais 4)
                                               (conta-letras (vector-ref vec i) #\u)))
                      (aux (add1 i)))))))
      (aux 0))))
;
; -------------------------------------
; pergunta 2
; -------------------------------------
;
(define todas-as-vendas (list (cons "Manuel" 235) (cons "Maria" 45) (cons "Manuel" 25) (cons "Maria" 55) (cons "Ana" 457)(cons "Manuel" 40)))

; toma um vector de vendas
; e gera uma lista, não ordenada, de vendas acumuladas 

(define acumula!
  (lambda (lista)
    (letrec ((remove-prox! ; remove o segundo elemento da lista, caso tenha o nome passado como argumento
              (lambda (lista nome)
                (if (not (or (null? lista) (null? (cdr lista))))
                    (if (string-ci=? (caadr lista) nome) ; nome do segundo da lista
                        (begin  ; elimina o segundo da lista
                          (set-cdr! lista (cddr lista))
                          (remove-prox! lista nome))
                        (remove-prox! (cdr lista) nome)))))
             (soma-todos
              (lambda (lista nome)
                (if (null? lista)
                    0
                    (if (string-ci=? (caar lista) nome)
                        (+ (cdar lista) (soma-todos (cdr lista) nome)) 
                        (soma-todos (cdr lista) nome))))))
    (if (not (null? lista)) 
        (begin
          (set-cdr! (car lista) (soma-todos lista (caar lista))) ; acumula os valores
          (remove-prox! lista (caar lista)) ; elimina repetições
          (acumula! (cdr lista)))))))
