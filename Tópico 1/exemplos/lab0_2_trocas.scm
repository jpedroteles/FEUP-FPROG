
; precisa de listas 

; (conta-trocas-$$ quantia unidades-monetarias)
; devolve numero de hipoteses de troca de uma quantia
; tendo em conta as unidades monetarias na lista (segundo parametro)

; (troca-$$ quantia unidades-monetarias)
; devolve hipotese de troca com menos unidades monetarias
; tendo em conta as unidades monetarias na lista (segundo parametro)

; (conta-trocas quantia)
; devolve numero de hipoteses de troca de uma quantia

; (troca quantia)
; devolve hipotese de troca com menos unidades monetarias

(define unidades-monetarias
  ; lista das unidades monetarias especificadas em centimos
  (list 50000 20000 10000 5000 2000 1000 500 200 100 50 20 10 5 2 1))

; ex: (conta-trocas-$$ 5 unidades-monetarias)

(define conta-trocas-$$
  (lambda (quantia lista-unid-monetaria)
    (if (or (null? lista-unid-monetaria)
            (negative? quantia))
        0
        (let* ((unidade (car lista-unid-monetaria))
               (resto (- quantia unidade)))
          (cond
            ((zero? resto)
             (add1 (conta-trocas-$$ quantia (cdr lista-unid-monetaria))))
            ((negative? resto)
             (conta-trocas-$$ quantia (cdr lista-unid-monetaria)))
            (else
             (+ (conta-trocas-$$ resto lista-unid-monetaria)
                (conta-trocas-$$ quantia (cdr lista-unid-monetaria)))))))))

;
; (conta-trocas 15)
;
(define conta-trocas
  (lambda (quantia)
    (conta-trocas-$$ quantia unidades-monetarias)))



(define troca                          ; determina e visualiza
  (lambda (quantia)                    ; as unidades monetarias minimas
    (troca-$$ quantia unidades-monetarias)))  ; para trocar quantia

(define troca-$$
  (lambda (quantia lista-unid-monetaria)
    (if (or (null? lista-unid-monetaria)
            (not (positive? quantia)))
        'ok
        (let ((unidade (car lista-unid-monetaria)))
          (if (>= quantia unidade)
              (begin ;;;;;;
                (if (< unidade 100)
                    (begin
                      (display unidade)
                      (display " centimo"))
                    (begin
                      (display (/ unidade 100))
                      (display " euro")))   ;;;;;;;;;;
                (newline)
                (troca-$$ (- quantia unidade) lista-unid-monetaria))
              (troca-$$ quantia (cdr lista-unid-monetaria)))))))
