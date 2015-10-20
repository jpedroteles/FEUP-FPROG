; para ter acesso à funcionalidade associada 
; à abstracção audio
(require (lib "audio.scm" "user-feup"))

(define diz-numeros-em-ordem-decrescente
  (lambda (n-partida)
    (if (zero? n-partida)
        (som "bomba1")
        (begin
          (dizer-num-3-digitos n-partida)
          (diz-numeros-em-ordem-decrescente (sub1 n-partida))))))
;
(define dizer-num-3-digitos
  (lambda (n)
    ; separar os dígitos 
    ;    c - das centenas, d - dezenas e u - unidades
    (let ((c (quotient n 100))
          (d (remainder (quotient n 10) 10))
          (u (remainder n 10)))
      
      ; dizer as centenas do número
      (dizer-centenas n)
      ; se necessário, liga centenas com dezenas
      (if (and (> c 0) (> d 0))
          (som "e"))
      ; dizer as dezenas do número...
      ; incluindo os casos especiais de 10 a 19
      (dizer-dezenas n)
      
      ; se não for caso de 10 a 19
      ;      se necessário, liga dezenas com unidades do número
      ;      e dizer as unidades do número
      (if  (not (= d 1))
           (begin
             (if (and 
                  (or (> c 0) (> d 0))
                  (> u 0))
                 (som "e"))
             (dizer-unidades n))))))
;
(define dizer-centenas
  (lambda (n)
    ; se for caso especial 100
    (if (= n 100)
        (som "100")
        ; se não, casos que concatenam com as dezenas ou as unidades
        ; separa digito das centenas
        (let ((c (quotient n 100)))
          ; se o dígito das centenas for 9, 8, ..., 1,
          ; assim dirá "900",  "800", ..., "cento".
          (cond
            ((= c 9) 
             (som "900"))
            ((= c 8)
             (som "800"))
            ((= c 7)
             (som "700"))
            ((= c 6)
             (som "600"))
            ((= c 5)
             (som "500"))
            ((= c 4)
             (som "400"))
            ((= c 3)
             (som "300"))
            ((= c 2)
             (som "200"))
            ((= c 1)
             (som "c100")))))))
;
(define dizer-dezenas 
  (lambda (n) 
    ; separa digitos das dezenas e das unidades
    (let ((d (remainder (quotient n 10) 10))
          (u (remainder n 10)))
      ; se casos especiais de 19 a 10...
      ; se o dígito das unidades for 9, 8, .., ou 0
      ; assim dirá "19", "18", ... ou "10"
      (if (= d 1)
          (cond 
            ((= u 9)(som "19"))
            ((= u 8)(som "18"))
            ((= u 7)(som "17"))
            ((= u 6)(som "16"))
            ((= u 5)(som "15"))
            ((= u 4)(som "14"))
            ((= u 3)(som "13"))
            ((= u 2)(som "12"))
            ((= u 1)(som "11"))
            ((= u 0)(som "10")))
          ; se não, casos que concatenam com as unidades: 20, 30, ... ou 90
          ; se o dígito das dezenas for 2, 3, ... ou 9
          ; assim dirá "20", "30", ... ou "90".
          (cond
            ((= d 2)(som "20"))
            ((= d 3)(som "30"))
            ((= d 4)(som "40"))
            ((= d 5)(som "50"))
            ((= d 6)(som "60"))
            ((= d 7)(som "70"))
            ((= d 8)(som "80"))
            ((= d 9)(som "90")))))))
;
(define dizer-unidades 
  (lambda (n)  
    (let ((u (remainder n 10)))
      ;          ; u de 1 a 9
      ;          ; u = 0 não é "dito"...
      (cond ((= u 1)
             (som "1"))
            ((= u 2)
             (som "2"))
            ((= u 3)
             (som "3"))
            ((= u 4)
             (som "4"))
            ((= u 5)
             (som "5"))
            ((= u 6)
             (som "6"))
            ((= u 7)
             (som "7"))
            ((= u 8)
             (som "8"))
            ((= u 9)
             (som "9"))))))
  