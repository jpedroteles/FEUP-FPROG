; para ter acesso à funcionalidade associada 
; à abstracção audio
(require (lib "audio.scm" "user-feup"))

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
