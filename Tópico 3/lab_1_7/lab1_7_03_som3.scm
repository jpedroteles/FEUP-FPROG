; para ter acesso à funcionalidade associada 
; à abstracção audio
(require (lib "audio.scm" "user-feup"))

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

