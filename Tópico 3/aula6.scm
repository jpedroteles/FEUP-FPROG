(define area-int-ret
  (lambda (x1m x1M y1m y1M x2m x2M y2m y2M)
    (let (
          ; 1. calcular deltax
          (deltax (if (or (< x2M x1m) (> x2m x1M))
                      0
                      (- (min x1M x2M)
                         (max x1m x2m))))
          ; 2. calcular deltay
          (deltay (if (or (< y2M y1m) (> y2m y1M))
                      0
                      (- (min y1M y2M)
                         (max y1m y2m)))))
      ; 3. valor da area
      (* deltax deltay))))
          
                      
; testes
(define testa
  (lambda (teste correto)
    (if (= teste correto) 
        (display "OK")(display "ERRO"))
    (newline)))


(testa (area-int-ret 1 2 1 2 3 4 1 2) 0); 0
(testa (area-int-ret 1 5 0 4 2 7 2 6) 6); 6
(testa (area-int-ret 1 5 0 4 1 5 0 4) 16) ; 16
(testa (area-int-ret 1 5 0 4 1 5 10 14) 0) ; 0

        