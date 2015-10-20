
; Tempo O(1); Espaco: O(1)
(define dobro
  (lambda (n)
    (* n 2)))

; Tempo O(n); Espaco: O(n)
(define somatorio
  (lambda (a b)
    (if (= a b)
        a
        (+ a (somatorio (add1 a) b)))))

; Tempo O(n); Espaco: O(1)
(define somatorio-iter
  (lambda (a b)
    (letrec ((aux
             (lambda (i soma)
               (if (> i b)
                   soma
                   (aux (add1 i) (+ i soma))))))
      (aux a 0))))

; Tempo O(1); Espaco: O(1)
(define somatorio-melhorado
  (lambda (a b)
    (/ (* (add1 (- b a))
          (+ a b))
       2)))

; Tempo O(n2); Espaco: O(1)
(define tabela
  (lambda (n)
    (letrec ((aux
              (lambda (l c)
                (cond
                  ((zero? c) 
                   (newline)
                   (if (> l 1)
                       (aux (sub1 l) n)))
                  (else
                   (display "O")
                   (aux l (sub1 c)))))))
      (aux n n))))


; Tempo O(n); Espaco: O(n)
(define potencia
  (lambda (b e)
    (display ".")
    (if (zero? e)
        1
        (* b (potencia b (sub1 e))))))

; Tempo O(log2(n)); Espaco: O(log2(n))
(define potencia-melhorada
  (lambda (b e)
    (letrec ((quadrado
              (lambda (x)
                (* x x))))
      (display ".")
      (cond
        ((zero? e) 1)
        ((even? e) (quadrado (potencia-melhorada b (/ e 2))))
        ((odd? e) (* b (potencia-melhorada b (sub1 e))))))))