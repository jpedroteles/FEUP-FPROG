(define valores (vector 10 20 30))
; retorna o ultimo valor de um vetor
(define vec-ultimo
  (lambda (v)
    (vector-ref v (- (vector-length v) 1))))
    
; soma os elementos de um vetor
(define vec-somatorio
  (lambda (v)
    (letrec ((aux
              (lambda (i soma)
                (if (< i (vector-length v))
                    (aux (add1 i) (+ soma (vector-ref v i)))
                    soma))))
      (aux 0 0))))
                    
; modificador - altera cada valor para o seu dobro
(define vec-dobro!
  (lambda (v)
    (letrec ((aux
              (lambda (i)
                (if (< i (vector-length v))
                    (begin
                      (vector-set! v i (* 2 (vector-ref v i)))
                      (aux (add1 i)))))))
      (aux 0))))
    
; construtor de um novo vetor com o dobro dos elementos
(define vec-dobro
  (lambda (v)
    (letrec ((novo (make-vector (vector-length v)))
             (aux
              (lambda (i)
                (if (< i (vector-length v))
                    (begin
                      (vector-set! novo i (* 2 (vector-ref v i)))
                      (aux (add1 i)))
                    novo))))
      (aux 0))))
