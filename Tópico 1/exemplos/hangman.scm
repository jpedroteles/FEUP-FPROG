;
; para lançar o programa
; (hangman)
;
;------------------------------------------------------
(define *palavras*
  '((p a l a v r a)
    (n o t a)
    (l i v r o)
    (j a n t a r)
    (e r r o)
    (a d i v i n h a r)
    (l e t r a)
    (c a r r o)
    (f u t e b o l)
    (a n d e b o l)
    (a t l e t i s m o)
    (c o m p u t a d o r)
    (p r e s i d e n t e)
    (t e s o u r e i r o)
    (a l u n o)
    (p r o f e s s o r)
    (d o c e n t e)
    (s e c r e t a r i a)
    (m o r a d i a)
    (r u a)
    (a v e n i d a)))

(define roleta        ; gera numero aleatorio entre 1 e limite
  (lambda (limite)
    (add1 (random limite))))

(define hangman
  (lambda()
    ; sortear palavra
    (letrec ((palavra
               (list-ref *palavras*
                         (sub1 (roleta (length *palavras*)))))
             (pal-a-adivi (limpa palavra))
             (letras-erradas '(ini))
             (aux
               (lambda()
                 (display "palavra a adivinhar: ")
                 (display pal-a-adivi)
                 (newline)
                 (display "letras erradas: ")
                 (display (cdr letras-erradas))
                 (cond
                   ((equal? palavra pal-a-adivi)
                    (newline)
                    (display "Acertou...")
                    (newline))
                   (else
                    (newline)
                    (display "Proxima letra: ")
                     (let ((letra (read)))
                       (cond
                         ((member letra palavra)
                          (actualiza! pal-a-adivi letra palavra))
                         (else
                           (display "errou...")
                           (newline)
                           (acrescenta! letras-erradas letra)))
                       (aux)))))))
      (aux))))

(define limpa
  (lambda (lista)
    (cond
      ((null? lista)
       '())
      (else
        (cons '*
              (limpa (cdr lista)))))))

(define actualiza!
  (lambda (adivi letra pal)
    (if (not (null? pal))
        (begin
          (if (equal? (car pal) letra)
              (set-car! adivi letra))
          (actualiza! (cdr adivi) letra (cdr pal))))))

(define acrescenta!
  (lambda (let-err letra)
    (append! let-err
             (list letra))))
