(define maiuscula
  (lambda (c)
    (let ((diferenca (- (char->integer #\a) (char->integer #\A))))
;    (if (and (>= (char->integer c) (char->integer #\a))
;             (<= (char->integer c) (char->integer #\z)))
    (if (and (char>=? c #\a)
             (char<=? c #\z))
        (integer->char (- (char->integer c) diferenca))
        c))))

; strings
(define saudacao1 (string #\B #\o #\m #\space #\d #\i #\a))
(define saudacao2 "Bom dia") ; imutavel
(define saudacao3 (string-copy "Bom dia") )
;(make-string 5 #\*)
;(substring saudacao1 0 3) ; Bom
;(substring saudacao1 4 7) ; dia
;(string-fill! saudacao3 #\*)
(define simpatico
  (lambda ()
    (let ((nome (begin (display "O seu nome: ") (read-line))))
      (display (string-append saudacao1 " " nome)))))

; maiusculas
(define maiusculas!
  (lambda (str)
    (letrec ((aux
              (lambda (i)
                (if (< i (string-length str))
                    ; converte para maiusculas
                    (begin 
                      (string-set! str i (maiuscula (string-ref str i)))
                      (aux (add1 i)))))))
      (aux 0))))
      
    
