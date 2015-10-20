; Exercicio 1.1

(define fruta
  (lambda (laranjas peras)
    (cond ((> laranjas peras)(display "1"))
          ((< laranjas peras)(display "2"))
          ((and (zero? peras)(zero? laranjas))
           (display "0"))
          ((= laranjas peras)(display "3")))))

; Exercicio 1.2

(define fruta
  (lambda (laranjas peras)
    (cond ((or (negative? peras)(negative? laranjas))
           (display "-1"))
          ((> laranjas peras)(display "1"))
          ((< laranjas peras)(display "2"))
          ((and (zero? peras)(zero? laranjas))
           (display "0"))
          ((= laranjas peras)(display "3")))))

; Exercicio 1.3

(define muita-fruta
  (lambda (laranjas peras mangas bananas figos)
    (cond ((and ((< figos bananas)(> (+ laranjas peras)(+ bananas figos))))(display "3"))
          ((and ((> figos bananas)(> (+ laranjas peras)(+ bananas figos))))(display "4"))
          ((> (+ laranjas peras)(+ bananas figos))(display "1"))
          ((< (+ laranjas peras)(+ bananas figos))(display "2")))))


; Exercicio 2.2

(define falso->verdadeiro
  (lambda (num-falso)
    (cond ((= num-falso 0)(display"0"))
          ((= num-falso 2)(display"1"))
          ((= num-falso 4)(display"2"))
          ((= num-falso 6)(display"3"))
          ((= num-falso 8)(display"4"))
          ((= num-falso 1)(display"5"))
          ((= num-falso 3)(display"6"))
          ((= num-falso 5)(display"7"))
          ((= num-falso 7)(display"8"))
          ((= num-falso 9)(display"9")))))

