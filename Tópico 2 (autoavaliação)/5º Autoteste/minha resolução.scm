;Problema 1

(define raccons
  (lambda (num denom)
    (cons num denom)))

(define racnum
  (lambda (frac)
    (car frac)))

(define racdenom
  (lambda (frac)
    (cdr frac)))

(define racvisu
  (lambda (frac)
    (display (racnum frac))
    (display "/")
    (display (racdenom frac))))


(define racreduz
  (lambda (frac)
    (let ((redu (gcd (racnum frac) 
                     (racdenom frac))))
      (raccons (/ (racnum frac) 
                  redu)
               (/ (racdenom frac) 
                  redu)
      ))))


;Problema 2

(define carta '(as duque tres quatro cinco seis sete oito nove dez valete dama rei))
(define naipe '(copas ouros paus espadas))

(define da-carta
  (lambda ()
    (cons (list-ref carta (random 13))
          (list-ref naipe (random 4)))))

;testes
;(define f (raccons 2 4))
;(racnum f)
;(racdenom f)
;(racvisu f)
;(define f1 (racreduz f))
;(racvisu f1)
;(define f2 (raccons 5 5))
;(define f3 (raccons 0 5))
;(racvisu f2)
;(racvisu f3)
;(define f4 (racreduz f2))
;(define f5 (racreduz f3))
;(racvisu f4)
;(racvisu f5)