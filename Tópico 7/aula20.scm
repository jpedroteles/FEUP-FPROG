;;;;;;;;;;;;;;;;;;
; Abstracao fila ;
;;;;;;;;;;;;;;;;;;

; construtor
(define cria-fila
  (lambda ()
    (list 'fila:)))

; seletores
(define frente-fila
  (lambda (fila)
    (if (null? (cdr fila))
        #f
        (cadr fila))))
    

; modificadores
(define entra-na-fila!
  (lambda (fila elem)
    (append! fila (list elem))))

(define sai-da-fila!
  (lambda (fila)
    (if (not (null? (cdr fila)))
        (set-cdr! fila (cddr fila)))))

; testes da fila
;(define cantina (cria-fila))
;(frente-fila cantina)
;(entra-na-fila! cantina 'joao)
;(frente-fila cantina)
;(entra-na-fila! cantina 'maria)
;(frente-fila cantina)
;(entra-na-fila! cantina 'jose)
;(frente-fila cantina)
;(sai-da-fila! cantina)
;(frente-fila cantina)
;(sai-da-fila! cantina)
;(frente-fila cantina)

;;;;;;;;;;;;;;;;;;;
; Abstração Pilha ;
;;;;;;;;;;;;;;;;;;;

; construtor
(define cria-pilha
  (lambda ()
    (list 'pilha:)))

; seletores
(define topo-pilha
  (lambda (pilha)
    (if (null? (cdr pilha))
        #f
        (cadr pilha))))

; modificadores
(define poe-na-pilha!
  (lambda (pilha elem)
    (set-cdr! pilha (cons elem (cdr pilha)))))

(define tira-da-pilha!
  (lambda (pilha)
    (if (not (null? (cdr pilha)))
        (set-cdr! pilha (cddr pilha)))))

; testes da pilha
;(define discoteca (cria-pilha))
;(topo-pilha discoteca)
;(poe-na-pilha! discoteca 'uhf)
;(topo-pilha discoteca)
;(poe-na-pilha! discoteca 'acdc)
;(topo-pilha discoteca)
;(poe-na-pilha! discoteca 'pink-floyd)
;(topo-pilha discoteca)
;(tira-da-pilha! discoteca)
;(topo-pilha discoteca)
;(tira-da-pilha! discoteca)
;(topo-pilha discoteca)
;(tira-da-pilha! discoteca)
;(topo-pilha discoteca)

;;;;;;;;;;;;;;;;;;;;
; Abstracao Tabela ;
;;;;;;;;;;;;;;;;;;;;

; construtor
(define cria-tabela
  (lambda ()
    (list 'tabela:)))

; seletor
(define procura-na-tabela
  (lambda (tabela chave)
    (let ((pesquisa (assoc chave (cdr tabela))))
      (if (pair? pesquisa)
          (cdr pesquisa)
          pesquisa))))

; modificadores
(define insere-na-tabela!
  (lambda (tabela chave valor)
    (set-cdr! tabela (cons (cons chave valor) (cdr tabela)))))

; testes da tabela
;(define pauta (cria-tabela))
;pauta
;(insere-na-tabela! pauta 'joao 18)
;pauta
;(insere-na-tabela! pauta 'ana 19)
;pauta
;(insere-na-tabela! pauta 'tiago 20)
;pauta
;(insere-na-tabela! pauta 'luis 17)
;pauta
;(insere-na-tabela! pauta 'jose 15)
;pauta
;
;(procura-na-tabela pauta 'joao)
;(procura-na-tabela pauta 'joana)
;(procura-na-tabela pauta 'jose)
