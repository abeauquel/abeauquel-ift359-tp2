#lang racket
(provide losange-iter)
(provide losange-rec)

; ***************** IFT359 / TP2 Groupe 1
; ***************** Beauquel, Alexandre 19 034 135

(define (losange-iter l)

  0
)

(define (losange-rec l)
  (if (or (not (number? l)) (zero? l) (negative? l) (> l 40))
   (displayln "Données invalides")
   (if (eq? l 1)
       (displayln "x")
       (losange-rec-private 1 0 (-(* l 2 )1) 1 1)
       )
   )
)

(define (losange-rec-private ligne colonne maxLigne resteX nbX)
   (if (eq? ligne maxLigne)
       (displayln "x");fin
       (if(> resteX 0)
          (and (display "x") (losange-rec-private ligne (+ colonne 1) maxLigne (- resteX 1) nbX))
          (and (newline) (losange-rec-private
                          (+ ligne 1) 0 maxLigne
                          (get-nombre-x (+ ligne 1) maxLigne nbX)
                          (get-nombre-x (+ ligne 1) maxLigne nbX)))
       )
     )
   )

(define (recuperer-ligne-suivante ligne maxLigne)
  (+ ligne 1)
)

(define (get-nombre-x ligne max nbX)
  (if(> ligne (+(/ max 2)1))
     (- nbX 2)
     (-(* ligne 2 ) 1)
  )
)
(define (get-suivant l max)
  (if(l > max)
     (- l 1)
     (+ l 1)
  )
)


(losange-rec 1)
(displayln "---------------")
(losange-rec 3)
(displayln "---------------")
(losange-rec 4)
(displayln "---------------")