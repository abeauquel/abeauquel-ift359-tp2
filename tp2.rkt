#lang racket
(provide losange-iter)
(provide losange-rec)

; ***************** IFT359 / TP2 Groupe 1
; ***************** Beauquel, Alexandre 19 034 135

(define (losange-iter l)
  (define (losange-iter-private ligne colonne maxLigne resteX nbX resteChar nbChar resultat)
    (if (eq? ligne maxLigne)
        (display (string-append resultat "                                       x\n"))
        (if(> resteChar (/ nbChar 2))
           (losange-iter-private ligne
                                (+ colonne 1)
                                maxLigne
                                resteX
                                nbX
                                (- resteChar 1)
                                nbChar
                                (string-append resultat " "))

           (if(> resteX 0)
              (losange-iter-private ligne
                                   (+ colonne 1)
                                   maxLigne
                                   (- resteX 1)
                                   nbX resteChar nbChar (string-append resultat "x"))
              (losange-iter-private
               (+ ligne 1) 0 maxLigne
               (recuperer-nombre-x (+ ligne 1) maxLigne nbX)
               (recuperer-nombre-x (+ ligne 1) maxLigne nbX)
               (recuperer-reste-character (recuperer-nombre-x (+ ligne 1) maxLigne nbX))
               (recuperer-reste-character (recuperer-nombre-x (+ ligne 1) maxLigne nbX))
               (string-append resultat "\n")
               )
              )
           )
       )
    )
  
  (if (or (not (number? l)) (zero? l) (negative? l) (> l 40))
   (displayln "Données invalides")
   (if (eq? l 1)
       (afficher-dernierX )
       (losange-iter-private 1 0 (-(* l 2 )1) 1 1 78 78 "")
       )
   )

)

;Vérification des entrées et appel de la fonction recursif
(define (losange-rec l)
  (if (or (not (number? l)) (zero? l) (negative? l) (> l 40))
   (displayln "Données invalides")
   (if (eq? l 1)
       (afficher-dernierX )
       (losange-rec-private 1 0 (-(* l 2 )1) 1 1 78 78)
       )
   )
)

;Fontion recursive pour le losange
(define (losange-rec-private ligne colonne maxLigne resteX nbX resteChar nbChar)
   (if (eq? ligne maxLigne)
       (afficher-dernierX )
       (if(> resteChar (/ nbChar 2))
             (and (display " ") (losange-rec-private ligne (+ colonne 1) maxLigne resteX nbX (- resteChar 1) nbChar))

             (if(> resteX 0)
                (and (display "x") (losange-rec-private ligne (+ colonne 1) maxLigne (- resteX 1) nbX resteChar nbChar))
                (and (newline) (losange-rec-private
                                (+ ligne 1) 0 maxLigne
                                (recuperer-nombre-x (+ ligne 1) maxLigne nbX)
                                (recuperer-nombre-x (+ ligne 1) maxLigne nbX)
                                (recuperer-reste-character (recuperer-nombre-x (+ ligne 1) maxLigne nbX))
                                (recuperer-reste-character (recuperer-nombre-x (+ ligne 1) maxLigne nbX))
                                ))
                )
       )
     )
   )

(define (recuperer-nombre-x ligne max nbX)
  (if(> ligne (+(/ max 2)1))
     (- nbX 2)
     (-(* ligne 2 ) 1)
  )
)
(define (recuperer-reste-character resteX)
  (- 78 resteX)
)

(define (afficher-dernierX )
       (displayln "                                       x");fin
)

(losange-rec 1)
(displayln "---------------")
(losange-rec 4)
(displayln "---------------")
(losange-iter 4)
(displayln "---------------")