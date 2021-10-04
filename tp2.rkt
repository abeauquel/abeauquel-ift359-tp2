#lang racket
(provide losange-iter)
(provide losange-rec)

; ***************** IFT359 / TP2 Groupe 1
; ***************** Beauquel, Alexandre 19 034 135
; ***************** Poulin, Sebastien 19 093 892


;Fonction iterative pour le losange
(define (losange-iter l)
  (define (losange-iter-private ligne colonne maxLigne resteX nbX resteChar nbChar resultat) ;Definition de la fonction de recursion interne
    (if (eq? ligne maxLigne)
        (display "                                       x\n")
        (if(> resteChar (/ nbChar 2))
           (losange-iter-private ligne
                                (+ colonne 1)
                                maxLigne
                                resteX
                                nbX
                                (- resteChar 1)
                                nbChar
                                (display " "))

           (if(> resteX 0)
              (losange-iter-private ligne
                                   (+ colonne 1)
                                   maxLigne
                                   (- resteX 1)
                                   nbX resteChar nbChar (display "x"))
              (losange-iter-private
               (+ ligne 1) 0 maxLigne
               (recuperer-nombre-x (+ ligne 1) maxLigne nbX)
               (recuperer-nombre-x (+ ligne 1) maxLigne nbX)
               (recuperer-reste-character (recuperer-nombre-x (+ ligne 1) maxLigne nbX))
               (recuperer-reste-character (recuperer-nombre-x (+ ligne 1) maxLigne nbX))
               (display "\n")
               )
              )
           )
       )
    )

  ;Vérification des entrées
    (cond
   [(or (not (number? l)) (not (integer? l)) (negative? l)) (displayln "Données invalides")] ; Si la long. n'est pas un nombre, negatif ou decimal
   [(eq? l 1) (afficher-dernierX )]
   [(eq? l 0) (display "\n")]
   [(> l 39) (displayln "Losange trop grand")]
   [else (losange-iter-private 1 0 (-(* l 2 )1) 1 1 78 78 "")] ;Appel initial
   )

)

;Vérification des entrées et appel de la fonction recursif
(define (losange-rec l)

  (cond
   [(or (not (number? l)) (not (integer? l)) (negative? l)) (displayln "Données invalides")]
   [(eq? l 1) (afficher-dernierX )]
   [(eq? l 0) (display "\n")]
   [(> l 39) (displayln "Losange trop grand")]
   [else (losange-rec-private 1 0 (-(* l 2 )1) 1 1 78 78)]
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

;recuperer le nombre de x à afficher en fonction de la ligne,
;du max de ligne dans le losange et du nombre precedent de x
(define (recuperer-nombre-x ligne max nbX)
  (if(> ligne (+(/ max 2)1))
     (- nbX 2)
     (-(* ligne 2 ) 1)
  )
)
;recuperer le nombre de character qu'il reste à afficher 
(define (recuperer-reste-character resteX)
  (- 78 resteX)
)

; afficher la derniere ligne du losange
(define (afficher-dernierX )
       (displayln "                                       x");fin
)


;(losange-rec 1)
;(losange-iter 15)