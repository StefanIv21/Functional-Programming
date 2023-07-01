#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)


      
     
  (let*  ((reverse-eng (map (lambda (x) (cons (cdr x) (car x))) engagements))
           (prima-lista  (filter  (lambda (x) (better-match-exists? (cdr x) (car x) (get-pref-list mpref (cdr x)) wpref engagements))engagements))
           (doua-lista   (filter  (lambda (x) (better-match-exists? (cdr x) (car x) (get-pref-list wpref (cdr x)) mpref reverse-eng)) reverse-eng))
           (invers-doua-lista (map (lambda (x) (cons (cdr x) (car x))) doua-lista))
           (lista-prov (append prima-lista invers-doua-lista)))
     
      (let construire ((lista-prov lista-prov) (lista-finala '()))
        (if (null? lista-prov) lista-finala
            (if (member (car lista-prov) lista-finala)
                (construire (cdr lista-prov)   lista-finala)
                (construire (cdr lista-prov)  (cons (car lista-prov) lista-finala)))))))
          


; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
   (let  construire ((free-men free-men) (engagements engagements))
    (if (null? free-men) engagements
        

(let construire2 ((lista  (get-pref-list mpref (car free-men))) (men  (car free-men)) )
       (let * (( partener (get-partner engagements (car lista)))
               (men-prefered (get-pref-list wpref (car lista))))
       (if (null? lista) engagements
          (if (not partener)
             
                (construire (cdr free-men) (cons (cons (car lista) men) engagements) )
                    
             

              (if (preferable? men-prefered  men partener)
                  
                  
                  (construire (cdr (append  free-men  (list partener)))

                   (map (lambda (x) (if (equal? (car lista) (car x))
                                            (cons (car x) men)
                                            (cons (car x) (cdr x)
                                           ))) engagements))
                         
                    
                      (construire2 (cdr lista) men  )))))))))

     




; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
 (engage (get-men mpref) '() mpref wpref))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
   (foldl (lambda (x acc) (append (list (car x)) (list (cdr x)) acc)) null pair-list))

