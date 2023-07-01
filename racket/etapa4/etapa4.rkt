#lang racket

(require "etapa2.rkt")
(require "etapa3.rkt")

(provide (all-defined-out))

;; Preferințele bărbaților și femeilor din problemă se pot schimba
;; în timp, dar de obicei ele nu se schimbă radical de la un moment
;; la altul. De aceea, în loc să rulăm de la zero algoritmul
;; Gale-Shapley de fiecare dată când se schimbă ceva, preferăm să
;; pornim de la lista de logodne stabile obținută în pasul anterior
;; și să o actualizăm, conform algoritmului următor:
;; - eliminăm din engagements cuplurile care au devenit instabile
;;   în urma modificărilor de preferințe
;;   - cuplurile rămase sunt stabile între ele și considerăm că
;;     se găsesc împreună într-o cameră, în timp ce membrii cuplurilor
;;     destrămate stau la coadă la intrarea în cameră
;; - cât timp coada nu este goală
;;   - prima persoană p din coadă intră în cameră și încearcă să se
;;     cupleze cu cineva care este deja acolo, astfel:
;;     - p-list = lista de preferințe a lui p
;;     - determină prima persoană p' din p-list care este în cameră
;;     - dacă p' nu e logodită, logodește p' cu p
;;     - dacă p' e logodită
;;       - dacă p' îl preferă pe p partenerului actual p''
;;         - logodește p' cu p
;;         - încearcă să îl cuplezi pe p'' cu altcineva din cameră
;;           (folosind același algoritm)
;;       - altfel, treci la următoarea persoană din p-list (dacă
;;         aceasta există, altfel p rămâne temporar fără partener)


; TODO 1
; Implementați funcția match care primește o persoană person care
; intră în cameră, lista engagements a cuplurilor din cameră
; (cuplurile având pe prima poziție persoanele de gen opus lui 
; person), o listă pref1 care conține preferințele celor de același 
; gen cu person, o listă pref2 cu preferințele celor de gen diferit, 
; respectiv o coadă queue a persoanelor din afara camerei,
; și întoarce lista de cupluri actualizată astfel încât noile
; cupluri să fie stabile între ele.
; Această listă se obține ca rezultat al încercării de a cupla pe
; person cu cineva din cameră (person va încerca în ordine persoanele 
; din lista sa de preferințe), care poate duce la destrămarea
; unui cuplu și necesitatea de a cupla noua persoană rămasă singură
; cu altcineva din cameră, etc. Procesul continuă până când:
; - ori avem numai cupluri stabile între ele în cameră, nimeni
;   nefiind singur
; - ori toate persoanele rămase singure nu ar fi preferate de nimeni
;   altcineva din cameră, și în acest caz convenim să "logodim"
;   aceste persoane cu valoarea #f, astfel încât funcția să
;   întoarcă în aceeași listă atât informația despre cine din
;   cameră este logodit, cât și despre cine este singur
(define (match person engagements pref1 pref2 queue)

    (let construire ((lista-pref (get-pref-list pref1 person)) (engagements engagements) (person person))

                               (if (null? lista-pref) (cons (cons false person) engagements)
        (if (not (member (car lista-pref) queue))
                     (if (equal? (get-partner engagements (car lista-pref)) #f)
                         (map (lambda (x) (if (equal? (car lista-pref) (car x))
                                            (cons (car x) person)
                                           (cons (car x) (cdr x)
                                         ))) engagements)
                         
                        
                     (if (preferable? (get-pref-list pref2 (car lista-pref)) person (get-partner engagements (car lista-pref)))
                        (construire (get-pref-list pref1 (get-partner engagements (car lista-pref)))    (map (lambda (x) (if (equal? (car lista-pref) (car x))
                                            (cons (car x) person)
                                           (cons (car x) (cdr x)
                                         ))) engagements) (get-partner engagements (car lista-pref)))
                        
                     (construire (cdr lista-pref) engagements person)))
               (construire (cdr lista-pref) engagements person)))))
                   
            


; TODO 2
; Implementați funcția path-to-stability care primește lista
; engagements a cuplurilor din cameră, o listă de preferințe 
; masculine mpref, o listă de preferințe feminine wpref, respectiv
; coada queue a persoanelor din afara camerei, și întoarce lista
; completă de logodne stabile, obținută după ce fiecare persoană
; din queue este introdusă pe rând în cameră și supusă procesului
; descris de funcția match.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; - persoanele nelogodite din cameră apar în engagements sub forma
;   (#f . nume-bărbat) sau (nume-femeie . #f)
(define (path-to-stability engagements mpref wpref queue)
  (let combina ((queue  queue) (engagements engagements))
    (if (null? queue) engagements
  (if (null? (filter (lambda (x)  (equal? (car x) (car queue) )) mpref))
  
      (combina (cdr queue) (map (lambda (x) (cons (cdr x) (car x))) (match (car queue)  (map (lambda (x) (cons (cdr x) (car x))) engagements) wpref mpref (cdr queue))))
      (combina (cdr queue) (match (car queue)  engagements mpref wpref (cdr queue)))))))
     
      
      


; TODO 3
; Implementați funcția update-stable-match care primește o listă 
; completă de logodne engagements (soluția anterioară), o listă de 
; preferințe masculine mpref și o listă de preferințe feminine wpref 
; (adică preferințele modificate față de cele pe baza cărora s-a 
; obținut soluția engagements), și calculează o nouă listă de logodne 
; stabile - conform cu noile preferințe, astfel:
; - unstable = cuplurile instabile din engagements
; - room-engagements = engagements - unstable
; - queue = persoanele din unstable
; - aplică algoritmul path-to-stability
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
(define (update-stable-match engagements mpref wpref)
 (let* ((unstable(get-unstable-couples engagements mpref wpref))
       (queue (get-couple-members unstable))
       (room-engagements (filter (lambda (x) (not (member (car x) queue))) engagements)))
       (path-to-stability room-engagements mpref wpref queue)))
        
       
   
   
   
  
       


; TODO 4
; Implementați funcția build-stable-matches-stream care primește
; un flux pref-stream de instanțe SMP și întoarce fluxul de 
; soluții SMP corespunzător acestor instanțe.
; O instanță SMP este o pereche cu punct între o listă de preferințe
; masculine și o listă de preferințe feminine.
; Fluxul rezultat se va obține în felul următor:
; - primul element se calculează prin aplicarea algoritmului
;   Gale-Shapley asupra primei instanțe
; - următoarele elemente se obțin prin actualizarea soluției
;   anterioare conform algoritmului implementat în etapa 4 a temei
; Trebuie să lucrați cu interfața pentru fluxuri. Dacă rezolvați
; problema folosind liste și doar convertiți în/din fluxuri,
; punctajul pe acest exercițiu se anulează în totalitate.
(define (build-stable-matches-stream pref-stream)
  (if(stream-empty? pref-stream)
    empty-stream
    (let ((eng (gale-shapley (stream-first (stream-first pref-stream)) (stream-rest (stream-first pref-stream)))))
     
     (let loop ( (rest (stream-rest pref-stream)) (eng eng ))
       (if (stream-empty? rest)
        (list eng)
        (stream-cons eng
                   (loop  (stream-rest rest) (update-stable-match eng (stream-first (stream-first rest)) (stream-rest (stream-first rest))))))))))
   
  
 
    
    


