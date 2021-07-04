#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define-struct counter (index tt et queue status) #:transparent)

(define (empty-counter index)
  (make-counter index 0 0  empty-queue 1))
;pun 1 pt casele deschise

(define (update f counters index)
    (cond
     ((null? (filter (lambda (C) (= (counter-index C) index)) counters)) counters) 
     ( else (update-helper f counters index '()))))

(define (update-helper f counters index l)
  (if (null? counters)
      l
      (if (= (counter-index (car counters)) index)
                (update-helper f (cdr counters) index ( append l (list (f (car counters)))))
                (update-helper f (cdr counters) index ( append l (list (car counters)))))))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if (queue-empty? (counter-queue C)) (struct-copy counter C [tt (+ (counter-tt C) items)][et (+ (counter-et C) items)][queue (enqueue (cons name items) (counter-queue C))])
    (struct-copy counter C [tt (+ (counter-tt C) items)][queue (enqueue (cons name items) (counter-queue C))]))))


(define (general-min counters elem)
   (general-min-helper counters 0 99999999 (cons 0 99999999) elem))


(define (general-min-helper counters index time List elem)
  (cond
  ((null? counters) List)
  ( (< (elem (car counters)) time) ( general-min-helper (cdr counters) (counter-index (car counters)) (elem (car counters)) (cons (counter-index (car counters)) (elem (car counters)))  elem))
  (else ( general-min-helper (cdr counters) index time List elem))))

(define (min-tt counters)
  (general-min counters counter-tt))

(define (min-et counters)
  (general-min counters counter-et))


(define (remove-first-from-counter C)
  (cond
    ((null? (top (counter-queue C))) (struct-copy counter C [tt 0][et 0][queue empty-queue]))
    ((null? (top(dequeue(counter-queue C)))) (struct-copy counter C [tt 0][et 0][queue empty-queue]))
    (else  (struct-copy counter C [tt (sum-helper (stream-cons (queue-left(dequeue(counter-queue C))) (queue-right(dequeue(counter-queue C)))) 0) ][et (cdr(top(dequeue (counter-queue C))))][queue (dequeue (counter-queue C))]))))


(define (sum-helper counters sum)
  (cond
    ((null? counters) sum)
    ( else (sum-helper (stream-rest counters) (+ sum (stream-rest (stream-first counters)))))))

(define (time-helper C)
  (cond
    ((null? (cdr (counter-queue C))) 0)
    (else (cdr (car (cdr (counter-queue C)))))))

(define ttandet+
  (lambda (minutes)
    (lambda (C)
    (struct-copy counter C [tt (+ (counter-tt C) minutes)][et (+ (counter-et C) minutes)]))))



; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (cond
      ((and (< (- (counter-tt C) minutes) 0) (< (- (counter-et C) minutes) 0)) (struct-copy counter C [tt 0][et 0][queue (counter-queue C)]))
      ((and (< (- (counter-tt C) minutes) 0) (> (- (counter-et C) minutes) 0)) (struct-copy counter C [tt 0][et (- (counter-et C) minutes)][queue (counter-queue C)]))
      ((and (> (- (counter-tt C) minutes) 0) (< (- (counter-et C) minutes) 0)) (struct-copy counter C [tt (tt- (counter-tt C) minutes)][et 0][queue (counter-queue C)]))
      (else (struct-copy counter C [tt (tt- (counter-tt C) minutes)][et (- (counter-et C) minutes)][queue (counter-queue C)]   )))))


(define (pass-time-through-counter-and-dequeue minutes)
  (λ (C)
    (cond
       ((queue-empty? (dequeue(counter-queue C))) (struct-copy counter C [tt (tt- (counter-tt C) (counter-et C))][et 0 ][queue (dequeue(counter-queue C))]))
       (else (struct-copy counter C [tt (tt- (counter-tt C) (counter-et C))][et (cdr (top(dequeue(counter-queue C)))) ][queue (dequeue(counter-queue C))]   )))))

(define (tt- x y)
  (if (> (- x y) 0) (- x y)
      0))


  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)

; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei


(define (select lista pairlist)
  (cond
    ((null? lista) pairlist)
    (else (select (cdr lista) (append pairlist (list (cons (counter-index (car lista)) (counter-queue (car lista)))))))))


(define (close-a-counter C)
  (struct-copy counter C [status 0]))

(define (close-specific-counter counters index aux)
  (cond
    ((null? counters) aux)
    ((= (counter-index (car counters)) index) (close-specific-counter (cdr counters) index  (append aux (list(close-a-counter (car counters))))))
    (else (close-specific-counter (cdr counters) index  (append aux (list(car counters)))))))



(define (x-helper-clients-one-counter q-counters counters clients x )
  (if (or (queue-empty? q-counters) (<= x 0)) clients
         (cond
    ((< x (counter-et counters)) (x-helper-clients-one-counter empty-queue  (struct-copy counter counters [et (- (counter-et counters) x) ][tt (- (counter-tt counters) x) ]) clients x))
    ((= x (counter-et counters)) (if (queue-empty? (dequeue q-counters))  (x-helper-clients-one-counter empty-queue (struct-copy counter counters [et 0 ][tt ( - (counter-tt counters) x) ]) (append clients (list (cons (cons (counter-index counters) (car (top q-counters)))(- x (counter-et counters))))) x)
     (x-helper-clients-one-counter empty-queue (struct-copy counter counters [et (cdr(top(dequeue q-counters))) ][tt ( - (counter-tt counters) x) ]) (append clients (list (cons (cons (counter-index counters) (car (top q-counters)))(- x (counter-et counters))))) x)))
      (else (if (queue-empty? (dequeue q-counters)) (x-helper-clients-one-counter (dequeue q-counters) (struct-copy counter counters [et 0 ][tt ( - (counter-tt counters) x) ]) (append clients (list (cons (cons (counter-index counters) (car (top q-counters)))(- x (counter-et counters))))) (- x (counter-et counters)))
     (x-helper-clients-one-counter (dequeue q-counters) (struct-copy counter counters [et (cdr(top(dequeue q-counters))) ][tt ( - (counter-tt counters) x) ]) (append clients (list (cons (cons (counter-index counters) (car (top q-counters)))(- x (counter-et counters))))) (- x (counter-et counters))))))))

(define (x-helper-clients clients  counters  x)
  (cond
    ((null? counters) clients)
    (else (if (null? (x-helper-clients-one-counter (counter-queue (car counters)) (car counters) null x))  (x-helper-clients clients (cdr counters) x)
     (x-helper-clients (append clients  (x-helper-clients-one-counter (counter-queue (car counters)) (car counters) null x)) (cdr counters) x)))))


(define (case-pline counters aux)
  (cond
    ((null? counters) aux)
    (else (if (queue-empty? (counter-queue (car counters))) (case-pline (cdr counters) aux)
              (case-pline (cdr counters) (append aux (list (car counters))))))))
                      


(define (remove-from-counter C x)
     (if (queue-empty? (counter-queue C)) ((pass-time-through-counter x)C)
      (cond
        ((< x (counter-et C)) ((pass-time-through-counter x)C))
        (else (if (queue-empty? (dequeue(counter-queue C))) ((pass-time-through-counter-and-dequeue x)C)
               (cond
               ((= x (counter-et C)) (remove-from-counter ((pass-time-through-counter-and-dequeue x)C) 0))
               (else (remove-from-counter ((pass-time-through-counter-and-dequeue x)C) (- x (counter-et C))))))))))

(define (remove-from-counters counters x auxlist)
  (cond
    ((null? counters) auxlist)
    (else (remove-from-counters (cdr counters) x (append auxlist (list(remove-from-counter (car counters) x)))))))
                      
                              
                    
 (define (sort-by-cdr-number lst) 
  (define (object-greater? a b)
    (> (cdr a) (cdr b)))
  (sort lst object-greater?))

(define (choose-open-counters counters aux)
  (cond
    ((null? counters) aux)
    ((= (counter-status (car counters)) 1) (choose-open-counters (cdr counters) (append aux (list (car counters)))))
    (else  (choose-open-counters (cdr counters) aux))))
    
(define (time-sum counters)
  (time-sum-helper 0 counters))

(define (time-sum-helper acc counters)
  (cond
    ((null? counters) acc)
    (else (time-sum-helper (+ acc (counter-tt (car counters))) (cdr counters)))))

(define (number-of-counters counters)
  (number-of-counters-helper counters 0))

(define (number-of-counters-helper counters acc)
  (if (null? counters)
      acc
      (number-of-counters-helper (cdr counters) (+ acc 1))))

(define (add-new-counter allcounters counters average index)
  (if (> (/ (time-sum allcounters) (number-of-counters allcounters)) average)
        (add-new-counter (append allcounters (list (empty-counter index))) (append counters (list (empty-counter index))) average index)
        counters))
    


(define (serve-aux requests fast-counters slow-counters clients)
  (if (null? requests)
      (cons clients (select (case-pline (append fast-counters slow-counters) '()) '()))
      (match (car requests)
        [(list 'delay index minutes)
         (if (null? (filter (lambda (C) (= (counter-index C) index)) fast-counters))
             (serve-aux (cdr requests) fast-counters (update (ttandet+ minutes) slow-counters index) clients)
             (serve-aux (cdr requests) (update (ttandet+ minutes) fast-counters index) slow-counters clients))]
         [(list 'ensure average)
         (cond
           ( (> (/ (time-sum (choose-open-counters (append fast-counters slow-counters) '())) (number-of-counters (choose-open-counters (append fast-counters slow-counters) '()))) average) (serve-aux (cdr requests) fast-counters (add-new-counter (choose-open-counters (append fast-counters slow-counters) '()) slow-counters average (+ (number-of-counters (append fast-counters slow-counters)) 1)) clients))
           ( else (serve-aux (cdr requests) fast-counters slow-counters clients)))]
         [(list 'close index)
          (serve-aux (cdr requests) (close-specific-counter fast-counters index '()) (close-specific-counter slow-counters index '()) clients)]
        [(list name n-items)
         (if (<= n-items ITEMS)
             (if (null? (filter (lambda (C) (= (counter-index C) (car (min-tt (choose-open-counters (append fast-counters slow-counters) '()))))) fast-counters))
                 (serve-aux (cdr requests) fast-counters (update (add-to-counter name n-items)  slow-counters  (car (min-tt (choose-open-counters slow-counters '())))) clients)
                 (serve-aux (cdr requests) (update (add-to-counter name n-items)  fast-counters (car (min-tt (choose-open-counters fast-counters '())))) slow-counters clients))
             (serve-aux (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters  (car (min-tt (choose-open-counters slow-counters '())))) clients))]
        [ x
          (if (null? (x-helper-clients null (append fast-counters slow-counters)  x)) (serve-aux (cdr requests) (remove-from-counters fast-counters x '())  (remove-from-counters slow-counters x '()) clients)
            (serve-aux (cdr requests) (remove-from-counters fast-counters x '())  (remove-from-counters slow-counters x '()) (append clients (map car (sort-by-cdr-number(x-helper-clients null (append fast-counters slow-counters) x))))))
        
        ])))
        
            

(define (serve requests fast-counters slow-counters)
    (serve-aux requests fast-counters slow-counters '() ))