;zzobj.scm
;objectified hyper-orthogonal data structures
;jpt4
;UTC20151202

;guile v2.2

(define origin-cell-index 0)
(define origin-cell-content 0)
(define origin-neighbor-list '((0 0)))
(define origin-zzcell 
  (cons (list origin-cell-index origin-cell-content) origin-neighbor-list))
(define default-zzstruct (list origin-zzcell))

(define (mk-zzst)
  (define cell-list (list origin-zzcell))
  (define (view-cell cell-index)
    (list-ref cell-list cell-index))
  (define (add-cell new-cell)
    (set! cell-list (append cell-list (list new-cell))))
  (define (self msg)
    (case (car msg)
      ['get-entire cell-list]
      ['view-cell (view-cell (cadr msg))]
      ['add-cell (add-cell (cadr msg))]
      [else `(error unknown message ,msg)]
      ))
  self)

(define zzst-tst (mk-zzst))
(define (test-suite zzstruct-to-test test-expected-list)
  (let ([zzst zzstruct-to-test])         
    (define (perform-test test-expected-pair)
      (letrec* ([t (car test-expected-pair)]
                [e (cadr test-expected-pair)]
                [result (zzst t)])
               (cond
                [(equal? e '_) result]
                [(equal? result e) 'pass]
                [else `(failed test: ,t expected: ,e produced: ,result)])))
    (map perform-test test-expected-list)
    ))

(define cell-tst-1 (cons (list '1 'content) '((0 1) (1 0))))

(define test-list 
  `(((get-entire) ,default-zzstruct)
    ((view-cell 0) ,origin-zzcell)
    ((add-cell ,cell-tst-1) _)
    )
)

(test-suite zzst-tst test-list)

#|
(use-modules (srfi srfi-9))

(define-record-type <zzcell>
  (mk-zzcell id content neighbor-list)
  zzcell?
  (id cell-id)
  (content cell-content set-cell-content!)
  (neighbor-list cell-neighbor-list set-cell-neighbor-list!))

(define (zz egg)
  (define default-zzstruct '((cell 0 0) (0 0)))
  (define struct default-zzstruct)
  (if (zzstruct? egg)
      (set! struct egg)
      else
      (display `(,egg not a valid zzstruct))
      (newline)
      (display `(current struct is ,struct)))
|#

