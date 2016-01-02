;zzobj.scm
;objectified hyper-orthogonal data structures
;jpt4
;UTC20151202

<<<<<<< HEAD
(define (zz egg)
  (define default-zzstruct '(((cell 0 0) (0 0))))
  (define struct default-zzstruct)
  (define (self msg)
    (case (car msg)
      ['get-struct struct]
      ))
  (begin
    (cond
     [(zz-struct? egg) (set! struct egg)]
     [else (display "error") (newline) (display struct) (newline)]
     )
    self)  
  )

;test suite
;compile time
(define zztst (zz '(((cell 0 0) (0 0))))) ;test zz object
(define zzerr (zz '(((cell 0 0) (0 0) (1 2))))) ;should yield error msg
;run time
(define (tests)
    (display (zzt '(get-struct)))
    (newline)
)

;public auxiliaries
(define (zz-struct? st) 
  (let* ([nl-num (length (cdar st))] ;length of first cell's neighbor list
         [pass? (lambda (en) ;is an element well-formed and evenly sized?
                  (and (zz-entry? en st) (eq? (length (cdr en)) nl-num)))])
    (and-map pass? st)))

(define (zz-entry? en st) ;is en an entry in zzstruct st?
  (and (zz-cell? (car en)) (neighbor-list? (cdr en) st)))

(define (zz-cell? cl) ;is cl a well-formed cell?
  (and (eq? (car cl) 'cell) (number? (cadr cl)) (eq? (length cl) 3)))

(define (neighbor-list? nl st) ;is nl a neighbor list in zzstruct st?
  (let ([nl-guard (lambda (ne) 
                    (let* ([l (car ne)] [r (cadr ne)] 
                           [max-index (- (length st) 1)])
                    (and (ordered-pair? ne) (number? l) (number? r)
                         (<= l max-index) (<= r max-index))))])
    (and-map nl-guard nl)))

;;to lessen cadadddar'ing
(define (nlist cl)
  (cdr cl))
(define (cell-id cl)
  (car cl))
(define (cell-index cl)
  (cadar cl))

;for guile prelude
(define (ordered-pair? op)  
  (eq? (length op) 2))
(define (atom? a)
  (not (pair? a)))

=======
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
>>>>>>> dev
