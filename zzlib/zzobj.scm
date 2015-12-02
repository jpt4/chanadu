;zzobj.scm
;objectified hyper-orthogonal data structures
;jpt4
;UTC20151202

(define (zz egg)
	(define default-zzstruct '(((cell 0 0) (0 0))))
	(define struct default-zzstruct)
	(cond
	 [(zz-struct? egg) (set! struct egg)]
	 [else (display "error") (newline) (display struct) (newline)]
	 )
)
	
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

(define (ordered-pair? op)  
	(eq? (length op) 2))
(define (atom? a)
  (not (pair? a)))
