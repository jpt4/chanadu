;zzobj.scm
;objectified hyper-orthogonal data structures
;jpt4
;UTC20151202

;guile v2.2

;;TODO: resolve inconsistent zzcell structure assumptions
#|
zzstruct := (zzcell ...)
zzcell := (index content neighbor-list)
neighbor-list := (neighbor-pair ...)
neighbor-pair := (upstream downstream)
index := nat
content := data
upstream := nat
downstream := nat

zzstruct = ((3 'grandson ((2 4) (upstream downstream) neighbor-pair ...))
            zzcell
            ...)
|#

;default data structure
(define origin-cell-index 0)
(define origin-cell-content 0)
(define origin-neighbor-list '((0 0)))
(define origin-zzcell 
  (append (list origin-cell-index origin-cell-content) 
          (list origin-neighbor-list)))
(define default-zzstruct (list origin-zzcell))
;;zzstruct carving
(define (cell-index cell)
  (car cell))
(define (cell-id cell) ;index + content
  (list-head cell 2))
(define (neighbor-list cell)
  (caddr cell))
(define (upstream neighbor-pair)
  (car neighbor-pair))
(define (downstream neighbor-pair)
  (cadr neighbor-pair))
;data prototypes
(define proto-zzcell '(nat data ((nat nat))))
(define proto-zzstruct (list proto-zzcell))
(define proto-cell-id (cell-id proto-zzcell))
(define proto-neighbor-list (neighbor-list proto-zzcell))
;;build zzcell from [[[index content] | id] neighbor-list]
(define (build-zzcell i c . n)
	(if (null? n) ;arguments are id and neighbor-list
			(append i (list c))
			(list i c (car n)))) ;arg are index, content, and neighbor-list	

;;declare zzstruct
(define (mk-zzst)
  (define cell-list default-zzstruct)
  (define (view-cell cell-index)
    (list-ref cell-list cell-index))
  (define (add-cell new-cell)
		(dispnl `(enter add-cell ,new-cell))
		(if (equal? 'yes (valid-to-add? new-cell))
				(begin
					(let ([new-cell-list (gen-new-cell-list new-cell)])
						(set! cell-list new-cell-list) ;alas, no monads yet
						))
				`(error ,new-cell ,(valid-to-add? new-cell))))
	(define (gen-new-cell-list new)
		(dispnl* `(gen-new-cell-list (initial-czs ,cell-list)))
		(let next ([czs cell-list] ;current zzstruct
							 [nnl (neighbor-list new)] ;neighbor-list of cell to add
							 ;\/new zzcell - init as origin cell id plus null neighbor-list
							 [nzc 
								(build-zzcell (cell-id (car cell-list)) proto-neighbor-list)]
							 [nzs '()]) ;new zzstruct
			(dispnl `(czs ,czs))
			(cond
			 ;\/current cell-list empty
			 [(null? czs) (dispnl `(only see this once nzs ,nzs)) (append nzs (list new))]
			 ;\/new cell neighbor list empty
			 [(null? nnl) (dispnl* `(null-nnl ,(cell-id (car czs))))
				(next (cdr czs) (neighbor-list new)
							(if (null? (cdr czs))
									'()
							(build-zzcell (cell-id (cadr czs)) 
														proto-neighbor-list))
							(append nzs (list nzc)))]
			 ;\/current cell named in first neighbor-pair of new neighbor-list
			 [(member (cell-index (car czs)) (car nnl))
				(cond
				 [(eq? (cell-index (car czs)) (upstream (car nnl)))
					(next czs (cdr nnl) ;keep current zzstruct, see next neighbor-pair
								(build-zzcell (cell-id nzc) 
															(if (eq? proto-neighbor-list (neighbor-list nzc))
																	`((,(cell-index (car czs))
																		 ,(cell-index new)))
																	(append (neighbor-list nzc)
																					`((,(cell-index (car czs))
																						 ,(cell-index new))))))
								nzs)
					]
				 [(eq? (cell-index (car czs)) (downstream (car nnl)))
					(next czs (cdr nnl) ;keep current zzstruct, see next neighbor-pair
								(build-zzcell (cell-id nzc) 
															(if (eq? proto-neighbor-list (neighbor-list nzc))
																	`((,(cell-index new)
																		 ,(cell-index (car czs))))
																	(append (neighbor-list nzc)
																					`((,(cell-index new)
																						 ,(cell-index (car czs)))))))
								nzs)
					])]
			 ;\/current cell not named, next neighbor-pair, same current cell-list
			 [else (next czs (cdr nnl) nzc nzs)]
			 )))		
	(define (valid-to-add? c)
		(begin (display "look at me!!!!!!!!")
					 (newline)
					 (display (car cell-list))
					 (newline))
		(if (>= (zzcell-dimensionality c) (zzcell-dimensionality (car cell-list)))
				'yes
				'(error ,c under-dimensioned)))
	(define (zzcell-dimensionality cell)
		(display
		(length (neighbor-list cell)))
		(newline)
		(length (neighbor-list cell)))
;;message dispatch
  (define (self msg)
    (case (car msg)
      ['view-cell-list cell-list]
      ['view-cell (view-cell (cadr msg))]
      ['add-cell (add-cell (cadr msg))]
      [else `(error unknown message ,msg)]
      ))
  self)

;;test suite
(define zzst-tst (mk-zzst))
(define (test-suite zzstruct-to-test test-expected-list)
  (let ([zzst zzstruct-to-test])         
    (define (perform-test test-expected-pair)
      (letrec* ([t (car test-expected-pair)]
                [e (cadr test-expected-pair)]
                [result (zzst t)])
               (cond
								;\/no output/expected output unknown
                [(equal? e '_) `(,t ,result)] 
                [(equal? result e) `(pass ,result)]
                [else `(failed test: ,t expected: ,e produced: ,result)])))
    (map perform-test test-expected-list)
    ))

(define zzcl-tst-1 (build-zzcell '1 'content '((0 1) (0 1))))
(define zzcl-tst-2 (build-zzcell '2 'content '((1 2) (1 0))))

(define test-list 
  `(((view-cell-list) ,default-zzstruct)
    ((view-cell 0) ,origin-zzcell)
    ((add-cell ,zzcl-tst-1) _)
		((view-cell-list) _)
    ((add-cell ,zzcl-tst-2) _)
		((view-cell-list) _)
    )
)

(define (run-tests) (test-suite zzst-tst test-list))

;generic utilities
(define (dispnl msg)
	(begin (display msg) (newline)))
(define (dispnl* msg-ls)
	(map (lambda (m) (begin (display m) (newline))) msg-ls))
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

