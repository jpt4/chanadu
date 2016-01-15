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

#|
naming conventions in context
zzstruct in comments, zzst in functions, zst in arguments/assignments/variables
zzcell, zzcl, zcl
zzcell-index, zzix, zix
zzcell-content, zzco, zco
zzcell-id, zzid, zid
zzcell-neighbor-list, zznl, znl
zzcell-neighbor-pair, zznp, znp
|#

;;build zzcell from [[[index content] | id] neighbor-list]
(define (build-zzcl i c . n)
	(if (null? n) ;arguments are zzcell-id and zzcell-neighbor-list
			(append i (list c))
			(list i c (car n)))) ;arg are index, content, and neighbor-list	
;;build zzstruct from >=1 zzcells
(define (build-zzst c . ls)
	(if (null? ls) (list c)	(cons c ls)))

;;zzstruct carving
(define (zzst-head zst) (car zst))    ;first zzcell of zzstruct
(define (zzcl-ref zst zix)            ;zzcell with given index in zzstruct
	(list-ref zst zix)) 
(define (zzix zcl) (car zcl))         ;zzcell-index
(define (zzco zcl) (cadr zcl))        ;zzcell-content
(define (zzid zcl) (list-head zcl 2)) ;zzcell-id, index+content
(define (zznl zcl) (caddr zcl))       ;zzcell-neighbor-list of zzcell
(define (zznl-head znl) (car znl))    ;first zzcell-neighbor-pair of 
                                     ;zzcell-neighbor-list
(define (zznp-ref znl nix)            ;zzcell-neighbor-pair with given index in
	(list-ref znl nix))                ;zzcell-neighbor-list
(define (zzcl-first-zznp zcl) (car (zznl zcl))) ;first zzcell-neighbor-pair of
                                               ;zzcell
(define (zznp-at-axis zcl ax) (list-ref (zznl zcl) ax)) ;zzcell-neighbor-pair
																				               ;at an axis in zzcell
(define (upstream znp) (car znp))
(define (downstream znp) (cadr znp))
(define (zzix-null? zix) (equal? '_ zix))

;;pad zzcell-neighborlist of pre to length of tar
(define (pad-zzcl pre tar)
	(letrec* ([diff (- (length (zznl tar)) (length (zznl pre)))]
						[pad (map (lambda (n) `(,(zzix pre) ,(zzix pre)))	(iota diff))])
	  (build-zzcl (zzid pre) (append (zznl pre) pad))))

(define (pad-zzcl-test)
	(let* ([tst0 (build-zzcl '0 'short '((0 0)))]
				 [tst1 (build-zzcl '1 'long '((0 1) (2 3) (0 5)))])
		(pad-zzcl tst0 tst1))
	)

;;view the zzcell-neighbor-pair data for an axis of a zzstruct, plus 
;;zzcell-content if verbose
(define (all-zznp-along-axis zst ax . v)
	(let ([axis (map (lambda (zcl) (zznp-at-axis zcl ax)) zst)])
		(if (equal? '(v) v)
				(pair-zip (map zzid zst) axis)
				axis)))

;;rank - a series of locally connected cells in a dimension
#;(define (zzst-rank zst zix ax)
	(zzcl-ref zzst (min (zzst-rank-dir zst zix 'up) (zzst-rank-dir zst zix 'down)
)))

;;head zzcell of a rank, the most upstream
;;in the case of a ringrank, the lowest index zzcell is chosen, tagged as such
#|
A ringrank may have a headcell; ideally, this is the cell most
desirable to jump to quickly. It may be chosen by the user or by some
algorithmic method.
|#
(define (zzst-rank-head zst zix ax)
	(let ([zcl (zzcl-ref zst zix)]
				[up (upstream (zznp-at-axis zcl zx))])
		(cond
		 [(equal? up '_) zcl]
		 [(equal? up zix) (zzcl-ref zst (min (map zzix (zzst-rank zst zix ax))))]
		 )))
		

;;all ranks present in a dimension
(define (zzst-global-rank zst dim)
	'zzst-global-rank)

;;nearest neighbor zzcells
(define (zzcl-neighbors-on-axis zst zix ax)
	(letrec* ([znp (zznp-at-axis (zzcl-ref zst zix) ax)]
					 [up (upstream znp)]
					 [down (downstream znp)])
		(list (if (zzix-null? up) '_ (zzcl-ref zst up))
					(if (zzix-null? down) '_ (zzcl-ref zst down))
					)))

;;upstream zzcell, target zzcell, downstream zzcell
(define (zzcl-local-neighborhood zst zix ax)
	(let ([znbs (zzcl-neighbors-on-axis zst zix ax)])
		(list (upstream znbs) (zzcl-ref zst zix) (downstream znbs))))

;;two lists of elements yield one list of ordered pairs
(define (pair-zip m n)
	(cond
	 [(not (equal? (length m) (length n)))
		`(error length ,(length m) m != length ,(length n) n)]
	 [(or-map null? (list m n)) '()]
	 [else (cons `(,(car m) ,(car n)) (pair-zip (cdr m) (cdr n)))]
))		

(define (print-zzst zst) (dispnl* zst))

;;walk up or down axis ax, from zzcell zcl to its nearest dimensional neighbor
(define (zzcl-axial-neighbor zst zix ax dir)
	(let ([zcl (zzcl-ref zst zix)])
		(case dir
			['up (let ([up-zix (upstream (zznp-at-axis zcl ax))])
						 (cond
							[(equal? '_ up-zix) 
							 `(error no neighbor ,dir from zzcell ,zix along axis ,ax)]
							[(zzcl-ref zst up-zix)]
							))]
			['down (let ([down-zix (downstream (zznp-at-axis zcl ax))])
							 (cond
								[(equal? '_ down-zix) 
								 `(error no neighbor ,dir from zzcell ,zix along axis ,ax)]
								[(zzcl-ref zst down-zix)]
								))]
			)))

;;(eval) compatible with Chez Scheme
(define-syntax eval 
	(syntax-rules ()
		[(eval exp) (primitive-eval exp)]
		[(eval exp env) (eval exp env)]
))

;;genericized internal (result) via (eval) on dynamic data
;;uses 'upstream as both function reference and literal symbol
(define (alt-zzcl-axial-neighbor zst zix ax dir)
	(letrec* ([zcl (zzcl-ref zst zix)] ;zzcell of interest
						[znp (zznp-at-axis zcl ax)] ;zzcell-neighbor-pair of interest
						;\/mad with dynamic power
						[new-zix (eval `(,(symbol-append dir 'stream) (quote ,znp)))]
						[new-zcl
						 (cond
							[(equal? '_ new-zix) 
							 `(error no neighbor ,dir from zzcell ,zix along axis ,ax)]
							[else (zzcl-ref zst new-zix)]
							)])
					 new-zcl
					 ))

;;test zzstruct 0-1-2-3-4-5; 2-0-5-4-3-1; 4-3-2; 0-1-2->; 0-5, 1-4-2
;;application note: though possible, a zzcell should in practice not exist
;;as a disconnected singleton (a single point cycle) in a dimension. This
;;differs from a zzcell which does not intersect with an axis (i.e. whose 
;;neighbor-pair is '(_ _) at that index).
(define tstzzst0 
	(build-zzst (build-zzcl '0 'zero '((_ 1) (2 5) (_ _) (2 1) (_ 5)))
							(build-zzcl '1 'one '((0 2) (3 _) (_ _) (0 2) (_ 4)))
							(build-zzcl '2 'two '((1 3) (_ 0) (3 2) (1 0) (4 _)))
							(build-zzcl '3 'three '((2 4) (4 1) (4 2) (_ _) (_ _)))
							(build-zzcl '4 'four '((3 5) (5 3) (_ 3) (_ _) (1 2)))
							(build-zzcl '5 'five '((4 _) (0 4) (_ _) (_ _) (0 _)))
))

;;generic utilities
(define (dispnl msg)
	(begin (display msg) (newline)))
(define (dispnl* msg-ls)
	(for-each (lambda (m) (begin (display m) (newline))) msg-ls))

;;default data structure
(define origin-zzix 0)
(define origin-zzco 0)
(define origin-zznl '((0 0)))
(define origin-zzcl (build-zzcl origin-zzix origin-zzco origin-zznl))
(define origin-zzid (zzid origin-zzcl))
(define default-zzst (build-zzst origin-zzcl))

;symbolic prototypes
(define proto-zzcl (build-zzcl 'nat 'data '((nat nat))))
(define proto-zzst (build-zzst proto-zzcl))
(define proto-zzix (zzix proto-zzcl))
(define proto-zzid (zzid proto-zzcl))
(define proto-zznl (zznl proto-zzcl))

#|
;;declare zzstruct
(define (mk-zzst)
  (define cell-list default-zzst)
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
								(build-zzcl (cell-id (car cell-list)) proto-neighbor-list)]
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
							(build-zzcl (cell-id (cadr czs)) 
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

(define zzcl-tst-1 (build-zzcl '1 'content '((0 1) (0 1))))
(define zzcl-tst-2 (build-zzcl '2 'content '((1 2) (1 0))))

(define test-list 
  `(((view-cell-list) ,default-zzst)
    ((view-cell 0) ,origin-zzcl)
    ((add-cell ,zzcl-tst-1) _)
		((view-cell-list) _)
    ((add-cell ,zzcl-tst-2) _)
		((view-cell-list) _)
    )
)

(define (run-tests) (test-suite zzst-tst test-list))

|#



#|
;record type experiment
(use-modules (srfi srfi-9))

(define-record-type <zzcell>
  (mk-zzcell index content neighbor-list)
  zzcell?
  (index zzcell-index)
  (content zzcell-content set-zzcell-content!)
  (neighbor-list zzcell-neighbor-list set-zzcell-neighbor-list!))

(define (pad-zzcell pre tar)
	(letrec* ([diff (- (length (zzcell-neighbor-list tar)) 
										 (length (zzcell-neighbor-list pre)))]
						[pad (map (lambda (n) `(,(zzcell-index pre) ,(zzcell-index pre)))
											(iota diff))])
	  (set-zzcell-neighbor-list! pre (append (zzcell-neighbor-list pre) pad))))
					 
(define short (mk-zzcell 4 'pay '((3 5) (0 4))))
(define long (mk-zzcell 5 'con '((4 6) (1 2) (3 4))))
(define cls (list 'root-zzcl (mk-zzcell 1 'first '((0 2) (2 0)))))
|#
