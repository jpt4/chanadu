;;zzstruct.scm
;;jpt4
;;UTC20151028

#| 

A ZZ structure (zzstruct) is an indexed collection of cells and their
neighbor lists. A cell is a generic container for arbitrary content,
formed by appending the tag "cell" to the list of a cell's index in
the zztruct and the content, which may be a deferentiable name.
Ex. In cell (cell 0 c0), "cell" is a literal symbol, "0" a literal
number, and "c0" a literal symbol, or name referencing other
content. A neighbor list is a list of ordered pairs identifying the
positive/upstream (left element), and negative/downstream (right
element) neighbors of a cell along each dimensional axis. All cells
are neighbors along the fundamental indexical axis, with positive and
negative ordering implicit, corresponding to the numerical order of
the cell indices. This fundamental index is used within the ordered
pairs for every axis of the neighbor list to identify cell
neighbors. The neighbor lists of all cells contain an entry for at
least this indexical axis, and for every axis inhabited by any cell,
thus maintaining length parity between neighbor lists. If a cell does
not have any neighbors in a dimension, it is its own upstream and
downstream neighbor. Therefore, the default minimal zzstruct is 
(((cell 0 0) (0 0))) - a zzstruct of one cell, which is a point along
the fundamental axis, and thus its own neighbor in both directions.

The "zz-" prefix is used to identify procedures as zzstruct specific.

|#

(define (zz-init cls)
	(zz-init-aux cls))

(define (zz-init-aux cls)
	(zz-init-direct cls)
)

(define (zz-init-direct cls)
	(cond
	 [(null? cls) (list '((cell 0 0) (0 0)))] ;default zzstruct - a zzstruct can never be empty
	 [(zz-struct? cls) cls]))

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

;;zzstructs are built like lists, from terminus forwards
;;zstd = downstream zstu = upstream
(define (zz-append zstd zstu)
  (zz-append-simple zstd zstu)
  ;(zz-append-pauli cl st)
  )

;is cell zc able to be appended to struct zs via the simple strategy?
(define (simple-append-acceptible? zc zs)
#t
)	
#;(define (zz-append-simple zsts zstt)
Re-architect: 
	Are source and target compatible <- equi-dimensional
	                                    each cell simply appendable
                                      collision checking
	Generate new neighbors between source and target cells, append.
  Buffer all new zs cells to appropriate square dimensionality.  
  
	(if (zz-st-compatible? zstd zstu)
			'a
			'b
			))

(define (zz-st-compatible? zd zu)
	(and (zz-struct? zd) (zz-struct? zu) 
			 (eq? (zz-st-dimension? zd) (zz-st-dimension? zu))))
	
(define (zz-st-dimension? zst)
	(max (map (lambda (z) (length (cdr z))) zst)))

(define (zz-append-simple cl zst)
  (let ([new-zst-simple 
         (lambda (ce)
           (let ([new-nbrs (map (lambda (pa) (up<->down (cell-index ce) pa 
																												(cell-index cl)))
                                (filter (lambda (np) 
																					(member (cell-index ce) np)) 
																				(nlist cl)))])
						 (if (null? new-nbrs)
								 ce
								 (cons (cell-id ce) (append (nlist ce) new-nbrs)))))])
    (cons cl (map new-zst-simple zst))))

(define (up<->down n p on) ; number, pair, original number
	(cond
	 [(eq? (car p) n) (cons (car p) (cons on '()))]
	 [(eq? (cadr p) n) (cons on (cons (cadr p) '()))]
))		

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

(define (zz-generate-random) 'gr)

(define (test)
	(zz-init '())
	(zz-init (zz-generate-random)))
