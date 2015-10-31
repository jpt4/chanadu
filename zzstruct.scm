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

|#

(define (zz-init cls)
	(zz-mk-aux cls))

(define (zz-init-aux cls)
	(zz-mk-acc cls '())
	;(zz-mk-direct cls)
)

(define (zz-init-direct cls)
	(cond
	 [(null? cls) (list '((cell 0 0) (0 0)))] ;default zzstruct - a zzstruct can never be empty
	 [(zzstruct? cls) cls]))

(define (zz-init-acc cls acc)
	(cond
	 [(and (null? cls) (null? acc)) (list '((cell 0 0) (0 0)))] ;default zzstruct - a zzstruct can never be empty
	 [(null? cls) acc]
	 [(zz-entry? (car cls)) 
		(zz-mk-acc (cdr cls) (list acc (car cls)))]))

(define (zzstruct? zs) 
	(let* ([nl-num (length (cdar zs))] ;length of first cell's neighbor list
				 [pass? (lambda (ze) ;is an element well-formed and evenly sized?
									(and (zz-entry? ze) (eq? (length (cdr ze)) nl-num)))])
		(and-map pass? zs)))

(define (zz-entry? cr)
	(and (cell? (car cr)) (neighbor-list? (cdr cr))))

(define (cell? cl)
	(and (eq? (car cl) 'cell) (number? (cadr cl)) (eq? (length cl) 3)))

(define (neighbor-list? nl) ;TODO check if neighbors exist
	(and-map (lambda (ne) (and (ordered-pair? ne) (number? (car ne)) (number? (cadr ne)))) nl))

(define (ordered-pair? op)  
	(eq? (length op) 2))

(define (atom? a)
	(or (symbol? a) (number? a)))

(define (zz-generate-random) 'gr)

(define (test)
	(zz-init '())
	(zz-init (zz-generate-random)))
