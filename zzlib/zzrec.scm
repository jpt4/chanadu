;zzrec.scm
;zzstructs via record types
;jpt4  
;UTC20160113

(use-modules (srfi srfi-9 gnu))

(define-immutable-record-type <zzcell>
	(make-zzcl index content neighbor-registry)
	zzcl?
	(index zix?)
	(content zco? zco!)
	(neighbor-registry znr? znl!))

(define-immutable-record-type <zzstruct>
	(make-zzst (head tail))
	zzst?
	(head zzst-head)
	(tail zzst-tail))

;;build zzstruct from >=1 zzcells
#;(define (build-zzst c . ls)
	(if (null? ls) (list c)	(cons c ls)))