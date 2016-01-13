;zzrec.scm
;zzstruct record type
;jpt4  
;UTC20160113

(use-modules (srfi srfi-9))

(define-record-type <zzstruct>
	(build-zzst index content neighbor-list)
	zzst?
	(index zix)
	(content zco zco!)
	(neighbor-list znl znl!))
	
