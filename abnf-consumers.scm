;;
;;  Convenience procedures and macros for manipulating items in the
;;  stream of consumed tokens returned by an abnf-based parser.
;;
;;   Copyright 2009-2018 Ivan Raikov
;;
;;   This program is free software: you can redistribute it and/or
;;   modify it under the terms of the GNU General Public License as
;;   published by the Free Software Foundation, either version 3 of
;;   the License, or (at your option) any later version.
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;   General Public License for more details.
;;
;;   A full copy of the GPL license can be found at
;;   <http://www.gnu.org/licenses/>.


(module abnf-consumers

	*

   (import scheme (chicken base) srfi-1
           abnf utf8 utf8-srfi-14)

;; collects all consumed objects of type obj?
(define (consumed-objects obj?)
  (lambda (cs) 
    (and (or (pair? cs) (null? cs))
	 (let loop ((cs cs) (ax (list)))
	   (cond ((null? cs)   (list ax))
		 ((obj? (car cs))
		  (loop (cdr cs) (cons (car cs) ax)))
		 (else (cons ax cs)))))))

;; construct composite objects from consumed objects
(define (consumed-objects-lift get-consumed)
  (lambda rest
    (let-optionals rest ((kons identity))
      (let ((make (if (procedure? kons) kons (lambda (x) (and (list? x) `(,kons . ,x))))))
	(lambda (x)
	  (let ((x1 (get-consumed x)))
	    (and x1 (pair? x1) (list? (car x1)) 
		 (let ((item (make (car x1))))
		   (if item
                       (cons item (cdr x1))
		       (cdr x1))
		   ))
            ))
        ))
    ))

(define consumed-chars (consumed-objects char?))
(define consumed-chars->list (consumed-objects-lift consumed-chars))

(define (trim-ws-char-list cs)
  (let* ((cs1 (let loop ((cs cs))
	       (cond ((null? cs) (reverse cs))
		     ((char-set-contains? char-set:whitespace (car cs))
		      (loop (cdr cs)))
		     (else (reverse cs)))))
	 (cs2  (let loop ((cs cs1))
	       (cond ((null? cs) (reverse cs))
		     ((char-set-contains? char-set:whitespace (car cs))
		      (loop (cdr cs)))
		     (else (reverse cs))))))
    cs2))
    
;; construct strings from consumed chars
(define consumed-chars->string 
  (consumed-chars->list list->string))
;; construct symbols from consumed chars; trailing and preceding white
;; space is stripped
(define consumed-chars->symbol
  (consumed-chars->list
   (compose string->symbol list->string trim-ws-char-list)))


(define consumed-strings (consumed-objects string?))
(define consumed-strings->list (consumed-objects-lift consumed-strings))

(define consumed-pairs (consumed-objects pair?))
(define consumed-pairs->list (consumed-objects-lift consumed-pairs))

;; shortcut for (bind (consumed-chars->list) ... )
(define-syntax bind-consumed->list
  (syntax-rules () 
    ((_ p)      (bind (consumed-chars->list) p))
    ((_ l p)    (bind (consumed-chars->list l) p))
    ))

;; shortcut for (bind consumed-chars->string ... )
(define-syntax bind-consumed->string
  (syntax-rules () 
    ((_ p)     (bind consumed-chars->string p))
    ))

;; shortcut for (bind consumed-chars->symbol ... )
(define-syntax bind-consumed->symbol
  (syntax-rules () 
    ((_ p)    (bind consumed-chars->symbol p))
    ))

;; shortcut for (bind (consumed-strings->list ...) ... )
(define-syntax bind-consumed-strings->list
  (syntax-rules () 
    ((_ l p)    (bind (consumed-strings->list l)  p))
    ((_ p)      (bind (consumed-strings->list)    p))
    ))

;; shortcut for (bind (consumed-pairs->list ...) ... )
(define-syntax bind-consumed-pairs->list
  (syntax-rules () 
    ((_ l p)    (bind (consumed-pairs->list l)  p))
    ((_ p)      (bind (consumed-pairs->list)    p))
    ))

)
