;;
;;  Parser for the grammar defined in RFC4234, "Augmented BNF for
;;  Syntax Specifications: ABNF".
;;
;;
;;   Copyright 2009-2021 Ivan Raikov
;;
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


(module abnf 

	(
	 (concatenation lex:seq) (alternatives lex:bar) 
	 variable-repetition repetition repetition1 repetition-n
	 optional-sequence 

	 pass drop-consumed bind bind*

	 ( :: lex:seq) :?  :!  :*  :+ 
         range set set-from-string char lit alpha 
         binary decimal hexadecimal ascii-char cr lf crlf ctl 
         dquote htab lwsp octet sp vchar unicode-vchar wsp 

	 )

        (import scheme (chicken base) 
                (only srfi-1 fold list-tabulate)
                utf8 utf8-srfi-14
                (prefix lexgen lex:))

        


(define pass lex:pass)
(define drop-consumed lex:drop)

(define bind lex:bind)
(define bind* lex:bind*)

(define char lex:char)
(define set lex:set)

;;;; ABNF operators

;; Concatenation (RFC 4234, Section 3.1)
(define-syntax concatenation
  (syntax-rules () 
    ((_)     lex:pass)
    ((_ a)    a)
    ((_ a b)  (lex:seq a b))
    ((concatenation a b ...) 
     (lex:seq a (concatenation b ...)))
    ))
    
;; Alternatives (RFC 4234, Section 3.2)
(define-syntax alternatives
  (syntax-rules () 
    ((_)      lex:pass)
    ((_ a)    a)
    ((_ a b)  (lex:bar a b))
    ((alternatives a b ...) 
     (lex:bar a (alternatives b ...)))
    ))

;; Value range alternatives (RFC 4234, Section 3.4)
(define range lex:range)


;;  Specific repetition (RFC 4234, Section 3.7)
(define (repetition-n n p)
  (let ((ps (list-tabulate n (lambda (i) p))))
    (lex:lst ps)))

;; Variable repetition (RFC 4234, Section 3.6)

;; * repetition
(define (repetition p)  (lex:star p))

;; 1* repetition
(define repetition1 lex:pos)

(define (variable-repetition min max p)
  (if (< max min) (variable-repetition max min p)
      (let loop ((i (- max 1)) (k (+ min 1)) (r (if (positive? min) (repetition-n min p) lex:pass)))
	(cond ((>= i min)  (loop (- i 1) (+ k 1) (lex:bar (repetition-n k p) r)))
	      (else r)))))

		
(define optional-sequence lex:opt)


;;;; Terminal values (RFC 4234, Section 2.3)

;; Matches a literal string (case-insensitive)

(define (lit s)
  (let* ((f  (lambda (t) (lex:tok t (lex:try char-ci=?))))
         (ps (map f (if (string? s) (string->list s) s))))
    (lex:lst ps)))

;;;; Core Rules (RFC 4234, Appendix B)

;; Match any character of the alphabet.
(define alpha (set char-set:letter))

;; Match [0..1]
(define binary (range #\0 #\1))

;; Match [0..9]
(define decimal (range #\0 #\9))

;; Match [0..9] and [A..F,a..f] 
(define hexadecimal (set char-set:hex-digit))

;; Match any 7-bit US-ASCII character except for NUL (ASCII value 0, that is).

(define ascii-char (set (ucs-range->char-set 1 127)))

;; Match the carriage return character \r.

(define cr (char (integer->char 13)))

;; Match the linefeed character \n.

(define lf (char (integer->char 10)))

;; Match the Internet newline \r\n.

;; cr lf
(define crlf (lex:seq cr lf))

;; Match any US-ASCII control character. That is any character with a
;; decimal value in the range of [0..31,127].

(define ctl (set char-set:iso-control))

;; Match the double quote character "

(define dquote (char #\"))

;; Match the tab \t character

(define htab (char (integer->char 9)))

;; Match either 'sp' or 'htab'.

(define wsp (set (char-set #\space #\tab)))

;; Match linear white space: *(WSP / CRLF WSP)

(define lwsp (lex:star (lex:bar wsp (lex:seq (lex:drop crlf) wsp))))


;; Match /any/ character.
(define octet (set char-set:full))

;; Match the space character

(define sp (char #\space))

;; Match any printable ASCII character. (The "v" stands for
;; "visible".) That is any character in the decimal range of
;; [33..126].

(define vchar  (set char-set:graphic))

;; As vchar, but include Unicode characters
(define unicode-vchar
  (set (char-set-union 
	char-set:graphic
	(char-set-difference
	 char-set:full 
	 char-set:ascii))))
		  
;;;; Additional convenience procedures and parser combinators

;; match any character from a set defined as a string
(define  (set-from-string s)
    (set (string->char-set s)))

;;;; Syntactic abbreviations
;;;; Based on a proposal by Moritz Heidkamp

(define :? optional-sequence)
(define :! drop-consumed)
(define :* repetition)
(define :+ repetition1)

(define-syntax ::
  (syntax-rules () ((_ e1 e2 ...) (concatenation e1 e2 ...))))

)
