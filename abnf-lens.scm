;;
;;  Bidirectional (lens-like) counterpart to the ABNF parser
;;  combinators in abnf.scm: every combinator here carries both a
;;  parser, which is an abnf/lexgen matcher and a printer, which is
;;  the structural dual, which consumes domain values from the front
;;  of a value list instead of characters from the front of a
;;  character stream.
;;
;;   Copyright 2009-2026 Ivan Raikov
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


(module abnf-lens

	(
	 bp? make-bp bp-parser bp-printer

	 bi-pass bi-seq bi-concatenation
	 bi-iso bi-maybe

	 bi-char bi-drop-char bi-lit bi-drop-lit
	 bi-cr bi-lf bi-crlf bi-drop-crlf
	 bi-alpha bi-binary bi-decimal bi-hexadecimal bi-ascii-char
	 bi-ctl bi-dquote bi-htab bi-octet bi-sp bi-vchar bi-unicode-vchar bi-wsp
	 bi-drop-lwsp
	 bi-set bi-range bi-set-from-string

	 bi-repetition bi-repetition1 bi-repetition-n bi-variable-repetition
	 bi-optional-sequence

	 bi-alt bi-alternatives

	 define-bi-rule define-bi-datatype

	 bp-parse bp-print
	 )

	(import scheme (chicken base) (chicken syntax)
                srfi-1 utf8 utf8-srfi-14
                matchable datatype
                (prefix abnf abnf:)
                (prefix lexgen lex:))
        (import-for-syntax matchable)


;;;; Core bidirectional-parser record

;; A bp record pairs a parser, which is an abnf/lexgen matcher,
;; (lambda (sk fk strm) ...) with a printer. A printer is
;; (lambda (vals) (or #f (cons chars vals-rest))): given the list of
;; domain values still to be printed, it consumes an element of that list
;; and returns the printed characters together with what remains, or #f
;; if it cannot proceed. A printer must return #f when vals is empty or
;; its head is not the type/range it expects; 

(define-record-type bp
  (make-bp parser printer)
  bp?
  (parser  bp-parser)
  (printer bp-printer))


;;;; Sequencing

(define (bi-print-seq pr1 pr2)
  (lambda (vals)
    (let ((r1 (pr1 vals)))
      (and r1
           (let ((r2 (pr2 (cdr r1))))
             (and r2 (cons (append (car r1) (car r2)) (cdr r2))))))))

(define (bi-seq bp1 bp2)
  (make-bp (lex:seq (bp-parser bp1) (bp-parser bp2))
           (bi-print-seq (bp-printer bp1) (bp-printer bp2))))

;; Identity bp: matches/prints nothing, always succeeds.
(define bi-pass
  (make-bp lex:pass (lambda (vals) (cons '() vals))))

(define-syntax bi-concatenation
  (syntax-rules ()
    ((_)      bi-pass)
    ((_ a)    a)
    ((_ a b)  (bi-seq a b))
    ((_ a b ...)
     (bi-seq a (bi-concatenation b ...)))))


;;;; bi-iso: the bidirectional counterpart of abnf's bind

;; construct : flat list of sub-values -> domain value, or #f
;; deconstruct : domain value -> flat list of sub-values, or #f if the
;;   value does not belong to this rule
;; p : the wrapped bp, typically a bi-concatenation

(define (bi-iso construct deconstruct p)
  (make-bp
   ;; lex:bind hands its function the sub-values in
   ;; most-recently-matched- first order; construct expects them in
   ;; declaration order, so reverse that here.
   (lex:bind
    (lambda (flat) (let ((v (construct (reverse flat)))) (and v (list v))))
    (bp-parser p))
   (lambda (vals)
     (and (pair? vals)
          (let ((flat (deconstruct (car vals))))
            (and flat
                 (let ((r ((bp-printer p) flat)))
                   ;; p's printer must fully consume what deconstruct
                   ;; produced; an arity mismatch fails here rather
                   ;; than silently dropping data.
                   (and r (null? (cdr r)) (cons (car r) (cdr vals))))))))))


;;;; bi-maybe: an optional field that always occupies exactly one
;;;; position in the flat value list (#f when absent), unlike
;;;; bi-optional-sequence below, whose contribution is variable-arity.

(define (bi-maybe p)
  (make-bp
   (lex:bind (lambda (flat) (list (and (pair? flat) (car flat))))
             (lex:opt (bp-parser p)))
   (lambda (vals)
     (and (pair? vals)
          (if (car vals)
              (let ((r ((bp-printer p) (list (car vals)))))
                (and r (null? (cdr r)) (cons (car r) (cdr vals))))
              (cons '() (cdr vals)))))))


;;;; Terminal leaves

;; Builds a value-producing terminal bp from an existing abnf/lexgen
;; parser that matches exactly one character, plus a predicate the
;; matched character must satisfy in order to be printed.
(define (terminal-bp parser char-ok?)
  (make-bp
   parser
   (lambda (vals)
     (and (pair? vals) (char? (car vals)) (char-ok? (car vals))
          (cons (list (car vals)) (cdr vals))))))

(define (bi-char c)
  (make-bp
   (abnf:char c)
   (lambda (vals)
     (and (pair? vals) (char? (car vals)) (char=? (car vals) c)
          (cons (list c) (cdr vals))))))

(define (bi-drop-char c)
  (make-bp
   (lex:drop (abnf:char c))
   (lambda (vals) (cons (list c) vals))))

(define (bi-lit s)
  (let ((chars (if (string? s) (string->list s) s)))
    (make-bp
     (abnf:lit s)
     (lambda (vals)
       (let loop ((cs chars) (vs vals))
         (cond ((null? cs) (cons (string->list s) vs))
               ((and (pair? vs) (char? (car vs)) (char-ci=? (car vs) (car cs)))
                (loop (cdr cs) (cdr vs)))
               (else #f)))))))

(define (bi-drop-lit s)
  (make-bp
   (lex:drop (abnf:lit s))
   (lambda (vals) (cons (string->list s) vals))))

(define bi-cr (terminal-bp abnf:cr (lambda (ch) (char=? ch (integer->char 13)))))
(define bi-lf (terminal-bp abnf:lf (lambda (ch) (char=? ch (integer->char 10)))))
(define bi-crlf (bi-seq bi-cr bi-lf))

(define bi-drop-crlf
  (make-bp
   (lex:drop abnf:crlf)
   (lambda (vals) (cons (list (integer->char 13) (integer->char 10)) vals))))

(define bi-alpha        (terminal-bp abnf:alpha        (lambda (ch) (char-set-contains? char-set:letter ch))))
(define bi-binary        (terminal-bp abnf:binary       (lambda (ch) (or (char=? ch #\0) (char=? ch #\1)))))
(define bi-decimal       (terminal-bp abnf:decimal      (lambda (ch) (and (char>=? ch #\0) (char<=? ch #\9)))))
(define bi-hexadecimal   (terminal-bp abnf:hexadecimal  (lambda (ch) (char-set-contains? char-set:hex-digit ch))))
(define bi-ascii-char    (terminal-bp abnf:ascii-char   (lambda (ch) (let ((n (char->integer ch))) (and (>= n 1) (< n 128))))))
(define bi-ctl           (terminal-bp abnf:ctl          (lambda (ch) (char-set-contains? char-set:iso-control ch))))
(define bi-dquote        (terminal-bp abnf:dquote       (lambda (ch) (char=? ch #\"))))
(define bi-htab          (terminal-bp abnf:htab         (lambda (ch) (char=? ch (integer->char 9)))))
(define bi-octet         (terminal-bp abnf:octet        (lambda (ch) (char-set-contains? char-set:full ch))))
(define bi-sp            (terminal-bp abnf:sp           (lambda (ch) (char=? ch #\space))))
(define bi-vchar         (terminal-bp abnf:vchar        (lambda (ch) (char-set-contains? char-set:graphic ch))))
(define bi-unicode-vchar
  (terminal-bp abnf:unicode-vchar
               (lambda (ch)
                 (char-set-contains?
                  (char-set-union char-set:graphic
                                   (char-set-difference char-set:full char-set:ascii))
                  ch))))
(define bi-wsp           (terminal-bp abnf:wsp          (lambda (ch) (or (char=? ch #\space) (char=? ch (integer->char 9))))))

;; Folding whitespace, like abnf:lwsp, is only ever meaningful as
;; something to drop: a printer cannot reconstruct an arbitrary run of
;; whitespace without a value backing each character, and grammars only
;; ever use it decoratively. The canonical printed form is empty.
(define bi-drop-lwsp
  (make-bp
   (lex:drop abnf:lwsp)
   (lambda (vals) (cons '() vals))))

(define (bi-set s)
  (let ((cs (if (char-set? s) s (list->char-set (if (string? s) (string->list s) s)))))
    (terminal-bp (abnf:set cs) (lambda (ch) (char-set-contains? cs ch)))))

(define (bi-range a b)
  (let-values (((lo hi) (if (char<? b a) (values b a) (values a b))))
    (terminal-bp (abnf:range a b) (lambda (ch) (and (char>=? ch lo) (char<=? ch hi))))))

(define (bi-set-from-string s)
  (bi-set (string->char-set s)))


;;;; Repetition family

(define (bi-print-star pr)
  (lambda (vals)
    (let loop ((vals vals) (chars '()))
      (let ((r (pr vals)))
        (if r
            (loop (cdr r) (append chars (car r)))
            (cons chars vals))))))

(define (bi-repetition p)
  (make-bp (lex:star (bp-parser p)) (bi-print-star (bp-printer p))))

(define (bi-repetition1 p)
  (bi-seq p (bi-repetition p)))

(define (bi-repetition-n n p)
  (make-bp
   (abnf:repetition-n n (bp-parser p))
   (lambda (vals)
     (let loop ((i n) (vals vals) (chars '()))
       (if (= i 0)
           (cons chars vals)
           (let ((r ((bp-printer p) vals)))
             (and r (loop (- i 1) (cdr r) (append chars (car r))))))))))

(define (bi-variable-repetition mn mx p)
  (if (< mx mn)
      (bi-variable-repetition mx mn p)
      (make-bp
       (abnf:variable-repetition mn mx (bp-parser p))
       (lambda (vals)
         (let loop ((i 0) (vals vals) (chars '()))
           (if (>= i mx)
               (cons chars vals)
               (let ((r ((bp-printer p) vals)))
                 (if r
                     (loop (+ i 1) (cdr r) (append chars (car r)))
                     (if (>= i mn) (cons chars vals) #f)))))))))

;; Direct, variable-arity dual of abnf:optional-sequence: on parse,
;; contributes p's items if p matches, otherwise nothing. Kept for
;; manual composition; define-bi-rule fields should prefer bi-maybe,
;; which always occupies exactly one flat-list slot.
(define (bi-optional-sequence p)
  (make-bp
   (lex:opt (bp-parser p))
   (lambda (vals)
     (or ((bp-printer p) vals) (cons '() vals)))))


;;;; Alternatives

;; Printing never backtracks: a well-typed value's shape already picks
;; the right branch, so this just tries each branch's printer in turn.
(define (bi-alt bp1 bp2)
  (make-bp
   (lex:bar (bp-parser bp1) (bp-parser bp2))
   (lambda (vals) (or ((bp-printer bp1) vals) ((bp-printer bp2) vals)))))

(define-syntax bi-alternatives
  (syntax-rules ()
    ((_ a)    a)
    ((_ a b)  (bi-alt a b))
    ((_ a b ...)
     (bi-alt a (bi-alternatives b ...)))))


;;;; define-bi-rule: generate a record type and its bi-parser from one
;;;; declaration.

;; Split a list of define-bi-rule clauses into record fields and
;; bp-expressions, in matching order.
;;
;; A clause is a field when it is a two-element list whose second
;; element is a bare identifier: (fieldname bp-expr). Any other
;; clause, including a bare identifier on its own, is a fixed part,
;; parsed and printed but not stored in the record.
;;
;; Returns two values, both in clause order: the field names, and
;; every clause's bp-expression.
(define-for-syntax (bi-rule-classify-clauses clauses)
  (let loop ((cls clauses) (fields '()) (bps '()))
    (if (null? cls)
        (values (reverse fields) (reverse bps))
        (let ((cl (car cls)))
          (if (and (pair? cl) (pair? (cdr cl)) (null? (cddr cl)) (symbol? (cadr cl)))
              (loop (cdr cls) (cons (car cl) fields) (cons (cadr cl) bps))
              (loop (cdr cls) fields (cons cl bps)))))))

;; Define a record type and its paired parser/printer (bp) from a list
;; of clauses.
;;
;; Each clause is either (fieldname bp-expr), naming a record field
;; that occupies one flat-list slot, or a bare bp-expression --
;; typically a call to a "drop"-style leaf such as bi-drop-char or
;; bi-drop-lit, which is parsed and printed but not stored. A fixed
;; part whose own bp-expression would otherwise read as a field clause
;; (a compound expression taking a single identifier argument) should
;; be factored into its own top-level definition and referenced by
;; name instead.
;;
;; Generates a record type named typename, with a constructor, a
;; predicate, and one accessor per field, plus a bp named bi-typename
;; that parses input into a typename record and prints one back out,
;; carrying the fixed parts through unchanged.
(define-syntax define-bi-rule
  (er-macro-transformer
   (lambda (x r c)
     (match-let (((_ typename . clauses) x))
       (let ((%begin              (r 'begin))
             (%define             (r 'define))
             (%define-record-type (r 'define-record-type))
             (%lambda              (r 'lambda))
             (%apply               (r 'apply))
             (%list                (r 'list))
             (%and                 (r 'and))
             (%bi-iso              (r 'bi-iso))
             (%bi-concatenation    (r 'bi-concatenation)))
         (let-values (((fields bps) (bi-rule-classify-clauses clauses)))
           (let* ((mk        (symbol-append 'make- typename))
                  (pred      (symbol-append typename '?))
                  (accessors (map (lambda (f) (symbol-append typename '- f)) fields))
                  (bi-name   (symbol-append 'bi- typename))
                  (flat-var  (gensym 'flat))
                  (v-var     (gensym 'v)))
             `(,%begin
               (,%define-record-type ,typename
                 (,mk ,@fields)
                 ,pred
                 ,@(map (lambda (f a) (list f a)) fields accessors))
               (,%define ,bi-name
                 (,%bi-iso
                  (,%lambda (,flat-var) (,%apply ,mk ,flat-var))
                  (,%lambda (,v-var)
                    (,%and (,pred ,v-var)
                           (,%list ,@(map (lambda (a) (list a v-var)) accessors))))
                  (,%bi-concatenation ,@bps)))))))))))


;;;; define-bi-datatype: the sum-type counterpart, generating a
;;;; define-datatype (from the datatype egg) with one variant per
;;;; clause, wired together with bi-alternatives. Each variant's
;;;; clauses follow the same field/fixed-part convention as
;;;; define-bi-rule. Variant field predicates are left permissive
;;;; ((lambda (x) #t)): the bp's own parser/printer already enforce the
;;;; field's shape, so a second, independently-maintained predicate
;;;; here would only be able to drift out of sync with it.

(define-syntax define-bi-datatype
  (er-macro-transformer
   (lambda (x r c)
     (match-let (((_ typename pred . variants) x))
       (let ((%begin             (r 'begin))
             (%define            (r 'define))
             (%define-datatype   (r 'define-datatype))
             (%lambda             (r 'lambda))
             (%apply              (r 'apply))
             (%list               (r 'list))
             (%and                (r 'and))
             (%cases              (r 'cases))
             (%else               (r 'else))
             (%bi-iso             (r 'bi-iso))
             (%bi-alternatives    (r 'bi-alternatives))
             (%bi-concatenation   (r 'bi-concatenation)))
         (let* ((specs
                 (map (lambda (variant)
                        (let-values (((fields bps) (bi-rule-classify-clauses (cdr variant))))
                          (list (car variant) fields bps)))
                      variants))
                (datatype-clauses
                 (map (lambda (spec)
                        (let ((vname (car spec)) (fields (cadr spec)))
                          `(,vname ,@(map (lambda (f) (list f (list %lambda (list 'x) #t))) fields))))
                      specs))
                (bi-clauses
                 (map (lambda (spec)
                        (let* ((vname (car spec)) (fields (cadr spec)) (bps (caddr spec))
                               (flat-var (gensym 'flat)) (v-var (gensym 'v)))
                          `(,%bi-iso
                            (,%lambda (,flat-var) (,%apply ,vname ,flat-var))
                            (,%lambda (,v-var)
                              (,%and (,pred ,v-var)
                                     (,%cases ,typename ,v-var
                                              (,vname ,fields (,%list ,@fields))
                                              (,%else #f))))
                            (,%bi-concatenation ,@bps))))
                      specs)))
           `(,%begin
             (,%define-datatype ,typename ,pred ,@datatype-clauses)
             (,%define ,(symbol-append 'bi- typename)
               (,%bi-alternatives ,@bi-clauses)))))))))


;;;; Entry points

(define (bp-parse bp string error)
  (lex:lex (bp-parser bp) error string))

(define (bp-print bp value . rest)
  (let-optionals rest ((error (lambda (v) (error "abnf-lens: cannot print value: " v))))
    (let ((r ((bp-printer bp) (list value))))
      (if (and r (null? (cdr r)))
          (list->string (car r))
          (error value)))))

)
