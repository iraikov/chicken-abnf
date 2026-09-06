;;
;;  abnf-lens tutorial code: a small, simplified slice
;;  of the RFC 5322 "Internet Message Format" grammar, built with
;;  bidirectional rules so that each rule below both parses text into a
;;  value and prints that value back out as text.
;;
;;  Run with:  csi -q -s examples/rfc5322-tutorial.scm


(import abnf-lens abnf abnf-consumers srfi-1)

(define (parse-error s)
  (error "could not parse input" s))


;;;; Step 1: atext (RFC 5322 section 3.2.3, simplified)
;;
;; atext is the character set that words like "foo" or "jane-doe" are
;; made of: letters, digits, and a handful of punctuation marks. The
;; real grammar also allows atext to include comments and folding
;; whitespace around it (cfws) and Unicode characters; both are left
;; out here to keep the tutorial small.

(define bi-atext
  (bi-alternatives bi-alpha bi-decimal (bi-set-from-string "!#$%&'*+-/=?^_`{|}~")))


;;;; Step 2: atom (RFC 5322 section 3.2.4)
;;
;; An atom is one or more atext characters, folded into a single
;; string. bi-iso is what performs that fold: its first argument turns
;; a run of matched characters into the value a caller sees (a
;; string); its second argument does the reverse, breaking a string
;; back down into the characters a printer needs to emit.

(define bi-atom
  (bi-iso
   (lambda (chars) (and (pair? chars) (list->string chars)))
   (lambda (s) (and (string? s) (positive? (string-length s)) (string->list s)))
   (bi-repetition1 bi-atext)))


;;;; Step 3: dot-atom-text (RFC 5322 section 3.2.4)
;;
;; A dot-atom-text is one or more atoms separated by dots, such as
;; "jane.doe". It is represented here as a list of the atom strings
;; ("jane" "doe"), with the dots added back in on print and stripped
;; out again on parse.

(define bi-dot-atom-text
  (bi-iso
   (lambda (atoms) atoms)
   (lambda (v) (and (list? v) (pair? v) (every string? v) v))
   (bi-seq bi-atom (bi-repetition (bi-seq (bi-drop-char #\.) bi-atom)))))


;;;; Step 4: addr-spec (RFC 5322 section 3.4.1)
;;
;; An addr-spec is a local-part, an @ sign, and a domain -- the
;; "jane.doe@example.com" shape of an e-mail address. define-bi-rule
;; generates a record type (with a constructor and accessors) and a
;; matching bidirectional rule from a single declaration: named
;; clauses become record fields; the bare bi-drop-char clause is
;; punctuation that is parsed and printed but not stored.
;;
;; The real grammar allows a quoted-string local-part and a
;; domain-literal domain (e.g. "[192.0.2.1]"); we leave this
;; out in this tutorial.

(define-bi-rule addr-spec
  (local-part    bi-dot-atom-text)
  (bi-drop-char  #\@)
  (domain        bi-dot-atom-text))


;;;; Step 5: a Subject: header line (RFC 5322 sections 3.6.5 and 3.2.5)
;;
;; unstructured is free text such as a subject line: any run of
;; visible characters and spaces. The real grammar also allows folding
;; whitespace to break the line across multiple physical lines; we
;; leave this out in this tutorial.


(define bi-unstructured
  (bi-iso
   (lambda (chars) (list->string chars))
   (lambda (s) (and (string? s) (string->list s)))
   (bi-repetition (bi-alternatives bi-vchar bi-sp))))

;; A full header line: the fixed keyword "Subject:", a single space,
;; the unstructured text, and a trailing CRLF. Every clause except
;; text is a fixed part, so the generated record has just one field.

(define-bi-rule subject-field
  (bi-drop-lit "Subject:")
  (bi-drop-char #\space)
  (text bi-unstructured)
  bi-drop-crlf)


;;;; Round-trip conversion

(define (round-trip label bp text)
  (let* ((parsed (car (car (bp-parse bp text parse-error))))
         (printed (bp-print bp parsed)))
    (print label ": " text " -> " parsed " -> " printed)))

(round-trip "atom"          bi-atom          "foo")
(round-trip "dot-atom-text" bi-dot-atom-text "jane.doe")
(round-trip "addr-spec"     bi-addr-spec     "jane.doe@example.com")
(round-trip "subject-field" bi-subject-field "Subject: Dinner Friday\r\n")

(print "constructed addr-spec printed: "
       (bp-print bi-addr-spec (make-addr-spec (list "jane" "doe") (list "example" "com"))))

(print "constructed subject-field printed: "
       (bp-print bi-subject-field (make-subject-field "Team meeting notes")))
