
(import lexgen abnf)

(define fws
  (concatenation
   (optional-sequence 
    (concatenation
     (repetition wsp)
     (drop-consumed 
      (alternatives crlf lf cr))))
   (repetition1 wsp)))


(define (between-fws p)
  (concatenation
   (drop-consumed (optional-sequence fws)) p 
   (drop-consumed (optional-sequence fws))))

;; Date and Time Specification from RFC 5322 (Internet Message Format)

;; The following abnf parser combinators parse a date and time
;; specification of the form
;;
;;   Thu, 19 Dec 2002 20:35:46 +0200
;;
; where the weekday specification is optional. 
			     
;; Match the abbreviated weekday names

(define day-name 
  (alternatives
   (lit "Mon")
   (lit "Tue")
   (lit "Wed")
   (lit "Thu")
   (lit "Fri")
   (lit "Sat")
   (lit "Sun")))

;; Match a day-name, optionally wrapped in folding whitespace

(define day-of-week (between-fws day-name))


;; Match a four digit decimal number

(define year (between-fws (repetition-n 4 decimal)))

;; Match the abbreviated month names

(define month-name (alternatives
		    (lit "Jan")
		    (lit "Feb")
		    (lit "Mar")
		    (lit "Apr")
		    (lit "May")
		    (lit "Jun")
		    (lit "Jul")
		    (lit "Aug")
		    (lit "Sep")
		    (lit "Oct")
		    (lit "Nov")
		    (lit "Dec")))

;; Match a month-name, optionally wrapped in folding whitespace

(define month (between-fws month-name))


;; Match a one or two digit number

(define day (concatenation
	     (drop-consumed (optional-sequence fws))
	     (alternatives 
	      (variable-repetition 1 2 decimal)
	      (drop-consumed fws))))

;; Match a date of the form dd:mm:yyyy
(define date (concatenation day month year))

;; Match a two-digit number 

(define hour      (repetition-n 2 decimal))
(define minute    (repetition-n 2 decimal))
(define isecond   (repetition-n 2 decimal))

;; Match a time-of-day specification of hh:mm or hh:mm:ss.

(define time-of-day (concatenation
		     hour (drop-consumed (char #\:))
		     minute (optional-sequence 
			     (concatenation (drop-consumed (char #\:))
 					 isecond))))

;; Match a timezone specification of the form
;; +hhmm or -hhmm 

(define zone (concatenation 
	      (drop-consumed fws)
	      (alternatives (char #\-) (char #\+))
	      hour minute))

;; Match a time-of-day specification followed by a zone.

(define itime (concatenation time-of-day zone))

(define date-time (concatenation
		   (optional-sequence
		    (concatenation
		     day-of-week
		     (drop-consumed (char #\,))))
		   date
		   itime
		   (drop-consumed (optional-sequence fws))))

(define (err s)
  (print "lexical error on stream: " s)
  `(error))

(print (lex date-time err "Thu, 19 Dec 2002 20:35:46 +0200"))


;;
;;  Round-trip unit tests for abnf-lens, the bidirectional (lens-like)
;;  counterpart to the abnf parser combinators.
;;

(import abnf-lens abnf abnf-consumers test srfi-1)

(define (test-error s)
  (list 'parse-error s))

;; A few small rules shared across groups below.

(define bi-word
  (bi-iso
   (lambda (flat) (and (pair? flat) (list->string flat)))
   (lambda (v) (and (string? v) (pair? (string->list v)) (string->list v)))
   (bi-repetition1 bi-alpha)))

(define bi-digits
  (bi-iso
   (lambda (flat) (and (pair? flat) (list->string flat)))
   (lambda (v) (and (string? v) (pair? (string->list v)) (string->list v)))
   (bi-repetition1 bi-decimal)))


;;;; Core engine: bi-seq / bi-concatenation, bi-iso, bi-maybe

(test-group "core engine"

  (test "bi-seq print threads chars and remaining vals"
        '((#\a #\b) ())
        (let ((r ((bp-printer (bi-seq bi-alpha bi-alpha)) (list #\a #\b))))
          (list (car r) (cdr r))))

  (test "bi-concatenation of one bp is that bp"
        #\a
        (let ((r ((bp-printer (bi-concatenation bi-alpha)) (list #\a))))
          (car (car r))))

  (test "bi-pass prints nothing and consumes nothing"
        '(() (1 2))
        (let ((r ((bp-printer bi-pass) (list 1 2))))
          (list (car r) (cdr r))))

  (test "bi-iso round-trip: parse then print reproduces the string"
        "hello"
        (bp-print bi-word (car (car (bp-parse bi-word "hello" test-error)))))

  (test "bi-iso round-trip: print then parse reproduces the value"
        "hello"
        (car (car (bp-parse bi-word (bp-print bi-word "hello") test-error))))

  (test "bi-iso deconstruct rejecting a value fails the print"
        #f
        ((bp-printer bi-word) (list 42)))

  (test "bi-iso fails (rather than hangs) on an empty value list"
        #f
        ((bp-printer bi-word) '()))

  (test "bi-iso arity guard: printer under-consuming what deconstruct produced fails loudly"
        #f
        (let ((bad (bi-iso
                    (lambda (flat) (car flat))
                    (lambda (v) (list v v))       ; two items ...
                    (bi-concatenation bi-word))))  ; ... but this only consumes one
          ((bp-printer bad) (list "x"))))

  (test "bi-maybe: present value round-trips"
        "hi"
        (bp-print (bi-maybe bi-word) "hi"))

  (test "bi-maybe: absent value prints as empty text and still consumes its slot"
        '("" ())
        (let ((r ((bp-printer (bi-maybe bi-word)) (list #f))))
          (list (list->string (car r)) (cdr r))))

  (test "bi-maybe: parsing text present yields the value"
        "hi"
        (car (car (bp-parse (bi-maybe bi-word) "hi" test-error))))
  )


;;;; Terminal leaves

(test-group "terminal leaves"

  (test "bi-char prints the exact character it was given"
        "@"
        (bp-print (bi-char #\@) #\@))

  (test "bi-char rejects a mismatched character"
        #f
        ((bp-printer (bi-char #\@)) (list #\!)))

  (test "bi-drop-char prints its fixed character without consuming a value"
        '("@" (1 2))
        (let ((r ((bp-printer (bi-drop-char #\@)) (list 1 2))))
          (list (list->string (car r)) (cdr r))))

  (test "bi-drop-char parses and drops the character"
        '()
        (car (bp-parse (bi-drop-char #\@) "@" test-error)))

  (test "bi-drop-lit prints its canonical spelling regardless of casing at definition"
        "MAIL"
        (list->string (car ((bp-printer (bi-drop-lit "MAIL")) '()))))

  (test "bi-drop-lit parses case-insensitively"
        '()
        (car (bp-parse (bi-drop-lit "MAIL") "mail" test-error)))

  (test "bi-drop-lit fails to parse a non-matching string"
        #t
        (equal? (car (bp-parse (bi-drop-lit "MAIL") "fail" test-error)) 'parse-error))

  (test "bi-crlf round-trip"
        "\r\n"
        (list->string (car ((bp-printer bi-crlf) (list (integer->char 13) (integer->char 10))))))

  (test "bi-drop-crlf prints CRLF without consuming a value"
        "\r\n"
        (list->string (car ((bp-printer bi-drop-crlf) '()))))

  (test "bi-alpha prints a matching letter"
        "x"
        (bp-print bi-alpha #\x))

  (test "bi-alpha rejects a non-letter value"
        #f
        ((bp-printer bi-alpha) (list #\5)))

  (test "bi-alpha rejects a non-character value"
        #f
        ((bp-printer bi-alpha) (list 5)))

  (test "bi-alpha fails (not hangs) on an empty value list"
        #f
        ((bp-printer bi-alpha) '()))

  (test "bi-decimal rejects a letter"
        #f
        ((bp-printer bi-decimal) (list #\a)))

  (test "bi-set accepts a member of the set"
        "a"
        (bp-print (bi-set-from-string "aeiou") #\a))

  (test "bi-set rejects a non-member"
        #f
        ((bp-printer (bi-set-from-string "aeiou")) (list #\b)))

  (test "bi-range accepts a value inside the range"
        "5"
        (bp-print (bi-range #\0 #\9) #\5))

  (test "bi-range rejects a value outside the range"
        #f
        ((bp-printer (bi-range #\0 #\9)) (list #\x)))

  (test "bi-range normalizes a reversed bound the same way abnf:range does"
        "5"
        (bp-print (bi-range #\9 #\0) #\5))
  )


;;;; Repetition family

(test-group "repetition family"

  (test "bi-repetition1 round-trips a run of letters"
        "abc"
        (bp-print bi-word "abc"))

  (test "bi-repetition-n prints exactly n repetitions"
        "2002"
        (bp-print bi-digits "2002"))

  (test "bi-repetition-n fails to print a value shorter than n"
        #f
        ((bp-printer (bi-repetition-n 4 bi-decimal)) (list #\2 #\0 #\0)))

  (test "bi-repetition-n leaves the excess unconsumed for a value longer than n"
        '(#\5)
        (cdr ((bp-printer (bi-repetition-n 4 bi-decimal)) (list #\2 #\0 #\0 #\2 #\5))))

  (test "bi-repetition-n as the whole content of a rule rejects an oversized value"
        #f
        (bp-print (bi-iso list->string string->list (bi-repetition-n 4 bi-decimal)) "20025"
                  (lambda (v) #f)))

  (test "bi-variable-repetition accepts a count within range"
        "255"
        (bp-print (bi-iso list->string string->list (bi-variable-repetition 1 3 bi-decimal)) "255"))

  (test "bi-variable-repetition rejects a count below the minimum"
        #f
        (bp-print (bi-iso list->string string->list (bi-variable-repetition 2 3 bi-decimal)) "1"
                  (lambda (v) #f)))

  (test "bi-variable-repetition rejects a count above the maximum (leftover unconsumed)"
        #f
        (bp-print (bi-iso list->string string->list (bi-variable-repetition 1 3 bi-decimal)) "1234"
                  (lambda (v) #f)))

  (test "bi-optional-sequence prints nothing and touches no values when its bp does not match"
        '(() (42))
        (let ((r ((bp-printer (bi-optional-sequence bi-word)) (list 42))))
          (list (car r) (cdr r))))

  (test "bi-optional-sequence prints its bp's output when it does match"
        "hi"
        (list->string (car ((bp-printer (bi-optional-sequence bi-word)) (list "hi")))))
  )


;;;; bi-alternatives

(test-group "bi-alternatives"

  (define-bi-rule tagged-a (a bi-word))
  (define-bi-rule tagged-b (bi-drop-char #\#) (b bi-digits))
  (define bi-either (bi-alternatives bi-tagged-a bi-tagged-b))

  (test "bi-alternatives picks the first branch whose value it accepts"
        "hi"
        (bp-print bi-either (make-tagged-a "hi")))

  (test "bi-alternatives falls through to a later branch"
        "#42"
        (bp-print bi-either (make-tagged-b "42")))

  (test "bi-alternatives parses via the first matching branch"
        #t
        (tagged-a? (car (car (bp-parse bi-either "hi" test-error)))))

  (test "bi-alternatives fails when no branch's deconstruct accepts the value"
        #f
        ((bp-printer bi-either) (list 42)))
  )


;;;; define-bi-rule

(test-group "define-bi-rule"

  (define-bi-rule addr
    (local  bi-word)
    (bi-drop-char #\@)
    (domain bi-word))

  (test "define-bi-rule generates a working record constructor/accessors"
        (list "foo" "bar")
        (let ((a (make-addr "foo" "bar")))
          (list (addr-local a) (addr-domain a))))

  (test "define-bi-rule: parse-then-print reproduces the original text"
        "foo@bar"
        (bp-print bi-addr (car (car (bp-parse bi-addr "foo@bar" test-error)))))

  (test "define-bi-rule: print-then-parse reproduces the original value"
        '("foo" "bar")
        (let ((a (car (car (bp-parse bi-addr (bp-print bi-addr (make-addr "foo" "bar")) test-error)))))
          (list (addr-local a) (addr-domain a))))

  (test "define-bi-rule: printing a value of an unrelated type fails"
        #f
        ((bp-printer bi-addr) (list 42)))
  )


;;;; define-bi-datatype

(test-group "define-bi-datatype"

  (define-bi-datatype shape shape?
    (Circle (r bi-digits))
    (Square (s bi-digits))
    (Origin))

  (test "define-bi-datatype: each variant round-trips through its own branch"
        "5"
        (bp-print bi-shape (Circle "5")))

  (test "define-bi-datatype: a nullary variant prints as empty text"
        ""
        (bp-print bi-shape (Origin)))

  (test "define-bi-datatype: parsing selects a variant and round-trips"
        "7"
        (bp-print bi-shape (car (car (bp-parse bi-shape "7" test-error)))))
  )


;;;; Prototype checkpoint: a chicken-smtp-shaped slice (local-part@domain),
;;;; reimplemented with define-bi-rule as a test fixture only -- no changes
;;;; are made to the chicken-smtp repository. Dotted local-part and domain
;;;; are kept as a list of parts rather than smtp.scm's joined string, to
;;;; keep the fixture focused on exercising the bp machinery.

(test-group "chicken-smtp-shaped prototype"

  (define bi-atext
    (bi-alternatives bi-alpha bi-decimal (bi-set-from-string "!#$%&'*+-/=?^_`{|}~")))

  (define bi-atom
    (bi-iso
     (lambda (flat) (and (pair? flat) (list->string flat)))
     (lambda (v) (and (string? v) (pair? (string->list v)) (string->list v)))
     (bi-repetition1 bi-atext)))

  (define bi-dot-parts
    (bi-iso
     (lambda (flat) flat)
     (lambda (v) (and (list? v) (pair? v) (every string? v) v))
     (bi-seq bi-atom (bi-repetition (bi-seq (bi-drop-char #\.) bi-atom)))))

  (define-bi-rule mailbox
    (local-part bi-dot-parts)
    (bi-drop-char #\@)
    (domain bi-dot-parts))

  (test "prototype: parses local-part and domain into a mailbox record"
        (list (list "foo" "bar") (list "example" "com"))
        (let ((m (car (car (bp-parse bi-mailbox "foo.bar@example.com" test-error)))))
          (list (mailbox-local-part m) (mailbox-domain m))))

  (test "prototype: prints a mailbox record back to the same text"
        "foo.bar@example.com"
        (bp-print bi-mailbox (make-mailbox (list "foo" "bar") (list "example" "com"))))

  (test "prototype: parse then print reproduces the original text exactly"
        "foo.bar@example.com"
        (bp-print bi-mailbox (car (car (bp-parse bi-mailbox "foo.bar@example.com" test-error)))))
  )

(test-exit)
