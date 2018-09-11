
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
