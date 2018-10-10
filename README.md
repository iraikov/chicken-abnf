# chicken-abnf
Parser combinators for Augmented BNF grammars (RFC 4234)


## Description

{{abnf}} is a collection of combinators to help constructing parsers
for Augmented Backus-Naur form (ABNF) grammars
([[http://www.ietf.org/rfc/rfc4234.txt|RFC 4234]]).

## Library Procedures

The combinator procedures in this library are based on the interface
provided by the [[lexgen]] library.

### Terminal values and core rules 

The following procedures are provided as fields in the {{<CoreABNF>}} typeclass:

<procedure>(char CHAR) => MATCHER</procedure>

Procedure {{char}} builds a pattern matcher function that matches a
single character.

<procedure>(lit STRING) => MATCHER</procedure>

{{lit}} matches a literal string (case-insensitive).


The following primitive parsers match the rules described in RFC 4234, Section 6.1.

<procedure>(alpha STREAM-LIST) => STREAM-LIST</procedure>

Matches any character of the alphabet.

<procedure>(binary STREAM-LIST) => STREAM-LIST</procedure>

Matches [0..1].

<procedure>(decimal STREAM-LIST) => STREAM-LIST</procedure>

Matches [0..9].

<procedure>(hexadecimal STREAM-LIST) => STREAM-LIST</procedure>

Matches [0..9] and [A..F,a..f].

<procedure>(ascii-char STREAM-LIST) => STREAM-LIST</procedure>

Matches any 7-bit US-ASCII character except for NUL (ASCII value 0).

<procedure>(cr STREAM-LIST) => STREAM-LIST</procedure>

Matches the carriage return character.

<procedure>(lf STREAM-LIST) => STREAM-LIST</procedure>

Matches the line feed character.

<procedure>(crlf STREAM-LIST) => STREAM-LIST</procedure>

Matches the Internet newline.

<procedure>(ctl STREAM-LIST) => STREAM-LIST</procedure>

Matches any US-ASCII control character. That is, any character with a
decimal value in the range of [0..31,127].

<procedure>(dquote STREAM-LIST) => STREAM-LIST</procedure>

Matches the double quote character.

<procedure>(htab STREAM-LIST) => STREAM-LIST</procedure>

Matches the tab character.

<procedure>(lwsp STREAM-LIST) => STREAM-LIST</procedure>

Matches linear white-space. That is, any number of consecutive
{{wsp}}, optionally followed by a {{crlf}} and (at least) one more
{{wsp}}.

<procedure>(sp STREAM-LIST) => STREAM-LIST</procedure>

Matches the space character.

<procedure>(vspace STREAM-LIST) => STREAM-LIST</procedure>

Matches any printable ASCII character.  That is, any character in the
decimal range of [33..126].

<procedure>(wsp STREAM-LIST) => STREAM-LIST</procedure>

Matches space or tab.

<procedure>(quoted-pair STREAM-LIST) => STREAM-LIST</procedure>

Matches a quoted pair. Any characters (excluding CR and LF) may be
quoted.

<procedure>(quoted-string STREAM-LIST) => STREAM-LIST</procedure>

Matches a quoted string. The slash and double quote characters must be
escaped inside a quoted string; CR and LF are not allowed at all.

The following additional procedures are provided for convenience:

<procedure>(set CHAR-SET) => MATCHER</procedure>

Matches any character from an SRFI-14 character set.

<procedure>(set-from-string STRING) => MATCHER</procedure>

Matches any character from a set defined as a string.


### Operators

<procedure>(concatenation MATCHER-LIST) => MATCHER</procedure>

{{concatenation}} matches an ordered list of rules. (RFC 4234, Section 3.1)


<procedure>(alternatives MATCHER-LIST) => MATCHER</procedure>

{{alternatives}} matches any one of the given list of rules. (RFC 4234, Section 3.2)


<procedure>(range C1 C2) => MATCHER</procedure>

{{range}} matches a range of characters. (RFC 4234, Section 3.4)

<procedure>(variable-repetition MIN MAX MATCHER) => MATCHER</procedure>

{{variable-repetition}} matches between {{MIN}} and {{MAX}} or more consecutive
elements that match the given rule. (RFC 4234, Section 3.6)

<procedure>(repetition MATCHER) => MATCHER</procedure>

{{repetition}} matches zero or more consecutive elements that match the given rule. 

<procedure>(repetition1 MATCHER) => MATCHER</procedure>

{{repetition1}} matches one or more consecutive elements that match the given rule. 


<procedure>(repetition-n N MATCHER) => MATCHER</procedure>

{{repetition-n}} matches exactly {{N}} consecutive occurences of the given rule. (RFC 4234, Section 3.7)


<procedure>(optional-sequence MATCHER) => MATCHER</procedure>

{{optional-sequence}} matches the given optional rule. (RFC 4234, Section 3.8)

<procedure>(pass) => MATCHER</procedure>

This matcher returns without consuming any input.

<procedure>(bind F P) => MATCHER</procedure>

Given a rule {{P}} and function {{F}}, returns a matcher that first
applies {{P}} to the input stream, then applies {{F}} to the returned
list of consumed tokens, and returns the result and the remainder of
the input stream.

Note: this combinator will signal failure if the input stream is
empty.

<procedure>(bind* F P) => MATCHER</procedure>

The same as {{bind}}, but will signal success if the input stream is
empty.

<procedure>(drop-consumed P) => MATCHER</procedure>

Given a rule {{P}}, returns a matcher that always returns an empty
list of consumed tokens when {{P}} succeeds. 

### Abbreviated syntax

{{abnf}} supports the following abbreviations for commonly used combinators:

; {{::}} : {{concatenation}}
; {{:?}} : {{optional-sequence}}
; {{:!}} : {{drop-consumed}}
; {{:s}} : {{lit}}
; {{:c}} : {{char}}
; {{:*}} : {{repetition}}
; {{:+}} : {{repetition1}}


## Examples

The following parser libraries have been implemented with {{abnf}}, in
order of complexity:

* [[csv]] 
* [[internet-timestamp]] 
* [[json-abnf]] 
* [[mbox]]
* [[smtp]] 
* [[internet-message]] 
* [[mime]] 

### Parsing date and time

```scheme

(import lexgen abnf)

(define fws
  (concatenation
   (optional-sequence 
    (concatenation
     (repetition char-list/wsp)
     (drop-consumed 
      (alternatives char-list/crlf char-list/lf char-list/cr))))
   (repetition1 char-list/wsp)))


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
   (char-list/lit "Mon")
   (char-list/lit "Tue")
   (char-list/lit "Wed")
   (char-list/lit "Thu")
   (char-list/lit "Fri")
   (char-list/lit "Sat")
   (char-list/lit "Sun")))

;; Match a day-name, optionally wrapped in folding whitespace

(define day-of-week (between-fws day-name))


;; Match a four digit decimal number

(define year (between-fws (repetition-n 4 char-list/decimal)))

;; Match the abbreviated month names

(define month-name (alternatives
		    (char-list/lit "Jan")
		    (char-list/lit "Feb")
		    (char-list/lit "Mar")
		    (char-list/lit "Apr")
		    (char-list/lit "May")
		    (char-list/lit "Jun")
		    (char-list/lit "Jul")
		    (char-list/lit "Aug")
		    (char-list/lit "Sep")
		    (char-list/lit "Oct")
		    (char-list/lit "Nov")
		    (char-list/lit "Dec")))

;; Match a month-name, optionally wrapped in folding whitespace

(define month (between-fws month-name))


;; Match a one or two digit number

(define day (concatenation
	     (drop-consumed (optional-sequence fws))
	     (alternatives 
	      (variable-repetition 1 2 char-list/decimal)
	      (drop-consumed fws))))

;; Match a date of the form dd:mm:yyyy
(define date (concatenation day month year))

;; Match a two-digit number 

(define hour      (repetition-n 2 char-list/decimal))
(define minute    (repetition-n 2 char-list/decimal))
(define isecond   (repetition-n 2 char-list/decimal))

;; Match a time-of-day specification of hh:mm or hh:mm:ss.

(define time-of-day (concatenation
		     hour (drop-consumed (char-list/char #\:))
		     minute (optional-sequence 
			     (concatenation (drop-consumed (char-list/char #\:))
 					 isecond))))

;; Match a timezone specification of the form
;; +hhmm or -hhmm 

(define zone (concatenation 
	      (drop-consumed fws)
	      (alternatives (char-list/char #\-) (char-list/char #\+))
	      hour minute))

;; Match a time-of-day specification followed by a zone.

(define itime (concatenation time-of-day zone))

(define date-time (concatenation
		   (optional-sequence
		    (concatenation
		     day-of-week
		     (drop-consumed (char-list/char #\,))))
		   date
		   itime
		   (drop-consumed (optional-sequence fws))))

(define (err s)
  (print "lexical error on stream: " s)
  `(error))

(print (lex date-time err "Thu, 19 Dec 2002 20:35:46 +0200"))

```


## Requires

* [[lexgen]]
* [[typeclass]]
* [[input-classes]]

## Version History

* 7.0 Added bind* variant of bind [thanks to Peter Bex]
* 6.0 Using utf8 for char operations
* 5.1 Improvements to the CharLex->CoreABNF constructor
* 5.0 Synchronized with lexgen 5
* 3.2 Removed invalid identifier :|
* 3.0 Implemented typeclass interface
* 2.9 Bug fix in consumed-objects (reported by Peter Bex)
* 2.7 Added abbreviated syntax (suggested by Moritz Heidkamp)
* 2.6 Bug fixes in consumer procedures
* 2.5 Removed procedure memo
* 2.4 Moved the definition of bind and drop to lexgen
* 2.2 Added pass combinator
* 2.1 Added procedure variable-repetition
* 2.0 Updated to match the interface of lexgen 2.0
* 1.3 Fix in drop
* 1.2 Added procedures bind drop consume collect
* 1.1 Added procedures set and set-from-string
* 1.0 Initial release

## License


  Copyright 2009-2018 Ivan Raikov


  This program is free software: you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  A full copy of the GPL license can be found at
  <http://www.gnu.org/licenses/>.
