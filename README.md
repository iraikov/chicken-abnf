# chicken-abnf

[![Chicken Scheme](https://img.shields.io/badge/Chicken-Scheme-orange.svg)](https://call-cc.org/)

Parser combinators for Augmented BNF grammars (RFC 4234)


## Documentation

The `abnf` library provides a collection of combinators to help constructing parsers
for Augmented Backus-Naur form (ABNF) grammars [RFC 4234](http://www.ietf.org/rfc/rfc4234.txt).

## Library Procedures

The combinator procedures in this library are based on the interface
provided by the [lexgen](https://github.com/iraikov/chicken-lexgen) library.

### Terminal values and core rules

#### `char`

```
(char CHAR) => MATCHER
```

Builds a pattern matcher function that matches a single character.

#### `lit`

```
(lit STRING) => MATCHER
```

Matches a literal string (case-insensitive).

The following primitive parsers match the rules described in RFC 4234, Section 6.1.

#### `alpha`

```
(alpha STREAM-LIST) => STREAM-LIST
```

Matches any character of the alphabet.

#### `binary`

```
(binary STREAM-LIST) => STREAM-LIST
```

Matches [0..1].

#### `decimal`

```
(decimal STREAM-LIST) => STREAM-LIST
```

Matches [0..9].

#### `hexadecimal`

```
(hexadecimal STREAM-LIST) => STREAM-LIST
```

Matches [0..9] and [A..F,a..f].

#### `ascii-char`

```
(ascii-char STREAM-LIST) => STREAM-LIST
```

Matches any 7-bit US-ASCII character except for NUL (ASCII value 0).

#### `cr`

```
(cr STREAM-LIST) => STREAM-LIST
```

Matches the carriage return character.

#### `lf`

```
(lf STREAM-LIST) => STREAM-LIST
```

Matches the line feed character.

#### `crlf`

```
(crlf STREAM-LIST) => STREAM-LIST
```

Matches the Internet newline.

#### `ctl`

```
(ctl STREAM-LIST) => STREAM-LIST
```

Matches any US-ASCII control character. That is, any character with a
decimal value in the range of [0..31,127].

#### `dquote`

```
(dquote STREAM-LIST) => STREAM-LIST
```

Matches the double quote character.

#### `htab`

```
(htab STREAM-LIST) => STREAM-LIST
```

Matches the tab character.

#### `lwsp`

```
(lwsp STREAM-LIST) => STREAM-LIST
```

Matches linear white-space. That is, any number of consecutive
`wsp`, optionally followed by a `crlf` and (at least) one more
`wsp`.

#### `sp`

```
(sp STREAM-LIST) => STREAM-LIST
```

Matches the space character.

#### `vchar`

```
(vchar STREAM-LIST) => STREAM-LIST
```

Matches any printable ASCII character. That is, any character in the
decimal range of [33..126].

#### `unicode-vchar`

```
(unicode-vchar STREAM-LIST) => STREAM-LIST
```

As `vchar`, but also matches any Unicode character outside the ASCII range.

#### `octet`

```
(octet STREAM-LIST) => STREAM-LIST
```

Matches any character at all.

#### `wsp`

```
(wsp STREAM-LIST) => STREAM-LIST
```

Matches space or tab.

The following additional procedures are provided for convenience:

#### `set`

```
(set CHAR-SET) => MATCHER
```

Matches any character from an SRFI-14 character set.

#### `set-from-string`

```
(set-from-string STRING) => MATCHER
```

Matches any character from a set defined as a string.

### Operators

#### `concatenation`

```
(concatenation MATCHER-LIST) => MATCHER
```

`concatenation` matches an ordered list of rules. (RFC 4234, Section 3.1)

#### `alternatives`

```
(alternatives MATCHER-LIST) => MATCHER
```

`alternatives` matches any one of the given list of rules. (RFC 4234, Section 3.2)

#### `range`

```
(range C1 C2) => MATCHER
```

`range` matches a range of characters. (RFC 4234, Section 3.4)

#### `variable-repetition`

```
(variable-repetition MIN MAX MATCHER) => MATCHER
```

`variable-repetition` matches between `MIN` and `MAX` or more consecutive
elements that match the given rule. (RFC 4234, Section 3.6)

#### `repetition`

```
(repetition MATCHER) => MATCHER
```

`repetition` matches zero or more consecutive elements that match the given rule.

#### `repetition1`

```
(repetition1 MATCHER) => MATCHER
```

`repetition1` matches one or more consecutive elements that match the given rule.

#### `repetition-n`

```
(repetition-n N MATCHER) => MATCHER
```

`repetition-n` matches exactly `N` consecutive occurences of the given rule. (RFC 4234, Section 3.7)

#### `optional-sequence`

```
(optional-sequence MATCHER) => MATCHER
```

`optional-sequence` matches the given optional rule. (RFC 4234, Section 3.8)

#### `pass`

```
(pass) => MATCHER
```

This matcher returns without consuming any input.

#### `bind`

```
(bind F P) => MATCHER
```

Given a rule `P` and function `F`, returns a matcher that first
applies `P` to the input stream, then applies `F` to the returned
list of consumed tokens, and returns the result and the remainder of
the input stream.

Note: this combinator will signal failure if the input stream is
empty.

#### `bind*`

```
(bind* F P) => MATCHER
```

The same as `bind`, but will signal success if the input stream is
empty.

#### `drop-consumed`

```
(drop-consumed P) => MATCHER
```

Given a rule `P`, returns a matcher that always returns an empty
list of consumed tokens when `P` succeeds.

### Abbreviated syntax

`abnf` supports the following abbreviations for commonly used combinators:

* `::` : `concatenation`
* `:?` : `optional-sequence`
* `:!` : `drop-consumed`
* `:*` : `repetition`
* `:+` : `repetition1`

## Examples

The following parser libraries have been implemented with `abnf`, in
order of complexity:

* csv
* internet-timestamp
* json-abnf
* mbox
* smtp
* internet-message
* mime

### Parsing date and time

```scheme
(import abnf)

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
```

## abnf-lens: bidirectional parsers and printers

The `abnf-lens` library adds a bidirectional layer on top of `abnf`:
one grammar declaration produces both a parser, which reads text into
a Scheme value, and a printer, which writes that value back out as
text that the same parser accepts. This closes a gap in libraries
built on `abnf` alone where the printing direction would otherwise be
have to manually written and kept in sync.

`abnf-lens` builds directly on `abnf` and `lexgen`: every bidirectional
rule's parser half reuses the exact matcher `abnf` already provides, and
its printer half is that matcher's structural mirror image, reading
domain values off a list instead of characters off a stream. A tutorial
with worked examples from RFC 5322 follows the reference material below.

### The `bp` record

A bidirectional parser is a `bp`: a record pairing a parser (an ordinary
`abnf`/`lexgen` matcher) with a printer. A printer is a procedure
`(lambda (vals) (or #f (cons chars vals-rest)))`: given the list of domain
values still to be printed, it consumes a prefix of that list and returns
the printed characters together with what remains, or `#f` if it cannot
proceed -- for instance, because `vals` is empty or its first value is not
the type the rule expects.

#### `bp?`

```
(bp? OBJECT) => BOOL
```

Bidirectional-parser predicate.

#### `make-bp`

```
(make-bp PARSER PRINTER) => BP
```

Constructs a `bp` from a `PARSER` and a `PRINTER`.

#### `bp-parser`

```
(bp-parser BP) => PARSER
```

Returns the parser half of `BP`.

#### `bp-printer`

```
(bp-printer BP) => PRINTER
```

Returns the printer half of `BP`.

### Sequencing and choice

#### `bi-pass`

```
bi-pass => BP
```

Matches and prints nothing, and always succeeds. The bidirectional
counterpart of `pass`.

#### `bi-seq`

```
(bi-seq BP1 BP2) => BP
```

Matches (and prints) `BP1` followed by `BP2`. The bidirectional
counterpart of `lexgen`'s `seq`, which `concatenation` is built from.

#### `bi-concatenation`

```
(bi-concatenation BP ...) => BP
```

Matches (and prints) an ordered list of bidirectional parsers. The
bidirectional counterpart of `concatenation`.

#### `bi-alt`

```
(bi-alt BP1 BP2) => BP
```

Matches using either `BP1` or `BP2`. Printing never backtracks: the
value's own shape already picks the branch, so this tries each side's
printer in turn and uses the first that accepts the value. The
bidirectional counterpart of `lexgen`'s `bar`.

#### `bi-alternatives`

```
(bi-alternatives BP ...) => BP
```

Matches using any one of the given bidirectional parsers. The
bidirectional counterpart of `alternatives`.

### `bi-iso` and `bi-maybe`

#### `bi-iso`

```
(bi-iso CONSTRUCT DECONSTRUCT BP) => BP
```

The bidirectional counterpart of `bind`. `CONSTRUCT` turns the flat list
of sub-values matched by `BP` into a single domain value, or returns `#f`
to reject it. `DECONSTRUCT` does the reverse: it turns a domain value
into the flat list `BP`'s printer expects, or returns `#f` if the value
does not belong to this rule. Printing fails if `BP`'s printer does not
fully consume what `DECONSTRUCT` produced, so a rule that is missing or
has extra parts fails loudly rather than silently dropping data.

#### `bi-maybe`

```
(bi-maybe BP) => BP
```

An optional part that always occupies exactly one position in the
enclosing flat value list: the wrapped value when present, `#f` when
absent. Unlike `bi-optional-sequence` below, whose contribution to that
list is variable in length, `bi-maybe` is the combinator to reach for
when declaring an optional field with `define-bi-rule`.

### Terminal leaves

Every terminal value and core rule documented above under **Library
Procedures** has a `bi-`-prefixed bidirectional counterpart.

| Bidirectional rule | Matches the same character(s) as |
| --- | --- |
| `bi-alpha` | `alpha` |
| `bi-binary` | `binary` |
| `bi-decimal` | `decimal` |
| `bi-hexadecimal` | `hexadecimal` |
| `bi-ascii-char` | `ascii-char` |
| `bi-cr` | `cr` |
| `bi-lf` | `lf` |
| `bi-crlf` | `crlf` |
| `bi-ctl` | `ctl` |
| `bi-dquote` | `dquote` |
| `bi-htab` | `htab` |
| `bi-octet` | `octet` |
| `bi-sp` | `sp` |
| `bi-vchar` | `vchar` |
| `bi-unicode-vchar` | `unicode-vchar` |
| `bi-wsp` | `wsp` |

Two rules also have a "drop" counterpart, which consumes no value at
print time and instead always prints one fixed spelling: `bi-drop-crlf`,
and `bi-drop-lwsp` (the bidirectional counterpart of `lwsp` -- folding
whitespace carries no information of its own, so it has no non-dropped
form).

`bi-char`, `bi-drop-char`, `bi-lit`, and `bi-drop-lit` are the
bidirectional counterparts of `char` and `lit`, parametrized the same way:

#### `bi-char`

```
(bi-char CHAR) => BP
```

Matches (and prints) `CHAR` exactly, consuming one value at print time
that must equal `CHAR`.

#### `bi-drop-char`

```
(bi-drop-char CHAR) => BP
```

Matches `CHAR`, contributing nothing to the parsed value list; on print,
emits `CHAR` without consuming a value. This is the usual way to write a
fixed piece of punctuation -- a comma, an `@` sign -- inside
`define-bi-rule`.

#### `bi-lit`

```
(bi-lit STRING) => BP
```

Matches `STRING` case-insensitively, the same as `lit`; on print, always
emits the exact spelling of `STRING` given at definition time, regardless
of how the matched text was cased.

#### `bi-drop-lit`

```
(bi-drop-lit STRING) => BP
```

Matches `STRING` case-insensitively, contributing nothing to the parsed
value list; on print, emits `STRING`'s canonical spelling without
consuming a value. The usual way to write a fixed keyword inside
`define-bi-rule`.

`bi-set`, `bi-range`, and `bi-set-from-string` are the bidirectional
counterparts of `set`, `range`, and `set-from-string`: each matches, and
on print validates, a character against a character set, a range, or a
set of characters given as a string.

### Repetition and optional parts

#### `bi-repetition`

```
(bi-repetition BP) => BP
```

Matches (and prints) `BP` zero or more times in a row. The bidirectional
counterpart of `repetition`.

#### `bi-repetition1`

```
(bi-repetition1 BP) => BP
```

Matches (and prints) `BP` one or more times in a row. The bidirectional
counterpart of `repetition1`.

#### `bi-repetition-n`

```
(bi-repetition-n N BP) => BP
```

Matches (and prints) `BP` exactly `N` times in a row; printing fails if
fewer than `N` repetitions can be printed. The bidirectional counterpart
of `repetition-n`.

#### `bi-variable-repetition`

```
(bi-variable-repetition MIN MAX BP) => BP
```

Matches (and prints) `BP` between `MIN` and `MAX` times in a row. The
bidirectional counterpart of `variable-repetition`.

#### `bi-optional-sequence`

```
(bi-optional-sequence BP) => BP
```

Matches (and prints) `BP` if possible, or nothing at all. Its
contribution to the enclosing flat value list is variable in length --
zero or one items -- which makes it awkward as a `define-bi-rule` field;
`bi-maybe` above is usually the better choice there. The bidirectional
counterpart of `optional-sequence`.

### Generating record types

#### `define-bi-rule`

```
(define-bi-rule TYPENAME CLAUSE ...)
```

Generates a record type named `TYPENAME`, together with a matching
bidirectional parser named `bi-TYPENAME`, from one declaration. Each
`CLAUSE` is either `(FIELDNAME BP)`, which becomes both a record field
and a piece of the grammar, or a bare `BP` with no name, which is matched
and printed but not stored -- a fixed piece of syntax such as punctuation
or a keyword. A clause is read as a named field only when it is written
as a name next to a bare rule name; anything else, including a bare rule
name on its own, is read as a fixed part.

#### `define-bi-datatype`

```
(define-bi-datatype TYPENAME PRED VARIANT ...)
```

The sum-type counterpart of `define-bi-rule`: generates a `datatype` (via
the `datatype` egg) named `TYPENAME` with predicate `PRED`, one variant
per `VARIANT`, and a matching bidirectional parser named `bi-TYPENAME`
that tries each variant's rule in turn. Each `VARIANT` is `(VARIANT-NAME
CLAUSE ...)`, with clauses following the same field/fixed-part convention
as `define-bi-rule`.

### Entry points

#### `bp-parse`

```
(bp-parse BP STRING ERROR) => RESULT
```

Parses `STRING` using the parser half of `BP`, calling `ERROR` if it does
not match. The bidirectional counterpart of `lexgen`'s `lex`.

#### `bp-print`

```
(bp-print BP VALUE [ERROR]) => STRING
```

Prints `VALUE` using the printer half of `BP`, returning the result as a
string. Calls `ERROR` (by default, a procedure that raises an exception)
with `VALUE` if it cannot be printed -- for instance, because it is not
the type `BP` expects, or a nested rule's printer did not fully consume
the value's parts.

## Tutorial: abnf-lens

`abnf-lens` turns one grammar rule into two things at once: a parser that
reads text into a Scheme value, and a printer that writes that value back
out as text. You write the rule once; both directions come from it, so a
value your program builds is guaranteed to print as text your program can
also read back in. This tutorial builds up a handful of rules from RFC
5322, the "Internet Message Format" specification that defines e-mail
headers and addresses, and shows both directions working at each step.

The companion code is [examples/rfc5322-tutorial.scm](examples/rfc5322-tutorial.scm)
-- every snippet below is copied from that file, and you can run the whole
thing yourself, once the `abnf` egg (including its `abnf-lens` component)
is installed:

```bash
csi -q -s examples/rfc5322-tutorial.scm
```

This tutorial deliberately covers a *subset* of RFC 5322 -- enough to parse
and print an e-mail address and a `Subject:` header line. It leaves out
comments, folding whitespace inside a value, and Unicode, all of which the
`internet-message` egg's full, one-directional parser does handle. Each
simplification is called out where it happens.

### The one idea you need first

Every rule in `abnf-lens` is a `bp` -- short for "bidirectional parser." A
`bp` bundles a parser with a printer that is its structural mirror image: where
the parser reads characters off the front of the input text, the printer
reads values off the front of a list of values still waiting to be printed.
You will not usually build a `bp` by hand; the combinators in this tutorial
(`bi-alternatives`, `bi-iso`, `bi-seq`, `bi-repetition`, `define-bi-rule`)
build one for you from smaller `bp`s, the same way `abnf`'s ordinary
combinators build a parser from smaller parsers.

### Step 1: the character set a word is made of

RFC 5322 calls the character set that words like `foo` are built from
`atext`: letters, digits, and a fixed set of punctuation marks. `bi-alpha`
and `bi-decimal` are ready-made bidirectional rules for "a letter" and "a
digit"; `bi-set-from-string` builds one for an arbitrary set of characters;
and `bi-alternatives` combines several rules into "try the first, then the
next, and so on":

```scheme
(define bi-atext
  (bi-alternatives bi-alpha bi-decimal (bi-set-from-string "!#$%&'*+-/=?^_`{|}~")))
```

`bi-atext` is not yet very useful on its own -- it recognizes one
character at a time. The next step folds a whole run of them into a
string.

### Step 2: folding a run of characters into a word

An `atom` is one or more `atext` characters, treated as a single word. This
is the first place the two directions genuinely diverge: parsing folds a
list of matched characters down into a string, and printing has to unfold a
string back into the list of characters that will be checked and emitted
one at a time. `bi-iso` is the combinator that carries both halves of that
fold together:

```scheme
(define bi-atom
  (bi-iso
   (lambda (chars) (and (pair? chars) (list->string chars)))
   (lambda (s) (and (string? s) (positive? (string-length s)) (string->list s)))
   (bi-repetition1 bi-atext)))
```

`bi-iso` takes three arguments:

1. **construct** -- turns what the parser matched (here, a list of
   characters) into the value the caller sees (a string). Returning `#f`
   means "reject this," the same way a parser can fail to match.
2. **deconstruct** -- the reverse: turns a value back into the list the
   wrapped rule can print. Returning `#f` means "this value doesn't belong
   to this rule" -- printing a number here, instead of a non-empty string,
   correctly fails rather than crashing.
3. The wrapped rule itself -- `bi-repetition1 bi-atext`, "one or more
   atext characters," the bidirectional counterpart of `abnf`'s
   `repetition1`.

Try it both ways:

```scheme
(bp-parse bi-atom "foo" parse-error)   ; => (("foo") ())      -- text to value
(bp-print bi-atom "foo")               ; => "foo"             -- value to text
```

*Simplification: the real `atom` rule also allows comments and folding
whitespace around the word (RFC 5322 calls this `cfws`); this tutorial
leaves that out.*

### Step 3: joining words with dots

A `dot-atom-text`, the shape behind an address like `jane.doe`, is one or
more atoms separated by dots. Here it is represented as a list of the
atom strings -- `("jane" "doe")` -- with the dots added back on print and
stripped off on parse. Since the dots carry no information of their own,
they are matched by `bi-drop-char`, a fixed-punctuation rule that consumes
and emits a character without needing a value for it. That sets it apart
from a rule like `bi-alpha`: `bi-alpha` consumes one value (a letter) to
check and print, while `bi-drop-char` needs no value at all, because there
is only ever one character it could print.

```scheme
(define bi-dot-atom-text
  (bi-iso
   (lambda (atoms) atoms)
   (lambda (v) (and (list? v) (pair? v) (every string? v) v))
   (bi-seq bi-atom (bi-repetition (bi-seq (bi-drop-char #\.) bi-atom)))))
```

`bi-seq` and `bi-repetition` are the bidirectional counterparts of `abnf`'s
`concatenation` and `repetition`: `bi-seq` matches (and prints) two rules
back to back, and `bi-repetition` matches (and prints) a rule zero or more
times in a row. Because `construct` and `deconstruct` are both the
identity function here, the fold in this rule is trivial -- the list of
atoms produced by parsing *is* the value, with no extra step needed to
turn it into something more convenient.

```scheme
(bp-parse bi-dot-atom-text "jane.doe" parse-error)   ; => ((("jane" "doe")) ())
(bp-print bi-dot-atom-text (list "jane" "doe"))       ; => "jane.doe"
```

### Step 4: a record with two fields

An `addr-spec` -- the `local-part@domain` shape of an e-mail address -- is
where a named record type is worth generating instead of a bare list.
`define-bi-rule` does that in one declaration:

```scheme
(define-bi-rule addr-spec
  (local-part bi-dot-atom-text)
  (bi-drop-char #\@)
  (domain      bi-dot-atom-text))
```

Each clause is either `(field-name rule)`, which becomes both a record
field and a piece of the grammar, or a bare rule with no name, which is
matched and printed but not stored -- the `@` sign here, the same way the
dots were not stored in `bi-dot-atom-text`. This one declaration generates:

- a record type `addr-spec`, with a constructor `make-addr-spec`, a
  predicate `addr-spec?`, and accessors `addr-spec-local-part` and
  `addr-spec-domain`;
- a bidirectional rule named `bi-addr-spec` that parses text into an
  `addr-spec` record and prints an `addr-spec` record back to text.

```scheme
(bp-parse bi-addr-spec "jane.doe@example.com" parse-error)
;; => a list holding an addr-spec record whose local-part is
;;    ("jane" "doe") and whose domain is ("example" "com")

(bp-print bi-addr-spec (make-addr-spec (list "jane" "doe") (list "example" "com")))
;; => "jane.doe@example.com"
```

Rules built this way compose: `bi-dot-atom-text` did not change at all to
be used as a record field here, and a rule built with `define-bi-rule` can
just as easily be a field inside a larger rule.

*Simplification: the real `local-part` also allows a quoted string
(`"jane doe"`), and the real `domain` also allows a bracketed literal
address (`[192.0.2.1]`); this tutorial keeps to dot-atom-text for both.*

### Step 5: a whole header line

The last step puts a rule inside a fixed piece of syntax: an RFC 5322
`Subject:` header line is the keyword `Subject:`, one space, free text, and
a line ending. Free text (RFC 5322 calls it `unstructured`) is any run of
visible characters and spaces:

```scheme
(define bi-unstructured
  (bi-iso
   (lambda (chars) (list->string chars))
   (lambda (s) (and (string? s) (string->list s)))
   (bi-repetition (bi-alternatives bi-vchar bi-sp))))

(define-bi-rule subject-field
  (bi-drop-lit "Subject:")
  (bi-drop-char #\space)
  (text bi-unstructured)
  bi-drop-crlf)
```

Three more fixed-punctuation rules appear here: `bi-drop-lit` matches (and
canonically prints) a fixed keyword; `bi-drop-crlf` matches (and prints) a
line ending. Only `text` is a named field, so `subject-field` ends up with
one field -- `Subject:`, the space, and the line ending are structural, not
data. Because three of the four clauses are bare rules rather than
`(field-name rule)` pairs, this rule also shows the one naming convention
`define-bi-rule` depends on: a clause is treated as a named field only when
it is written as a name next to a bare rule name, like `(text
bi-unstructured)` above. Anything else -- a rule built by calling a
combinator with an argument, such as `(bi-drop-char #\space)`, or a bare
rule name on its own, such as `bi-drop-crlf` -- is a fixed part. If you ever
want to use a compound rule as a *bare*, unnamed part and it happens to
take a single bare rule name as its argument, define it under its own name
first and refer to that name; that sidesteps the ambiguity entirely.

```scheme
(bp-parse bi-subject-field "Subject: Dinner Friday\r\n" parse-error)
;; => a list holding a subject-field record whose text is "Dinner Friday"

(bp-print bi-subject-field (make-subject-field "Team meeting notes"))
;; => "Subject: Team meeting notes\r\n"
```

*Simplification: the real `unstructured` also allows folding whitespace,
which lets a long header value be broken across several physical lines;
this tutorial keeps it to one line.*

### Where to go from here

The rules above are deliberately small. `abnf-lens.scm` has bidirectional
counterparts for the rest of `abnf`'s combinators -- `bi-optional-sequence`
and `bi-maybe` for optional parts, `bi-repetition-n` and
`bi-variable-repetition` for fixed and bounded-length runs, and
`define-bi-datatype` for a rule with several distinct shapes (an
alternative, rather than a record) -- used the same way as `bi-seq` and
`define-bi-rule` above. [tests/lens-run.scm](tests/lens-run.scm) exercises
all of them, including one further worked example, a simplified
`local-part@domain` mailbox record built the same way as `addr-spec`
above.

## Version History

* 9.0 Added abnf-lens library and ported to CHICKEN 6
* 8.3 Removed unneeded dependency on yasos [thanks to Mario Domenech Goulart]
* 8.0 Ported to CHICKEN 5 and yasos collections interface 
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

```
Copyright 2009-2026 Ivan Raikov

This program is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

A full copy of the GPL license can be found at
<http://www.gnu.org/licenses/>.
```
