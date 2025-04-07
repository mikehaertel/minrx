# MinRX: A minimalist POSIX Extended Regular Expression matcher

## Description

MinRX is library for matching
[POSIX Extended Regular Expressions](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html#tag_09_04)
(EREs).

MinRX is written in C++ 20, but exports a C API similar to the POSIX
[`<regex.h>`](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/regex.h.html)
functions, with C entry points for both counted and NUL-terminated
strings.  The MinRX function and macro names are generally constructed
by prepending `minrx_` or `MINRX_` to the names of their POSIX `<regex.h>`
counterparts.

My goal for MinRX is to eventually have performance competitive with
the fastest extant matchers, but for now the development focus is on
correctness and simplicity.  When I am confident that the core algorithm
is stable and well-tested, I plan to rewrite MinRX in C to improve
portability and perhaps further reduce resource usage.

MinRX attempts to implement the `LC_CTYPE`-dependent locale features
specified for POSIX regular expressions (with some limitations because
MinRX can't directly access the host operating system's private locale
database, and POSIX doesn't provide a portable API for some necessary
queries).  These features consist of the obvious support for international
character sets and multibyte character encodings, together with some
syntax for bracket expressions that allows many traditional uses of
bracket expressions to be expressed in a way independent of character
set and character encoding.

In order to fully support international locales, the core algorithms
of MinRX operate on 32-bit wide characters, loosely assumed to contain
Unicode code points.  In UTF-8 `LC_CTYPE` locales, MinRX uses its own
custom code to convert the input to 32-bit codepoint values.  In non-UTF-8
locales, MinRX uses the C99 `mbrtowc()` function to transform input to
its internal representation.  It is implementation-dependent whether
`mbrtowc()` translates to Unicode code points (under GNU `libc` it does).
MinRX also has an option to force the use of native encoding in 1-byte
`LC_CTYPE` locales.

MinRX is a non-backtracking matcher: it makes a single forward scan
through the search text, one character at a time, and considers all
possible matches in parallel.

MinRX runs in O(M * P) space and roughly O(M * N) time, where M is the
overall regular expression length, N is the search string length, and
P is the maximum parentheses nesting depth in the regular expression.

Note that MinRX does not support POSIX Basic Regular Expressions (BREs).
Thanks to the inclusion of backreferences, BREs are not true regular
expressions at all, and BREs with backreferences do not correspond to
finite automata.  BRE matchers typically have worst-case time complexity
that is exponential.  Although a mutant version of the MinRX algorithm
could probably be constructed to match BREs, it would almost certainly
have exponential worst-case *space* complexity as well, and so would be
unlikely to be broadly useful.

A detailed description of MinRX's algorithm can be found in [ALGORITHM.txt](ALGORITHM.txt).

## Features

MinRX is a nearly-feature-complete implementation of POSIX 2024 EREs,
with just two caveats that I'm aware of:

* The `[. .]` syntax inside bracket expressions for "collating element
  names" is only partially implemented.  MinRX supports use of `[. .]`
  for quoting single characters, but does *not* support multi-character
  collating element names inside `[. .]`.  As far as I know, there is
  no portable POSIX API for looking up the locale information that
  would be needed to implement this syntax.

* The `[= =]` syntax inside bracket expressions for "equivalence classes"
  uses a semi-portable but horrendously slow and
  non-standard-conforming algorithm for finding equivalent characters.
  Each `[= =]` in a pattern likely requires several milliseconds to
  compile on a modern CPU.  Again, there is no portable POSIX API that
  offers efficient access to the necessary locale information.

MinRX also provides a few BSD extensions (`\< \>`) and GNU extensions
(<code>\\\` \\' \b \B \s \S \w \W</code>) that can be optionally enabled
via flags passed at regexp compile time, as well as a few syntax compatibility
options needed to enable use in GNU `awk`.

## Installation

The build system is a mix of GNU `make` and `meson`.  The included
GNU `Makefile` provides targets `compile`, `install`, and `uninstall` that
invoke the `meson` build, e.g. `make PREFIX=/some/dir install`.

There are two test programs `rxgrep` and `tryit` that are also built
by `make compile`. These will print usage messages if invoked
with no arguments.  (These are not installed by `make install`).

If you don't have `meson` you can `make rxgrep` and/or `make tryit`
to build the test programs.

`make clean` removes all build artifacts (both `meson` and traditional).

## Tools

The program `rxgrep` is a minimal version of `egrep`, using MinRX for
matching. It accepts the standard POSIX options for `grep`, but not `-E`
or `-F`, as currently MinRX only supports Extended Regular Expressions. For
the standard options, it also accepts the same corresponding long
options as used by GNU `grep`, for compatibility (as far as it goes)
with that program. `rxgrep --help` prints a short summary of the
available options.

Do *not* have high expectations for `rxgrep` performance. The code
was written with simplicity and correctness in mind, not speed, and
the MinRX matcher itself is currently slow.

## Future plans

My current development focus is correctness.  I'm hoping the public will
thoroughly exercise this matcher and bury me in a deluge of bug reports. :-)

At some point I will switch focus to performance and portability.

Currently planned work:

* Improve support for POSIX bracket expression collating elements
  and, if possible, implement POSIX bracket expression equivalence classes
  in a better way.

* Possibly a caching-DFA-like optimization: The MinRX SNFA automaton,
  with its stack of arbitrary integers, does not in general correspond
  to any deterministic *finite* automaton, but it is equivalent to a
  "deterministic *infinite* automaton", and it might be feasible to cache
  a useful working subset of this infinite automaton's states.

* Rewrite in C for improved portability.

## License

This code is released under the Simplified BSD (2-clause) license.

The manual page `minrx.3` (contributed by Arnold Robbins) is
under its own license, the text of which can be found in that file.

## Acknowledgements

Arnold Robbins pestered me for years to write this matcher, and enthusiastically
tested numerous early versions of it with GNU `awk`.
He contributed the manual page and the `rxgrep` program.

The `meson` build was contributed by shenleban tongying.

Development of this matcher also benefitted immensely from a POSIX regular
expression test suite developed by Douglas McIlroy, Glenn Fowler, and others.
