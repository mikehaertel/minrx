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
of MinRX operate on Unicode code points.  MinRX uses the C standard
multibyte-to-wide conversion functions to to transform input to its
internal representation.  Right now MinRX processes every input character
through `mbrtowc()`, which is typically slow.  As development progresses
MinRX will add fast(er) paths for the cases where the input is encoded
in a 1-byte character set, or where the input is known to be encoded
in UTF-8.

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

MinRX is a nearly-feature-complete implementation of POSIX EREs, with
just two caveats that I'm aware of:

* The `[. .]` syntax inside bracket expressions for "collating element
  names" is only partially implemented.  MinRX supports use of `[. .]`
  for quoting single characters, but does *not* support multi-character
  collating element names inside `[. .]`.  As far as I know, there is
  no portable POSIX API for looking up the locale information that
  would be needed to implement this syntax.

* The `[= =]` syntax inside bracket expressions for "collating element
  equivalence classes" is *entirely* unimplemented.  Again, as far
  as I know there is no portable POSIX API that gives access to access
  to the necessary locale information.

MinRX also provides a few BSD extensions (`\< \>`) and GNU extensions
(<code>\\` \\' \b \B \s \S \w \W</code>) that can be optionally enabled via flag
passed at regexp compile time, as well as a few syntax compatibility
options needed to enable use in Gawk.

## Tools

The program `rxgrep.c` is a minimal version of `egrep`, using MinRX for
matching. It accepts the standard POSIX options for `grep`, but not `-E`
or `-F`, as currently MinRX only supports Extended Regular Expressions. For
the standard options, it also accepts the same corresponding long
options as used by GNU `grep`, for compatibility (as far as it goes)
with that program. `rxgrep --help` prints a short summary of the
available options.

Do *not* have high expectations for its performance. The code was written
with simplicity and correctness in mind, not speed, and the MinRX matcher
itself is not yet performant.

You can compile `rxgrep.c` with either a C or a C++ compiler, but it
must be linked with a C++ compiler.

## Installation

A `meson` build has been contributed by shenleban tongying.  The included
GNU `Makefile` provides targets `compile`, `install`, and `uninstall` that
invoke the `meson` build, e.g. `make PREFIX=/some/dir install`.

There is a test program `tryit` that is built by `make compile`, which will
print a usage message if invoked with no arguments.  (This is not installed
by `make install`).

If you don't have `meson` you can `make tryit` to build the test program.

`make clean` removes all build artifacts (both `meson` and traditional).

## Future plans

My current development focus is correctness.  I'm hoping the public will
thoroughly exercise this matcher and bury me in a deluge of bug reports. :-)

At some point I will switch focus to performance.  Planned work:

* Faster support for well-known input encodings like UTF-8.

* "First character" optimization to speed up initial search.

* Possibly a caching-DFA-like optimization: The MinRX SNFA automaton,
  with its stack of arbitrary integers, does not in general correspond
  to any deterministic *finite* automaton, but it is equivalent to a
  "deterministic *infinite* automaton", and it might be feasible to cache
  a useful working subset of this infinite automaton's states.

## License

This code is released under the GNU Lesser General Public License (GPL),
version 3 or any later version.

I want to preserve the ability to relicense this code in the future
under a more-permissive license, such as BSD or LGPL-v2.1.  So I will
only accept contributions from contributers who agree in advance that
their contributions can be relicensed under BSD or LGPL-v2.1 licenses.

The text of the GNU GPL licenses is included in the files [COPYING](COPYING)
and [COPYING.LESSER](COPYING.LESSER).

The manual page `minrx.3` (contributed by Arnold Robbins) is
under its own license, the text of which can be found in that file.

## Acknowledgements

Arnold Robbins pestered me for years to write this matcher, and enthusiastically
tested numerous early versions of it with Gawk.
He contributed the man page and `rxgrep.c`.

Development of this matcher also benefitted immensely from a POSIX regular
expression test suite developed by Douglas McIlroy, Glenn Fowler, and others.
