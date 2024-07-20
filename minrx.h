//
// MinRX: a minimal matcher for POSIX Extended Regular Expressions.
// Copyright (C) 2023, 2024 Michael J. Haertel.
//
// This file is part of MinRX.
//
// MinRX is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
//
// MinRX is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef _MINRX_H
#define _MINRX_H

#include <stddef.h>
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Names and types in this file have been chosen so that if the minrx_ and MINRX_ prefixes
 * are erased throughout the result will be usable as a POSIX-conforming regex.h.
 */

typedef enum {				/* Flags for minrx_reg*comp() */
	MINRX_REG_EXTENDED = 1,		/* required in current version of MinRX */
	MINRX_REG_ICASE = 2,		/* ignore case in pattern and search */
	MINRX_REG_NEWLINE = 4,		/* exclude \n from . and [^...] and treat as boundary for ^ and $ */
	MINRX_REG_NOSUB = 8,		/* yes/no result only; no regmatch_t substrings results */
	MINRX_REG_BRACE_COMPAT = 16,	/* { begins interval expression only when followed by digit */
	MINRX_REG_BRACK_ESCAPE = 32,	/* bracket expressions [...] allow backslash escapes */
	MINRX_REG_EXTENSIONS_BSD = 64,	/* enable BSD extensions \< and \> */
	MINRX_REG_EXTENSIONS_GNU = 128	/* enable GNU extensions \b \B \s \S \w \W */
} minrx_regcomp_flags_t;

typedef enum {				/* Flags for minrx_reg*exec() */
	MINRX_REG_NOTBOL = 1,		/* don't match ^ at beginning of string */
	MINRX_REG_NOTEOL = 2,		/* don't match $ at end of string */
	MINRX_REG_RESUME = 4,		/* MinRX extension: resume search from rm[0].rm_eo */
} minrx_regexec_flags_t;

typedef enum {				/* Return values from minrx_reg*comp() and minrx_reg*exec() */
	MINRX_REG_SUCCESS = 0,		/* regcomp: compiled successfully; regexec: match found */
	MINRX_REG_BADPAT,		/* regcomp: bad pattern (generic) */
	MINRX_REG_BADBR,		/* regcomp: invalid contents of {} */
	MINRX_REG_BADRPT,		/* regcomp: ? * + or {interval} not preceded by valid subpattern */
	MINRX_REG_EBRACE,		/* regcomp: unbalanced { */
	MINRX_REG_EBRACK,		/* regcomp: unbalanced [ */
	MINRX_REG_ECOLLATE,		/* regcomp: invalid collating element */
	MINRX_REG_ECTYPE,		/* regcomp: invalid character class name */
	MINRX_REG_EESCAPE,		/* regcomp: invalid backslash */
	MINRX_REG_EPAREN,		/* regcomp: unbalanced ( */
	MINRX_REG_ERANGE,		/* regcomp: invalid range endpoint */
	MINRX_REG_ESPACE,		/* regcomp: memory allocation failed */
	MINRX_REG_ESUBREG,		/* regcomp: invalid \digit */
	MINRX_REG_NOMATCH,		/* regexec: match not found */
	MINRX_REG_UNKNOWN		/* unknown error code */
} minrx_result_t;

typedef struct {
	void *re_regexp;
	size_t re_nsub;
	minrx_regcomp_flags_t re_compflags;
} minrx_regex_t;

typedef struct {
	ptrdiff_t rm_so;
	ptrdiff_t rm_eo;
} minrx_regmatch_t;

/* Compile from user syntax to internal regex_t; returns minrx_result_t (as integer) */
int minrx_regcomp(minrx_regex_t *, const char * /* regexp (NUL terminated) */, int /* minrx_regcomp_flags_t */);
int minrx_regncomp(minrx_regex_t *, size_t /* nregexp */, const char * /* regexp[nregexp] */, int /* minrx_regcomp_flags_t */);

/* Search for previously-compiled regexp; return minrx_result_t (as integer) */
int minrx_regexec(minrx_regex_t *, const char * /* string NUL-terminated */, size_t /* nrm */, minrx_regmatch_t * /* rm[nrm] */, int /* minrx_regexec_flags_t */);
int minrx_regnexec(minrx_regex_t *, size_t /* nstring */, const char * /* string[nstring] */, size_t /* nrm */, minrx_regmatch_t * /* rm[nrm] */, int /* minrx_regexec_flags_t */);

size_t minrx_regerror(int /* minrx_result_t */, const minrx_regex_t *, char * /* errbuf[nerrbuf] */, size_t /* nerrbuf */);
void minrx_regfree(minrx_regex_t *);

#ifdef __cplusplus
} /* extern "C" */
#endif /* __cplusplus */
#endif /* MINRX_H */
