//
// MinRX: a minimal matcher for POSIX Extended Regular Expressions.
// Copyright (C) 2023, 2024 Michael J. Haertel.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS “AS IS” AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
// SUCH DAMAGE.
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
	MINRX_REG_EXTENDED = 1,		/* POSIX option: use "extended" syntax (N.B. this flag is required in the current version of MinRX) */
	MINRX_REG_ICASE = 2,		/* POSIX option: ignore case in pattern and search */
	MINRX_REG_MINIMAL = 4,		/* POSIX option: swap meaning of ? and ??, * and *?, and + and +? operators */
	MINRX_REG_NEWLINE = 8,		/* POSIX option: exclude \n from . and [^...] and treat as boundary for ^ and $ */
	MINRX_REG_NOSUB = 16,		/* POSIX option: yes/no result only; no regmatch_t substrings results */
	MINRX_REG_BRACE_COMPAT = 32,	/* MinRX extension: { begins interval expression only when followed by digit */
	MINRX_REG_BRACK_ESCAPE = 64,	/* MinRX extension: bracket expressions [...] allow backslash escapes */
	MINRX_REG_EXTENSIONS_BSD = 128,	/* MinRX extension: enable BSD extensions \< and \> */
	MINRX_REG_EXTENSIONS_GNU = 256,	/* MinRX extension: enable GNU extensions \b \B \s \S \w \W */
	MINRX_REG_NATIVE1B = 512,	/* MinRX extension: use native encoding for 8-bit character sets (MB_CUR_LEN == 1) */
	MINRX_REG_MINDISABLE = 1024	/* MinRX extension: disable POSIX 2024 minimal repetitions */
} minrx_regcomp_flags_t;

typedef enum {				/* Flags for minrx_reg*exec() */
	MINRX_REG_NOTBOL = 1,		/* POSIX option: don't match ^ at beginning of string */
	MINRX_REG_NOTEOL = 2,		/* POSIX option: don't match $ at end of string */
	MINRX_REG_FIRSTSUB = 4,		/* MinRX extension: repeated subexpressions capture their first occurrence (rather than last) */
	MINRX_REG_NOSUBRESET = 8,	/* MinRX extension: repeated subexpressions don't clear their contained subexpressions */
	MINRX_REG_RESUME = 16,		/* MinRX extension: resume search from rm[0].rm_eo */
	MINRX_REG_NOFIRSTBYTES = 32	/* MinRX extension: disable rapid skip-ahead over impossible first bytes (probably only of interest to MinRX developers) */
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
