/*
 * rxgrep.c --- A simple, almost POSIX, egrep that uses MinRX.
 */

/*
 * Copyright (C) 2024,
 * Arnold David Robbins
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS “AS IS” AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <stdio.h>
#include <errno.h>
#include <getopt.h>
#include <regex.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "minrx.h"

#define VERSION		"0.2"
#define COPYRIGHT_YEAR	2024

/* The name the program was invoked under, for error messages */
const char *myname;

/* Variables for options. They are int and not bool, for use in the option table */
int do_count = false;
int ignorecase = false;
int invert = false;
int list_files = false;
int print_errors = true;
int print_num = false;
int print_output = true;
int wholelines = false;
int word_match = false;
#define PRINT_HELP	1
#define USE_LIBC_REGEX	2

char *pattern = NULL;			// command line pattern(s)
char *pat_file = NULL;			// file with patterns
minrx_regex_t *minrx_regexps = NULL;	// array of compiled regexps
regex_t *regexps = NULL;		// array of compiled regexps
size_t num_regexps;		// how many we actually have
int minrx_syntax_flags;		// how to do the matching
int syntax_flags;		// how to do the matching

bool do_filenames = false;	// include filename in output
bool use_minrx = true;		// false if USE_LIBC_REGEX
size_t total;

static void parse_args(int argc, char **argv);
static void usage(int exit_status);
static void version(void);

static char **get_patterns(const char *filename);
static char **build_pattern_list(char *pattern_list);
static void build_regexps(char **pattern_list);
static void build_libc_regexps(char **pattern_list);
static void process(const char *filename, FILE *fp);
static bool minrx_matches(int i, char *buf, ptrdiff_t len);
static bool libc_matches(int i, char *buf, ptrdiff_t len);
static bool (*matchfunc)(int i, char *buf, ptrdiff_t len);
static void set_syntax_flags(void);


/* main --- process args, build regexps, run input */

int
main(int argc, char **argv)
{
	char **pattern_list = NULL;
	int i;

	parse_args(argc, argv);
	set_syntax_flags();

	if (pat_file != NULL) {
		pattern_list = get_patterns(pat_file);
	} else if (pattern == NULL) {
		pattern = argv[optind++];

		if (pattern[0] == '\0')
			usage(EXIT_FAILURE);

	} else
		pattern_list = build_pattern_list(pattern);

	if (use_minrx) {
		build_regexps(pattern_list);
		matchfunc = minrx_matches;
	} else {
		build_libc_regexps(pattern_list);
		matchfunc = libc_matches;
	}

	if (argc - optind > 1)
		do_filenames = true;

	if (optind >= argc)
		process("(standard input)", stdin);
	else {
		for (i = optind; i < argc; i++) {
			if (strcmp(argv[i], "-") == 0)
				process("(standard input)", stdin);
			else {
				FILE *fp = fopen(argv[i], "r");

				if (fp == NULL) {
					if (print_errors)
						fprintf(stderr, "%s: %s: could not open: %s\n",
							myname, argv[i], strerror(errno));
					continue;
				}
				process(argv[i], fp);
				fclose(fp);
			}
		}
	}

	return total != 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}

/* set_syntax flags --- choose syntax bits */

static void
set_syntax_flags(void)
{
	if (use_minrx) {
		minrx_syntax_flags = MINRX_REG_EXTENDED	// default is EREs
			| MINRX_REG_NOSUB		// don't need submatch info
			| MINRX_REG_NEWLINE		// \n isn't matched
			| MINRX_REG_BRACE_COMPAT	// { not followed by digit isn't special
			| MINRX_REG_EXTENSIONS_BSD	// \< and \>
			| MINRX_REG_EXTENSIONS_GNU	// \b \B \s \S \w \W
			| MINRX_REG_NATIVE1B;		// All bytes are valid in a single byte locale

		if (ignorecase)
			minrx_syntax_flags |= MINRX_REG_ICASE;	// Ignore case (duh)
	} else {
		syntax_flags = REG_EXTENDED
			| REG_NOSUB
			| REG_NEWLINE;
		if (ignorecase)
			syntax_flags |= REG_ICASE;
	}
}

/* minrx_matches --- return true if regexp[i] matches buf */

static bool
minrx_matches(int i, char *buf, ptrdiff_t len)
{
	bool matches = false;
	minrx_regmatch_t rm;

	rm.rm_so = rm.rm_eo = 0;

	if (minrx_regexec(& minrx_regexps[i], buf, 1, & rm, 0) == 0) {
		matches = true;
		if (matches && wholelines && (rm.rm_so != 0 || rm.rm_eo != len))
			matches = false;
	}

	return matches;
}

/* libc_matches --- return true if regexp[i] matches buf */

static bool
libc_matches(int i, char *buf, ptrdiff_t len)
{
	bool matches = false;
	regmatch_t rm;

	rm.rm_so = rm.rm_eo = 0;

	if (regexec(& regexps[i], buf, 1, & rm, 0) == 0) {
		matches = true;
		if (matches && wholelines && (rm.rm_so != 0 || rm.rm_eo != len))
			matches = false;
	}

	return matches;
}

/* process --- look for matches */

static void
process(const char *filename, FILE *fp)
{
	static char *buf = NULL;
	static size_t n = 0;
	int fcount = 0;
	int nbytes;
	int line_number;

	for (line_number = 1; (nbytes = getline(& buf, & n, fp)) != EOF; line_number++) {
		bool matches = false;
		ptrdiff_t len = strlen(buf);

		if (buf[len-1] == '\n')
			len--;

		for (size_t i = 0; i < num_regexps; i++) {
			matches = matchfunc(i, buf, len);

			if (invert)
			        matches = ! matches;

			fcount += !! matches;	// 1 or 0

			if (! matches)
				continue;

			// at this point we have a match
			if (! do_count) {
				if (! print_output)
					goto out;

				if (list_files) {
					printf("%s\n", filename);
					goto out;
				}

				if (do_filenames) {
					if (print_num)
						printf("%s:%d:%s", filename, line_number, buf);
					else
						printf("%s:%s", filename, buf);
				} else if (print_num)
					printf("%d:%s", line_number, buf);
				else
					printf("%s", buf);

				if (buf[len] != '\n')	// in case last line doesn't have newline
					putchar('\n');
			}
			break;	// after any match, get the next line
		}
	}

	if (print_output && do_count) {
		if (list_files)
			printf("%s:%d\n", filename, fcount);
		else
			printf("%d\n", fcount);
	}

out:
	total += fcount;
}

/* parse_args --- do the getopt_long thing */

static void
parse_args(int argc, char **argv)
{
	/*
	 * The + on the front tells GNU getopt not to rearrange argv.
	 */
	const char *optlist = "+ce:f:ilnqsvxV";	// Add 'w' for word match
	int c;
	/*
	 * Sorted by long option name!
	 * Long options are the same as GNU grep's.
	 */
	static const struct option optab[] = {
		{ "count",		no_argument,		& do_count,	'c' },
		{ "file",		required_argument,	NULL,		'f' },
		{ "files-with-matches",	no_argument,		& list_files,	'l' },
		{ "help",		no_argument,		NULL,	PRINT_HELP },
		{ "ignore-case",	no_argument,		& ignorecase,	'i' },
		{ "invert-match",	no_argument,		& invert,	'v' },
		{ "libc-regex",		no_argument,		NULL,	USE_LIBC_REGEX },
		{ "line-number",	no_argument,		&print_num,	'n' },
		{ "line-regexp",	no_argument,		& wholelines,	'x' },
		{ "no-messages",	no_argument,		NULL,		's' },
		{ "quiet",		no_argument,		NULL,		'q' },
		{ "regexp",		required_argument,	NULL,		'e' },
		{ "silent",		no_argument,		NULL,		'q' },
		{ "version",		no_argument,		NULL,		'V' },
	/*	{ "word-regexp",	no_argument,		& word_match,	'w' }, */
		{ NULL, 0, NULL, '\0' }
	};

	myname = strrchr(argv[0], '/');
	if (myname == NULL)
		myname = argv[0];
	else
		myname++;

	opterr = 1;	// let getopt report errors
	for (optopt = 0;
	     (c = getopt_long(argc, argv, optlist, optab, NULL)) != EOF;) {
		switch (c) {
		case 'c':
			do_count = true;
			break;
		case 'e':
			pattern = optarg;
			break;
		case 'f':
			pat_file = optarg;
			break;
		case 'i':
			ignorecase = true;
			break;
		case 'l':
			list_files = true;
			break;
		case 'n':
			print_num = true;
			break;
		case 'q':
			print_output = false;
			break;
		case 's':
			print_errors = false;
			break;
		case 'v':
			invert = true;
			break;
		case 'w':
			word_match = true;
			break;
		case 'x':
			wholelines = true;
			break;
		case 'V':
			version();	// exits
			break;
		case USE_LIBC_REGEX:
			use_minrx = false;
			break;
		case PRINT_HELP:
			usage(EXIT_SUCCESS);
			break;
		case '?':
		default:
			usage(EXIT_FAILURE);
		}
	}

	if (pattern == NULL && pat_file == NULL) {
		if (optind < argc) {
			pattern = argv[optind];
			optind++;
		} else
			usage(EXIT_FAILURE);
	}
}

/* usage --- print the usage and exit */

static void
usage(int exit_status)
{
	fprintf(stderr, "Usage: %s [options] [pattern | -f file] [file ...]\n"
			"\t-c, --count\t\t\tPrint count of lines\n"
			"\t-e <pat>, --regexp=<pat>\tUse <pat> as pattern\n"
			"\t-f <file>, --file=<file>\tRead patterns from file\n"
			"\t-i, --ignore-case\t\tIgnore case in regexp and input\n"
			"\t-l, --files-with-matches\tList matching file names\n"
			"\t-n, --line-number\t\tPrint line numbers of matching lines\n"
			"\t-q, --quiet, --silent\t\tDon't print lines, rely on exit status\n"
			"\t-s, --no-messages\t\tDon't print error messages\n"
			"\t-v, --invert-match\t\tPrint lines that don't match\n"
		/*	"\t-w, --word-regexp\t\tPattern must match a whole word\n" */
			"\t-x, --line-regexp\t\tOnly print if the whole line matches\n"
			"\t-V, --version\t\t\tPrint program version\n"
			"\t--libc-regex\t\t\tUse <regex.h> functions from libc\n"
			"\t--help\t\t\t\tPrint this help\n",
		myname);
	exit(exit_status);
}

/* version --- print version information */

static void
version(void)
{
	fprintf(stderr, "rxgrep version %s.\n", VERSION);
	fprintf(stderr, "Copyright (C) 2024-%d, Arnold David Robbins\n", COPYRIGHT_YEAR);
	fprintf(stderr, "This program is Free Software, published under the GNU GPL Version 3.\n");

	exit(EXIT_SUCCESS);
}

/* get_patterns --- get a list of patterns from file */

static char **
get_patterns(const char *filename)
{
	struct stat sbuf;
	char *buffer;
	int fd;
	ssize_t count;
	char **result;

	if (strcmp(filename, "-") == 0) {
		fd = fileno(stdin);
		filename = "(standard input)";
	} else if ((fd = open(filename, O_RDONLY)) < 0) {
		if (print_errors)
			fprintf(stderr, "%s: %s: could not open: %s\n",
					myname, filename, strerror(errno));
		exit(EXIT_FAILURE);
	}

	if (fstat(fd, & sbuf) < 0) {
		if (print_errors)
			fprintf(stderr, "%s: %s: could not stat: %s\n",
					myname, filename, strerror(errno));
		if (fd != fileno(stdin))
			close(fd);
		exit(EXIT_FAILURE);
	}

	if ((sbuf.st_mode & S_IFMT) != S_IFREG) {
		if (print_errors)
			fprintf(stderr, "%s: %s: argument to -f must be a regular file\n",
					myname, filename);
		if (fd != fileno(stdin))
			close(fd);
		exit(EXIT_FAILURE);
	}

	if ((buffer = (char *) malloc(sbuf.st_size + 2)) == NULL) {
		if (print_errors)
			fprintf(stderr, "%s: out of memory: %s\n",
					myname, strerror(errno));
		if (fd != fileno(stdin))
			close(fd);
		exit(EXIT_FAILURE);
	}

	if ((count = read(fd, buffer, sbuf.st_size)) != sbuf.st_size) {
		if (print_errors)
			fprintf(stderr, "%s: %s: could not read: %s\n",
					myname, filename, strerror(errno));
		free(buffer);
		if (fd != fileno(stdin))
			close(fd);
		exit(EXIT_FAILURE);
	}

	if (fd != fileno(stdin))
		close(fd);
	buffer[sbuf.st_size] = '\0';

	result = build_pattern_list(buffer);
	return result;
}

/* build_pattern_list --- convert a long newline separated string into an array of patterns */

static char **
build_pattern_list(char *pattern_list)
{
	char **list;
	int allocated;
	int count = 0;
	char *cp, *end;

	allocated = 10;	// start small
	list = (char **) malloc(sizeof(char *) * allocated);
	if (list == NULL) {
		if (print_errors)
			fprintf(stderr, "%s: out of memory: %s\n", myname, strerror(errno));
		exit(EXIT_FAILURE);
	}

	for (cp = pattern_list; *cp != '\0';) {
		if (count >= allocated) {
			// realloc
			int new_amount = allocated * 2;
			list = (char **) realloc((void *) list, new_amount * sizeof(char *));
			if (list == NULL) {
				if (print_errors)
					fprintf(stderr, "%s: out of memory: %s\n", myname, strerror(errno));
				exit(EXIT_FAILURE);
			}
			allocated = new_amount;
		}
		list[count++] = cp;
		end = strchr(cp, '\n');
		if (end != NULL) {
			*end = '\0';
			cp = end + 1;
		} else
			break;
	}
	num_regexps = count;

	return list;
}

/* build_regexps --- compile the patterns into regexp objects */

static void
build_regexps(char **pattern_list)
{
	size_t i;

	minrx_regexps = (minrx_regex_t *) malloc(num_regexps * sizeof(minrx_regex_t));
	if (minrx_regexps == NULL) {
		if (print_errors)
			fprintf(stderr, "%s: out of memory: %s\n", myname, strerror(errno));
		exit(EXIT_FAILURE);
	}

	for (i = 0; i < num_regexps; i++) {
		int error = minrx_regcomp(& minrx_regexps[i], pattern_list[i], minrx_syntax_flags);
		if (error != MINRX_REG_SUCCESS) {
			if (print_errors) {
				char errbuf[BUFSIZ];

				minrx_regerror(error, & minrx_regexps[i], errbuf, sizeof(errbuf));
				fprintf(stderr, "%s:pattern %s: %s\n", myname, pattern_list[i], errbuf);
			}

			exit(EXIT_FAILURE);
		}
	}
}

/* build_libc_regexps --- compile the patterns into libc regexp objects */

static void
build_libc_regexps(char **pattern_list)
{
	size_t i;

	regexps = (regex_t *) malloc(num_regexps * sizeof(regex_t));
	if (regexps == NULL) {
		if (print_errors)
			fprintf(stderr, "%s: out of memory: %s\n", myname, strerror(errno));
		exit(EXIT_FAILURE);
	}

	for (i = 0; i < num_regexps; i++) {
		int error = regcomp(& regexps[i], pattern_list[i], syntax_flags);
		if (error != 0) {
			if (print_errors) {
				char errbuf[BUFSIZ];

				regerror(error, & regexps[i], errbuf, sizeof(errbuf));
				fprintf(stderr, "%s:pattern %s: %s\n", myname, pattern_list[i], errbuf);
			}

			exit(EXIT_FAILURE);
		}
	}
}
