/*
 * rxgrep.c --- A simple, almost POSIX, egrep that uses MinRX.
 */

/*
 * Copyright (C) 2024,
 * Arnold David Robbins
 *
 * rxgrep.c is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * rxgrep.c is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

#include <stdio.h>
#include <errno.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "minrx.h"

#define VERSION		"0.1"
#define COPYRIGHT_YEAR	2024

/* The name the program was invoked under, for error messages */
const char *myname;

/* Variables for options. They are int and not bool, for use in the option table */
int do_count = false;
int do_version = false;
int ignorecase = false;
int invert = false;
int list_files = false;
int print_errors = true;
int print_num = false;
int print_output = true;
int wholelines = false;
int word_match = false;
#define PRINT_HELP	1

char *pattern = NULL;		// command line pattern(s)
char *pat_file = NULL;		// file with patterns
minrx_regex_t *regexps = NULL;	// array of compiled regexps
size_t num_regexps;		// how many we actually have

int exit_val = EXIT_SUCCESS;

static void parse_args(int argc, char **argv);
static void usage(int exit_status);
static void version(void);

static char **get_patterns(const char *filename);
static char ** build_pattern_list(char *pattern_list);
static void build_regexps(char **pattern_list);


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

	build_regexps(pattern_list);

	if (argc - optind > 1)
		list_files = true;

	for (i = optind; i < argc; i++) {
		if (strcmp(argv[i], "-") == 0 || strcmp(argv[i], "/dev/stdin") == 0)
			process(stdin);
		else {
			FILE *fp = fopen(argv[i], "r");

			if (fp == NULL) {
				fprintf(stderr, "%s: %s: could not open: %s\n",
						myname, argv[i], strerror(errno));
				continue;
			}
			process(fp);
			fclose(fp);
		}
	}

	return exit_val;
#if 0
BEGINFILE {
    fcount = 0
    if (ERRNO && no_errors)
        nextfile
}
ENDFILE {
    if (! no_print && count_only) {
        if (list_files)
            print file ":" fcount
        else
            print fcount
    }

    total += fcount
}
{
    matches = match($0, pattern)
    if (matches && full_line && (RSTART != 1 || RLENGTH != length()))
         matches = 0

    if (invert)
        matches = ! matches

    fcount += matches    # 1 or 0

    if (! matches)
        next

    if (! count_only) {
        if (no_print)
            nextfile

        if (filenames_only) {
            print FILENAME
            nextfile
        }

        if (list_files)
            if (line_numbers)
               print FILENAME ":" FNR ":" $0
            else
               print FILENAME ":" $0
        else
            print
    }
}
END {
    exit (total == 0)
}
#endif
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
		{ "line-number",	no_argument,		&print_num,	'n' },
		{ "line-regexp",	no_argument,		& wholelines,	'x' },
		{ "no-messages",	no_argument,		NULL,		's' },
		{ "quiet",		no_argument,		NULL,		'q' },
		{ "regexp",		required_argument,	NULL,		'e' },
		{ "silent",		no_argument,		NULL,		'q' },
		{ "version",		no_argument,		& do_version,	'V' },
	/*	{ "word-regexp",	no_argument,		& word_match,	'w' }, */
		{ NULL, 0, NULL, '\0' }
	};

	myname = argv[0];

	/* we do error messages ourselves on invalid options */
	opterr = false;

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
			do_version = true;
			break;
		case PRINT_HELP:
			usage(EXIT_SUCCESS);
			break;
		case '?':
		default:
			usage(EXIT_FAILURE);
		}
	}

	if (do_version)
		version();	// exits

	if (pattern == NULL) {
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
			"\t-c, --count\tPrint count of lines\n"
			"\t-e <pat>, --regexp=<pat>\tUse <pat> as pattern\n"
			"\t-f <file>, --file=<file>\tRead patterns from file\n"
			"\t-l, --files-with-matches\tList matching file names\n"
			"\t-n, --line-number\tPrint line numbers of matching lines\n"
			"\t-q, --quiet, --silent\tDon't print lines, rely on exit status\n"
			"\t-s, --no-messages\tDon't print error messages\n"
			"\t-t-i, --ignore-case\tIgnore case in regexp and input\n"
			"\t-v, --invert-match\tPrint lines that don't match\n"
			"\t-w, --word-regexp\tPattern must match a whole word\n"
		/*	"\t-x, --line-regexp\tOnly print if the whole line matches\n" */
			"\t-V, --version\tPrint program version\n"
			"\t--help\tPrint this help\n",
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

	if (stat(filename, & sbuf) < 0) {
		fprintf(stderr, "%s: %s: could not stat: %s\n",
				myname, filename, strerror(errno));
		exit(EXIT_FAILURE);
	}

	if ((buffer = malloc(sbuf.st_size + 2)) == NULL) {
		fprintf(stderr, "%s: out of memory: %s\n",
				myname, strerror(errno));
		exit(EXIT_FAILURE);
	}

	if ((fd = open(filename, O_RDONLY)) < 0) {
		fprintf(stderr, "%s: %s: could not open: %s\n",
				myname, filename, strerror(errno));
		free(buffer);
		exit(EXIT_FAILURE);
	}

	if ((count = read(fd, buffer, sbuf.st_size)) != sbuf.st_size) {
		fprintf(stderr, "%s: %s: could not read: %s\n",
				myname, filename, strerror(errno));
		free(buffer);
		close(fd);
		exit(EXIT_FAILURE);
	}

	(void) close(fd);
	buffer[sbuf.st_size] = '\0';

	result = build_pattern_list(buffer);
	return result;
}
