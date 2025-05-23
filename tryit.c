#include <locale.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "minrx.h"

int
main(int argc, char *argv[])
{
	int cflags = MINRX_REG_EXTENDED, eflags = 0;
	ptrdiff_t lasteo = -1;
	setlocale(LC_ALL, "");
	while (argc > 3) {
		if (strcmp(argv[1], "-B") == 0)
			cflags |= MINRX_REG_EXTENSIONS_BSD;
		else if (strcmp(argv[1], "-G") == 0)
			cflags |= MINRX_REG_EXTENSIONS_GNU;
		else if (strcmp(argv[1], "-a") == 0)
			eflags |= MINRX_REG_RESUME;
		else if (strcmp(argv[1], "-c") == 0)
			cflags |= MINRX_REG_BRACE_COMPAT;
		else if (strcmp(argv[1], "-e") == 0)
			cflags |= MINRX_REG_BRACK_ESCAPE;
		else if (strcmp(argv[1], "-f") == 0)
			eflags |= MINRX_REG_FIRSTSUB;
		else if (strcmp(argv[1], "-i") == 0)
			cflags |= MINRX_REG_ICASE;
		else if (strcmp(argv[1], "-n") == 0)
			cflags |= MINRX_REG_NEWLINE;
		else if (strcmp(argv[1], "-r") == 0)
			eflags |= MINRX_REG_NOSUBRESET;
		else if (strcmp(argv[1], "--mindisable") == 0)
			cflags |= MINRX_REG_MINDISABLE;
		else if (strcmp(argv[1], "--minglobal") == 0)
			cflags |= MINRX_REG_MINGLOBAL;
		else if (strcmp(argv[1], "--minscoped") == 0)
			cflags |= MINRX_REG_MINSCOPED;
		else if (strcmp(argv[1], "--rptminfast") == 0)
			cflags |= MINRX_REG_RPTMINFAST;
		else if (strcmp(argv[1], "--rptminslow") == 0)
			cflags |= MINRX_REG_RPTMINSLOW;
		else if (strcmp(argv[1], "--gawk") == 0)
			cflags |= MINRX_REG_EXTENSIONS_BSD | MINRX_REG_EXTENSIONS_GNU | MINRX_REG_BRACE_COMPAT | MINRX_REG_BRACK_ESCAPE;
		--argc, ++argv;
	}
	if (argc != 3) {
		fprintf(stderr, "usage: tryit (-B|-G|-a|-c|-e|-i|-n|--gawk)* <regexp> <string>\n");
		fprintf(stderr, "\t-B\tenable BSD extensions \\< \\>\n");
		fprintf(stderr, "\t-G\tenable GNU extensions \\` \\' \\b \\B \\s \\S \\w \\W\n");
		fprintf(stderr, "\t-a\tfind all matches\n");
		fprintf(stderr, "\t-c\tenable curly brace compatibility\n");
		fprintf(stderr, "\t-e\tenable backslash quoting in [...]\n");
		fprintf(stderr, "\t-f\tsubexpressions capture their first occurrence (rather than last)\n");
		fprintf(stderr, "\t-i\tignore case\n");
		fprintf(stderr, "\t-n\texclude newline from . and [^...] and treat as ^$ delimiter\n");
		fprintf(stderr, "\t--mindisable\n\t\tenable the MINRX_REG_MINDISABLE compilation flag\n");
		fprintf(stderr, "\t--minglobal\n\t\tenable the MINRX_REG_MINGLOBAL compilation flag\n");
		fprintf(stderr, "\t--minscoped\n\t\tenable the MINRX_REG_MINSCOPED compilation flag\n");
		fprintf(stderr, "\t--rptminfast\n\t\tenable the MINRX_REG_RPTMINFAST compilation flag\n");
		fprintf(stderr, "\t--rptminslow\n\t\tenable the MINRX_REG_RPTMINSLOW compilation flag\n");
		fprintf(stderr, "\t--gawk\n\t\tequivalent to -B -G -c -e\n");
		exit(EXIT_FAILURE);
	}
	minrx_regex_t rx;
	int err;
	if ((err = minrx_regcomp(&rx, argv[1], cflags)) != 0) {
		char errmsg[100];
		minrx_regerror(err, &rx, errmsg, sizeof errmsg);
		printf("%s\n", errmsg);
		exit(EXIT_FAILURE);
	}
	minrx_regmatch_t rm[1000];
	rm[0].rm_eo = 0;
	while (minrx_regexec(&rx, argv[2], 1000, rm, eflags) == 0) {
		if (rm[0].rm_eo != lasteo) {
			for (size_t i = 0; i <= rx.re_nsub; ++i)
				if (rm[i].rm_so != -1)
					printf("(%d,%d)", (int) rm[i].rm_so, (int) rm[i].rm_eo);
				else
					printf("(?,?)");
			putchar('\n');
		}
		lasteo = rm[0].rm_eo;
		if ((eflags & MINRX_REG_RESUME) == 0 || argv[2][rm[0].rm_eo] == '\0')
			break;
		if (rm[0].rm_so == rm[0].rm_eo)
			++rm[0].rm_eo;
	}
	minrx_regfree(&rx);
	exit(EXIT_SUCCESS);
}
