/*
 * Copyright 1994 Vrije Universiteit, The Netherlands.
 * For full copyright and restrictions on use see the file COPYRIGHT in the
 * top level of the Amoeba distribution.
 */

#if !defined(HAVE_OPT)

extern int	opterr;
extern int	optind;
extern int	optopt;
extern char	*optarg;

#ifndef EOF
#define EOF	(-1)
#endif

int getopt	(
#if __STDC__
int argc , char **argv , char *opts
#endif
);
#else /* HAVE_OPT */
#include <unistd.h>
#endif /* HAVE_OPT */

