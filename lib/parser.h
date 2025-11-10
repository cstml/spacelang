#ifndef PARSER_H_SEEN
#define PARSER_H_SEEN

#include "term.h"

// Original function
int tokenise(char *in);

// New function: parses all terms and returns them
int parse_all(char *in, Term **out_terms, unsigned long long *out_count);

#endif // PARSER_H_SEEN