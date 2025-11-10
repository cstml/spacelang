#include "term.c"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

static char *cursor;

bool next() {
  char *next = cursor;
  next++;
  if (*next == '\0') {
    return false;
  }
  cursor++;
  return true;
}

bool reads(char c) {
  return (*cursor == c);
}

void err_unrecognised() {
  fprintf(stderr, "Unrecognised operator or operand \"%c\".\n", *cursor);
}

bool isZero() { return reads('0'); }

bool isNonZeroDigit() { return (*cursor >= '1' && *cursor <= '9'); }

bool isDigit() { return (isZero() || isNonZeroDigit()); }

bool isMinus() { return reads('-'); }

long long slurpNumber () {
  long long t = 0;
  while(isDigit()){
    t *= 10;
    t += *cursor - '0';
    cursor++;
  }
  return t;
}

/** Positive Number **/
Term* PNumber (){
  if (isNonZeroDigit()) {
    Term* response = malloc(sizeof(Term));
    long long t = slurpNumber();
    response->ty = INTEGER;
    response->i.v = t;
    return response;
  }
  return NULL;
}

Term* PTerm() {
  Term* t;

  t = PNumber();
  if (t != NULL) {
    return t;
  }

  err_unrecognised();
  return NULL;
}

int tokenise(Term *t, char *in) {
  int ix = 0;
  while (in[ix] != '\0') {

  }
  return 0;
}
