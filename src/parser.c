#include "term.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *cursor;
static char *source_filename = "<stdin>";

bool next() {
  if (*cursor == '\0') {
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
bool isSpace() {
  return (reads(' ') || reads('\n') || reads('\t'));
}

bool PSpace() {
  if (isSpace()) {
    while (isSpace()) {
      next();
    }
    return true;
  }
  return false;
}

bool PEnd() {
  return reads('\0');
}

long long slurpNumber() {
  long long t = 0;
  while (isDigit()) {
    t *= 10;
    t += *cursor - '0';
    cursor++;
  }
  return t;
}

Symbol* slurpSymbol() {
  Symbol *response = malloc(sizeof(Symbol));
  response->s = malloc(2024 * sizeof(char));
  char *b = response->s;
  response->len = 0;
  while (isalnum(*cursor)) {
    *b = *cursor;
    response->len += 1;
    b++;
    next();
  }
  return response;
}

Term* PSymbol() {
  if (isalpha(*cursor)) {
    Term *response = malloc(sizeof(Term));
    response->ty = SYMBOL;
    Symbol *symbol = slurpSymbol();
    response->sy = *symbol;
    free(symbol);
    
    // Set location (you can enhance this with actual tracking)
    response->l.fileName = source_filename;
    response->l.row = 0;  // TODO: track actual row
    response->l.column = 0;  // TODO: track actual column
    
    return response;
  }
  return NULL;
}

/** Positive Number **/
Term* PNumber() {
  if (isNonZeroDigit()) {
    Term* response = malloc(sizeof(Term));
    long long t = slurpNumber();
    response->ty = INTEGER;
    response->i.v = t;
    
    // Set location
    response->l.fileName = source_filename;
    response->l.row = 0;
    response->l.column = 0;
    
    return response;
  }
  return NULL;
}

Term* PThunkOpen() {
  if (reads('[')) {
    next();
    Term* response = malloc(sizeof(Term));
    response->ty = THUNK_OPEN;
    
    // Set location
    response->l.fileName = source_filename;
    response->l.row = 0;
    response->l.column = 0;
    
    return response;
  }
  return NULL;
}

Term* PThunkClose() {
  if (reads(']')) {
    next();
    Term* response = malloc(sizeof(Term));
    response->ty = THUNK_CLOSE;  // Fixed: was THUNK_OPEN
    
    // Set location
    response->l.fileName = source_filename;
    response->l.row = 0;
    response->l.column = 0;
    
    return response;
  }
  return NULL;
}

Term* PTerm() {
  Term* t;
  t = PNumber();
  if (t != NULL) { return t; }
  t = PThunkOpen();
  if (t != NULL) { return t; }
  t = PThunkClose();
  if (t != NULL) { return t; }
  t = PSymbol();
  if (t != NULL) { return t; }
  err_unrecognised();
  return NULL;
}

// New function: Parse and return all terms
int parse_all(char *in, Term **out_terms, unsigned long long *out_count) {
  cursor = in;
  
  // Initial capacity for terms array
  unsigned long long capacity = 16;
  unsigned long long count = 0;
  Term *terms = malloc(capacity * sizeof(Term));
  
  if (!terms) {
    fprintf(stderr, "Memory allocation failed\n");
    return -1;
  }
  
  // Optional: skip leading whitespace
  PSpace();
  
  while (!PEnd()) {
    Term *t = PTerm();
    
    if (t == NULL) {
      // Cleanup on error
      free(terms);
      return -1;
    }
    
    // Grow array if needed
    if (count >= capacity) {
      capacity *= 2;
      Term *new_terms = realloc(terms, capacity * sizeof(Term));
      if (!new_terms) {
        fprintf(stderr, "Memory reallocation failed\n");
        free(terms);
        return -1;
      }
      terms = new_terms;
    }
    
    // Copy the term (not just pointer)
    terms[count] = *t;
    count++;
    free(t);  // Free the temporary term struct
    
    // Optional whitespace between terms
    PSpace();
  }
  
  *out_terms = terms;
  *out_count = count;
  return 0;
}

// Original tokenise function (kept for backward compatibility)
int tokenise(char *in) {
  cursor = in;
  while (!PEnd()) {
    Term *t = PTerm();
    if (t == NULL) {
      return -1;
    }
    printf("t: %i\n", t->ty);
    free(t);
    
    PSpace();  // Made optional - removed the error check
  }
  return 0;
}