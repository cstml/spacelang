#include "term.h"
#include <stdio.h>

// Helper to print indentation
static void print_indent(int indent) {
  for (int i = 0; i < indent; i++) {
    printf("  ");
  }
}

// Helper to print location
static void print_location(const Location *loc) {
  if (loc->fileName) {
    printf("[%s:%u:%u] ", loc->fileName, loc->row, loc->column);
  } else {
    printf("[<unknown>:%u:%u] ", loc->row, loc->column);
  }
}

void term_print(const Term *term, int indent, int show_location) {
  if (!term) {
    print_indent(indent);
    printf("<NULL>\n");
    return;
  }

  print_indent(indent);
  
  // Print location if requested
  if (show_location) {
    print_location(&term->l);
  }
  
  switch (term->ty) {
    case NOOP:
      printf("NOOP\n");
      break;
      
    case KEYWORD:
      printf("KEYWORD: \"%.*s\"\n", term->kw.len, term->kw.s);
      break;
      
    case SYMBOL:
      printf("SYMBOL: \"%.*s\"\n", term->sy.len, term->sy.s);
      break;
      
    case INTEGER:
      printf("INTEGER: %lld\n", term->i.v);
      break;
      
    case DOUBLE:
      printf("DOUBLE: %Lf\n", term->d.v);
      break;
      
    case THUNK_CLOSE:
      printf("THUNK_CLOSE\n");
      break;
      
    case THUNK_OPEN:
      printf("THUNK_OPEN\n");
      break;
      
    case EVAL:
      printf("EVAL\n");
      break;
      
    case BIND:
      printf("BIND\n");
      break;
      
    case THUNK:
      printf("THUNK (%llu terms):\n", term->t.term_count);
      for (unsigned long long i = 0; i < term->t.term_count; i++) {
        term_print(&term->t.ts[i], indent + 1, show_location);
      }
      break;
      
    default:
      printf("UNKNOWN TYPE: %d\n", term->ty);
      break;
  }
}

void term_println(const Term *term) {
  term_print(term, 0, 0);  // Don't show location
}

void term_println_with_location(const Term *term) {
  term_print(term, 0, 1);  // Show location
}