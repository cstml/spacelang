#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "parser.h"
#include "term.h"
#include "memory.h"
#include "evaluator.h"

#define BUF_SIZE 2048


int main() {
  Memory *m = MkMemory();

  // Check if stdin is a pipe/redirect or a terminal
  int is_interactive = isatty(STDIN_FILENO);
  
  char line[BUF_SIZE+1];
  
  // Collect all terms
  Term *all_terms = NULL;
  unsigned long long total_count = 0;
  unsigned long long total_capacity = 0;
  
  // Read line by line
  while (fgets(line, BUF_SIZE, stdin) != NULL) {
    // Remove trailing newline
    size_t len = strlen(line);
    if (len > 0 && line[len-1] == '\n') {
      line[len-1] = '\0';
    }
    
    // Skip empty lines
    if (strlen(line) == 0) {
      continue;
    }
    
    // Parse this line
    Term *line_terms = NULL;
    unsigned long long line_count = 0;
    
    if (parse_all(line, &line_terms, &line_count) != 0) {
      fprintf(stderr, "Parse error on line: %s\n", line);
      continue;
    }
    
    // Grow all_terms array if needed
    if (total_count + line_count > total_capacity) {
      total_capacity = (total_capacity == 0) ? 16 : total_capacity;
      while (total_capacity < total_count + line_count) {
        total_capacity *= 2;
      }
      Term *new_terms = realloc(all_terms, total_capacity * sizeof(Term));
      if (!new_terms) {
        fprintf(stderr, "Memory allocation failed\n");
        free(all_terms);
        free(line_terms);
        return 1;
      }
      all_terms = new_terms;
    }
    
    // Add line terms to all_terms
    memcpy(&all_terms[total_count], line_terms, line_count * sizeof(Term));
    total_count += line_count;
    
    free(line_terms);
    
    // If interactive, print immediately after each line
    if (is_interactive && line_count > 0) {
      printf("Parsed %llu term(s):\n", line_count);
      for (unsigned long long i = total_count - line_count; i < total_count; i++) {
        printf("  ");
        term_println(&all_terms[i]);
        evaluate(m, &all_terms[i]);
      }
      printf("\n");
      memory_print(m);
    }
  }
  
  // If NOT interactive (piped), print everything at the end
  if (!is_interactive) {
    printf("╔════════════════════════════════════════╗\n");
    printf("║  Parsed %llu term(s) total\n", total_count);
    printf("╚════════════════════════════════════════╝\n\n");
    
    for (unsigned long long i = 0; i < total_count; i++) {
      printf("─── Term #%llu ───\n", i + 1);
      term_println(&all_terms[i]);
      printf("\n");
      evaluate(m, &all_terms[i]);
    }
    
    printf("─────────────────────────────────────────\n");
    printf("Total terms processed: %llu\n", total_count);
    memory_print(m);
  }

  
  // Cleanup
  free(all_terms);
  
  return 0;
}