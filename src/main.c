#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#define BUF_SIZE 2048

int main() {
  char in[BUF_SIZE+1];
  struct stat s;
  fstat(STDIN_FILENO,&s);
  while (read(STDIN_FILENO, &in, BUF_SIZE-1) != 0){
    printf("%s\n",in);
  }
  return 0;
}
