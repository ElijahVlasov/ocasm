#include <bfd.h>
#include <stdio.h>

void err_handler(const char *c, ...) {
  printf("LLLL\n");
  va_list argptr;
  va_start(argptr, c);
  printf(c, argptr);
  va_end(argptr);
}

int main() {
  bfd_init();
  bfd *b = bfd_openw("lol", "elf32-littleriscv");
  bfd_set_error_handler((bfd_error_handler_type)err_handler);
  if (b != NULL) {
    printf("hi");
    if (!bfd_close_all_done(b)) {
      printf("%u", (unsigned int)bfd_get_error());
    }
  }
  return 0;
}
