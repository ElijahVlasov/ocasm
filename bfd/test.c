#include <bfd.h>
#include <stdio.h>

void err_handler(const char *c, ...) {
  printf("LLLL\n");
  va_list argptr;
  va_start(argptr, c);
  printf(c, argptr);
  va_end(argptr);
}

const char *text = ".text";

int main() {
  bfd_init();
  bfd *b = bfd_openw("lol", "elf32-littleriscv");
  bfd_set_error_handler((bfd_error_handler_type)err_handler);
  bfd_set_format(b, bfd_object);
  if (b != NULL) {
    int data[3];
    data[0] = 0;
    data[1] = 1;
    data[2] = 2;
    printf("hi");
    asection *sec = bfd_make_section(b, text);
    if (!bfd_set_section_flags(sec, SEC_HAS_CONTENTS)) {
      printf("wtf");
    }
    printf("sofarsogood");
    bfd_set_section_size(sec, 3 * sizeof(int));
    if (!bfd_set_section_contents(b, sec, (void *)data, 0x0, 3 * sizeof(int))) {
      bfd_perror("fdsffsdf");
      return 0;
    }

    if (!bfd_close(b)) {
      printf("%u", (unsigned int)bfd_get_error());
    }
  }
  return 0;
}
