#include <bfd.h>
#include <stdio.h>

const char *text = ".text";

int main() {
  bfd_init();
  bfd *b = bfd_openw("lol", "elf32-x86-64");

  printf("%u", b->output_has_begun);
  bfd_set_format(b, bfd_object);
  printf("%u", b->output_has_begun);
  if (b != NULL) {
    asymbol *ptrs[2];
    asymbol *new;

    int data[3];
    data[0] = 42;
    data[1] = 1;
    data[2] = 2;
    printf("hi");
    asection *sec = bfd_make_section(b, text);
    if (!bfd_set_section_flags(sec, SEC_HAS_CONTENTS)) {
      printf("wtf");
    }
    printf("%u", b->output_has_begun);
    printf("sofarsogood");
    bfd_set_section_size(sec, 3 * sizeof(int));
    printf("%u", b->output_has_begun);

    printf("%u", b->output_has_begun);

    new = bfd_make_empty_symbol(b);
    new->name = "dummy_symbol";
    new->section = sec;
    new->flags = BSF_GLOBAL;
    new->value = 0x69;

    ptrs[0] = new;
    ptrs[1] = (asymbol *)0;
    printf("%u", b->output_has_begun);

    if (!bfd_set_symtab(b, ptrs, 1)) {
      printf("HMM");
    }
    if (!bfd_set_section_contents(b, sec, (void *)data, 0x0, 3 * sizeof(int))) {
      bfd_perror("fdsffsdf");
      return 0;
    }
    printf("%u", b->output_has_begun);
    if (!bfd_close(b)) {
      printf("%u", (unsigned int)bfd_get_error());
    }
  }
  return 0;
}
