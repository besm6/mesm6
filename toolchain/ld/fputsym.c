
#include <stdio.h>
#include "mesm6/a.out.h"

void fputsym(register struct nlist *s, register FILE *file)
{
    register int i;

    putc(s->n_len, file);
    putc(s->n_type, file);
    fputh(s->n_value, file);
    for (i=0; i<s->n_len; i++)
        putc(s->n_name[i], file);
}
