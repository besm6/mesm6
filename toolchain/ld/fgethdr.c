
#include <stdio.h>
#include "microbesm/a.out.h"

int fgethdr(register FILE *text, register struct exec *h)
{
    h->a_magic = fgeth(text);  fgeth(text);
    h->a_const = fgeth(text);  fgeth(text);
    h->a_text  = fgeth(text);  fgeth(text);
    h->a_data  = fgeth(text);  fgeth(text);
    h->a_bss   = fgeth(text);  fgeth(text);
    h->a_abss  = fgeth(text);  fgeth(text);
    h->a_syms  = fgeth(text);  fgeth(text);
    h->a_entry = fgeth(text);  fgeth(text);
    h->a_flag  = fgeth(text);  fgeth(text);
    return 1;
}
