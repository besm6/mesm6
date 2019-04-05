
#include <stdio.h>
#include "mesm6/a.out.h"

long fgeth(register FILE *f)
{
    register long h;

    h = getc(f);
    h |= getc(f) << 8;
    h |= (long) getc(f) << 16;
    return h | (long) getc(f) << 24;
}
