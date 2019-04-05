
#include <stdio.h>
#include "mesm6/a.out.h"

void fputh(long h, FILE *f)
{
    putc((int) h, f);
    putc((int) (h >> 8), f);
    putc((int) (h >> 16), f);
    putc((int) (h >> 24), f);
}
