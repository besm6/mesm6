
#include <stdio.h>
#include "mesm6/a.out.h"

int fgetint(register FILE *f, register int *i)
{
    *i = fgeth(f);
    fgeth(f);
    return 1;
}
