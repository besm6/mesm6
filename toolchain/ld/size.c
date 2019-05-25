/*
 * Print section sizes and total size of object files.
 *
 * Copyright (c) 2019 Serge Vakulenko
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "stdobj.h"

int format = 'd';
const char *progname;

void size(char *fname)
{
    FILE *fd;
    obj_image_t obj = {0};
    static int header_printed;
    unsigned sum;

    fd = fopen(fname, "r");
    if (!fd) {
        fprintf(stderr, "%s: %s not found\n", progname, fname);
        return;
    }
    if (obj_read_header(fd, &obj) < 0) {
        fclose(fd);
        fprintf(stderr, "%s: %s not an object file\n", progname, fname);
        return;
    }
    fclose(fd);

    sum = obj.cmd_len + obj.const_len + obj.bss_len;
    if (!header_printed) {
        if (format == 'o')
            printf("   text    data     bss     oct     hex filename\n");
        else
            printf("   text    data     bss     dec     hex filename\n");
        header_printed = 1;
    }
    switch (format) {
    default:
        printf("%7d %7d %7d %7d %7x %s\n",
            obj.cmd_len, obj.const_len, obj.bss_len, sum, sum, fname);
        break;
    case 'x':
        printf("%#7x %#7x %#7x %#7x %7x %s\n",
            obj.cmd_len, obj.const_len, obj.bss_len, sum, sum, fname);
        break;
    case 'o':
        printf("%#7o %#7o %#7o %#7o %7x %s\n",
            obj.cmd_len, obj.const_len, obj.bss_len, sum, sum, fname);
        break;
    }
}

void usage(int retcode)
{
    printf("Show section sizes of BESM-6 object files\n");
    printf("Usage:\n");
    printf("    %s [options] file...\n", progname);
    printf("Options:\n");
    printf("    -d      Print sizes in decimal (default)\n");
    printf("    -o      Print sizes in octal\n");
    printf("    -x      Print sizes in hexadecimal\n");
    exit(retcode);
}

int main(int argc, char **argv)
{
    int yesarg = 0;

    // Get program name.
    progname = strrchr(argv[0], '/');
    if (progname)
        progname++;
    else
        progname = argv[0];

    if (argc == 1)
        usage(0);
    for (;;) {
        switch (getopt(argc, argv, "-dox")) {
        case EOF:
            break;
        case 1:
            size(optarg);
            yesarg = 1;
            continue;
        case 'd':
            format = 'd';
            continue;
        case 'o':
            format = 'o';
            continue;
        case 'x':
            format = 'x';
            continue;
        default:
            usage(-1);
        }
        break;
    }

    if (! yesarg)
        size("a.out");
    return 0;
}
