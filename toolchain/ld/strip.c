/*
 * Discard symbols from object files.
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
#include <signal.h>
#include <unistd.h>
#include <getopt.h>
#include "stdobj.h"

#define WORDSZ 6

const char *progname;
int status = 0;

void strip(char *fname)
{
    FILE *fd;
    obj_image_t obj = {0};
    unsigned size;

    fd = fopen(fname, "r+");
    if (!fd) {
        printf("%s: %s not found\n", progname, fname);
        status = 1;
        return;
    }
    if (obj_read_header(fd, &obj) < 0) {
        fclose(fd);
        printf("%s: %s not an object file\n", progname, fname);
        status = 1;
        return;
    }
    if (!obj.sym_len && !obj.long_len) {
        fclose(fd);
        printf("%s already stripped\n", fname);
        return;
    }
    if (!obj.base_addr || !obj.entry || obj.cmd_off != 11) {
        fclose(fd);
        printf("%s: cannot strip relocatable file %s\n", progname, fname);
        return;
    }

    size = (obj.table_off + 1) * WORDSZ;
    obj.sym_len = 0;
    obj.long_len = 0;

    if (obj_write_header(fd, &obj) < 0) {
        fclose(fd);
        printf("%s: cannot rewrite header\n", fname);
        status = 1;
        return;
    }
    fclose(fd);

    if (truncate(fname, size) < 0) {
        printf("%s: cannot truncate\n", fname);
        status = 1;
        return;
    }
}

void usage(int retcode)
{
    printf("Strip symbols from BESM-6 object files\n");
    printf("Usage:\n");
    printf("    %s file...\n", progname);
    exit(retcode);
}

int main(int argc, char **argv)
{
    // Get program name.
    progname = strrchr(argv[0], '/');
    if (progname)
        progname++;
    else
        progname = argv[0];

    if (argc == 1)
        usage(0);

    signal(SIGHUP, SIG_IGN);
    signal(SIGINT, SIG_IGN);
    signal(SIGQUIT, SIG_IGN);
    for (;;) {
        switch (getopt(argc, argv, "-")) {
        case EOF:
            break;
        case 1:
            strip(optarg);
            continue;
        default:
            usage(-1);
        }
        break;
    }
    return status;
}
