/*
 * Print symbol tables of BESM-6 object files.
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
#include <archive.h>
#include <archive_entry.h>
#include "stdobj.h"

int numsort_flg;
int undef_flg;
int revsort_flg;
int globl_flg;
int nosort_flg;
int prep_flg;
const char *progname;
const char *ar_name;
obj_image_t obj;

typedef struct {
    int n_type;
    int n_value;
    char *n_name;
} symtab_t;

symtab_t *symp;                 // symbol table
int symindex;                   // next free table entry
int symplen;                    // table length

//
// Compare two symbols alphabetically.
//
int compare(const void *p1, const void *p2)
{
    const symtab_t *a = p1;
    const symtab_t *b = p2;
    int rez;

    if (numsort_flg) {
        int d = a->n_value - b->n_value;

        if (d > 0)
            rez = 1;
        else if (d == 0)
            rez = 0;
        else
            rez = -1;
    } else {
        rez = strcmp(a->n_name, b->n_name);
    }
    if (revsort_flg)
        rez = -rez;
    return rez;
}

//
// Convert a name in TEXT format to utf8 string.
//
const char *text_to_utf(uint64_t word)
{
    static const char *text[64] = {
        " ", ".", "Б", "Ц", "Д", "Ф", "Г", "И",
        "(", ")", "*", "Й", "Л", "Я", "Ж", "/",
        "0", "1", "2", "3", "4", "5", "6", "7",
        "8", "9", "Ь", ",", "П", "-", "+", "Ы",
        "З", "A", "B", "C", "D", "E", "F", "G",
        "H", "I", "J", "K", "L", "M", "N", "O",
        "P", "Q", "R", "S", "T", "U", "V", "W",
        "X", "Y", "Z", "Ш", "Э", "Щ", "Ч", "Ю",
    };
    static char buf[64];
    int i, c;

    buf[0] = 0;
    for (i = 42; i >= 0; i -= 6) {
        c = (word >> i) & 077;
        if (c == 0)
            break;
        strcat(buf, text[c]);
    }
    return buf;
}

//
// Does the address belong to code segment?
//
int in_text(unsigned addr)
{
    return (addr >= obj.base_addr) &&
           (addr < obj.base_addr + obj.cmd_len);
}

//
// Does the address belong to data segment?
//
int in_data(unsigned addr)
{
    return (addr >= obj.base_addr + obj.cmd_len) &&
           (addr < obj.base_addr + obj.cmd_len + obj.const_len);
}

//
// Does the address belong to BSS segment?
//
int in_bss(unsigned addr)
{
    return (addr >= obj.base_addr + obj.cmd_len + obj.const_len) &&
           (addr < obj.base_addr + obj.cmd_len + obj.const_len + obj.bss_len);
}

//
// Does the symbol belong to transient data segment?
//
int in_tdata(nlist_t *sp)
{
    return (sp->f.n_ref == 00200000) &&
           (sp->f.n_addr >= obj.base_addr + obj.cmd_len + obj.const_len) &&
           (sp->f.n_addr < obj.base_addr + obj.cmd_len + obj.const_len + obj.data_len);
}

void add_symbol(const char *fname, uint64_t tname, int type, unsigned value)
{
    symtab_t sym;

    sym.n_type = type;
    sym.n_value = value;
    sym.n_name = strdup(text_to_utf(tname));
    if (!sym.n_name) {
        fprintf(stderr, "%s: out of memory in %s\n", progname, fname);
        exit(2);
    }
    if (symindex == symplen) {
        symplen += 2048;
        if (!symp) {
            symp = (symtab_t*) malloc(symplen * sizeof(symtab_t));
        } else {
            symp = (symtab_t*) realloc(symp, symplen * sizeof(symtab_t));
        }
        if (!symp) {
            fprintf(stderr, "%s: out of memory in %s\n", progname, fname);
            exit(2);
        }
    }
    symp[symindex++] = sym;
}

//
// Print symbols from the object file.
//
void nm(const char *fname, int narg)
{
    nlist_t *sp;                        // current symbol
    uint64_t tname;                     // symbol name in TEXT encoding
    int i, type;

    if (obj.sym_len == 0 && obj.nentries == 0) {
        fprintf(stderr, "%s: %s: ", progname, fname);
        if (ar_name)
            fprintf(stderr, "%s: ", ar_name);
        fprintf(stderr, "no symbol table\n");
        return;
    }

    // Build symbol table.
    symp = 0;
    symplen = 0;
    symindex = 0;
    for (i = 0; i < obj.sym_len; i++) {
        sp = (nlist_t*) &obj.word[i + 1 + obj.table_off];

        switch (sp->f.n_type) {
        default:
            continue;

        case SYM_EXT_S:
        case SYM_EXT_L:
            // External reference.
            type = 'U';
            break;

        case SYM_ENTRY_S:
        case SYM_ENTRY_L:
            // Entry, relocatable.
            if (undef_flg)
                continue;
            type = in_text(sp->f.n_addr) ? 'T' :
                   in_data(sp->f.n_addr) ? 'D' : 'B';
                   in_bss(sp->f.n_addr)  ? 'B' : '?';
            break;

        case SYM_PRIVATE_S:
        case SYM_PRIVATE_L:
            // Private block.
            if (undef_flg || globl_flg)
                continue;
            type = 'c';
            break;

        case SYM_COMMON_S:
        case SYM_COMMON_L:
            // Common block.
            if (undef_flg)
                continue;
            type = 'C';
            break;

        case SYM_PPAGE_S:
        case SYM_PPAGE_L:
            // Private block, page aligned.
            if (undef_flg || globl_flg)
                continue;
            type = 'p';
            break;

        case SYM_CPAGE_S:
        case SYM_CPAGE_L:
            // Common block, page aligned.
            if (undef_flg)
                continue;
            type = 'P';
            break;

        case SYM_PSECT_S:
        case SYM_PSECT_L:
            // Private block, sector aligned.
            if (undef_flg || globl_flg)
                continue;
            type = 's';
            break;

        case SYM_CSECT_S:
            // Common block, sector aligned.
            if (undef_flg)
                continue;
            type = 'S';
            break;
        }
        tname = (sp->f.n_type & SYM_LONG_NAME) ?
            obj.word[(sp->f.n_ref & 03777) + obj.table_off] :
            sp->u64 & 07777777700000000;
        add_symbol(fname, tname, type, sp->f.n_addr);
    }

    // Add entries.
    for (i = 0; i < obj.nentries; i++) {
        uint64_t addr = obj.word[2*i + 2] & 077777;

        tname = obj.word[2*i + 1];
        add_symbol(fname, tname, 'T', addr);
    }

    // Add local symbols.
    if (!undef_flg && !globl_flg) {
        for (i = 0; i < obj.debug_len; i += 2) {
            sp = (nlist_t*) &obj.word[i + obj.debug_off];
            if (sp->u64 == 0x3cf3cf3cf3cf)
                break;

            tname = sp->u64;
            sp++;
            switch (sp->f.n_type) {
            default:
                continue;

            case 0010:
                // Local label: Madlen, Fortran-Dubna.
                type = in_text(sp->f.n_addr) ? 't' :
                       in_data(sp->f.n_addr) ? 'd' :
                       in_tdata(sp) ?          'i' : // init (set) section
                       in_bss(sp->f.n_addr)  ? 'b' : '?';
                break;

            case 0000:
                // Local label: Fortran-GDR.
                if (sp->f.n_ref != 01000000)
                    continue;
                type = in_text(sp->f.n_addr) ? 't' :
                       in_data(sp->f.n_addr) ? 'd' :
                       in_bss(sp->f.n_addr)  ? 'b' : '?';
                break;

            case 0001:
                // Expression.
                type = 'e';
                break;
            }
            add_symbol(fname, tname, type, sp->f.n_addr);
        }
    }

    // Sorting.
    if (! nosort_flg)
        qsort(symp, symindex, sizeof(symtab_t), compare);

    // Print the result.
    if ((ar_name || narg>1) && !prep_flg) {
        if (ar_name)
            printf("\n%s:\n", ar_name);
        else
            printf("\n%s:\n", fname);
    }
    for (i=0; i<symindex; i++) {
        if (prep_flg) {
            printf("%s:\t", fname);
            if (ar_name)
                printf("%s:\t", ar_name);
        }
        if (! undef_flg) {
            if (symp[i].n_type == 'e' || symp[i].n_type == 'U') {
                // Undefined symbols and expressions have no value
                // until finally linked.
                printf("     ");
            } else {
                printf("%05o", symp[i].n_value);
            }
            printf(" %c ", symp[i].n_type);
        }
        printf("%s\n", symp[i].n_name);
    }

    // Deallocate the symbol table.
    while (symindex > 0)
        free(symp[--symindex].n_name);
    if (symplen)
        free(symp);
}

//
// Read a 48-bit word at the current file position.
//
uint64_t fread6(FILE *fd)
{
    uint64_t val = 0;
    int i;

    for (i = 0; i < 6; ++i) {
        val <<= 8;
        val |= getc(fd);
    }
    return val;
}

//
// Read callback for archive.
//
ssize_t myread(struct archive *a, void *fd, const void **pbuf)
{
    static char buf[4096];

    *pbuf = buf;
    return fread(buf, 1, sizeof(buf), (FILE*)fd);
}

void usage(int retcode)
{
    printf("Show section sizes of BESM-6 object files\n");
    printf("Usage:\n");
    printf("    %s [options] file...\n", progname);
    printf("Options:\n");
    printf("    -g      Display only external symbols\n");
    printf("    -n      Sort symbols numerically by address\n");
    printf("    -o      Print name of the input file before every symbol\n");
    printf("    -p      Do not sort the symbols\n");
    printf("    -r      Reverse the sense of the sort\n");
    printf("    -u      Display only undefined symbols\n");
    exit(retcode);
}

int main(int argc, char **argv)
{
    int narg;

    // Get program name.
    progname = strrchr(argv[0], '/');
    if (progname)
        progname++;
    else
        progname = argv[0];

    if (argc == 1)
        usage(0);
    for (;;) {
        switch (getopt(argc, argv, "gnopru")) {
        case EOF:
            break;
        case 'g':       /* globl symbols only */
            globl_flg++;
            continue;
        case 'n':       /* sort numerically */
            numsort_flg++;
            continue;
        case 'o':       /* prepend a name to each line */
            prep_flg++;
            continue;
        case 'p':       /* don't sort -- symbol table order */
            nosort_flg++;
            continue;
        case 'r':       /* sort in reverse order */
            revsort_flg++;
            continue;
        case 'u':       /* undefined symbols only */
            undef_flg++;
            continue;
        default:
            usage(-1);
        }
        break;
    }
    argc -= optind;
    argv += optind;
    if (argc < 1)
        usage(0);

    narg = argc;
    while (argc-- > 0) {
        const char *fname = *argv++;
        uint64_t magic;
        FILE *fi;

        fi = fopen(fname, "r");
        if (!fi) {
            fprintf(stderr, "%s: Cannot open %s\n", progname, fname);
            continue;
        }

        magic = fread6(fi);
        if (feof(fi)) {
            fprintf(stderr, "%s: %s: File truncated\n", progname, fname);
            fclose(fi);
            continue;
        }
        fseek(fi, 0L, 0);

        if (magic == BESM6_MAGIC) {
            //
            // Regular file.
            //
            ar_name = 0;
            if (obj_read_fd(fi, &obj) < 0) {
                fprintf(stderr, "%s: %s: Bad format\n", progname, fname);
                fclose(fi);
                continue;
            }
            nm(fname, narg);
        } else {
            //
            // Archive.
            //
            struct archive *a = archive_read_new();
            struct archive_entry *entry;

            if (narg > 1)
                printf("\n%s:\n", fname);
            archive_read_support_filter_all(a);
            archive_read_support_format_all(a);
            archive_read_open(a, fi, NULL, myread, NULL);
            for (;;) {
                int ret = archive_read_next_header(a, &entry);
                if (ret == ARCHIVE_EOF) {
                    break;
                } else if (ret == ARCHIVE_RETRY) {
                    continue;
                } else if (ret == ARCHIVE_WARN) {
                    fprintf(stderr, "%s: Archive %s warning\n", progname, fname);
                } else if (ret != ARCHIVE_OK) {
                    fprintf(stderr, "%s: %s: Bad archive\n", progname, fname);
                    break;
                }

                ar_name = archive_entry_pathname(entry);
                unsigned nbytes = archive_entry_size(entry);
                char data[MAXSZ*6];

                if (nbytes > sizeof(data)) {
                    fprintf(stderr, "%s: Too long archive item %s\n", fname, ar_name);
                    continue;
                }
                if (archive_read_data(a, data, nbytes) != nbytes) {
                    fprintf(stderr, "%s(%s): Read error\n", fname, ar_name);
                    continue;
                }
                if (obj_read_data(data, nbytes, &obj) < 0) {
                    fprintf(stderr, "%s: %s(%s): Bad format\n", progname, fname, ar_name);
                    continue;
                }
                nm(fname, narg);
            }
        }
        fclose(fi);
    }
    return 0;
}
