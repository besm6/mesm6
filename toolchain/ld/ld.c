/*
 * Linker for MESM-6 project.
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
#include <unistd.h>
#include <stdarg.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <archive.h>
#include <archive_entry.h>
#include "stdobj.h"

#include "mesm6/a.out.h"

#define NSYM    2000            // max number of symbols
#define LOCSYM  'L'             // temporary symbol names start with 'L'

//
// Input and output files.
//
FILE *input;
FILE *outb, *toutb, *doutb, *troutb, *droutb, *soutb;

//
// List of input object files.
//
obj_image_t *obj_head, *obj_tail;

struct nlist cursym;            // current symbol
struct nlist symtab[NSYM];      // symbol table
struct nlist **symhash[NSYM];   // pointers to hash table
struct nlist *lastsym;          // last symbol created
struct nlist *hshtab[NSYM+2];   // hash table of symbols
int symindex;                   // next free symtab index
int basaddr = 1;                // base address of loading

//
// Internal symbols: _etext, _edata, _end, entry point.
//
struct nlist *p_etext, *p_edata, *p_end, *entrypt;

//
// Flags.
//
int     trace;                  // internal trace flag
int     xflag;                  // discard local symbols
int     Xflag;                  // discard locals starting with LOCSYM
int     Sflag;                  // discard all except locals and globals
int     rflag;                  // preserve relocation bits, don't define commons
int     arflag;                 // original copy of rflag
int     sflag;                  // discard all symbols
int     dflag;                  // define common even with rflag

//
// Cumulative sizes set in pass 1 (in words).
//
int     text_size, data_size, bss_size;

//
// Symbol relocation: both passes.
//
int     cur_text_rel, cur_data_rel, cur_bss_rel;

int     o_flag;
char    *ofilename = "l.out";
int     errlev;
char    tfname[] = "/tmp/ldaXXXXXX";
char    libname[256];

const char libpattern[] = "/usr/local/lib/besm6/lib";
const char *filname;
const char *progname;

//
// Needed after pass 1.
//
int     torigin;
int     dorigin;
int     borigin;

//
// Signal handler: delete temporary output file and finish.
//
void delexit()
{
    unlink("l.out");
    exit(-1);
}

void error(int n, char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    if (!errlev)
        printf("%s: ", progname);
    if (filname)
        printf("%s: ", filname);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");
    if (n > 1)
        delexit();
    errlev = n;
}

//
// Read a 48-bit word at the current file position.
//
static uint64_t fread6(FILE *fd)
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
// Open object file.
// Return 0 in case of regular file,
// and 1 in case of archive.
//
int open_input(char *name, int libflag)
{
    uint64_t magic;

//printf("--- %s(name = '%s', libflag = %u)\n", __func__, name, libflag);
    input = 0;
    if (libflag) {
        strcpy(libname, libpattern);
        strcat(libname, name);
        strcat(libname, ".a");
        filname = libname;
//printf("--- %s() open '%s'\n", __func__, filname);
        input = fopen(filname, "r");
        if (! input)
            filname += 4;
    } else {
        filname = name;
    }
//if (! input)
//printf("--- %s() open '%s'\n", __func__, filname);
    if (! input && ! (input = fopen(filname, "r")))
        error(2, "Cannot open");

    magic = fread6(input);
    if (feof(input))
        error(1, "Unexpected EOF");
    fseek(input, 0L, 0);

    if (magic == BESM6_MAGIC)
        return 0;       // regular file

    return 1;           // probably archive
}

void symreloc()
{
    switch (cursym.n_type) {

    case N_TEXT:
    case N_EXT+N_TEXT:
        cursym.n_value += cur_text_rel;
        break;

    case N_DATA:
    case N_EXT+N_DATA:
        cursym.n_value += cur_data_rel;
        break;

    case N_BSS:
    case N_EXT+N_BSS:
        cursym.n_value += cur_bss_rel;
        break;

    case N_EXT+N_UNDF:
    case N_EXT+N_COMM:
        break;

    default:
        if (cursym.n_type & N_EXT)
            cursym.n_type = N_EXT+N_ABS;
        break;
    }
}

int enter(struct nlist **hp)
{
    struct nlist *sp;

    if (! *hp) {
        if (symindex >= NSYM)
            error(2, "Symbol table overflow");
        symhash[symindex] = hp;
        *hp = lastsym = sp = &symtab[symindex++];
        sp->n_len = cursym.n_len;
        sp->n_name = cursym.n_name;
        sp->n_type = cursym.n_type;
        sp->n_value = cursym.n_value;
        return 1;
    } else {
        lastsym = *hp;
        return 0;
    }
}

struct nlist **lookup()
{
    int i, clash;
    char *cp, *cp1;
    struct nlist **hp;

    i = 0;
    for (cp = cursym.n_name; *cp; i = (i << 1) + *cp++)
        ;
    for (hp = &hshtab[(i & 077777) % NSYM + 2]; *hp != 0;) {
        cp1 = (*hp)->n_name;
        clash = 0;
        for (cp = cursym.n_name; *cp;)
            if (*cp++ != *cp1++) {
                clash = 1;
                break;
            }
        if (clash) {
            if (++hp >= &hshtab[NSYM+2])
                hp = hshtab;
        } else
            break;
    }
    return hp;
}

void merge_symbols(obj_image_t *obj)
{
    struct nlist *sp;
    int type, symlen;

    //TODO: merge symbols from the object file into a common symbol table.
    for (;;) {
        symlen = fgetsym(input, &cursym);
        if (symlen == 0)
            error(2, "Out of memory");
        if (symlen == 1)
            break;
        type = cursym.n_type;
        if (Sflag && ((type & N_TYPE) == N_ABS ||
            (type & N_TYPE) > N_COMM))
        {
            free(cursym.n_name);
            continue;
        }
        if (! (type & N_EXT)) {
            if (!sflag && !xflag && (!Xflag || cursym.n_name[0] != LOCSYM)) {
                //nsym++;
            }
            free(cursym.n_name);
            continue;
        }
        symreloc();
        if (enter(lookup()))
            continue;
        free(cursym.n_name);
        if (cursym.n_type == N_EXT+N_UNDF)
            continue;
        sp = lastsym;
        if (sp->n_type == N_EXT+N_UNDF ||
            sp->n_type == N_EXT+N_COMM)
        {
            if (cursym.n_type == N_EXT+N_COMM)
            {
                // Common block: update type and size.
                sp->n_type = cursym.n_type;
                if (cursym.n_value > sp->n_value)
                    sp->n_value = cursym.n_value;
            }
            else if (sp->n_type == N_EXT+N_UNDF ||
                cursym.n_type == N_EXT+N_DATA ||
                cursym.n_type == N_EXT+N_BSS)
            {
                // Resolve undefined symbol.
                sp->n_type = cursym.n_type;
                sp->n_value = cursym.n_value;
            }
        }
    }
}

//
// Check whether the object file provides any symbols we need.
//
int need_this_obj(obj_image_t *obj)
{
    //TODO
    return 1;
}

//
// Append object structure to the single-linked list.
//
void append_to_obj_list(obj_image_t *obj)
{
    if (!obj)
        error(2, "Out of memory");

    obj->next = 0;

    if (obj_tail) {
        obj_tail->next = obj;
    } else {
        obj_head = obj;
    }
    obj_tail = obj;
}

//
// Pass 1: load one object file.
// The file can be opened as file descriptor fd,
// or supplied as byte array of given size.
// Return 1 in case any names have been resolved,
// otherwise return 0.
//
int load1obj(FILE *fd, char *data, unsigned nbytes)
{
    obj_image_t obj = {0};

    if (fd) {
printf("--- %s(fd = %u)\n", __func__, fileno(fd));
        if (obj_read_fd(fd, &obj) < 0)
            error(2, "Bad format");
    } else {
printf("--- %s(nbytes = %u)\n", __func__, nbytes);
        if (obj_read_data(data, nbytes, &obj) < 0)
            error(2, "Bad format");

        // Does this component have anything useful for us?
        if (!need_this_obj(&obj)) {
printf("--- %s() ignore this module\n", __func__);
            return 0;
        }
    }
printf("--- %s() link obj file: cmd=%u, const=%u, data=%u\n", __func__, obj.cmd_len, obj.const_len, obj.data_len);

    // Link in the object file.
    cur_text_rel = 0;
    cur_data_rel = - obj.cmd_len;
    cur_bss_rel = - obj.cmd_len - obj.const_len;

    cur_text_rel += text_size;
    cur_data_rel += data_size;
    cur_bss_rel += bss_size;

    text_size += obj.cmd_len;
    data_size += obj.const_len;
    bss_size += obj.data_len;

    // Merge symbols into a common symbol table.
    merge_symbols(&obj);

    // Reallocate the object image and add to the list.
    append_to_obj_list(obj_copy(&obj));
    return 1;
}

struct nlist **slookup(char *s)
{
    cursym.n_len = strlen(s) + 1;
    cursym.n_name = s;
    cursym.n_type = N_EXT+N_UNDF;
    cursym.n_value = 0;
    return lookup();
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

//
// Scan file to find defined symbols.
//
void load1name(char *fname, int libflag)
{
    if (open_input(fname, libflag) == 0) {
        //
        // Regular file.
        //
//printf("--- %s(fname = '%s', libflag = %u) regular file\n", __func__, fname, libflag);
        if (trace > 1)
            printf("%s\n", fname);
        load1obj(input, NULL, 0);
    } else {
        //
        // Archive.
        //
        struct archive *a = archive_read_new();
        struct archive_entry *entry;

//printf("--- %s(fname = '%s', libflag = %u) archive\n", __func__, fname, libflag);
        archive_read_support_filter_all(a);
        archive_read_support_format_all(a);
        archive_read_open(a, input, NULL, myread, NULL);
        for (;;) {
            int ret = archive_read_next_header(a, &entry);
//printf("--- %s() archive ret = %d\n", __func__, ret);
            if (ret != ARCHIVE_OK) {
                if (ret == ARCHIVE_EOF)
                    break;
                else if (ret == ARCHIVE_RETRY)
                    continue;
                else if (ret == ARCHIVE_WARN)
                    error(0, "Archive warning");
                else
                    error(2, "Bad archive");
            }

            const char *name = archive_entry_pathname(entry);
            unsigned nbytes = archive_entry_size(entry);
            static char data[MAXSZ*6];

            if (trace > 1)
                printf("%s(%s):\n", fname, name);

            if (nbytes > sizeof(data)) {
                error(2, "Too long array entry");
            }
            if (archive_read_data(a, data, nbytes) != nbytes) {
                error(2, "Read error");
            }
            load1obj(NULL, data, nbytes);
        }
    }
    fclose(input);
}

void usage(int retcode)
{
    printf("Usage:\n");
    printf("    %s [options] file...\n", progname);
    printf("Options:\n");
    printf("    -d              Force common symbols to be defined\n");
    printf("    -e symbol       Set entry address\n");
    printf("    -llibname       Search for library `libname'\n");
    printf("    -o filename     Set output file name\n");
    printf("    -r              Generate relocatable output\n");
    printf("    -s              Strip all symbols\n");
    printf("    -S              Strip debugging symbols\n");
    printf("    -t              Trace file opens\n");
    printf("    -T address      Set base address\n");
    printf("    -u symbol       Start with undefined reference to `symbol'\n");
    printf("    -x              Discard all local symbols\n");
    printf("    -X              Discard temporary local symbols (default)\n");
    exit(retcode);
}

//
// Scan files once to find symdefs.
//
void pass1(int argc, char **argv)
{
    for (;;) {
        filname = 0;
        switch (getopt(argc, argv, "-de:l:o:rsStT:u:xX")) {
        case EOF:
            break;

        case 1:
            // Input file name.
            load1name(optarg, 0);
            continue;

        case 'd':
            // Force allocation of commons.
            dflag++;
            continue;

        case 'e':
            // Set `entry' symbol.
            enter(slookup(optarg));
            entrypt = lastsym;
            continue;

        case 'l':
            // Library name.
            load1name(optarg, 1);
            continue;

        case 'o':
            // Output file name.
            ofilename = optarg;
            o_flag++;
            continue;

        case 'r':
            // Generate relocatable output.
            rflag++;
            arflag++;
            continue;

        case 's':
            // Strip all symbols.
            sflag++;
            xflag++;
            continue;

        case 'S':
            // Strip debugging symbols.
            Sflag++;
            continue;

        case 't':
            // Enable tracing.
            trace++;
            continue;

        case 'T':
            // Set base address.
            basaddr = strtoul(optarg, 0, 0);
            continue;

        case 'u':
            // Mark `symbol' as undefined.
            enter(slookup(optarg));
            continue;

        case 'x':
            // Discard all local symbols.
            xflag++;
            continue;

        case 'X':
            // Discard temporary local symbols.
            Xflag++;
            continue;

        default:
            usage(-1);
        }
        break;
    }
}

void ldrsym(struct nlist *sp, long val, int type)
{
    if (sp == 0) return;
    if (sp->n_type != N_EXT+N_UNDF) {
        printf("%s: ", sp->n_name);
        error(1, "Name redefined");
        return;
    }
    sp->n_type = type;
    sp->n_value = val;
}

void middle()
{
    struct nlist *sp, *symp;
    long t;
    long cmsize;
    int undef_count;
    long cmorigin;

    p_etext = *slookup("_etext");
    p_edata = *slookup("_edata");
    p_end = *slookup("_end");

    //
    // If there are any undefined symbols, save the relocation bits.
    //
    symp = &symtab[symindex];
    if (!rflag) {
        for (sp=symtab; sp<symp; sp++)
            if (sp->n_type == N_EXT+N_UNDF &&
                sp != p_end && sp != p_edata && sp != p_etext)
            {
                rflag++;
                dflag = 0;
                break;
            }
    }
    if (rflag)
        sflag = 0;

    //
    // Assign common locations.
    //
    cmsize = 0;
    if (dflag || !rflag) {
        ldrsym(p_etext, text_size, N_EXT+N_TEXT);
        ldrsym(p_edata, data_size, N_EXT+N_DATA);
        ldrsym(p_end, bss_size, N_EXT+N_BSS);
        for (sp=symtab; sp<symp; sp++)
            if ((sp->n_type & N_TYPE) == N_COMM) {
                t = sp->n_value;
                sp->n_value = cmsize;
                cmsize += t;
            }
    }

    //
    // Now set symbols to their final value
    //
    torigin = basaddr;
    dorigin = torigin + text_size;
    cmorigin = dorigin + data_size;
    borigin = cmorigin + cmsize;
    undef_count = 0;
    for (sp=symtab; sp<symp; sp++) {
        switch (sp->n_type) {
        case N_EXT+N_UNDF:
            if (!arflag)
                errlev |= 1;
            if (!arflag) {
                if (!undef_count)
                    printf("Undefined:\n");
                undef_count++;
                printf("\t%s\n", sp->n_name);
            }
            break;
        default:
        case N_EXT+N_ABS:
            break;
        case N_EXT+N_TEXT:
            sp->n_value += torigin;
            break;
        case N_EXT+N_DATA:
            sp->n_value += dorigin;
            break;
        case N_EXT+N_BSS:
            sp->n_value += borigin;
            break;
        case N_COMM:
        case N_EXT+N_COMM:
            sp->n_type = N_EXT+N_BSS;
            sp->n_value += cmorigin;
            break;
        }
        if (sp->n_value & ~0777777777)
            error(1, "Long address: %s=0%lo", sp->n_name, sp->n_value);
    }
    bss_size += cmsize;
}

void tcreat(FILE **buf, int tempflg)
{
    *buf = fopen(tempflg ? tfname : ofilename, "w+");
    if (! *buf)
        error(2, tempflg ?
            "Cannot create temporary file" :
            "Cannot create output file");
    if (tempflg)
        unlink(tfname);
}

void setupout()
{
    int fd = mkstemp(tfname);
    if (fd == -1) {
        error(2, "Cannot create temporary file %s", tfname);
    } else {
        close(fd);
    }
    tcreat(&outb, 0);
    tcreat(&toutb, 1);
    tcreat(&doutb, 1);
    if (!sflag || !xflag) tcreat(&soutb, 1);
    if (rflag) {
        tcreat(&troutb, 1);
        tcreat(&droutb, 1);
    }
#if 0
    // Write header.
    filhdr.a_magic = FMAGIC;
    filhdr.a_text = text_size;
    filhdr.a_data = data_size;
    filhdr.a_bss = bss_size;
    filhdr.a_syms = ALIGN(symtab_size, WSZ);
    if (entrypt) {
        if (entrypt->n_type != N_EXT+N_TEXT &&
            entrypt->n_type != N_EXT+N_UNDF)
            error(1, "Entry out of text");
        else filhdr.a_entry = entrypt->n_value;
    } else {
        filhdr.a_entry = torigin;
    }

    if (rflag) {
        filhdr.a_flag &= ~RELFLG;
    } else {
        filhdr.a_flag |= RELFLG;
    }

    fputhdr(&filhdr, outb);
#endif
}

int reltype(int stype)
{
    switch (stype & N_TYPE) {
    case N_UNDF:    return 0;
    case N_ABS:     return RABS;
    case N_TEXT:    return RTEXT;
    case N_DATA:    return RDATA;
    case N_BSS:     return RBSS;
    case N_STRNG:   return RDATA;
    case N_COMM:    return RBSS;
    case N_FN:      return 0;
    default:        return 0;
    }
}

void relhalf(/*struct local *lp,*/ long t, long *pt)
{
    long a, ad;

    if (trace > 2)
        printf("%08lx", t);

    // Extract address from command.
    switch ((int) t & RSHORT) {
    case 0:
        a = t & 0777777777;
        break;
    case RLONG:
    case RTRUNC:
        a = t & 03777777;
        break;
    case RSHORT:
        a = t & 07777;
        break;
    case RSHIFT:
        a = t & 077777;
        a <<= 12;
        break;
    default:
        a = 0;
        break;
     }

    // Compute address shift `ad'.
    // Update relocation word.
    ad = 0;
    switch ((int) t & REXT) {
    case RTEXT:
        ad = cur_text_rel;
        break;
    case RDATA:
        ad = cur_data_rel;
        break;
    case RBSS:
        ad = cur_bss_rel;
        break;
#if 0
    case REXT:
        struct nlist *sp = lookloc(lp, (int) RGETIX(t));
        t &= RSHORT;
        if (sp->n_type == N_EXT+N_UNDF ||
            sp->n_type == N_EXT+N_COMM)
        {
            t |= REXT | RPUTIX(nsym + (sp - symtab));
            break;
        }
        t |= reltype(sp->n_type);
        ad = sp->n_value;
        break;
#endif
    }

    // Add updated address to command.
    switch ((int) t & RSHORT) {
    case 0:
        t &= ~0777777777;
        t |= (a + ad) & 0777777777;
        break;
    case RSHORT:
        t &= ~07777;
        t |= (a + ad) & 07777;
        break;
    case RLONG:
        t &= ~03777777;
        t |= (a + ad) & 03777777;
        break;
    case RSHIFT:
        t &= ~03777777;
        t |= (a + ad) >> 12 & 03777777;
        break;
    case RTRUNC:
        t &= ~03777777;
        t |= (a + (ad & 07777)) & 03777777;
        break;
    }

    if (trace > 2)
        printf(" -> %08lx\n", t);

    *pt = t;
}

void relocate(/*struct local *lp,*/ FILE *b1, FILE *b2, long len)
{
#if 0
    long t;

    len /= WSZ/2;
    while (len--) {
        t = fgeth(input);
        relhalf(/*lp,*/ t, &t);
        fputh(t, b1);
    }
#endif
}

void load2obj(obj_image_t *obj)
{
    struct nlist *sp;
    int symno;
    int type;
    long count;

    cur_text_rel = 0;
    cur_data_rel = - obj->cmd_len;
    cur_bss_rel = - obj->cmd_len - obj->const_len;

    cur_text_rel += torigin;
    cur_data_rel += dorigin;
    cur_bss_rel += borigin;

    if (trace > 1)
        printf("cur_text_rel=%#x, cur_data_rel=%#x, cur_bss_rel=%#x\n",
            cur_text_rel, cur_data_rel, cur_bss_rel);
    //
    // Re-read the symbol table, recording the numbering
    // of symbols for fixing external references.
    //
    symno = -1;
    for (;;) {
        symno++;
        count = fgetsym(input, &cursym);
        if (count == 0)
            error(2, "Out of memory");
        if (count == 1)
            break;
        symreloc();
        type = cursym.n_type;
        if (Sflag && ((type & N_TYPE) == N_ABS ||
            (type & N_TYPE) > N_COMM))
        {
            free(cursym.n_name);
            continue;
        }
        if (! (type & N_EXT)) {
            if (!sflag && !xflag &&
                (!Xflag || cursym.n_name[0] != LOCSYM))
                fputsym(&cursym, soutb);
            free(cursym.n_name);
            continue;
        }
        if (! (sp = *lookup()))
            error(2, "Internal error: symbol not found");
        free(cursym.n_name);
        if (cursym.n_type == N_EXT+N_UNDF ||
            cursym.n_type == N_EXT+N_COMM)
        {
            continue;
        }
        if (cursym.n_type != sp->n_type ||
            cursym.n_value != sp->n_value)
        {
            printf("%s: ", cursym.n_name);
            error(1, "Name redefined");
        }
    }

    if (trace > 1)
        printf("** TEXT **\n");
    relocate(toutb, troutb, obj->cmd_len);

    if (trace > 1)
        printf("** DATA **\n");
    relocate(doutb, droutb, obj->const_len);

    torigin += obj->cmd_len;
    dorigin += obj->const_len;
    borigin += obj->data_len;
}

void pass2(int argc, char **argv)
{
    obj_image_t *obj;

    for (obj = obj_head; obj; obj = obj->next) {
        load2obj(obj);
    }
}

void copy(FILE *buf)
{
    int c;

    rewind(buf);
    while ((c = getc(buf)) != EOF)
        putc(c, outb);
    fclose(buf);
}

void finishout()
{
    struct nlist *p;

    copy(toutb);
    copy(doutb);
    if (rflag) {
        copy(troutb);
        copy(droutb);
    }
    if (! sflag) {
        if (! xflag)
            copy(soutb);
        for (p=symtab; p<&symtab[symindex]; ++p)
            fputsym(p, outb);
        putc(0, outb);
    }
    fclose(outb);
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
    if (signal(SIGINT, SIG_IGN) != SIG_IGN)
        signal(SIGINT, delexit);
    if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
        signal(SIGTERM, delexit);

    //
    // First pass: compute segment sizes, symbol table and entry point.
    //
    pass1(argc, argv);
    filname = 0;

    //
    // Relocate symbols.
    //
    middle();

    //
    // Create new header.
    //
    setupout();

    //
    // Second pass: relocate the code.
    //
    pass2(argc, argv);

    //
    // Copy output files.
    //
    finishout();

    if (!o_flag) {
        // Rename a.out into l.out.
        unlink("a.out");
        link("l.out", "a.out");
        ofilename = "a.out";
    }
    unlink("l.out");
    if (errlev == 0 && !arflag) {
        // Linking succeeded and no -r option: make output executable.
        chmod(ofilename, 0777 & ~umask(0));
    }
    return 0;
}
