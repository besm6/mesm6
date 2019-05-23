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

#define MAXSYMBOLS      2000    // max number of symbols
#define MAXNAMES        2000    // max number of names
#define LOCSYM          'L'     // temporary symbol names start with 'L'

//
// Input and output files.
//
FILE *input;
FILE *outb, *toutb, *doutb, *troutb, *droutb, *soutb;

//
// List of input object files.
//
obj_image_t *obj_head, *obj_tail;

//
// Symbol table.
//
nlist_t symtab[MAXSYMBOLS];     // symbol table
unsigned nsymbols;              // number of symbols
uint64_t nametab[MAXNAMES];     // name table
unsigned nnames;                // number of names
nlist_t *entrypt;               // entry point

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

unsigned basaddr = 1;           // base address of resulting image

//
// Cumulative sizes set in pass 1 (in words).
//
int     text_size, data_size, bss_size;

//
// Symbol relocation: both passes.
//
int     cur_text_rel, cur_data_rel, cur_bss_rel;

//
// Needed after pass 1.
//
int     torigin;
int     dorigin;
int     borigin;

int     o_flag;
char    *ofilename = "l.out";
int     errlev;
char    tfname[] = "/tmp/ldaXXXXXX";
char    libname[256];

const char libpath[] = "/usr/local/lib/besm6/lib";
const char *filname;
const char *progname;

//
// Signal handler: delete temporary output file and finish.
//
void delexit()
{
    unlink("l.out");
    exit(-1);
}

//
// Print message, including program name and input file name.
//
void vmessage(char *fmt, va_list ap)
{
    if (!errlev)
        printf("%s: ", progname);

    if (filname)
        printf("%s: ", filname);

    vprintf(fmt, ap);
    printf("\n");
}

//
// Print warning message.
//
void warning(char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vmessage(fmt, ap);
    va_end(ap);
}

//
// Print error message and set error level.
//
void error(char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vmessage(fmt, ap);
    va_end(ap);

    errlev = 1;
}

//
// Print error message and terminate.
//
void fatal(char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vmessage(fmt, ap);
    va_end(ap);

    delexit();
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

uint8_t unicode_to_text(uint16_t uc)
{
    static const uint8_t tab0[256] = {
        /* 00 -- 07 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* 08 -- 0f */  0,    0,    0,    0,    0,    0,    0,    0,
        /* 10 -- 17 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* 18 -- 1f */  0,    0,    0,    0,    0,    0,    0,    0,
        /* 20 -- 27 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* ()*+,-./ */  0x08, 0x09, 0x0a, 0x1e, 0x1b, 0x1d, 0x01, 0x0f,
        /* 01234567 */  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
        /* 89       */  0x18, 0x19, 0,    0,    0,    0,    0,    0,
        /*  ABCDEFG */  0,    0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
        /* HIJKLMNO */  0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
        /* PQRSTUVW */  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
        /* XYZ    / */  0x38, 0x39, 0x3a, 0,    0,    0,    0,    0x0f,
        /*  abcdefg */  0,    0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
        /* hijklmno */  0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
        /* pqrstuvw */  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
        /* xyz      */  0x38, 0x39, 0x3a, 0,    0,    0,    0,    0,
    };
    static const uint8_t tab4[256] = {
        /* 00 -- 07 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* 08 -- 0f */  0,    0,    0,    0,    0,    0,    0,    0,
        /* АБВГДЕЖЗ */  0x21, 0x02, 0x22, 0x06, 0x04, 0x25, 0x0e, 0x20,
        /* ИЙКЛМНОП */  0x07, 0x0b, 0x2b, 0x0c, 0x2d, 0x28, 0x2f, 0x1c,
        /* РСТУФХЦЧ */  0x30, 0x23, 0x34, 0x39, 0x05, 0x38, 0x03, 0x3e,
        /* ШЩЪЫЬЭЮЯ */  0x3b, 0x3d, 0x1a, 0x1f, 0x1a, 0x3c, 0x3f, 0x0d,
        /* абвгдежз */  0x21, 0x02, 0x22, 0x06, 0x04, 0x25, 0x0e, 0x20,
        /* ийклмноп */  0x07, 0x0b, 0x2b, 0x0c, 0x2d, 0x28, 0x2f, 0x1c,
        /* рстуфхцч */  0x30, 0x23, 0x34, 0x39, 0x05, 0x38, 0x03, 0x3e,
        /* шщъыьэюя */  0x3b, 0x3d, 0x1a, 0x1f, 0x1a, 0x3c, 0x3f, 0x0d,
    };

    switch (uc >> 8) {
    case 0x00:
        return tab0[uc];
    case 0x04:
        return tab4[(uint8_t) uc];
    }
    return 0;
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
        strcpy(libname, libpath);
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
        fatal("Cannot open");

    magic = fread6(input);
    if (feof(input))
        error("Unexpected EOF");
    fseek(input, 0L, 0);

    if (magic == BESM6_MAGIC)
        return 0;       // regular file

    return 1;           // probably archive
}

//
// Return symbol name as printable string.
//
const char *sym_name(nlist_t *sp)
{
    switch (sp->f.n_type) {
    case SYM_ENTRY_S:
    case SYM_EXT_S:
    case SYM_PRIVATE_S:
    case SYM_COMMON_S:
        // Short name.
        return text_to_utf(sp->u64 & 07777777700000000);

    case SYM_ENTRY_L:
    case SYM_EXT_L:
    case SYM_PRIVATE_L:
    case SYM_COMMON_L:
        // Long name.
        return text_to_utf(nametab[sp->f.n_ref]);

    default:
        return "<Unknown>";
    }
}

//
// Return true when a symbol name matches the given name
// in TEXT encoding.
// Private blocks never match by name.
//
int sym_name_match(nlist_t *sp, uint64_t name)
{
    switch (sp->f.n_type) {
    case SYM_ENTRY_S:
    case SYM_EXT_S:
    case SYM_COMMON_S:
        // Short name.
        return (sp->u64 & 07777777700000000) == name;

    case SYM_ENTRY_L:
    case SYM_EXT_L:
    case SYM_COMMON_L:
        // Long name.
        return nametab[sp->f.n_ref] == name;

    default:
        return 0;
    }
}

//
// Find existing symbol by name in TEXT encoding.
//
nlist_t *sym_find(uint64_t name)
{
    nlist_t *sp;

    for (sp=symtab; sp<&symtab[nsymbols]; sp++) {
        if (sym_name_match(sp, name)) {
            return sp;
        }
    }
    return 0;
}

//
// Create new name (in TEXT encoding).
//
unsigned create_name(uint64_t name)
{
    int i;

    for (i=0; i<nnames; i++) {
        if (nametab[i] == name) {
            return i;
        }
    }
    if (nnames >= MAXNAMES)
        fatal("Name table overflow");
    nametab[nnames] = name;
    return nnames++;
}

//
// Convert a name from UTF-8 string into TEXT encoding.
// Length is limited by 8 symbols.
//
uint64_t utf_to_text(const char *str)
{
    uint64_t name = 0;
    int i;

    for (i=42; *str && i>=0; i-=6) {
        uint16_t c = (uint8_t) *str++;

        // Decode utf8 into 16-bit character code.
        if (c & 0x80) {
            uint8_t c2 = *str++;
            if (! (c & 0x20)) {
                c = (c & 0x1f) << 6 | (c2 & 0x3f);
            } else {
                uint8_t c3 = *str++;
                c = (c & 0x0f) << 12 | (c2 & 0x3f) << 6 | (c3 & 0x3f);
            }
        }

        name |= unicode_to_text(c) << i;
    }
    return name;
}

//
// Relocate address for the object file.
//
unsigned relocate_address(obj_image_t *obj, unsigned addr)
{
    if (addr < obj->cmd_len) {
        return addr + cur_text_rel;
    }
    if (addr < obj->cmd_len + obj->const_len) {
        return addr + cur_data_rel;
    }
    if (addr < obj->cmd_len + obj->const_len + obj->bss_len) {
        return addr + cur_bss_rel;
    }
    fatal("Module %s: relocatable address %05o out of range",
        text_to_utf(obj->word[obj->table_off]), addr);
    return 0;
}

//
// Create new symbol: external reference.
// In case the symbol already exists - return a pointer.
//
nlist_t *create_extref(const char *str)
{
    uint64_t name = utf_to_text(str);
    nlist_t  *sp  = sym_find(name);

    if (sp) {
        // Name already exists.
        return sp;
    }

    // Allocate new symbol: external reference.
    if (nsymbols >= MAXSYMBOLS)
        fatal("Symbol table overflow");
    sp = &symtab[nsymbols];
    nsymbols++;
    if (name & 077777777) {
        // Long name.
        sp->f.n_type = SYM_EXT_L;
        sp->f.n_ref = create_name(name);
    } else {
        // Short name.
        sp->f.n_type = SYM_EXT_S;
        sp->f.n_ref = name >> 24;
    }
    return sp;
}

//
// Create new symbol: entry.
//
nlist_t *create_entry(uint64_t name, unsigned addr)
{
    nlist_t *sp;

    for (sp=symtab; sp<&symtab[nsymbols]; sp++) {
        if (sym_name_match(sp, name)) {
            // Name already exists.
            if (sp->f.n_type != SYM_EXT_S &&
                sp->f.n_type != SYM_EXT_L) {
                fatal("Name %s redefined", text_to_utf(name));
            }

            // Replace the symbol
            if (name & 077777777) {
                // Long name.
                sp->f.n_type = SYM_ENTRY_L;
            } else {
                // Short name.
                sp->f.n_type = SYM_ENTRY_S;
            }
            sp->f.n_addr = addr;
            return sp;
        }
    }

    // Allocate new symbol.
    if (nsymbols >= MAXSYMBOLS)
        fatal("Symbol table overflow");
    sp = &symtab[nsymbols];
    nsymbols++;
    if (name & 077777777) {
        // Long name.
        sp->f.n_type = SYM_ENTRY_L;
        sp->f.n_ref = create_name(name);
    } else {
        // Short name.
        sp->f.n_type = SYM_ENTRY_S;
        sp->f.n_ref = name >> 24;
    }
    sp->f.n_addr = addr;
    return sp;
}

//
// Merge symbols from the object file into a common symbol table.
//
void merge_symbols(obj_image_t *obj)
{
    int i;
printf("--- %s() symtab length = %u\n", __func__, obj->sym_len);

    // Add entries.
    for (i = 0; i < obj->nentries; i++) {
        uint64_t name = obj->word[2*i + 1];
        unsigned addr = obj->word[2*i + 2] & 077777;

        create_entry(name, addr);
    }

    // Merge symbols.
    for (i = 0; i < obj->sym_len; i++) {
        uint64_t *wp = &obj->word[i + 1 + obj->table_off];
        nlist_t sym;
        unsigned old_ref;

        // Store new index in place of the symbol.
        sym.u64 = *wp;
        *wp = nsymbols;

        switch (sym.f.n_type) {
        case SYM_ABS:
        case SYM_CONST:
        case SYM_PRIVATE_S:
            // Absolute address.
            // External reference (short name).
            // Private block (short name).
            break;

        case SYM_EXT_S:
            //TODO

        case SYM_EXT_L:
            //
            // External reference (long name).
            //
            // Update the reference field with new name index.
            sym.f.n_ref = 04000 | nnames;
            //TODO
            break;

        case SYM_RELOC:
            //
            // Relocatable address.
            //
            // Update the address field.
            sym.f.n_addr = relocate_address(obj, sym.f.n_addr);
            break;

        case SYM_OFFSET:
            //
            // Offset from another symbol.
            //
            // Update the reference field with new index.
            old_ref = sym.f.n_ref & 03777;
            sym.f.n_ref = 04000 | obj->word[old_ref + 1 + obj->table_off];
            break;

        case SYM_INDIRECT:
        case SYM_EXPRESSION:
            //
            // Dereference or expression.
            //
            // Update the address field with new index.
            old_ref = sym.f.n_addr;
            sym.f.n_addr = obj->word[old_ref + 1 + obj->table_off];
            break;

        case SYM_ADD:
        case SYM_SUBTRACT:
        case SYM_MULTIPLY:
        case SYM_DIVIDE:
            //
            // Add/subtract/multiply/divide two symbols.
            //
            // Update the reference field with new index.
            old_ref = sym.f.n_ref & 03777;
            sym.f.n_ref = 04000 | obj->word[old_ref + 1 + obj->table_off];

            // Update the address field with new index.
            old_ref = sym.f.n_addr;
            sym.f.n_addr = obj->word[old_ref + 1 + obj->table_off];
            break;

//TODO
        case SYM_ENTRY_S:

        case SYM_ENTRY_L:

        case SYM_COMMON_S:
            // Common block (short name).
            //text_to_buf(buf, word & 07777777700000000);
            break;

        case SYM_PRIVATE_L:
            // Private block (long name).
            //text_to_buf(buf, obj->word[ref + obj->table_off]);
            break;

        case SYM_COMMON_L:
            // Common block (long name).
            //text_to_buf(buf, obj->word[ref + obj->table_off]);
            break;

        default:
            fatal("Unknown symbol type %03o", sym.f.n_type);
        }

        // Append to the symbol table.
        if (nsymbols >= MAXSYMBOLS)
            fatal("Symbol table overflow");
        symtab[nsymbols++] = sym;
    }
#if 0
    struct nlist *sp;
    int type, symlen;

    for (;;) {
        symlen = fgetsym(input, &cursym);
        if (symlen == 0)
            fatal("Out of memory");
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
        //symreloc();
        if (enter(lookup_cursym()))
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
#endif
}

//
// Check whether the object file provides any symbols we need.
//
int need_this_obj(obj_image_t *obj)
{
    if (obj->nentries > 0) {
        // Is any of proposed entries referenced by our module?
        int i;
        for (i = 0; i < obj->nentries; i++) {
            uint64_t name = obj->word[2*i + 1];
            nlist_t  *sp  = sym_find(name);

            if (sp && (sp->f.n_type == SYM_EXT_L ||
                       sp->f.n_type == SYM_EXT_S))
                return 1;
        }
        return 0;
    }
    //TODO: No entries in standard array; search for symbols of Entry type.
    return 1;
}

//
// Append object structure to the single-linked list.
//
void append_to_obj_list(obj_image_t *obj)
{
    if (!obj)
        fatal("Out of memory");

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
//printf("--- %s(fd = %u)\n", __func__, fileno(fd));
        if (obj_read_fd(fd, &obj) < 0)
            fatal("Bad format");
    } else {
//printf("--- %s(nbytes = %u)\n", __func__, nbytes);
        if (obj_read_data(data, nbytes, &obj) < 0)
            fatal( "Bad format");

        // Does this component have anything useful for us?
        if (!need_this_obj(&obj)) {
//printf("--- %s() ignore this module\n", __func__);
            return 0;
        }
    }
//printf("--- %s() link obj file: cmd=%u, const=%u, data=%u\n", __func__, obj.cmd_len, obj.const_len, obj.data_len);

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
                    warning("Archive warning");
                else
                    fatal("Bad archive");
            }

            const char *name = archive_entry_pathname(entry);
            unsigned nbytes = archive_entry_size(entry);
            char data[MAXSZ*6];

            if (trace > 1)
                printf("%s(%s):\n", fname, name);

            if (nbytes > sizeof(data)) {
                fatal("Too long array entry");
            }
            if (archive_read_data(a, data, nbytes) != nbytes) {
                fatal("Read error");
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
            entrypt = create_extref(optarg);
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
            create_extref(optarg);
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

//
// Allocate a symbol _etext, _edata or _ebss.
//
void ldrsym(uint64_t name, long val)
{
#if 0
    //TODO: allocate symbol like _text
    if (sp == 0)
        return;
    if (sp->f.n_type != N_EXT+N_UNDF) {
        printf("%s: ", sp->n_name);
        error("Name redefined");
        return;
    }
    sp->n_type = type;
    sp->n_value = val;
#endif
}

void middle()
{
    nlist_t *sp, *symlast;
    uint64_t name_etext, name_edata, name_end;
    long t;
    long cmsize;
    int undef_count;
    long cmorigin;

    name_etext = utf_to_text("_etext");
    name_edata = utf_to_text("_edata");
    name_end = utf_to_text("_end");

    //
    // If there are any undefined symbols, save the relocation bits.
    //
    symlast = &symtab[nsymbols];
    if (!rflag) {
        for (sp=symtab; sp<symlast; sp++) {
            if ((sp->f.n_type == SYM_EXT_S ||
                 sp->f.n_type == SYM_EXT_L) &&
                !sym_name_match(sp, name_end) &&
                !sym_name_match(sp, name_edata) &&
                !sym_name_match(sp, name_etext))
            {
                rflag++;
                dflag = 0;
                break;
            }
        }
    }
    if (rflag)
        sflag = 0;

    //
    // Assign common locations.
    //
    cmsize = 0;
    if (dflag || !rflag) {
        ldrsym(name_etext, text_size);
        ldrsym(name_edata, data_size);
        ldrsym(name_end, bss_size);

        // Allocate common-blocks.
        for (sp=symtab; sp<symlast; sp++) {
            if ((sp->f.n_type == SYM_COMMON_S ||
                 sp->f.n_type == SYM_COMMON_L))
            {
                t = sp->f.n_addr;
                sp->f.n_addr = cmsize;
                cmsize += t;
            }
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
    for (sp=symtab; sp<symlast; sp++) {
        switch (sp->f.n_type) {
        case SYM_EXT_S:
        case SYM_EXT_L:
            if (!arflag)
                errlev |= 1;
            if (!arflag) {
                if (!undef_count)
                    printf("Undefined:\n");
                undef_count++;
                printf("\t%s\n", sym_name(sp));
            }
            break;
#if 0
        //TODO: update other symbols
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
#endif
        }
    }
    bss_size += cmsize;
}

void tcreat(FILE **buf, int tempflg)
{
    *buf = fopen(tempflg ? tfname : ofilename, "w+");
    if (! *buf)
        fatal(tempflg ?
            "Cannot create temporary file" :
            "Cannot create output file");
    if (tempflg)
        unlink(tfname);
}

void setupout()
{
    int fd = mkstemp(tfname);
    if (fd == -1) {
        fatal("Cannot create temporary file %s", tfname);
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
    //TODO: Write header.
    filhdr.a_magic = FMAGIC;
    filhdr.a_text = text_size;
    filhdr.a_data = data_size;
    filhdr.a_bss = bss_size;
    filhdr.a_syms = ALIGN(symtab_size, WSZ);
    if (entrypt) {
        if (entrypt->n_type != N_EXT+N_TEXT &&
            entrypt->n_type != N_EXT+N_UNDF)
            error("Entry out of text");
        else
            filhdr.a_entry = entrypt->n_value;
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

#if 0
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
#endif

void relocate(/*struct local *lp,*/ FILE *b1, FILE *b2, long len)
{
#if 0
    //TODO: relocate a section.
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
    cur_text_rel = 0;
    cur_data_rel = - obj->cmd_len;
    cur_bss_rel = - obj->cmd_len - obj->const_len;

    cur_text_rel += torigin;
    cur_data_rel += dorigin;
    cur_bss_rel += borigin;

    if (trace > 1)
        printf("cur_text_rel=%#x, cur_data_rel=%#x, cur_bss_rel=%#x\n",
            cur_text_rel, cur_data_rel, cur_bss_rel);
#if 0
    //
    //TODO: Re-read the symbol table, recording the numbering
    // of symbols for fixing external references.
    //
    nlist_t *sp;
    int symno = -1;
    for (;;) {
        symno++;
        long count = fgetsym(input, &cursym);
        if (count == 0)
            fatal("Out of memory");
        if (count == 1)
            break;
        //symreloc();
        int type = cursym.n_type;
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
        if (! (sp = *lookup_cursym()))
            fatal("Internal error: symbol not found");
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
            error("Name redefined");
        }
    }
#endif
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

    copy(toutb);
    copy(doutb);
    if (rflag) {
        copy(troutb);
        copy(droutb);
    }
    if (! sflag) {
        if (! xflag)
            copy(soutb);
#if 0
        //TODO: write a symbol table.
        nlist_t *p;
        for (p=symtab; p<&symtab[nsymbols]; ++p)
            fputsym(p, outb);
#endif
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
