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
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <signal.h>
#include <archive.h>
#include <archive_entry.h>
#include "stdobj.h"

#define MAXSYMBOLS      2000    // max number of symbols
#define MAXNAMES        2000    // max number of names

//
// Input file.
//
FILE *input;
const char *inputname;

//
// List of input object files.
//
obj_image_t *obj_head, *obj_tail;
obj_image_t *arch_head, *arch_tail;

//
// Output object image.
//
obj_image_t aout;

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
int trace;                      // internal trace flag
int r_flag;                     // option -r as requested by user
int s_flag;                     // discard all symbols
int d_flag;                     // force allocation of common/private blocks
int o_flag;                     // output name specified
char *ofilename = "l.out";      // output file name
int emit_relocatable;           // generate relocatable output

unsigned text_base = 1;         // base address of code section
unsigned data_base;             // separate address space for data/BSS

unsigned max_set_address = 0;   // max address touched by SET directive

//
// Cumulative sizes set in pass 1 (in words).
//
int text_size, data_size, bss_size;
int tdata_size, set_size;

//
// Symbol relocation: passes 2 and 3.
//
int offset_text, offset_data, offset_bss;

int error_status;               // true when error found
char libname[256];
const char libpath[] = "/usr/local/lib/besm6/lib";
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
    if (!error_status)
        printf("%s: ", progname);

    if (inputname)
        printf("%s: ", inputname);

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

    error_status = 1;
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

//
// Convert a 16-bit unicode character to 6-bit TEXT encoding.
//
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
// Print the symbol table.
//
void dump_symtab()
{
    int i;
    uint64_t word;

    printf("--- Symbol table:\n");
    for (i = 0; i < nsymbols; i++) {
        word = symtab[i].u64;
        printf("--- %4d: %04o %04o %04o %04o\n",
            i, (unsigned)(word >> 36) & 07777, (unsigned)(word >> 24) & 07777,
            (unsigned)(word >> 12) & 07777, (unsigned)word & 07777);
    }
    printf("--- Names:\n");
    for (i = 0; i < nnames; i++) {
        word = nametab[i];
        printf("--- %4d: %04o %04o %04o %04o %s\n",
            i, (unsigned)(word >> 36) & 07777, (unsigned)(word >> 24) & 07777,
            (unsigned)(word >> 12) & 07777, (unsigned)word & 07777,
            text_to_utf(word));
    }
}

//
// Open object file.
// Return 0 in case of regular file,
// and 1 in case of archive.
//
int open_input(char *name, int libflag)
{
    uint64_t magic;

    input = 0;
    if (libflag) {
        strcpy(libname, libpath);
        strcat(libname, name);
        strcat(libname, ".a");
        inputname = libname;
        input = fopen(inputname, "r");
        if (! input)
            inputname += 4;
    } else {
        inputname = name;
    }
    if (! input && ! (input = fopen(inputname, "r")))
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
    case SYM_DENTRY_S:
    case SYM_EXT_S:
    case SYM_PRIVATE_S:
    case SYM_COMMON_S:
    case SYM_PPAGE_S:
    case SYM_CPAGE_S:
    case SYM_PSECT_S:
    case SYM_CSECT_S:
        // Short name.
        return text_to_utf(sp->u64 & 07777777700000000);

    case SYM_ENTRY_L:
    case SYM_DENTRY_L:
    case SYM_EXT_L:
    case SYM_PRIVATE_L:
    case SYM_COMMON_L:
    case SYM_PPAGE_L:
    case SYM_CPAGE_L:
    case SYM_PSECT_L:
    case SYM_CSECT_L:
        // Long name.
        return text_to_utf(nametab[sp->f.n_ref & 03777]);

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
    uint64_t sp_name;

    switch (sp->f.n_type) {
    case SYM_ENTRY_S:
    case SYM_DENTRY_S:
    case SYM_EXT_S:
    case SYM_COMMON_S:
    case SYM_CPAGE_S:
    case SYM_CSECT_S:
        // Short name.
        return (sp->u64 & 07777777700000000) == name;

    case SYM_ENTRY_L:
    case SYM_DENTRY_L:
    case SYM_EXT_L:
    case SYM_COMMON_L:
    case SYM_CPAGE_L:
    case SYM_CSECT_L:
        // Long name.
        sp_name = nametab[sp->f.n_ref & 03777];
        return sp_name == name;

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
    if (trace > 1)
        printf("--- Add name[%u] %s\n", nnames, text_to_utf(name));
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

    for (i=42; i>=0; i-=6) {
        uint16_t c = (uint8_t) *str;
        if (c)
            str++;

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

        name |= (uint64_t) unicode_to_text(c) << i;
    }
    return name;
}

//
// Relocate address for the object file.
//
unsigned relocate_address(obj_image_t *obj, unsigned addr)
{
    if (addr < obj->cmd_len) {
        return addr + offset_text;
    }
    if (addr < obj->cmd_len + obj->const_len) {
        return addr + offset_data;
    }
    if (addr < obj->cmd_len + obj->const_len + obj->bss_len) {
        return addr + offset_bss;
    }
    if (addr < obj->cmd_len + obj->const_len + obj->data_len) {
        // Transient data section.
        return addr + offset_bss;
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
    if (trace > 1)
        printf("--- Add extref[%u] %s\n", nsymbols, text_to_utf(name));

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
nlist_t *create_entry(uint64_t name, char type, unsigned addr)
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
                sp->f.n_type = (type == 't') ? SYM_ENTRY_L : SYM_DENTRY_L;
            } else {
                // Short name.
                sp->f.n_type = (type == 't') ? SYM_ENTRY_S : SYM_DENTRY_S;
            }
            sp->f.n_addr = addr;
            if (trace > 1)
                printf("--- Convert extref[%lu] %s into entry\n",
                    sp - symtab, text_to_utf(name));
            return sp;
        }
    }
    if (trace > 1)
        printf("--- Add entry[%u] %s\n", nsymbols, text_to_utf(name));

    // Allocate new symbol.
    if (nsymbols >= MAXSYMBOLS)
        fatal("Symbol table overflow");
    sp = &symtab[nsymbols];
    nsymbols++;
    if (name & 077777777) {
        // Long name.
        sp->f.n_type = (type == 't') ? SYM_ENTRY_L : SYM_DENTRY_L;
        sp->f.n_ref = create_name(name);
    } else {
        // Short name.
        sp->f.n_type = (type == 't') ? SYM_ENTRY_S : SYM_DENTRY_S;
        sp->f.n_ref = name >> 24;
    }
    sp->f.n_addr = addr;
    return sp;
}

//
// Create new symbol: relocatable address.
// Return index in symtab.
//
unsigned create_reloc(unsigned addr)
{
    nlist_t *sp;

    if (trace > 1)
        printf("--- Add reloc[%u] %05o\n", nsymbols, addr);

    // Allocate new symbol: external reference.
    if (nsymbols >= MAXSYMBOLS)
        fatal("Symbol table overflow");
    sp = &symtab[nsymbols];
    nsymbols++;
    sp->f.n_type = SYM_RELOC;
    sp->f.n_addr = addr;
    return sp - symtab;
}

//
// Merge symbols from the object file into a common symbol table.
//
void merge_symbols(obj_image_t *obj)
{
    nlist_t *sp;
    uint64_t name;
    unsigned addr;
    int i, type;

    if (trace > 1)
        printf("--- Merge %u entries and %u symbols from %s\n",
            obj->nentries, obj->sym_len, text_to_utf(obj->word[obj->table_off]));

    // Add entries.
    for (i = 0; i < obj->nentries; i++) {
        name = obj->word[2*i + 1];
        addr = obj->word[2*i + 2] & 077777;
        type = (addr < obj->cmd_len) ? 't' : 'd';
        addr = relocate_address(obj, addr);
        create_entry(name, type, addr);
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
            // Absolute address.
            if (trace > 1)
                printf("--- Add absolute address[%u] %05o\n", nsymbols, sym.f.n_addr);
            break;

        case SYM_CONST:
            // Constant value.
            if (trace > 1)
                printf("--- Add constant[%u] %05o\n", nsymbols, sym.f.n_addr);
            break;

        case SYM_RELOC:
            //
            // Relocatable address.
            //
            // Update the address field.
            if (sym.f.n_addr >= obj->cmd_len) {
                sym.f.n_type = SYM_DRELOC;
            }
            sym.f.n_addr = relocate_address(obj, sym.f.n_addr);
            if (trace > 1)
                printf("--- Relocate address[%u] %05o\n", nsymbols, sym.f.n_addr);
            break;

        case SYM_OFFSET:
            //
            // Offset from another symbol.
            //
            // Update the reference field with new index.
            old_ref = sym.f.n_ref & 03777;
            sym.f.n_ref = 04001 + obj->word[old_ref + obj->table_off];
            if (trace > 1)
                printf("--- Add offset[%u] %05o\n", nsymbols, sym.f.n_addr);
            break;

        case SYM_INDIRECT:
        case SYM_EXPRESSION:
            //
            // Dereference or expression.
            //
            // Update the address field with new index.
            old_ref = sym.f.n_addr;
            sym.f.n_addr = 1 + obj->word[old_ref + 1 + obj->table_off];
            if (trace > 1)
                printf("--- Add %s[%u] %05o\n", sym.f.n_type==SYM_INDIRECT ?
                    "indirect" : "expression", nsymbols, sym.f.n_addr);
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
            sym.f.n_ref = 04001 + obj->word[old_ref + obj->table_off];

            // Update the address field with new index.
            old_ref = sym.f.n_addr;
            sym.f.n_addr = 1 + obj->word[old_ref + obj->table_off];
            if (trace > 1)
                printf("--- Add arith op[%u]\n", nsymbols);
            break;

        case SYM_PRIVATE_S:
        case SYM_PPAGE_S:
        case SYM_PSECT_S:
            // Private block (short name).
            if (trace > 1)
                printf("--- Add private block[%u] %s size %u words\n",
                    nsymbols, text_to_utf(sym.u64 & 07777777700000000), sym.f.n_addr);
            break;

        case SYM_PRIVATE_L:
        case SYM_PPAGE_L:
        case SYM_PSECT_L:
            //
            // Private block (long name).
            //
            // Update the reference field with new name index.
            old_ref = sym.f.n_ref & 03777;
            name = obj->word[old_ref + obj->table_off];
            sym.f.n_ref = 04000 | create_name(name);
            if (trace > 1)
                printf("--- Add private block[%u] %s size %u words\n",
                    nsymbols, text_to_utf(name), sym.f.n_addr);
            break;

        case SYM_EXT_S:
            //
            // External reference (short name).
            //
            name = sym.u64 & 07777777700000000;
            sp = sym_find(name);
            if (sp) {
                // The symbol is already defined.
                // Redirect to it.
                *wp = sp - symtab;
                continue;
            }
            if (trace > 1)
                printf("--- Add extref[%u] %s\n", nsymbols, text_to_utf(name));
            break;

        case SYM_EXT_L:
            //
            // External reference (long name).
            //
            old_ref = sym.f.n_ref & 03777;
            name = obj->word[old_ref + obj->table_off];
            sp = sym_find(name);
            if (sp) {
                // The symbol is already defined.
                // Redirect to it.
                *wp = sp - symtab;
                continue;
            }
            // Update the reference field with new name index.
            sym.f.n_ref = 04000 | create_name(name);
            if (trace > 1)
                printf("--- Add extref[%u] %s\n", nsymbols, text_to_utf(name));
            break;

        case SYM_ENTRY_S:
        case SYM_DENTRY_S:
            //
            // Entry, relocatable (short name)
            //
            // Update the address field.
            sym.f.n_addr = relocate_address(obj, sym.f.n_addr);
            name = sym.u64 & 07777777700000000;
            sp = sym_find(name);
            if (sp) {
                // The symbol is already defined.
                if (sp->f.n_type != SYM_EXT_S)
                    fatal("Name %s redefined", text_to_utf(name));

                // Convert extref into entry.
                sp->f.n_type = sym.f.n_type;
                sp->f.n_addr = sym.f.n_addr;

                // Redirect to it.
                *wp = sp - symtab;
                continue;
            }
            if (trace > 1)
                printf("--- Add entry[%u] %s address %05o\n",
                    nsymbols, text_to_utf(name), sym.f.n_addr);
            break;

        case SYM_ENTRY_L:
        case SYM_DENTRY_L:
            //
            // Entry, relocatable (long name)
            //
            // Update the address field.
            sym.f.n_addr = relocate_address(obj, sym.f.n_addr);
            old_ref = sym.f.n_ref & 03777;
            name = obj->word[old_ref + obj->table_off];
            sp = sym_find(name);
            if (sp) {
                // The symbol is already defined.
                if (sp->f.n_type != SYM_EXT_L)
                    fatal("Name %s redefined", text_to_utf(name));

                // Convert extref into entry.
                sp->f.n_type = sym.f.n_type;
                sp->f.n_addr = sym.f.n_addr;

                // Redirect to it.
                *wp = sp - symtab;
                continue;
            }
            // Update the reference field with new name index.
            sym.f.n_ref = 04000 | create_name(name);
            if (trace > 1)
                printf("--- Add entry[%u] %s address %05o\n",
                    nsymbols, text_to_utf(name), sym.f.n_addr);
            break;

        case SYM_COMMON_S:
        case SYM_CPAGE_S:
        case SYM_CSECT_S:
            //
            // Common block (short name).
            //
            name = sym.u64 & 07777777700000000;
            sp = sym_find(name);
            if (sp) {
                // The symbol is already defined.
                if (sp->f.n_type == SYM_EXT_S) {
                    // Convert extref into common block.
                    sp->f.n_type = sym.f.n_type;
                    sp->f.n_addr = sym.f.n_addr;
                } else if (sp->f.n_type == SYM_COMMON_S ||
                           sp->f.n_type == SYM_CPAGE_S ||
                           sp->f.n_type == SYM_CSECT_S) {
                    // Common block size is max of two sizes.
                    if (sym.f.n_addr > sp->f.n_addr)
                        sp->f.n_addr = sym.f.n_addr;
                    // Alignment is max of two alignments.
                    if (sym.f.n_type == SYM_CPAGE_S)
                        sp->f.n_type = SYM_CPAGE_S;
                    else if (sp->f.n_type != SYM_CPAGE_S)
                        sp->f.n_type |= sym.f.n_type;
                }
                // Redirect to old symbol.
                *wp = sp - symtab;
                continue;
            }
            if (trace > 1)
                printf("--- Add common block[%u] %s size %u words\n",
                    nsymbols, text_to_utf(sym.u64 & 07777777700000000), sym.f.n_addr);
            break;

        case SYM_COMMON_L:
        case SYM_CPAGE_L:
        case SYM_CSECT_L:
            //
            // Common block (long name).
            //
            old_ref = sym.f.n_ref & 03777;
            name = obj->word[old_ref + obj->table_off];
            sp = sym_find(name);
            if (sp) {
                // The symbol is already defined.
                if (sp->f.n_type == SYM_EXT_L) {
                    // Convert extref into common block.
                    sp->f.n_type = sym.f.n_type;
                    sp->f.n_addr = sym.f.n_addr;
                } else if (sp->f.n_type == SYM_COMMON_L ||
                           sp->f.n_type == SYM_CPAGE_L ||
                           sp->f.n_type == SYM_CSECT_L) {
                    // Common block size is max of two sizes.
                    if (sym.f.n_addr > sp->f.n_addr)
                        sp->f.n_addr = sym.f.n_addr;
                    // Alignment is max of two alignments.
                    if (sym.f.n_type == SYM_CPAGE_L)
                        sp->f.n_type = SYM_CPAGE_L;
                    else if (sp->f.n_type != SYM_CPAGE_L)
                        sp->f.n_type |= sym.f.n_type;
                }
                // Redirect to old symbol.
                *wp = sp - symtab;
                continue;
            }
            // Update the reference field with new name index.
            sym.f.n_ref = 04000 | create_name(name);
            if (trace > 1)
                printf("--- Add common block[%u] %s size %u words\n",
                    nsymbols, text_to_utf(name), sym.f.n_addr);
            break;

        default:
            fatal("Unknown symbol type %03o", sym.f.n_type);
        }

        // Append to the symbol table.
        if (nsymbols >= MAXSYMBOLS)
            fatal("Symbol table overflow");
        symtab[nsymbols++] = sym;
    }
    if (trace > 1 && nsymbols > 0)
        dump_symtab();
}

//
// Merge only entries and external symbols, for pass 1.
// No need to compute addresses.
//
void merge_ext_symbols(obj_image_t *obj)
{
    nlist_t *sp;
    uint64_t name;
    int i;

    // Add entries.
    for (i = 0; i < obj->nentries; i++) {
        name = obj->word[2*i + 1];
        create_entry(name, 't', 0);
    }

    // Merge symbols.
    for (i = 0; i < obj->sym_len; i++) {
        nlist_t sym;
        unsigned old_ref;

        sym.u64 = obj->word[i + 1 + obj->table_off];
        switch (sym.f.n_type) {
        default:
            // Ignore.
            continue;

        case SYM_EXT_S:
            //
            // External reference (short name).
            //
            name = sym.u64 & 07777777700000000;
            sp = sym_find(name);
            if (sp) {
                // The symbol is already defined.
                continue;
            }
            break;

        case SYM_EXT_L:
            //
            // External reference (long name).
            //
            old_ref = sym.f.n_ref & 03777;
            name = obj->word[old_ref + obj->table_off];
            sp = sym_find(name);
            if (sp) {
                // The symbol is already defined.
                continue;
            }
            // Update the reference field with new name index.
            sym.f.n_ref = 04000 | create_name(name);
            break;

        case SYM_ENTRY_S:
        case SYM_DENTRY_S:
            //
            // Entry, relocatable (short name)
            //
            name = sym.u64 & 07777777700000000;
            sp = sym_find(name);
            if (sp) {
                // The symbol is already defined.
                // Convert extref into entry.
                sp->f.n_type = sym.f.n_type;
                continue;
            }
            break;

        case SYM_ENTRY_L:
        case SYM_DENTRY_L:
            //
            // Entry, relocatable (long name)
            //
            old_ref = sym.f.n_ref & 03777;
            name = obj->word[old_ref + obj->table_off];
            sp = sym_find(name);
            if (sp) {
                // The symbol is already defined.
                // Convert extref into entry.
                sp->f.n_type = sym.f.n_type;
                continue;
            }
            // Update the reference field with new name index.
            sym.f.n_ref = 04000 | create_name(name);
            break;
        }

        // Append to the symbol table.
        if (nsymbols >= MAXSYMBOLS)
            fatal("Symbol table overflow");
        symtab[nsymbols++] = sym;
    }
}

//
// Check whether the object file provides any symbols we need.
//
int need_this_obj(obj_image_t *obj)
{
    int i;
    uint64_t name;
    nlist_t *sp;

    if (obj->nentries > 0) {
        // Is any of proposed entries referenced by our module?
        for (i = 0; i < obj->nentries; i++) {
            name = obj->word[2*i + 1];
            sp = sym_find(name);
            if (sp && (sp->f.n_type == SYM_EXT_L ||
                       sp->f.n_type == SYM_EXT_S))
            {
                return 1;
            }
        }
        return 0;
    }

    // No entries in standard array; search for symbols of Entry type.
    for (i = 0; i < obj->sym_len; i++) {
        nlist_t sym;

        sym.u64 = obj->word[i + 1 + obj->table_off];
        if (sym.f.n_type == SYM_ENTRY_S ||
            sym.f.n_type == SYM_DENTRY_S)
            name = sym.u64 & 07777777700000000;
        else if (sym.f.n_type == SYM_ENTRY_L ||
                 sym.f.n_type == SYM_DENTRY_L)
            name = obj->word[(sym.f.n_ref & 03777) + obj->table_off];
        else
            continue;

        sp = sym_find(name);
        if (sp && (sp->f.n_type == SYM_EXT_L ||
                   sp->f.n_type == SYM_EXT_S))
        {
            return 1;
        }
    }
    return 0;
}

//
// Append object structure to the single-linked list.
//
void append_to_obj_list(obj_image_t *obj)
{
    obj->next = 0;

    if (obj_tail) {
        obj_tail->next = obj;
    } else {
        obj_head = obj;
    }
    obj_tail = obj;
}

void append_to_arch_list(obj_image_t *obj)
{
    if (!obj)
        fatal("Out of memory");
    obj->next = 0;

    if (arch_tail) {
        arch_tail->next = obj;
    } else {
        arch_head = obj;
    }
    arch_tail = obj;
}

//
// Load an object file.
// Accumulate the final image sizes: code, data and bss.
// Append the object image to the list.
//
void load1obj(obj_image_t *obj)
{
    if (!obj)
        fatal("Out of memory");
    if (trace > 1)
        printf("--- Size: cmd=%u, const=%u, bss=%u words\n",
            obj->cmd_len, obj->const_len, obj->bss_len);

    text_size += obj->cmd_len;
    data_size += obj->const_len;
    bss_size += obj->bss_len;
    tdata_size += obj->data_len;
    set_size += obj->set_len;

    merge_ext_symbols(obj);

    // Add image to the list.
    append_to_obj_list(obj);
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
// Load archive from file `input'.
// Put object images into list arch_head/arch_tail.
//
void load_archive()
{
    struct archive *a = archive_read_new();
    struct archive_entry *entry;
    obj_image_t *obj, img = {0};

    archive_read_support_filter_all(a);
    archive_read_support_format_all(a);
    archive_read_open(a, input, NULL, myread, NULL);
    for (;;) {
        int ret = archive_read_next_header(a, &entry);
        if (ret == ARCHIVE_EOF)
            break;

        if (ret == ARCHIVE_RETRY)
            continue;

        if (ret == ARCHIVE_WARN)
            warning("Archive warning");
        else if (ret != ARCHIVE_OK)
            fatal("Bad archive");

        const char *name = archive_entry_pathname(entry);
        unsigned nbytes = archive_entry_size(entry);
        char data[MAXSZ*6];

        if (nbytes > sizeof(data)) {
            fatal("Too long array entry");
        }
        if (archive_read_data(a, data, nbytes) != nbytes) {
            fatal("Read error");
        }
        if (trace > 2) {
            printf("--- Archive item %s, size %u bytes\n", name, nbytes);
        }
        if (obj_read_data(data, nbytes, &img) < 0) {
            fatal( "Bad format");
        }

        // Add image to the list.
        obj = obj_copy(&img);
        obj->filename = strdup(name);
        append_to_arch_list(obj);
    }
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
        obj_image_t img = {0};

        if (trace)
            printf("%s\n", fname);
        if (obj_read_fd(input, &img) < 0)
            fatal("Bad format");
        load1obj(obj_copy(&img));
    } else {
        //
        // Archive.
        //
        int need_this_archive;
        obj_image_t **nextp, *obj;

        load_archive();
        do {
            // Process all items in the arch list in sequence.
            // Finish the loop, when nothing useful found/.
            need_this_archive = 0;
            nextp = &arch_head;
            for (;;) {
                obj = *nextp;
                if (!obj) {
                    break;
                }

                if (need_this_obj(obj)) {
                    // This component has something useful for us.
                    need_this_archive = 1;

                    // Remove it from archive list and process.
                    *nextp = obj->next;
                    if (trace) {
                        printf("%s(%s)\n", inputname, obj->filename);
                    }
                    load1obj(obj);
                } else {
                    nextp = &obj->next;
                }
            }
        } while (need_this_archive);

        // Dispose the rest.
        while (arch_head) {
            obj = arch_head;
            arch_head = obj->next;
            if (obj->filename)
                free(obj->filename);
            free(obj);
        }
        arch_tail = 0;
    }
    fclose(input);
}

void usage(int retcode)
{
    printf("Linker for BESM-6 object files\n");
    printf("Usage:\n");
    printf("    %s [options] file...\n", progname);
    printf("Options:\n");
    printf("    -d              Force common symbols to be defined\n");
    printf("    -D address      Use a separate address space for data and BSS\n");
    printf("    -e symbol       Set entry address\n");
    printf("    -l name         Search for library `libname'\n");
    printf("    -o filename     Set output file name\n");
    printf("    -r              Generate relocatable output\n");
    printf("    -s              Strip all symbol information\n");
    printf("    -t              Trace names of linked files\n");
    printf("    -T address      Set base address (default 1)\n");
    printf("    -u symbol       Start with undefined reference to `symbol'\n");
    exit(retcode);
}

//
// Pass 1: Read input files and calculate section sizes.
//
void pass1(int argc, char **argv)
{
    for (;;) {
        inputname = 0;
        switch (getopt(argc, argv, "-dD:e:l:o:rstT:u:")) {
        case EOF:
            break;
        case 1:
            // Input file name.
            load1name(optarg, 0);
            continue;
        case 'd':
            // Force allocation of commons.
            d_flag++;
            continue;
        case 'D':
            // Separate address space for data.
            data_base = strtoul(optarg, 0, 0);
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
            r_flag++;
            text_base = 0;
            continue;
        case 's':
            // Strip all symbols.
            s_flag++;
            continue;
        case 't':
            // Enable tracing.
            trace++;
            if (trace == 2)
                printf("First pass:\n");
            continue;
        case 'T':
            // Set base address.
            text_base = strtoul(optarg, 0, 0);
            continue;
        case 'u':
            // Mark `symbol' as undefined.
            create_extref(optarg);
            continue;
        default:
            usage(-1);
        }
        break;
    }
    if (trace > 1)
        printf("--- Total size: text=%u, data=%u, bss=%u words\n",
            text_size, data_size, bss_size);

    // Forget the symbol table, created at pass 1.
    nsymbols = 0;
    nnames = 0;
    inputname = 0;
}

//
// Pass 2: build the symbol table.
// Allocate common and private blocks.
// Define _etext, _edata and _end symbols.
//
void pass2()
{
    obj_image_t *obj;
    nlist_t *sp;
    int text_origin, data_origin, bss_origin, cblock_origin;
    uint64_t name_etext = 0, name_bdata = 0, name_edata = 0, name_end = 0;

    if (trace > 1)
        printf("Second pass:\n");
    text_origin = text_base;
    data_origin = (data_base == 0) ? (text_origin + text_size) : data_base;
    bss_origin = data_origin + data_size;
    cblock_origin = bss_origin + bss_size;

    //
    // Now set symbols to their final value.
    //
    for (obj = obj_head; obj; obj = obj->next) {
        if (trace > 1)
            printf("%s\n", text_to_utf(obj->word[obj->table_off]));

        // Compute offsets for symbol relocation.
        offset_text = text_origin;
        offset_data = data_origin - obj->cmd_len;
        offset_bss = bss_origin - obj->cmd_len - obj->const_len;

        if (trace > 1)
            printf("--- Offsets: text %+d, data %+d, bss %+d words\n",
                offset_text, offset_data, offset_bss);

        // Merge symbols into a common symbol table.
        merge_symbols(obj);

        text_origin += obj->cmd_len;
        data_origin += obj->const_len;
        bss_origin += obj->bss_len;
    }

    //
    // If there are any undefined symbols, preserve the relocation info.
    //
    if (r_flag)
        emit_relocatable = 1;
    if (!emit_relocatable) {
        name_etext = utf_to_text("_etext");
        name_bdata = utf_to_text("_bdata");
        name_edata = utf_to_text("_edata");
        name_end = utf_to_text("_end");

        for (sp=symtab; sp<&symtab[nsymbols]; sp++) {
            if ((sp->f.n_type == SYM_EXT_S ||
                 sp->f.n_type == SYM_EXT_L) &&
                !sym_name_match(sp, name_end) &&
                !sym_name_match(sp, name_bdata) &&
                !sym_name_match(sp, name_edata) &&
                !sym_name_match(sp, name_etext))
            {
                // Undefined symbols found.
                // Switch to relocatable output.
                emit_relocatable = 1;

                // Don't allocate commons in this case.
                d_flag = 0;
                break;
            }
        }
    }
    if (emit_relocatable) {
        // Don't strip symbols.
        s_flag = 0;
    }

    //
    // Allocate common and private blocks.
    //
    if (d_flag || !emit_relocatable) {
        // Put unaligned blocks first.
        for (sp=symtab; sp<&symtab[nsymbols]; sp++) {
            unsigned size = sp->f.n_addr;

            switch (sp->f.n_type) {
            case SYM_COMMON_S:
            case SYM_COMMON_L:
            case SYM_PRIVATE_S:
            case SYM_PRIVATE_L:
                sp->f.n_addr = cblock_origin;
                cblock_origin += size;
                bss_size += size;
                break;
            }
        }
        // Then sector-aligned blocks.
        for (sp=symtab; sp<&symtab[nsymbols]; sp++) {
            unsigned size = sp->f.n_addr;
            int gap;

            switch (sp->f.n_type) {
            case SYM_CSECT_S:
            case SYM_CSECT_L:
            case SYM_PSECT_S:
            case SYM_PSECT_L:
                gap = -cblock_origin;
                cblock_origin = (cblock_origin + 255) / 256 * 256;
                gap += cblock_origin;

                sp->f.n_addr = cblock_origin;
                cblock_origin += size;
                bss_size += gap + size;
                break;
            }
        }
        // Finally page-aligned blocks.
        for (sp=symtab; sp<&symtab[nsymbols]; sp++) {
            unsigned size = sp->f.n_addr;
            int gap;

            switch (sp->f.n_type) {
            case SYM_CPAGE_S:
            case SYM_CPAGE_L:
            case SYM_PPAGE_S:
            case SYM_PPAGE_L:
                // Page aligned.
                gap = -cblock_origin;
                cblock_origin = (cblock_origin + 1023) / 1024 * 1024;
                gap += cblock_origin;

                sp->f.n_addr = cblock_origin;
                cblock_origin += size;
                bss_size += gap + size;
                break;
            }
        }
    }
    if (data_base == 0) {
        // Shared address space.
        if (text_base + text_size + data_size + bss_size > 077777)
            fatal("Program size %u words: memory overflow",
                text_base + text_size + data_size + bss_size);
    } else {
        // Separate address space.
        if (text_base + text_size > 077777)
            fatal("Code size %u words: memory overflow",
                text_base + text_size);
        if (data_base + data_size + bss_size > 077777)
            fatal("Data size %u words: memory overflow",
                data_base + data_size + bss_size);
    }

    if (!emit_relocatable) {
        // Define _etext, _edata and _end symbols.
        create_entry(name_etext, 't', text_base + text_size);
        if (data_base == 0) {
            // Shared address space.
            create_entry(name_edata, 'd', text_base + text_size + data_size);
            create_entry(name_end, 'b', text_base + text_size + data_size + bss_size);
            if (trace > 1)
                printf("--- /etext = %05o, /edata = %05o, /end = %05o\n",
                    text_base + text_size,
                    text_base + text_size + data_size,
                    text_base + text_size + data_size + bss_size);
        } else {
            // Separate address space.
            create_entry(name_bdata, 'd', data_base);
            create_entry(name_edata, 'd', data_base + data_size);
            create_entry(name_end, 'b', data_base + data_size + bss_size);
            if (trace > 1)
                printf("--- /etext = %05o, /bdata = %05o, /edata = %05o, /end = %05o\n",
                    text_base + text_size, data_base,
                    data_base + data_size,
                    data_base + data_size + bss_size);
        }
    }

    if (!r_flag) {
        // Print undefined symbols, if any.
        unsigned undef_count = 0;

        for (sp=symtab; sp<&symtab[nsymbols]; sp++) {
            switch (sp->f.n_type) {
            case SYM_EXT_S:
            case SYM_EXT_L:
                if (!undef_count)
                    printf("Undefined:\n");
                undef_count++;
                printf("    %s\n", sym_name(sp));
                error_status = 1;
                break;
            }
        }
    }
    if (trace > 1 && (d_flag || !emit_relocatable))
        dump_symtab();
    if (nsymbols + nnames >= 03777)
        fatal("Total %u symbols: symbol table overflow", nsymbols + nnames);
}

//
// Compute a final value of symbol.
//
unsigned sym_eval(unsigned index)
{
    nlist_t *sp = &symtab[index];
    unsigned ref, val;

    switch (sp->f.n_type) {
    default:
        return sp->f.n_addr;

    case SYM_RELOC:
    case SYM_DRELOC:
        // Relocatable address.
        return sp->f.n_addr;

    case SYM_OFFSET:
        // Offset from another symbol.
        // Update the reference field with new index.
        ref = (sp->f.n_ref & 03777) - 1;
        return sym_eval(ref) + sp->f.n_addr;

    case SYM_INDIRECT:
        // Dereference.
        ref = sym_eval(sp->f.n_addr);
        if (data_base == 0) {
            // Shared address space.
            if (ref < text_base || ref >= text_base + text_size + data_size)
                fatal("Indirect symbol out of text+data section: %05o", ref);
            return aout.word[11 + ref - text_base] & 077777;
        } else {
            // Separate address space.
            if (ref < data_base || ref >= data_base + data_size)
                fatal("Indirect symbol out of data section: %05o", ref);
            return aout.word[11 + text_size + ref - data_base] & 077777;
        }

    case SYM_EXPRESSION:
        // Expression.
        ref = sp->f.n_addr & 03777;
        return sym_eval(ref);

    case SYM_ADD:
        // Add two symbols.
        ref = (sp->f.n_ref & 03777) - 1;
        return sym_eval(ref) + sym_eval(sp->f.n_addr);

    case SYM_SUBTRACT:
        // Subtract two symbols.
        ref = (sp->f.n_ref & 03777) - 1;
        return sym_eval(ref) + sym_eval(sp->f.n_addr);

    case SYM_MULTIPLY:
        // Multiply two symbols.
        ref = (sp->f.n_ref & 03777) - 1;
        return sym_eval(ref) * sym_eval(sp->f.n_addr);

    case SYM_DIVIDE:
        // Divide two symbols.
        ref = (sp->f.n_ref & 03777) - 1;
        val = sym_eval(sp->f.n_addr);
        if (val == 0)
            fatal("Divide by zero symbol #%u", sp->f.n_addr);
        return sym_eval(ref) / val;
    }
}

//
// Compute a source address of SET directive.
//
unsigned sym_eval_tdata(unsigned index)
{
    nlist_t *sp = &symtab[index];
    unsigned ref, addr;

    switch (sp->f.n_type) {
    default:
        return sp->f.n_addr;

    case SYM_RELOC:
    case SYM_DRELOC:
        // Relocatable address.
        // If address is in BSS, meaning transient data:
        // relocate it relative to the initialized data section.
        addr = sp->f.n_addr;
        if (data_base == 0) {
            // Shared address space.
            if (addr >= text_base + text_size + data_size)
                addr -= data_size;
        } else {
            // Separate address space.
            if (addr >= data_base + data_size)
                addr -= data_size;
        }
        return addr;

    case SYM_OFFSET:
        // Offset from another symbol.
        // Update the reference field with new index.
        ref = (sp->f.n_ref & 03777) - 1;
        return sym_eval_tdata(ref) + sp->f.n_addr;
    }
}

//
// Relocate an instruction.
//
unsigned relocate_cmd(obj_image_t *obj, unsigned cmd)
{
    unsigned addr, index;

    if (cmd & 02000000) {
        // Long address.
        addr = cmd & 077777;
        if ((addr & 074000) == 074000) {
            index = obj->word[(cmd & 03777) + obj->table_off];
            if (emit_relocatable) {
                // Update symbol index.
                addr = 074001 + index;
            } else {
                addr = sym_eval(index);
            }
            cmd = (cmd & ~077777) | addr;
        } else if (cmd & 040000) {
            addr = relocate_address(obj, addr & 037777);
            if (emit_relocatable) {
                if (addr & 040000) {
                    // Address out of range.
                    // Need to use SYM_RELOC.
                    addr = 074001 + create_reloc(addr);
                } else {
                    addr |= 040000;
                }
            }
            cmd = (cmd & ~077777) | addr;
        }
    } else {
        // Short address.
        if (cmd & 04000) {
            index = obj->word[(cmd & 03777) + obj->table_off];
            if (emit_relocatable) {
                // Update symbol index.
                addr = 04001 + index;
            } else {
                addr = sym_eval(index);
            }
            cmd = (cmd & ~07777) | addr;
        }
    }
    return cmd;
}

//
// Relocate a section of code.
//
void relocate_code(obj_image_t *obj, uint64_t *to, uint64_t *from, unsigned nwords)
{
    unsigned a, b;

    for (; nwords > 0; nwords--, to++, from++) {
        a = relocate_cmd(obj, (*from >> 24) & 077777777);
        b = relocate_cmd(obj, *from & 077777777);
        *to = (uint64_t)a << 24 | b;
    }
}

//
// Relocate a `set` address.
//
unsigned relocate_ref(obj_image_t *obj, unsigned ref)
{
    if (ref & 04000) {
        // Update symbol index.
        ref = 04001 + obj->word[(ref & 03777) + obj->table_off];
    }
    return ref;
}

//
// Relocate a section of `set' instructions.
//
void relocate_set_section(obj_image_t *obj, uint64_t *to, uint64_t *from, unsigned nwords)
{
    unsigned a, b;

    for (; nwords > 0; nwords--, to++, from++) {
        a = relocate_ref(obj, (*from >> 24) & 07777);
        b = relocate_ref(obj, *from & 07777);
        *to = (*from & 07777000077770000) | (uint64_t)a << 24 | b;
    }
}

//
// Relocate a section of `set' instructions.
//
void apply_set_instructions(obj_image_t *obj, unsigned tdata_base,
    uint64_t *cmd, int ncommands, uint64_t *tdata, int ndata)
{
    if (trace > 1) {
        printf("--- %s set directives: ncommands = %u, ndata = %u, tdata_base = %05o\n",
            text_to_utf(obj->word[obj->table_off]), ncommands, ndata, tdata_base);
    }
    for (; ncommands > 0; ncommands--, cmd++) {
        unsigned size  = (*cmd >> 36) & 07777;
        unsigned from  = (*cmd >> 24) & 07777;
        unsigned count = (*cmd >> 12) & 07777;
        unsigned to    = *cmd & 07777;
        unsigned sym_index, i, to_offset, from_offset;

        if (trace > 2)
            printf("--- %04o %04o %04o %04o\n", size, from, count, to);

        //
        // Compute destination address.
        //
        if (to & 04000) {
            sym_index = obj->word[(to & 03777) + obj->table_off];
            to = sym_eval(sym_index);
            if (trace > 2)
                printf("--- destination symbol mapped to index %u, value %05o\n",
                    sym_index, to);
        } else {
            fatal("Wrong destination address in SET directive: %05o", to);
        }
        if (data_base == 0) {
            // Shared address space.
            if (to < text_base ||
                to + size*count > text_base + text_size + data_size + bss_size)
                fatal("Wrong destination in SET directive: %05o...%05o",
                    to, to + size*count - 1);
        } else {
            // Separate address space.
            if (to < data_base ||
                to + size*count > data_base + data_size + bss_size)
                fatal("Wrong destination in SET directive: %05o...%05o",
                    to, to + size*count - 1);
        }
        to_offset = (data_base == 0) ?
                    (to - text_base) :
                    (to - data_base + text_size);

        //
        // Compute source address and copy the data.
        // When source address is 0, just zero out the destination.
        //
        if (from & 04000) {
            sym_index = obj->word[(from & 03777) + obj->table_off];
            from = sym_eval_tdata(sym_index);

            if (trace > 2)
                printf("--- source symbol mapped to index %u, value %05o\n",
                    sym_index, from);

            if (symtab[sym_index].f.n_type == SYM_RELOC) {
                //
                // Copy from text section.
                //
                if (trace > 1) {
                    printf("--- copy %u words from code %05o to %05o", size, from, to);
                    if (count != 1)
                        printf(", replicate %u times", count);
                    printf("\n");
                }
                if (from < text_base || from + size > text_base + text_size)
                    fatal("Source address in SET directive out of code section");

                for (i = 0; i < count; i++) {
                    memcpy(&aout.word[11 + to - text_base + i*size],
                        &aout.word[11 + from - text_base],
                        size * sizeof(uint64_t));
                }
            } else if (from >= tdata_base) {
                //
                // Copy from transient data section.
                //
                from -= tdata_base;
                if (trace > 1) {
                    printf("--- copy %u words from tdata[%#o] to %05o", size, from, to);
                    if (count != 1)
                        printf(", replicate %u times", count);
                    printf("\n");
                }
                if (from + size > ndata)
                    fatal("Source address in SET directive exceeds size of DATA section");

                for (i = 0; i < count; i++) {
                    memcpy(&aout.word[11 + to_offset + i*size],
                        &tdata[from], size * sizeof(uint64_t));
                }
            } else {
                //
                // Copy from const section.
                //
                if (trace > 1) {
                    printf("--- copy %u words from const %05o to %05o", size, from, to);
                    if (count != 1)
                        printf(", replicate %u times", count);
                    printf("\n");
                }
                if (data_base == 0) {
                    // Shared address space.
                    if (from < text_base + text_size || from + size > text_base + text_size + data_size)
                        fatal("Source address in SET directive out of const section");
                    from_offset = from - text_base;
                } else {
                    // Separate address space.
                    if (from < data_base || from + size > data_base + data_size)
                        fatal("Source address in SET directive out of const section");
                    from_offset = from - data_base + text_size;
                }
                for (i = 0; i < count; i++) {
                    memcpy(&aout.word[11 + to_offset + i*size],
                        &aout.word[11 + from_offset],
                        size * sizeof(uint64_t));
                }
            }
        } else if (from == 0) {
            // Clear destination.
            if (size != 1)
                fatal("Wrong size in SET directive: %u", size);
            if (trace > 1)
                printf("--- clear %u words at %05o\n", count, to);

            memset(&aout.word[11 + to_offset], 0, size * sizeof(uint64_t));
        } else {
            fatal("Wrong source address in SET directive: %05o", from);
        }

        // May need to move BSS base.
        if (to + size*count > max_set_address)
            max_set_address = to + size*count;
    }
}

//
// Relocate a debug section.
//
unsigned relocate_debug(obj_image_t *obj, uint64_t *from, unsigned nwords)
{
    unsigned nprocessed = 0;
    nlist_t *sp;

    for (; nwords > 1; nwords -= 2, from += 2) {
        // Ignore everything after name '////////'.
        if (from[0] == 0x3cf3cf3cf3cf)
            break;

        sp = (nlist_t*) &from[1];
        if (sp->f.n_type == 0 && sp->f.n_ref == 01000000) {
            // Fix incompatibility of Fortran-GDR.
            sp->f.n_type = SYM_RELOC & 0277;
            sp->f.n_ref = 0;
        }

        if (sp->f.n_type == (SYM_RELOC & 0277)) {
            sp->f.n_addr = relocate_address(obj, sp->f.n_addr);
        } else {
            int old_index = sp->f.n_addr & 03777;
            int new_index = obj->word[old_index + obj->table_off];

            if (emit_relocatable) {
                // Update symbol index.
                sp->f.n_addr = 04001 + new_index;
            } else {
                // Compute final address.
                sp->f.n_addr = sym_eval(new_index);
            }
        }
        nprocessed += 2;
    }
    return nprocessed;
}

//
// Copy a debug section.
//
unsigned copy_debug(uint64_t *to, uint64_t *from, unsigned nwords)
{
    unsigned ncopied = 0;

    for (; nwords > 1; nwords -= 2) {
        // Ignore everything after name '////////'.
        if (*from == 0x3cf3cf3cf3cf)
            break;

        *to++ = *from++;
        *to++ = *from++;
        ncopied += 2;
    }
    return ncopied;
}

//
// Pass 3: Relocate the code.
// Build the output object image.
//
void pass3()
{
    int text_origin, data_origin, bss_origin;
    int tdata_origin, set_origin, debug_size = 0;
    obj_image_t *obj;
    uint64_t *aout_datap, *aout_tdatap, *aout_setp;

    if (trace > 1) {
        printf("Third pass:\n");
    }
    text_origin = text_base;
    data_origin = (data_base == 0) ? (text_origin + text_size) : data_base;
    bss_origin = data_origin + data_size;
    tdata_origin = bss_origin;
    set_origin = tdata_origin + tdata_size;

    aout_datap  = &aout.word[11 + text_size];
    aout_tdatap = &aout.word[11 + text_size + data_size];
    aout_setp   = &aout.word[11 + text_size + data_size + tdata_size];

    for (obj = obj_head; obj; obj = obj->next) {
        if (trace > 1)
            printf("%s\n", text_to_utf(obj->word[obj->table_off]));

        // Compute offsets for code relocation.
        offset_text = text_origin;
        offset_data = data_origin - obj->cmd_len;
        offset_bss = bss_origin - obj->cmd_len - obj->const_len;

        if (trace > 1)
            printf("--- Offsets: text %+d, data %+d, bss %+d words\n",
                offset_text, offset_data, offset_bss);

        // Relocate text section.
        if (obj->cmd_len > 0) {
            if (trace > 1)
                printf("--- text %u words\n", obj->cmd_len);
            relocate_code(obj, &aout.word[11 + text_origin - text_base],
                &obj->word[obj->cmd_off], obj->cmd_len);
        }
        // Copy data section.
        if (obj->const_len > 0) {
            if (trace > 1)
                printf("--- data %u words\n", obj->const_len);
            memcpy(aout_datap,
                &obj->word[obj->cmd_off + obj->cmd_len],
                obj->const_len * sizeof(uint64_t));
        }
        // Copy transient data section.
        if (obj->data_len > 0) {
            if (trace > 1)
                printf("--- transient data %u words\n", obj->data_len);
            if (emit_relocatable) {
                memcpy(aout_tdatap,
                    &obj->word[obj->cmd_off + obj->cmd_len + obj->const_len],
                    obj->data_len * sizeof(uint64_t));
            }
        }
        // Copy SET section.
        if (obj->set_len > 0) {
            if (trace > 1)
                printf("--- `set' %u words\n", obj->set_len);
            if (emit_relocatable) {
                // Relocate `set' instructions.
                relocate_set_section(obj, aout_setp,
                    &obj->word[obj->cmd_off + obj->cmd_len + obj->const_len + obj->data_len],
                    obj->set_len);
            } else {
                // Perform data transter.
                apply_set_instructions(obj, data_origin + obj->const_len,
                    &obj->word[obj->cmd_off + obj->cmd_len + obj->const_len + obj->data_len],
                    obj->set_len,
                    &obj->word[obj->cmd_off + obj->cmd_len + obj->const_len],
                    obj->data_len);
            }
        }
        // Relocate debug section.
        if (!s_flag && obj->debug_len > 0) {
            if (trace > 1)
                printf("--- debug info %u words\n", obj->debug_len);
            debug_size += relocate_debug(obj,
                &obj->word[obj->debug_off], obj->debug_len);
        }
        text_origin += obj->cmd_len;
        data_origin += obj->const_len;
        bss_origin += obj->bss_len;
        tdata_origin += obj->data_len;
        set_origin += obj->set_len;

        aout_datap  += obj->const_len;
        aout_tdatap += obj->data_len;
        aout_setp   += obj->set_len;
    }

    // As a result of SET directives, the boundary between
    // initialized and uninitialized data can change.
    int offset = (data_base == 0) ?
                 max_set_address - text_base - text_size - data_size :
                 max_set_address - data_base - data_size;
    if (offset > 0) {
        if (offset > bss_size)
            fatal("Max SET address out of BSS range");
        data_size += offset;
        bss_size -= offset;
        if (trace > 1)
            printf("--- extend data segment by %d words up to %05o\n",
                offset, (data_base == 0) ?
                    text_base + text_size + data_size :
                    data_base + data_size);
    }

    //
    // Fill output header.
    //
    aout.head_len  = 1;
    aout.sym_len   = s_flag ? 0 : nsymbols;
    aout.debug_len = s_flag ? 0 : debug_size;
    aout.set_len   = emit_relocatable ? set_size : 0;
    aout.data_len  = emit_relocatable ? tdata_size : 0;
    aout.long_len  = s_flag ? 0 : nnames;
    aout.cmd_len   = text_size;
    aout.bss_len   = bss_size;
    aout.const_len = data_size;
    aout.text_base = emit_relocatable ? 0 : text_base;
    aout.data_base = emit_relocatable ? 0 : data_base;
    if (emit_relocatable) {
        aout.entry = 0;
    } else if (entrypt) {
        aout.entry = entrypt->f.n_addr;
        if (aout.entry < text_base || aout.entry >= text_base + text_size)
            error("Entry out of text segment");
    } else {
        aout.entry = text_base;
    }

    aout.nwords = 1 + 10 + text_size + data_size + 1;
    if (emit_relocatable)
        aout.nwords += set_size + tdata_size;

    aout.head_off = 1;
    aout.cmd_off  = aout.head_off + 10;
    aout.table_off = aout.cmd_off + aout.cmd_len + aout.const_len + aout.data_len + aout.set_len;
    aout.long_off = aout.table_off + aout.head_len + aout.sym_len;
    aout.debug_off = aout.long_off + aout.long_len;

    // Set module name.
    aout.word[aout.table_off] = obj_head->word[obj_head->table_off];

    // Copy symbol table and name table.
    if (!s_flag) {
        aout.nwords += nsymbols + nnames + debug_size;

        // Update references to long names.
        nlist_t *sp;
        for (sp=symtab; sp<&symtab[nsymbols]; sp++) {
            switch (sp->f.n_type) {
            case SYM_ENTRY_L:
            case SYM_DENTRY_L:
            case SYM_EXT_L:
            case SYM_PRIVATE_L:
            case SYM_COMMON_L:
            case SYM_PPAGE_L:
            case SYM_CPAGE_L:
            case SYM_PSECT_L:
            case SYM_CSECT_L:
                sp->f.n_ref = (sp->f.n_ref & 03777) + nsymbols + 04001;
                break;
            }
        }

        if (nsymbols > 0)
            memcpy(&aout.word[1 + aout.table_off], &symtab[0],
                nsymbols * sizeof(uint64_t));

        if (nnames > 0)
            memcpy(&aout.word[aout.long_off], &nametab[0],
                nnames * sizeof(uint64_t));

        // Copy debug section.
        unsigned ndebug = 0;
        for (obj = obj_head; obj; obj = obj->next) {
            if (obj->debug_len > 0) {
                ndebug += copy_debug(&aout.word[aout.debug_off + ndebug],
                    &obj->word[obj->debug_off], obj->debug_len);
            }
        }
    }
}

//
// Write output file.
//
void emit()
{
    FILE *fd = fopen(ofilename, "wb");
    if (! fd)
        fatal("Cannot create output file");
    if (obj_write(fd, &aout) < 0)
        error("Write error");
    fclose(fd);

    if (!o_flag) {
        // Rename a.out into l.out.
        unlink("a.out");
        if (link("l.out", "a.out") < 0)
            fatal("Cannot link l.out to a.out");
        ofilename = "a.out";
    }
    unlink("l.out");
    if (!error_status && !r_flag) {
        // No undefined symbols and no -r option: make output executable.
        chmod(ofilename, 0777 & ~umask(0));
    }
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
    // First pass: compute segment sizes and entry point.
    //
    pass1(argc, argv);

    //
    // Second pass: relocate symbols.
    //
    pass2();

    //
    // Third pass: relocate the code.
    //
    pass3();

    //
    // Emit output file.
    //
    emit();
    return 0;
}
