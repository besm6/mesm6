/*
 * Show contents of BESM-6 object file.
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
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "stdobj.h"

#define WSZ 6           // word size in bytes

const char *lcmd_bemsh[16] = {
    "э20",  "э21",  "мода", "мод",  "уиа",  "слиа", "по",   "пе",
    "пб",   "пв",   "выпр", "стоп", "пио",  "пино", "э36",  "цикл",
};

const char *lcmd_madlen[16] = {
    "*20",  "*21",  "utc",  "wtc",  "vtm",  "utm",  "uza",  "u1a",
    "uj",   "vjm",  "ij",   "stop", "vzm",  "v1m",  "*36",  "vlm",
};

const char *scmd_bemsh[64] = {
    "зп",   "зпм",  "рег",  "счм",  "сл",   "вч",   "вчоб", "вчаб",
    "сч",   "и",    "нтж",  "слц",  "знак", "или",  "дел",  "умн",
    "сбр",  "рзб",  "чед",  "нед",  "слп",  "вчп",  "сд",   "рж",
    "счрж", "счмр", "уис",  "счис", "слпа", "вчпа", "сда",  "ржа",
    "уи",   "уим",  "счи",  "счим", "уии",  "сли",  "э46",  "э47",
    "э50",  "э51",  "э52",  "э53",  "э54",  "э55",  "э56",  "э57",
    "э60",  "э61",  "э62",  "э63",  "э64",  "э65",  "э66",  "э67",
    "э70",  "э71",  "э72",  "э73",  "э74",  "э75",  "э76",  "э77",
};

const char *scmd_madlen[64] = {
    "atx",  "stx",  "*02",  "xts",  "a+x",  "a-x",  "x-a",  "amx",
    "xta",  "aax",  "aex",  "arx",  "avx",  "aox",  "a/x",  "a*x",
    "apx",  "aux",  "acx",  "anx",  "e+x",  "e-x",  "asx",  "xtr",
    "rte",  "yta",  "atz",  "zta",  "e+n",  "e-n",  "asn",  "ntr",
    "ati",  "sti",  "ita",  "its",  "mtj",  "j+m",  "*46",  "*47",
    "*50",  "*51",  "*52",  "*53",  "*54",  "*55",  "*56",  "*57",
    "*60",  "*61",  "*62",  "*63",  "*64",  "*65",  "*66",  "*67",
    "*70",  "*71",  "*72",  "*73",  "*74",  "*75",  "*76",  "*77",
};

const char **long_name = lcmd_madlen, **short_name = scmd_madlen;
int debug_flag;
int raw_flag;
const char *progname;

static const char *text_to_utf[] = {
    " ", ".", "Б", "Ц", "Д", "Ф", "Г", "И",
    "(", ")", "*", "Й", "Л", "Я", "Ж", "/",
    "0", "1", "2", "3", "4", "5", "6", "7",
    "8", "9", "Ь", ",", "П", "-", "+", "Ы",
    "З", "A", "B", "C", "D", "E", "F", "G",
    "H", "I", "J", "K", "L", "M", "N", "O",
    "P", "Q", "R", "S", "T", "U", "V", "W",
    "X", "Y", "Z", "Ш", "Э", "Щ", "Ч", "Ю",
};

const char *getlabel(char ch, int n)
{
    static char buf[16];

    snprintf(buf, sizeof(buf), "%c%o", ch, n);
    return buf;
}

int getiso(uint64_t word, int n)
{
    int c = word >> (40 - n * 8);
    c &= 0377;
    if (c >= ' ' && c <= '~')
        return c;
    return '.';
}

const char *getsymtext(uint64_t word, int n)
{
    int c = word >> (42 - n * 6);
    c &= 077;
    return text_to_utf[c];
}

void print_word_as_text(uint64_t word)
{
    int i, c;

    for (i = 42; i >= 0; i -= 6) {
        c = (word >> i) & 077;
        if (c == 0)
            break;
        fputs(text_to_utf[c], stdout);
    }
}

void text_to_buf(char *p, uint64_t word)
{
    int i, c;

    for (i = 42; i >= 0; i -= 6) {
        c = (word >> i) & 077;
        if (c == 0)
            break;
        strcpy(p, text_to_utf[c]);
        p += strlen(p);
    }
    *p = 0;
}

static const char *octaldots(unsigned n)
{
    static char buf[8];

    n &= 07777;
    if (n == 0)
        return "....";
    snprintf(buf, sizeof(buf), "%04o", n);
    return buf;
}

const char *getsyminfo(obj_image_t *obj, uint64_t word, int verbose_flag, int transient_flag)
{
    static char buf[64];
    char buf2[64];
    unsigned type = (word >> 15) & 0777;
    unsigned addr = word & 077777;
    unsigned ref  = (word >> 24) & 03777;

    switch (type) {
    case SYM_OFFSET:
        // Offset from another symbol.
        getsyminfo(obj, obj->word[ref + obj->table_off], 0, transient_flag);
        strcat(buf, verbose_flag ? " + " : "+");
        sprintf(buf + strlen(buf), "%o", addr);
        break;

    case SYM_ADD:
        // Add two symbols.
        // Second symbol.
        getsyminfo(obj, obj->word[ref + obj->table_off], 0, transient_flag);
        strcpy(buf2, buf);

        // First symbol.
        getsyminfo(obj, obj->word[addr + obj->table_off], 0, transient_flag);
        strcat(buf, verbose_flag ? " + " : "+");
        strcat(buf, buf2);
        break;

    case SYM_SUBTRACT:
        // Subtract two symbols.
        // Second symbol.
        getsyminfo(obj, obj->word[addr + obj->table_off], 0, transient_flag);
        strcpy(buf2, buf);

        // First symbol.
        getsyminfo(obj, obj->word[ref + obj->table_off], 0, transient_flag);
        strcat(buf, verbose_flag ? " - " : "-");
        strcat(buf, buf2);
        break;

    case SYM_MULTIPLY:
        // Multiply two symbols.
        // Second symbol.
        getsyminfo(obj, obj->word[addr + obj->table_off], 0, transient_flag);
        strcpy(buf2, buf);

        // First symbol.
        getsyminfo(obj, obj->word[ref + obj->table_off], 0, transient_flag);
        strcat(buf, verbose_flag ? " * " : "*");
        strcat(buf, buf2);
        break;

    case SYM_DIVIDE:
        // Divide two symbols.
        // Second symbol.
        getsyminfo(obj, obj->word[addr + obj->table_off], 0, transient_flag);
        strcpy(buf2, buf);

        // First symbol.
        getsyminfo(obj, obj->word[ref + obj->table_off], 0, transient_flag);
        strcat(buf, verbose_flag ? " / " : "/");
        strcat(buf, buf2);
        break;

    case SYM_INDIRECT:
        // Dereference, like *ptr.
        getsyminfo(obj, obj->word[addr + obj->table_off], verbose_flag, transient_flag);
        memmove(buf+1, buf, 1 + strlen(buf));
        buf[0] = '[';
        strcat(buf, "]");
        break;

    case SYM_EXPRESSION:
        // Expression to compute at load time.
        getsyminfo(obj, obj->word[addr + obj->table_off], verbose_flag, transient_flag);
        memmove(buf+1, buf, 1 + strlen(buf));
        buf[0] = '(';
        strcat(buf, ")");
        break;

    case SYM_ABS:
    case SYM_CONST:
        // Absolute address.
        snprintf(buf, sizeof(buf), "%o", addr);
        break;

    case SYM_RELOC:
        // Relocatable address.
        snprintf(buf, sizeof(buf), "%c%o",
            (addr < obj->cmd_len) ? 'c' :
            transient_flag ? 't' : 'd', addr);
        break;

    case SYM_EXT_S:
        // External reference (short name).
        text_to_buf(buf, word & 07777777700000000);
        break;

    case SYM_ENTRY_S:
        // Entry, relocatable (short name).
        text_to_buf(buf, word & 07777777700000000);
        sprintf(buf+strlen(buf), " = %c%o",
            (addr < obj->cmd_len) ? 'c' :
            transient_flag ? 't' : 'd', addr);
        break;

    case SYM_PRIVATE_S:
        // Private block (short name).
        text_to_buf(buf, word & 07777777700000000);
        if (verbose_flag)
            sprintf(buf + strlen(buf), " (Private %u word%s)", addr, addr==1 ? "" : "s");
        break;

    case SYM_COMMON_S:
        // Common block (short name).
        text_to_buf(buf, word & 07777777700000000);
        if (verbose_flag)
            sprintf(buf + strlen(buf), " (Common %u word%s)", addr, addr==1 ? "" : "s");
        break;

    case SYM_EXT_L:
        // External reference (long name).
        text_to_buf(buf, obj->word[ref + obj->table_off]);
        break;

    case SYM_ENTRY_L:
        // Entry, relocatable (long name).
        text_to_buf(buf, obj->word[ref + obj->table_off]);
        sprintf(buf+strlen(buf), " = %c%o",
            (addr < obj->cmd_len) ? 'c' :
            transient_flag ? 't' : 'd', addr);
        break;

    case SYM_PRIVATE_L:
        // Private block (long name).
        text_to_buf(buf, obj->word[ref + obj->table_off]);
        if (verbose_flag)
            sprintf(buf + strlen(buf), " (Private %u word%s)", addr, addr==1 ? "" : "s");
        break;

    case SYM_COMMON_L:
        // Common block (long name).
        text_to_buf(buf, obj->word[ref + obj->table_off]);
        if (verbose_flag)
            sprintf(buf + strlen(buf), " (Common %u word%s)", addr, addr==1 ? "" : "s");
        break;

    default:
        sprintf(buf, "Unknown%03o", type);
        break;
    }
    return buf;
}

const char *getaddr(obj_image_t *obj, unsigned addr, int long_flag)
{
    static char buf[16];

//if (1) snprintf(buf, sizeof(buf), "<%o>", addr); else
    if (addr < 8) {
        snprintf(buf, sizeof(buf), "%u", addr);
    } else if ((long_flag == 2) && (addr & 04000)) {
        // Transient data.
        return getsyminfo(obj, obj->word[(addr & 03777) + obj->table_off], 0, 1);
    } else if ((long_flag == 0) && (addr & 04000)) {
        // Short address.
        return getsyminfo(obj, obj->word[(addr & 03777) + obj->table_off], 0, 0);
    } else if ((long_flag == 1) && (addr & 040000)) {
        // Long address.
        if ((addr & 074000) == 074000) {
            return getsyminfo(obj, obj->word[(addr & 03777) + obj->table_off], 0, 0);
        } else {
            int reladdr = addr & 037777;
            int intext = (reladdr < obj->cmd_len);
            snprintf(buf, sizeof(buf), "%s",
                getlabel(intext ? 'c' : 'd', reladdr));
        }
    } else if (addr <= 07777) {
        snprintf(buf, sizeof(buf), "%#o", addr);
    } else {
        snprintf(buf, sizeof(buf), "%o", addr);
    }
    return buf;
}

void print_insn(obj_image_t *obj, int opcode)
{
    unsigned op_lflag = (opcode >> 19) & 1;
    unsigned op_ir    = (opcode >> 20) & 017;
    unsigned op_lcmd  = (opcode >> 15) & 037;
    unsigned op_scmd  = (opcode >> 12) & 0177;
    unsigned op_addr;

    // Opcode and instruction name
    if (op_lflag) {
        op_addr = opcode & 077777;
        printf("%02o %02o %05o  %s ", op_ir, op_lcmd, op_addr,
            long_name[op_lcmd - 020]);
    } else {
        op_addr = opcode & 07777;
        printf("%02o %03o %04o  %s ", op_ir, op_scmd, op_addr,
            short_name[op_scmd & 077]);
        if (op_scmd & 0100)
            op_addr |= 070000;
    }

    // Address
    if (op_addr != 0) {
        printf("%s", getaddr(obj, op_addr, op_lflag));
    }

    // Register
    if (op_ir != 0) {
        printf("(%d)", op_ir);
    }
}

void disassemble(const char *fname)
{
    FILE *fd;
    obj_image_t obj = {0};
    int i;

    fd = fopen(fname, "r");
    if (!fd) {
        fprintf(stderr, "%s: %s not found\n", progname, fname);
        return;
    }
    if (obj_read_fd(fd, &obj) < 0) {
        fclose(fd);
        fprintf(stderr, "%s: %s not an object file\n", progname, fname);
        return;
    }
    fclose(fd);

    if (raw_flag) {
        // Dump raw data.
        int i;
        for (i = 0; i < obj.nwords; i++) {
            printf("%05o:  %04o %04o %04o %04o", i,
                (unsigned)(obj.word[i] >> 36) & 07777,
                (unsigned)(obj.word[i] >> 24) & 07777,
                (unsigned)(obj.word[i] >> 12) & 07777,
                (unsigned)(obj.word[i]) & 07777);
            printf("  %c%c%c%c%c%c\n",
                getiso(obj.word[i], 0), getiso(obj.word[i], 1),
                getiso(obj.word[i], 2), getiso(obj.word[i], 3),
                getiso(obj.word[i], 4), getiso(obj.word[i], 5));
        }
        printf("\n");
    }

    printf("%s: file format besm6\n", fname);
    printf("\n");
    printf("        Module: ");
    print_word_as_text(obj.word[obj.table_off]);
    printf("\n");
    printf("     Code size: %u words\n", obj.cmd_len);
    printf("    Const size: %u words\n", obj.const_len);
    printf("     Data size: %u words\n", obj.data_len);
    printf("      Set size: %u words\n", obj.set_len);
    printf("   Symhdr size: %u words\n", obj.head_len);
    printf("   Symtab size: %u words\n", obj.sym_len);
    printf("  Longsym size: %u words\n", obj.long_len);
    printf("    Debug size: %u words\n", obj.debug_len);
    printf("      BSS size: %u words\n", obj.bss_len);
#if 0
    printf(" Symhdr offset: %#o words\n", obj.table_off);
    printf("Longsym offset: %#o words\n", obj.long_off);
    printf("  Debug offset: %#o words\n", obj.debug_off);
    printf("Comment offset: %#o words\n", obj.comment_off);
#endif
    if (obj.base_addr != 0 || obj.entry != 0) {
        printf("  Base address: %05o\n", obj.base_addr);
        printf("         Entry: %05o\n", obj.entry);
    }

    //
    // Print entries.
    //
    if (obj.nentries > 0) {
        printf("\n");
        printf("Entries:\n");
        printf("\n");
        for (i = 0; i < obj.nentries; i++) {
            uint64_t name = obj.word[2*i + 1];
            uint64_t addr = obj.word[2*i + 2];

            printf(" %05o:  ", (unsigned)addr & 077777);
            print_word_as_text(name);
            printf("\n");
        }
    }

    //
    // Print code.
    //
    printf("\n");
    printf("Disassembly of code:\n");
    printf("\n");
    for (i = 0; i < obj.cmd_len; i++) {
        uint64_t word = obj.word[i + obj.cmd_off];

        printf("%6s:  ", getlabel('c', i + obj.base_addr));
        print_insn(&obj, word >> 24);
        printf("\n         ");
        print_insn(&obj, word & 077777777);
        printf("\n");
    }

    //
    // Print initialized data.
    //
    if (obj.const_len > 0) {
        printf("\n");
        printf("Initialized data:\n");
        printf("\n");
        for (i = 0; i < obj.const_len; i++) {
            uint64_t word = obj.word[i + obj.cmd_off + obj.cmd_len];

            printf("%6s:  %04o %04o %04o %04o",
                getlabel('d', i + obj.cmd_len + obj.base_addr),
                (unsigned)(word >> 36) & 07777,
                (unsigned)(word >> 24) & 07777,
                (unsigned)(word >> 12) & 07777,
                (unsigned)word & 07777);
            printf("  %c%c%c%c%c%c",
                getiso(word, 0), getiso(word, 1),
                getiso(word, 2), getiso(word, 3),
                getiso(word, 4), getiso(word, 5));
            printf("  %s%s%s%s%s%s%s%s\n",
                getsymtext(word, 0), getsymtext(word, 1),
                getsymtext(word, 2), getsymtext(word, 3),
                getsymtext(word, 4), getsymtext(word, 5),
                getsymtext(word, 6), getsymtext(word, 7));
        }
    }

    //
    // Print uninitialized data.
    //
    if (obj.bss_len > 0) {
        printf("\n");
        printf("Uninitialized data:\n");
        printf("\n");
        printf("%6s", getlabel('d', obj.cmd_len + obj.const_len + obj.base_addr));
        if (obj.bss_len > 1)
            printf("-%s",
                getlabel('d', obj.cmd_len + obj.const_len + obj.base_addr + obj.bss_len - 1));
        printf(":  %d word%s\n", obj.bss_len, obj.bss_len<2 ? "" : "s");
    }

    //
    // Print transient data section.
    //
    if (obj.data_len > 0) {
        printf("\n");
        printf("Transient data:\n");
        printf("\n");
        for (i = 0; i < obj.data_len; i++) {
            uint64_t word  = obj.word[i + obj.cmd_off + obj.cmd_len + obj.const_len];

            printf("%6s:  %04o %04o %04o %04o",
                getlabel('t', i + obj.cmd_len + obj.const_len),
                (unsigned)(word >> 36) & 07777,
                (unsigned)(word >> 24) & 07777,
                (unsigned)(word >> 12) & 07777,
                (unsigned)word & 07777);
            printf("  %c%c%c%c%c%c",
                getiso(word, 0), getiso(word, 1),
                getiso(word, 2), getiso(word, 3),
                getiso(word, 4), getiso(word, 5));
            printf("  %s%s%s%s%s%s%s%s\n",
                getsymtext(word, 0), getsymtext(word, 1),
                getsymtext(word, 2), getsymtext(word, 3),
                getsymtext(word, 4), getsymtext(word, 5),
                getsymtext(word, 6), getsymtext(word, 7));
        }
    }

    //
    // Print SET section.
    //
    if (obj.set_len > 0) {
        printf("\n");
        printf("SET directives:\n");
        printf("\n");
        for (i = 0; i < obj.set_len; i++) {
            uint64_t word  = obj.word[i + obj.cmd_off + obj.cmd_len + obj.const_len + obj.data_len];
            unsigned size  = (word >> 36) & 07777;
            unsigned from  = (word >> 24) & 07777;
            unsigned count = (word >> 12) & 07777;
            unsigned to    = word & 07777;

            printf("%6s:  %04o %04o %04o %04o",
                getlabel('t', i + obj.cmd_len + obj.const_len + obj.data_len),
                size, from, count, to);
            printf("  %u word%s from %s ",
                size, size<2 ? "" : "s", getaddr(&obj, from, 2));
            printf("to %s", getaddr(&obj, to, 0));
            if (count != 1)
                printf(", replicate %u times", count);
            printf("\n");
        }
    }

    //
    // Print symbol table.
    //
    if (obj.sym_len > 0) {
        printf("\n");
        printf("Symbol table:\n");
        printf("\n");
        for (i = 0; i < obj.sym_len; i++) {
            uint64_t word = obj.word[i + 1 + obj.table_off];

            printf("%6o:  %s ", i + 04001,
                octaldots(word >> 36));
            printf("%s %04o %04o  %s\n",
                octaldots(word >> 24),
                (unsigned)(word >> 12) & 07777,
                (unsigned)word & 07777,
                getsyminfo(&obj, word, 1, 0));
        }
        if (obj.long_len > 0) {
            printf("\n");
            printf("Names:\n");
            printf("\n");
            for (i = 0; i < obj.long_len; i++) {
                uint64_t word = obj.word[i + obj.long_off];

                printf("%6o:  %04o %04o %04o %04o",
                    i + 04001 + obj.sym_len,
                    (unsigned)(word >> 36) & 07777,
                    (unsigned)(word >> 24) & 07777,
                    (unsigned)(word >> 12) & 07777,
                    (unsigned)word & 07777);
                if ((word >> 42) != 0)
                    printf("  ");
                print_word_as_text(word);
                printf("\n");
            }
        }
    }

    //
    // Print Debug section.
    //
    if (debug_flag && obj.debug_len > 0) {
        printf("\n");
        printf("Debug information:\n");
        printf("\n");
        for (i = 0; i < obj.debug_len; i++) {
            uint64_t word  = obj.word[i + obj.debug_off];

            printf("%6o:  %s ", i, octaldots(word >> 36));
            printf("%s ", octaldots(word >> 24));
            printf("%s %04o", octaldots(word >> 12),
                (unsigned)word & 07777);
            if ((word >> 42) != 0)
                printf("  ");
            print_word_as_text(word);
            printf("\n");
        }
    }
}

void usage()
{
    printf("BESM6 Disassembler\n");
    printf("Usage:\n");
    printf("    %s [option...] infile...\n", progname);
    printf("Options:\n");
    printf("    -b      Use BEMSH mnemonics\n");
    printf("    -d      Print Debug section\n");
    printf("    -r      Dump raw data\n");
    exit(0);
}

int main(int argc, char **argv)
{
    // Get program name.
    progname = strrchr(argv[0], '/');
    if (progname)
        progname++;
    else
        progname = argv[0];

    for (;;) {
        switch (getopt(argc, argv, "bdr")) {
        case EOF:
            break;
        case 'b':
            // Use BEMSH mnemonics.
            long_name = lcmd_bemsh;
            short_name = scmd_bemsh;
            continue;
        case 'd':
            // Print Debug section.
            debug_flag++;
            continue;
        case 'r':
            // Dump raw data.
            raw_flag++;
            continue;
        default:
            usage();
        }
        break;
    }
    argc -= optind;
    argv += optind;
    if (argc < 1)
        usage();

    while (argc-- > 0) {
        disassemble(*argv++);
    }
    return 0;
}
