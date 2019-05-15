/*
 * Disassembler for MESM-6.
 */
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <getopt.h>

//
// Object file has the following format:
//  +---------+
//  | magic   |     "BESM6\0"
//  +---------+
//  | header  |     3 words
//  +---------+
//  | code    |     Executable code
//  +---------+
//  | const   |     Initialized data
//  +---------+
//  | data    |     Transient data for SET section
//  +---------+
//  | set     |     SET directives
//  +---------+
//  | symhdr  |     Name of the subroutine
//  +---------+
//  | symtab  |     Symbol table
//  +---------+
//  | longsym |     Names of 5 characters and more
//  +---------+
//  | debug   |     (unused)
//  +---------+
//  | comment |     (unused)
//  +---------+
//
typedef struct _obj_image_t {

    unsigned head_len;          // length of symhdr
    unsigned sym_len;           // length of symtab
    unsigned debug_len;         // length of debug section

    unsigned set_len;           // length of SET section
    unsigned data_len;          // length of data section
    unsigned long_len;          // length of longsym section

    unsigned cmd_len;           // length of code section
    unsigned bss_len;           // length of bss section
    unsigned const_len;         // length of const section

    unsigned head_off;          // offset of header
    unsigned cmd_off;           // offset of code section
    unsigned table_off;         // offset of symhdr section
    unsigned long_off;          // offset of longsym section
    unsigned debug_off;         // offset of debug section
    unsigned comment_off;       // offset of comment section

#define MAXSZ 50000
    uint64_t word[MAXSZ];

} obj_image_t;

//
// Symbol types
//
enum {
    SYM_OFFSET      = 000,          // Offset from another symbol
    SYM_ABS         = 040,          // Absolute address
    SYM_RELOC       = 041,          // Relocatable address
    SYM_EXT_S       = 043,          // External reference (short name)
    SYM_PRIVATE_S   = 046,          // Private block (short name)
    SYM_COMMON_S    = 047,          // Common block (short name)
    SYM_EXT_L       = 063,          // External reference (long name)
    SYM_PRIVATE_L   = 066,          // Private block (long name)
    SYM_COMMON_L    = 067,          // Common block (long name)
};

//
// First word of object file: magic key.
//
const uint64_t BESM6_MAGIC = 0x4245534d3600;

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

//
// Read a 48-bit word at the current file position.
//
uint64_t freadw(FILE *fd)
{
    uint64_t val = 0;
    int i;

    for (i = 0; i < 6; ++i) {
        val <<= 8;
        val |= getc (fd);
    }
    return val;
}

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

const char *getsyminfo(obj_image_t *obj, uint64_t word, int verbose_flag, int transient_flag)
{
    static char buf[64];
    unsigned type = (word >> 18) & 077;
    unsigned addr = word & 077777;
    unsigned ref  = (word >> 24) & 03777;

    switch (type) {
    case SYM_OFFSET:      // Offset from another symbol
        getsyminfo(obj, obj->word[ref + obj->table_off], 0, transient_flag);
        strcat(buf, verbose_flag ? " + " : "+");
        sprintf(buf + strlen(buf), "%o", addr);
        break;

    case SYM_ABS:         // Absolute address
        snprintf(buf, sizeof(buf), "%o", addr);
        break;

    case SYM_RELOC:       // Relocatable address
        snprintf(buf, sizeof(buf), "%c%o",
            (addr < obj->cmd_len) ? 'c' :
            transient_flag ? 't' : 'd', addr);
        break;

    case SYM_EXT_S:       // External reference (short name)
        text_to_buf(buf, word & 07777777700000000);
        break;

    case SYM_PRIVATE_S:   // Private block (short name)
        text_to_buf(buf, word & 07777777700000000);
        if (verbose_flag)
            sprintf(buf + strlen(buf), " (Private %u word%s)", addr, addr==1 ? "" : "s");
        break;

    case SYM_COMMON_S:    // Common block (short name)
        text_to_buf(buf, word & 07777777700000000);
        if (verbose_flag)
            sprintf(buf + strlen(buf), " (Common %u word%s)", addr, addr==1 ? "" : "s");
        break;

    case SYM_EXT_L:       // External reference (long name)
        text_to_buf(buf, obj->word[ref + obj->table_off]);
        break;

    case SYM_PRIVATE_L:   // Private block (long name)
        text_to_buf(buf, obj->word[ref + obj->table_off]);
        if (verbose_flag)
            sprintf(buf + strlen(buf), " (Private %u word%s)", addr, addr==1 ? "" : "s");
        break;

    case SYM_COMMON_L:    // Common block (long name)
        text_to_buf(buf, obj->word[ref + obj->table_off]);
        if (verbose_flag)
            sprintf(buf + strlen(buf), " (Common %u word%s)", addr, addr==1 ? "" : "s");
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

//
// Read object image from a file.
// Return negative in case of failure.
//
int obj_read(const char *fname, obj_image_t *obj)
{
    uint64_t word1, word2, word3;
    FILE *fd;
    unsigned nwords;

    fd = fopen(fname, "r");
    if (!fd) {
        fprintf(stderr, "dis: %s not found\n", fname);
        return -1;
    }

    // Read file contents.
    nwords = 0;
    obj->word[nwords++] = freadw(fd);
    for (;; nwords++) {
        if (nwords >= MAXSZ) {
            fprintf(stderr, "File too large\n");
            return -1;
        }
        obj->word[nwords] = freadw(fd);
        if (feof(fd))
            break;
    }
    fclose(fd);

#if 0
    // Dump raw data.
    int i;
    for (i = 0; i < nwords; i++) {
        printf("%05o:  %04o %04o %04o %04o", i,
            (unsigned)(obj->word[i] >> 36) & 07777,
            (unsigned)(obj->word[i] >> 24) & 07777,
            (unsigned)(obj->word[i] >> 12) & 07777,
            (unsigned)(obj->word[i]) & 07777);
        printf("  %c%c%c%c%c%c\n",
            getiso(obj->word[i], 0), getiso(obj->word[i], 1),
            getiso(obj->word[i], 2), getiso(obj->word[i], 3),
            getiso(obj->word[i], 4), getiso(obj->word[i], 5));
    }
#endif
    if (obj->word[0] != BESM6_MAGIC) {
        // Check file magic.
        fprintf(stderr, "Bad magic: %#jx\n", (intmax_t)obj->word[0]);
        return -1;
    }

    // Detect packed or unpacked header.
    obj->head_off = 1;
    if ((obj->word[2] >> 45) != 0) {

        // Skip entry table.
        while ((obj->word[obj->head_off + 1] >> 45) != 0) {
            obj->head_off += 2;
        }

        // Unpacked header.
        obj->head_len  = obj->word[obj->head_off];
        obj->sym_len   = obj->word[obj->head_off + 1];
        // Unknown: obj->word[obj->head_off + 2]
        obj->debug_len = obj->word[obj->head_off + 3];
        obj->long_len  = obj->word[obj->head_off + 4];
        obj->cmd_len   = obj->word[obj->head_off + 5];
        obj->bss_len   = obj->word[obj->head_off + 6];
        obj->const_len = obj->word[obj->head_off + 7];
        obj->data_len  = obj->word[obj->head_off + 8];
        obj->set_len   = obj->word[obj->head_off + 9];

        obj->cmd_off = obj->head_off + 10;
    } else {
        // Packed header.
        obj->head_len  = obj->word[obj->head_off] & 07777;
        obj->sym_len   = (obj->word[obj->head_off] >> 12) & 07777;
        obj->debug_len = (obj->word[obj->head_off] >> 36);

        obj->set_len  = obj->word[obj->head_off + 1] & 077777;
        obj->data_len = (obj->word[obj->head_off + 1] >> 15) & 077777;
        obj->long_len = (obj->word[obj->head_off + 1] >> 30) & 077777;

        obj->cmd_len   = obj->word[obj->head_off + 2] & 077777;
        obj->bss_len   = (obj->word[obj->head_off + 2] >> 15) & 077777;
        obj->const_len = (obj->word[obj->head_off + 2] >> 30) & 077777;

        obj->cmd_off = obj->head_off + 3;
    }

    obj->table_off = obj->cmd_off + obj->cmd_len + obj->const_len + obj->data_len + obj->set_len;
    obj->long_off = obj->table_off + obj->head_len + obj->sym_len;
    obj->debug_off = obj->long_off + obj->long_len;
    obj->comment_off = obj->debug_off + obj->debug_len;
    return 0;
}

void disassemble(const char *fname)
{
    obj_image_t obj = {0};
    int i;

    if (obj_read(fname, &obj) < 0) {
        fprintf(stderr, "dis: %s not an object file\n", fname);
        return;
    }

    printf("%s: file format besm6\n", fname);
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

    //
    // Print code.
    //
    printf("\n");
    printf("Disassembly of code:\n");
    printf("\n");
    print_word_as_text(obj.word[obj.table_off]);
    printf(":\n");
    for (i = 0; i < obj.cmd_len; i++) {
        uint64_t word = obj.word[i + obj.cmd_off];

        printf("%6s:  ", getlabel('c', i));
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
                getlabel('d', i + obj.cmd_len),
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
        printf("%6s", getlabel('d', obj.cmd_len + obj.const_len));
        if (obj.bss_len > 1)
            printf("-%s",
                getlabel('d', obj.cmd_len + obj.const_len + obj.bss_len - 1));
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
                (unsigned)(word >> 36) & 07777,
                (unsigned)(word >> 24) & 07777,
                (unsigned)(word >> 12) & 07777,
                (unsigned)word & 07777);
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
    printf("\n");
    printf("Symbol table:\n");
    printf("\n");
    for (i = 0; i < obj.sym_len; i++) {
        uint64_t word = obj.word[i + 1 + obj.table_off];

        printf("%6o:  %04o %04o %04o %04o  %s\n",
            i + 04001,
            (unsigned)(word >> 36) & 07777,
            (unsigned)(word >> 24) & 07777,
            (unsigned)(word >> 12) & 07777,
            (unsigned)word & 07777,
            getsyminfo(&obj, word, 1, 0));
    }
    if (obj.long_len > 0) {
        printf("\n");
        printf("Long names:\n");
        printf("\n");
        for (i = 0; i < obj.long_len; i++) {
            uint64_t word = obj.word[i + obj.long_off];

            printf("%6o:  %04o %04o %04o %04o  ",
                i + 04001 + obj.sym_len,
                (unsigned)(word >> 36) & 07777,
                (unsigned)(word >> 24) & 07777,
                (unsigned)(word >> 12) & 07777,
                (unsigned)word & 07777);
            print_word_as_text(word);
            printf("\n");
        }
    }
}

void usage()
{
    printf("BESM6 Disassembler\n");
    printf("Usage:\n");
    printf("    besm6-dis [option...] infile...\n");
    printf("Options:\n");
    printf("    -b      Use BEMSH mnemonics\n");
    exit(0);
}

int main(int argc, char **argv)
{
    for (;;) {
        switch (getopt(argc, argv, "b")) {
        case EOF:
            break;
        case 'b':
            // Use BEMSH mnemonics.
            long_name = lcmd_bemsh;
            short_name = scmd_bemsh;
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
