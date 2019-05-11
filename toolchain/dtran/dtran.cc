/*
 * A disassembler for the BESM-6 object files in the Dubna
 * Monitor System format.
 *
 * Compile with "make dtran"; it requires only a C++ compiler.
 * To produce assembly code that can be re-assembled
 * with *MADLEN (not *ASSEMBLER!):
 * dtran -e -l pascompl.o > pascompl.asm
 *
 * To produce pseudo-code that can be given to the "decompiler":
 * dtran -d pascompl.o > pascompl.out
 *
 * Copyright 2017 Leonid Broukhis
 */

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <cstdarg>
#include <string>
#include <vector>
#include <map>
#include <sys/stat.h>
#include "unistd.h"

/*
 * BESM-6 opcode types.
 */
typedef enum {
OPCODE_ILLEGAL,
OPCODE_STR1,		/* short addr */
OPCODE_STR2,		/* long addr */
OPCODE_IMM,		/* e.g. NTR */
OPCODE_REG1,		/* e.g. ATI */
OPCODE_JUMP,		/* UJ */
OPCODE_BRANCH,		/* UZA, U1A, VZM, V1M, VLM */
OPCODE_CALL,		/* VJM */
OPCODE_IMM64,		/* e.g. ASN */
OPCODE_ADDRMOD,		/* UTC, WTC */
OPCODE_REG2,		/* VTM, UTM */
OPCODE_IMMEX,		/* *50, ... */
OPCODE_ADDREX,		/* *64, *70, ... */
OPCODE_STOP,		/* *74 */
OPCODE_DEFAULT
} opcode_e;

struct opcode {
	const char *name;
	uint opcode;
	uint mask;
	opcode_e type;
} op[] = {
  /* name,	pattern,  mask,	    opcode type */
  { "ATX",	0x000000, 0x0bf000, OPCODE_STR1 },
  { "STX",	0x001000, 0x0bf000, OPCODE_STR1 },
  { "XTS",	0x003000, 0x0bf000, OPCODE_STR1 },
  { "A+X",	0x004000, 0x0bf000, OPCODE_STR1 },
  { "A-X",	0x005000, 0x0bf000, OPCODE_STR1 },
  { "X-A",	0x006000, 0x0bf000, OPCODE_STR1 },
  { "AMX",	0x007000, 0x0bf000, OPCODE_STR1 },
  { "XTA",	0x008000, 0x0bf000, OPCODE_STR1 },
  { "AAX",	0x009000, 0x0bf000, OPCODE_STR1 },
  { "AEX",	0x00a000, 0x0bf000, OPCODE_STR1 },
  { "ARX",	0x00b000, 0x0bf000, OPCODE_STR1 },
  { "AVX",	0x00c000, 0x0bf000, OPCODE_STR1 },
  { "AOX",	0x00d000, 0x0bf000, OPCODE_STR1 },
  { "A/X",	0x00e000, 0x0bf000, OPCODE_STR1 },
  { "A*X",	0x00f000, 0x0bf000, OPCODE_STR1 },
  { "APX",	0x010000, 0x0bf000, OPCODE_STR1 },
  { "AUX",	0x011000, 0x0bf000, OPCODE_STR1 },
  { "ACX",	0x012000, 0x0bf000, OPCODE_STR1 },
  { "ANX",	0x013000, 0x0bf000, OPCODE_STR1 },
  { "E+X",	0x014000, 0x0bf000, OPCODE_STR1 },
  { "E-X",	0x015000, 0x0bf000, OPCODE_STR1 },
  { "ASX",	0x016000, 0x0bf000, OPCODE_STR1 },
  { "XTR",	0x017000, 0x0bf000, OPCODE_STR1 },
  { "RTE",	0x018000, 0x0bf000, OPCODE_IMM },
  { "YTA",	0x019000, 0x0bf000, OPCODE_IMM },
  { "E+N",	0x01c000, 0x0bf000, OPCODE_IMM64 },
  { "E-N",	0x01d000, 0x0bf000, OPCODE_IMM64 },
  { "ASN",	0x01e000, 0x0bf000, OPCODE_IMM64 },
  { "NTR",	0x01f000, 0x0bf000, OPCODE_IMM },
  { "ATI",	0x020000, 0x0bf000, OPCODE_REG1 },
  { "STI",	0x021000, 0x0bf000, OPCODE_REG1 },
  { "ITA",	0x022000, 0x0bf000, OPCODE_REG1 },
  { "ITS",	0x023000, 0x0bf000, OPCODE_REG1 },
  { "MTJ",	0x024000, 0x0bf000, OPCODE_REG1 },
  { "M+J",	0x025000, 0x0bf000, OPCODE_REG1 },
  { "*50",	0x028000, 0x0bf000, OPCODE_IMMEX },
  { "*51",	0x029000, 0x0bf000, OPCODE_IMMEX },
  { "*52",	0x02a000, 0x0bf000, OPCODE_IMMEX },
  { "*53",	0x02b000, 0x0bf000, OPCODE_IMMEX },
  { "*54",	0x02c000, 0x0bf000, OPCODE_IMMEX },
  { "*55",	0x02d000, 0x0bf000, OPCODE_IMMEX },
  { "*56",	0x02e000, 0x0bf000, OPCODE_IMMEX },
  { "*57",	0x02f000, 0x0bf000, OPCODE_IMMEX },
  { "*60",	0x030000, 0x0bf000, OPCODE_ADDREX },
  { "*61",	0x031000, 0x0bf000, OPCODE_ADDREX },
  { "*62",	0x032000, 0x0bf000, OPCODE_IMMEX },
  { "*63",	0x033000, 0x0bf000, OPCODE_IMMEX },
  { "*64",	0x034000, 0x0bf000, OPCODE_ADDREX },
  { "*65",	0x035000, 0x0bf000, OPCODE_IMMEX },
  { "*66",	0x036000, 0x0bf000, OPCODE_IMMEX },
  { "*67",	0x037000, 0x0bf000, OPCODE_ADDREX },
  { "*70",	0x038000, 0x0bf000, OPCODE_ADDREX },
  { "*71",	0x039000, 0x0bf000, OPCODE_ADDREX },
  { "*72",	0x03a000, 0x0bf000, OPCODE_ADDREX },
  { "*73",	0x03b000, 0x0bf000, OPCODE_ADDREX },
  { "*74",	0x03c000, 0x0bf000, OPCODE_STOP },
  { "CTX",	0x03d000, 0x0bf000, OPCODE_ADDREX },
  { "*76",	0x03e000, 0x0bf000, OPCODE_IMMEX },
  { "*77",	0x03f000, 0x0bf000, OPCODE_IMMEX },
  { "UTC",	0x090000, 0x0f8000, OPCODE_ADDRMOD },
  { "WTC",	0x098000, 0x0f8000, OPCODE_ADDRMOD },
  { "VTM",	0x0a0000, 0x0f8000, OPCODE_REG2 },
  { "UTM",	0x0a8000, 0x0f8000, OPCODE_REG2 },
  { "UZA",	0x0b0000, 0x0f8000, OPCODE_BRANCH },
  { "U1A",	0x0b8000, 0x0f8000, OPCODE_BRANCH },
  { "UJ",	0x0c0000, 0x0f8000, OPCODE_JUMP },
  { "VJM",	0x0c8000, 0x0f8000, OPCODE_CALL },
  { "VZM",	0x0e0000, 0x0f8000, OPCODE_BRANCH },
  { "V1M",	0x0e8000, 0x0f8000, OPCODE_BRANCH },
  { "VLM",	0x0f8000, 0x0f8000, OPCODE_BRANCH },
/* This entry MUST be last; it is a "catch-all" entry that will match when no
 * other opcode entry matches during disassembly.
 */
  { "",		0x0000, 0x0000, OPCODE_ILLEGAL },
};

typedef unsigned long long uint64;
typedef unsigned int uint32;


typedef unsigned int uint;

std::string strprintf(const char * fmt, ...) {
    std::string ret;
    char * str;
    va_list ap;
    va_start (ap, fmt);
    if (vasprintf(&str, fmt, ap) < 0) {
        perror("vasprintf");
    }
    va_end(ap);
    ret = str;
    free(str);
    return ret;
}

static const char * text_to_utf[] = {
    " ", ".", "Б", "Ц", "Д", "Ф", "Г", "И",
    "(", ")", "*", "Й", "Л", "Я", "Ж", "/",
    "0", "1", "2", "3", "4", "5", "6", "7",
    "8", "9", "Ь", ",", "П", "-", "+", "Ы",
    "З", "A", "B", "C", "D", "E", "F", "G",
    "H", "I", "J", "K", "L", "M", "N", "O",
    "P", "Q", "R", "S", "T", "U", "V", "W",
    "X", "Y", "Z", "Ш", "Э", "Щ", "Ч", "Ю"
};

struct Dtran {
    uint head_len;
    uint sym_len;
    uint debug_len;
    uint data_len;
    uint set_len;
    uint long_len;
    uint cmd_len;
    uint bss_len;
    uint const_len;
    uint table_off;
    uint debug_off;
    uint long_off;
    uint comment_off;
    uint basereg, baseaddr, baseop;
    bool nolabels, noequs, nooctal;

    void fill_lengths() {
        head_len = memory[0] & 07777;
        sym_len = (memory[0] >> 12) & 07777;
        debug_len = (memory[0] >> 36);
        set_len = memory[1] & 077777;
        data_len = (memory[1] >> 15) & 077777;
        long_len = (memory[1] >> 30) & 077777;
        cmd_len = memory[2] & 077777;
        bss_len = (memory[2] >> 15) & 077777;
        if (bss_len != 0) {
            fprintf(stderr, "BSS section not supported yet\n");
            exit(1);
        }
        const_len = (memory[2] >> 30) & 077777;
        table_off = 3 + cmd_len + const_len + data_len + set_len;
        long_off = table_off + head_len + sym_len;
        debug_off = long_off + long_len;
        comment_off = debug_off + debug_len;
        printf(" %s:,NAME, NEW DTRAN\n",
               get_text_word(memory[table_off-0]).c_str());
        printf("C Commands: %o\n", cmd_len);
        printf("C Constants: %o\n", const_len);
        printf("C BSS: %o\n", bss_len);
        printf("C Memory size: %o\n", cmd_len+const_len+bss_len);
        printf("C Header: %o\n", head_len);
        printf("C Symbols: %o\n", sym_len);
        printf("C Long symbols: %o\n", long_len);
        printf("C Debug: %o\n", debug_len);
        printf("C Data: %o + set: %o\n", data_len, set_len);
        printf("C Actual length of the object file is %o\n",
               comment_off);
        if (nolabels) {
            printf(" /:,BSS,\n");
        }
    }

    uint64 memory[32768];

/*
 * Read a 48-bit word at the current file position.
 */
uint64
freadw (FILE *fd)
{
    uint64 val = 0;
    int i;
    for (i = 0; i < 6; ++i) {
        val <<= 8;
        val |= getc (fd);
    }
    return val;
}

void
mklabels(uint32 memaddr, uint32 opcode, bool litconst) {
    int arg1 = (opcode & 07777) + (opcode & 0x040000 ? 070000 : 0);
    int arg2 = opcode & 077777;
    int struc = opcode & 02000000;
    uint reg = opcode >> 20;
    if (basereg && basereg == reg) {
        if (baseaddr == ~0u &&
            (opcode & 03740000) == 02440000) {
            // Setting base reg by a relative address or a symbol
            if ((opcode & 074000) == 074000) {
                uint sympos = opcode & 03777;
                if (memory[table_off + sympos] >> 15 != 0410) {
                    fprintf(stderr, "@%05o Base register set to non-relative address\n", memaddr);
                    exit(1);
                }
                baseaddr = memory[table_off + sympos] & 077777;
            } else {
                baseaddr = opcode & 037777;
            }
            baseop = opcode;
            fprintf(stderr, "@%05o Base register set to %05o\n", memaddr, baseaddr);
        } else if (struc) {
            // Must be the same opcode as used originally.
            if (baseaddr == ~0u || baseop != opcode) {
                fprintf(stderr,
                        "@%05o Base register used in a long-address insn\n", memaddr);
                exit(1);
            }
        } else if (baseaddr == ~0u) {
            fprintf(stderr,
                    "@%05o Base register used but not yet set\n", memaddr);
            exit(1);
        } else if (arg1 >= 04000) {
            fprintf(stderr,
                    "@%05o Base register used with a symbol (%04o)\n", memaddr, arg1);
            exit(1);
        } else {
            uint off = baseaddr + arg1;
            if (off >= labels.size()) {
                fprintf(stderr, "@%05o Base offset %05o too large\n", memaddr, arg1);
                exit(1);
            }
            if (labels[off].empty())
                labels[off] = litconst ? get_literal(off) : strprintf("*%04oB", off);
        }
    } else if (struc && arg2 >= 074000) {
        // nothing
    } else if (struc && arg2 >= 040000 &&
               (arg2 & 037777) < labels.size()) {
        uint off = arg2 & 037777;
        if (labels[off].empty())
            labels[off] = off < 010000 ?
                                strprintf("*%04oB", off) :
                                strprintf("*%05o", off);
    }
}

void
prinsn (uint32 memaddr, uint32 opcode)
{
    int i;

    uint reg = opcode >> 20;
    int arg1 = (opcode & 07777) + (opcode & 0x040000 ? 070000 : 0);
    int arg2 = opcode & 077777;
    int struc = opcode & 02000000;

    for (i=0; op[i].mask; i++)
        if ((opcode & op[i].mask) == op[i].opcode)
            break;
    opcode_e type = op[i].type;
    std::string opname = op[i].name;
    if (type == OPCODE_ILLEGAL) {
        opname = struc ? strprintf("%2o", (opcode >> 15) & 037)
            : strprintf("%03o", (opcode >> 12) & 0177);
    }
    std::string operand;
    if (struc && arg2 >= 074000) {
        operand = strprintf("%s", symtab[arg2 & 03777].c_str());
    } else if (struc && arg2 >= 040000 && (arg2 & 037777) < labels.size()) {
        uint off = arg2 & 037777;
        if (labels[off].empty())
            labels[off] = off < 010000 ?
                                strprintf("*%04oB", off) :
                                strprintf("*%05o", off);
        operand = labels[off];
    } else if (!struc && arg1 >= 04000 && arg1 < 010000) {
        operand = symtab[arg2 & 03777];
    } else if (!struc && arg1 >= 070000) {
        operand = "dunno";
    } else {
        if (uint val = struc ? arg2 : arg1) {
            if (type == OPCODE_REG1 || val < 8)
                operand = strprintf("%d", val);
            else if (type == OPCODE_IMM64) {
                operand = strprintf("64%+d", val-64);
            } else
                operand = strprintf(nooctal ? "%d" : "%oB", val);
        }
    }

    if (basereg && reg == basereg) {
        if (op[i].opcode == 02400000) {
            opname = "BASE";
        } else {
            reg = 0;
            operand = labels[baseaddr + arg1];
        }
    }
    if (nolabels && operand[0] == '*' && !strchr(operand.c_str(), '+')) {
        char * end;
        int off = strtol(operand.c_str()+1, &end, 8);
        if (end - operand.c_str() > 4)
            operand = strprintf(nooctal ? "/+%d" : "/+%oB", off);
    }
    if (reg) printf("%d,", reg); else printf(",");
    printf("%s,%s\n", opname.c_str(), operand.c_str());
}

std::string get_literal(uint32 addr) {
    uint64 val = memory[addr + 3];
    std::string ret;
    if ((val >> 24) == 064000000) {
        uint d = val & 077777777;
        if (d > 10000)
            ret = strprintf("%08oB", d);
        else
            ret = strprintf("(%d)", d);
    } else if (is_likely_iso(val)) {
        ret = strprintf("iso('%s')", get_iso_word(val).c_str());
    } else if (is_likely_text(val)) {
        ret = strprintf("%lloC(*\"%s\"*)", val, get_text_word(val).c_str());
    } else if (val >= 0101 && val < 96) {
        ret = strprintf("char('%c')", char(val));
    } else {
        ret = strprintf("(%lloC)", val);
    }
    return ret;
}

//
// Convert a string: quote apostrophe symbols.
//
std::string quoteiso(std::string str)
{
    std::string buf;

    for(const char &c : str) {
        if (c == '\'')
            buf += "\'47";
        buf += c;
    }
    return buf;
}

void prconst (uint32 addr, uint32 len, bool litconst) {
    for (uint cur = addr; cur < addr + len; ++cur) {
        uint64 val = memory[cur+3];
        if (litconst) {
            printf(" /%d:", cur-cmd_len);
        } else if (labels[cur].empty()) {
            printf(" ");
        } else {
            printf(" %s:", labels[cur].c_str());
        }
        if ((val >> 24) == 064000000) {
            uint d = val & 077777777;
            if (d > 200000)
                printf(",INT,%08oB\n", d);
            else
                printf(",INT,%d\n", d);
        } else if (is_likely_iso(val)) {
            printf(",ISO, 6H%s . %s\n", quoteiso(get_iso_word(val)).c_str(), get_bytes(val).c_str());
        } else if (is_likely_text(val)) {
            printf(",TEXT, 8H%s\n", get_text_word(val).c_str());
        } else {
            printf(",LOG,%llo\n", val);
        }
    }
}

void prsets() {
    for (uint cur = table_off - set_len; cur < table_off; ++cur) {
        uint64 word = memory[cur];
        uint len = word >> 36;
        uint from = (word >> 24) & 03777;
        uint cnt = (word >> 12) & 07777;
        uint to = word & 03777;
        std::string src = symtab[from];
        if (len == 1 && src[0] == '*') {
            char * end;
            uint off = strtol(src.c_str()+1, &end, 8);
            do {
                switch (*end) {
                case '\0': break;
                case '+':
                    if (isdigit(end[1]))
                        off += atoi(end+1);
                    else
                        off = ~0;
                    break;
                case 'B':
                    ++end;
                    continue;
                default:
                    off = ~0;
                    break;
                }
                break;
            } while (1);
        }
        printf(" %d,SET,%s\n", len, src.c_str());
        printf(" %d,   ,%s\n", cnt, symtab[to].c_str());
    }
}

void prdata() {
    /*
     * Print data initializations in the form of assignments
     * if the source reference allows it.
     */
    for (uint cur = table_off - set_len; cur < table_off; ++cur) {
        uint64 word = memory[cur];
        uint len = word >> 36;
        uint from = (word >> 24) & 03777;
        uint cnt = (word >> 12) & 07777;
        uint to = word & 03777;
        std::string src = symtab[from];
        uint off = ~0;

        if (src[0] == '*') {
            char * end;
            off = strtol(src.c_str()+1, &end, 8);
            do {
                switch (*end) {
                case '\0': break;
                case '+':
                    if (isdigit(end[1]))
                        off += atoi(end+1);
                    else
                        off = ~0;
                    break;
                case 'B':
                    ++end;
                    continue;
                default:
                    off = ~0;
                    break;
                }
                break;
            } while (1);
        }
        if (off != ~0u) {
            for (uint tocnt = 0; tocnt < cnt; ++tocnt) {
                for (uint fromcnt = 0; fromcnt < len; ++fromcnt) {
                    printf(" ,XTA,%s\n", get_literal(off+fromcnt).c_str());
                    std::string temp;
                    size_t plus = symtab[to].find('+');
                    if (plus != std::string::npos && (tocnt || fromcnt)) {
                        uint was = atoi(symtab[to].c_str() + plus + 1);
                        temp = strprintf("%.*s+%d", plus, symtab[to].c_str(),
                                         was+ tocnt*len+fromcnt);
                    } else if (tocnt || fromcnt) {
                        temp = symtab[to] + strprintf("%+d", tocnt*len+fromcnt);
                    } else {
                        temp = symtab[to];
                    }
                    printf(" ,ATX,%s\n", temp.c_str());
                }
            }
        } else {
            printf(" %d,SET,%s\n", len, src.c_str());
            printf(" %d,   ,%s\n", cnt, symtab[to].c_str());
        }
    }
}


void prbss (uint32 addr, uint32 limit)
{
    // TODO: support BSS section
}

void
prtext (bool litconst)
{
    uint32 addr = 0;
    uint32 limit = cmd_len;
    for (uint32 cur = addr; cur < limit; ++cur) {
        uint64 & opcode = memory[cur+3];
        mklabels(cur, opcode >> 24, litconst);
        mklabels(cur, opcode & 0xffffff, litconst);
    }
    if (nolabels) {
        puts(" /:,BSS,");
    }
    for (; addr < limit; ++addr) {
        uint64 opcode;
        if (!labels[addr].empty()) {
            if (nolabels) {
                printf(" :");
            } else {
                printf(" %s:", labels[addr].c_str());
            }
        } else
            putchar(' ');
        opcode = memory[addr+3];
        prinsn (addr, opcode >> 24);
        // Do not print the non-insn part of a word
        // if it looks like a placeholder
        opcode &= 0xffffff;
        if (opcode == 02200000) {
            opcode = memory[addr+3] >> 24;
            opcode &= 03700000;
            if (opcode != 03100000 &&
                labels[addr+1].empty()) {
                labels[addr+1] = " ";
            }
        } else {
            putchar(' ');
            prinsn (addr, opcode);
        }
    }
}

Dtran(const char * fname, uint b, bool n, bool e, bool o) :
        basereg(b), baseaddr(~0u), nolabels(n), noequs(e), nooctal(o) {

    unsigned int addr = 0;
    struct stat st;

    FILE * textfd = fopen (fname, "r");
    if (! textfd) {
        fprintf (stderr, "dtran: %s not found\n", fname);
        exit(1);
    }
    stat (fname, &st);
    uint codelen = st.st_size / 6;

    if (codelen >= 32768) {
        fprintf(stderr, "File too large\n");
        exit(1);
    }
    while (!feof(textfd) && addr < 0100000) {
        memory[addr++] = freadw (textfd);
    }

    fill_lengths();
    if (codelen < comment_off) {
        fprintf(stderr, "File was too short: %d, expected %d\n", codelen, comment_off);
        exit (EXIT_FAILURE);
    }
    symtab.resize(head_len + sym_len);
    labels.resize(cmd_len + const_len + bss_len + data_len);
    fclose (textfd);
}

    void print_text_char (unsigned char ch) {
        puts(text_to_utf[ch & 077]);
    }

    std::string get_utf8(uint unic) {
        std::string ret;
        if (unic < 0x80) {
            ret = char(unic);
	} else
	if (unic < 0x800) {
            ret = char(unic >> 6 | 0xc0);
            ret += char((unic & 0x3f) | 0x80);
	} else {
            ret = char(unic >> 12 | 0xe0);
            ret += char(((unic >> 6) & 0x3f) | 0x80);
            ret += char ((unic & 0x3f) | 0x80);
        }
        return ret;
    }
    std::string get_text_char (unsigned char ch) {
        return text_to_utf[ch & 63];
    }

    std::string get_iso_char (unsigned char ch) {
        ch &= 0177;
        if (ch < 0140) { std::string ret; return ret=ch; }
        return std::string(&"ЮАБЦДЕФГХИЙКЛМНОПЯРСТУЖВЬЫЗШЭЩЧ\177"[2*(ch-0140)], 2);
    }

    std::string get_text_word(uint64 word) {
        std::string ret;
        for (uint i = 42; i <= 42; i-=6) {
            ret += get_text_char(word >> i);
        }
        return ret;
    }
    std::string get_iso_word(uint64 word) {
        std::string ret;
        for (uint i = 40; i <= 40; i-=8) {
            ret += get_iso_char(word >> i);
        }
        return ret;
    }
    std::string get_bytes(uint64 word) {
        std::string ret;
        for (uint i = 40; i <= 40; i-=8) {
            ret += strprintf("%03o ", int(word >> i) & 0377);
        }
        return ret;
    }
    void print_text_word(uint64 word) {
        for (uint i = 42; i <= 42; i-=6) {
            print_text_char(word >> i);
        }
    }

    bool is_likely_text(uint64 word) {
        // Of all groups of 6 bits, they should be left-
        // or right-aligned, there must be no spaces in the middle,
        // and no unused codes or Cyrillics.
        int seen0 = 0, seen_char = 0;
        bool last0 = false;
        for (uint i = 42; i <= 42; i-=6) {
            uint val = (word >> i) & 077;
            switch (val) {
            case 001 ... 011:
            case 013 ... 016:
            case 032 ... 034:
            case 037:
            case 040:
            case 073 ... 077:
                return false;
            case 0:
                if (seen_char && seen0 && !last0) return false;
                ++seen0;
                last0 = true;
                break;
            default:
                if (seen_char && seen0 && last0) return false;
                ++seen_char;
                last0 = false;
            }
        }
        return seen_char > 1;
    }

    bool is_likely_iso (uint64 word) {
        for (uint i = 0; i < 48; i += 8) {
            uint val = (word >> i) & 0377;
            if (val < 040 || val >= 0177)
                return false;
        }
        return true;
    }

    std::string gak(uint i) {
        std::string ret("G");
        ret += 'A'+i/128;
        ret += 'K'+i/8%16;
        ret += '/';
        ret += '0'+i%8;
        return ret;
    }

    std::vector<std::string> symtab;
    std::vector<std::string> labels;

    void dump_sym(uint i, uint offset) {
        uint64 word = memory[i+offset];
        if ((word >> 15) == 0400) {
            uint val = word & 077777;
            // printf("C %5o: absolute %05o\n", i, (uint)word & 077777);
            if (val > 0100000-100)
                symtab[i] = strprintf("-%d", 0100000-val);
            else
                symtab[i] = strprintf(nooctal ? "%d" : "%oB", val);
        } else if ((word >> 15) == 0410) {
            uint off = word & 077777;
            // printf("C %5o: local offset %05o\n", i, off);
            symtab[i] = off < 010000 ?
                              strprintf("*%04oB", off)
                              : strprintf("*%05o", off);
            labels[off] = symtab[i];
        } else {
            if (word >> 42) {
                // The first char is not a space
                symtab[i] = get_text_word(word & 07777777700000000LL);
            } else if ((word & 00000400020000000LL) == 00000400020000000LL) {
                // Long ID
                uint loff = (word >> 24) & 03777;
                symtab[i] = get_text_word(memory[table_off + loff]);
            }
            if ((word & 057777777) == 043000000) {
                printf(" %s:,SUBP,\n", symtab[i].c_str());
            } else if ((word & 057000000) == 047000000) {
                printf(" %s:,LC,%d\n", symtab[i].c_str(),
                       (uint)word & 0777777);
            } else if (((word >> 24) & 077774000) == 04000 &&
                       (word & 077777777) < 0100000) {
                uint sym = (word >> 24) & 03777;
                uint off = word & 077777;
                int ioff = off >= 040000 ? -off : off;
                if (noequs) {
                    symtab[i] = strprintf("%s%+d",
                                          symtab[sym].c_str(), ioff);
                } else {
                    symtab[i] = gak(i);
                    printf(" %s:,EQU,%s%+d\n", symtab[i].c_str(),
                           symtab[sym].c_str(), ioff);
                }
            } else  {
                printf("C %5o: %016llo (%s) dunno\n", i, word, gak(i).c_str());
            }
        }
    }

    void dump_symtab() {
        printf("C Symbol table offset is %o\n", table_off);
        for (uint i = head_len; i < head_len + sym_len; ++i) {
            dump_sym(i, table_off);
        }
    }
};

int
main (int argc, char **argv)
{
    int basereg = 0;
    bool nolabels = false, noequs = false, nooctal = false, litconst = false;

    const char * usage = "Usage: %s [-l] [-e] [-o] [-c] [-Rbase] [-d] objfile\n";
    char opt;

    while ((opt = getopt(argc, argv, "cdeloR:")) != -1) {
        switch (opt) {
        case 'l':
            // To produce a compilable assembly code,
            // not needed for decompile.
            nolabels = true;
            break;
        case 'e':
            // To produce a compilable assembly code
            // and to simplify decompilation.
            // If not used, the output will match DTRAN closely.
            noequs = true;
            break;
        case 'o':
            // If not used, the output will match DTRAN closely,
            // if used, the offsets will always be printed in decimal.
            nooctal = true;
            break;
        case 'c':
            litconst = true;
            // References to the const section printed as literals;
            // used for decompilation.
            break;
        case 'R':	/* -RN (decimal) use reg N as base */
            // Say -R8 for decompilation
            basereg = 0;
            while (*optarg >= '0' && *optarg <= '9') {
                basereg *= 10;
                basereg += *optarg - '0';
                ++optarg;
            }
            if (basereg == 0 || basereg > 017) {
                fprintf(stderr, "Bad base reg %o, need 1 <= R <= 15\n",
                        basereg);
                exit(1);
            }
            break;
        case 'd':
            // Decompilation all-in-one mode.
            noequs = true;
            nooctal = true;
            litconst = true;
            basereg = 8;
            break;
        default: /* '?' */
            fprintf(stderr, usage, argv[0]);
            exit(EXIT_FAILURE);
        }
    }

    if (optind >= argc) {
        fprintf (stderr, usage, argv[0]);
        exit (EXIT_FAILURE);
    }
    fprintf(stderr, "Decompiling file %s\n", argv[optind]);
    Dtran dtr(argv[optind], basereg, nolabels, noequs, nooctal);
    dtr.dump_symtab();
    dtr.prtext(litconst);
    dtr.prconst(dtr.cmd_len, dtr.const_len, litconst);
    if (dtr.data_len) {
        printf(" ,DATA,\n");
        if (litconst) {
            dtr.prdata();
        } else {
            dtr.prconst(dtr.cmd_len + dtr.const_len, dtr.data_len, false);
            dtr.prsets();
        }
    }
    printf(" ,END,\n");
    return 0;
}
