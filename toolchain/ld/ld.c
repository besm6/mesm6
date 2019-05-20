/*
 * Linker for micro-BESM.
 * Options:
 *      -o filename     output file name
 *      -u symbol       'use'
 *      -e symbol       'entry'
 *      -D size         set data size
 *      -Taddress       base address of loading
 *      -llibname       library
 *      -x              discard local symbols
 *      -X              discard locals starting with LOCSYM
 *      -S              discard all except locals and globals
 *      -r              preserve rel. bits, don't define common's
 *      -s              discard all symbols
 *      -d              define common even with rflag
 *      -t              tracing
 *      -k              align const and text on page boundary
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

#define WSZ     6               /* длина слова в байтах */
#define LOCSYM  'L'             /* убрать локальные символы, нач. с 'L' */

FILE *input;                    /* input file */

obj_image_t *obj_head, *obj_tail;
/*
 * output management
 */
FILE *outb, *toutb, *doutb, *troutb, *droutb, *soutb;

/*
 * symbol management
 */
struct local {
    long locindex;              /* index to symbol in file */
    struct nlist *locsymbol;    /* ptr to symbol table */
};

#define NSYM        2000
#define NSYMPR      1000
#define NCONST      512
#define LLSIZE      256
#define RANTABSZ    1000

struct nlist cursym;            /* текущий символ */
struct nlist symtab[NSYM];      /* собственно символы */
struct nlist **symhash[NSYM];   /* указатели на хэш-таблицу */
struct nlist *lastsym;          /* последний введенный символ */
struct nlist *hshtab[NSYM+2];   /* хэш-таблица для символов */
struct local local[NSYMPR];
int symindex;                   /* следующий свободный вход таблицы символов */
int newindex[NCONST];           /* таблица переиндексации констант */
long basaddr = 02000;           /* base address of loading */

/*
 * internal symbols
 */
struct nlist *p_etext, *p_edata, *p_end, *entrypt;

/*
 * flags
 */
int     trace;                  /* internal trace flag */
int     xflag;                  /* discard local symbols */
int     Xflag;                  /* discard locals starting with LOCSYM */
int     Sflag;                  /* discard all except locals and globals*/
int     rflag;                  /* preserve relocation bits, don't define commons */
int     arflag;                 /* original copy of rflag */
int     sflag;                  /* discard all symbols */
int     dflag;                  /* define common even with rflag */
int     alflag;                 /* const и text выровнены на границу листа */

/*
 * Cumulative sizes set in pass 1 (in words).
 */
long    text_size, data_size, bss_size;

/*
 * symbol relocation; both passes
 */
long    cur_text_rel, cur_data_rel, cur_bss_rel;

int     ofilfnd;
char    *ofilename = "l.out";
char    *filname;
int     errlev;
int     delarg    = 4;
char    tfname[] = "/tmp/ldaXXXXXX";
char    libname[] = "/usr/local/lib/besm6/libxxxxxxxxxxxxxxx";

#define LNAMLEN 17             /* originally 12 */

#define ALIGN(x,y)     ((x)+(y)-1-((x)+(y)-1)%(y))

/* Needed after pass 1 */
long    torigin;
long    dorigin;
long    borigin;

void delexit()
{
    unlink("l.out");
    if (!delarg && !arflag)
        chmod(ofilename, 0777 & ~umask(0));
    exit(delarg);
}

void error(int n, char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    if (!errlev)
        printf("ld: ");
    if (filname)
        printf("%s: ", filname);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");
    if (n > 1)
        delexit();
    errlev = n;
}

/*
 * Open object file.
 * Return 0 in case of regular file,
 * and 1 in case of archive.
 */
int open_input(char *name)
{
    int c;

    input = 0;
    filname = name;
    if (name[0] == '-' && name[1] == 'l') {
        if (name[2] == '\0')
            name = "-la";
        filname = libname;
        for (c = 0; name[c+2]; c++)
            filname[c + LNAMLEN] = name[c+2];
        filname[c + LNAMLEN] = '.';
        filname[c + LNAMLEN + 1] = 'a';
        filname[c + LNAMLEN + 2] = '\0';
        input = fopen(filname, "r");
        if (! input)
            filname += 4;
    }
    if (! input && ! (input = fopen(filname, "r")))
        error(2, "cannot open");

    if (! fgetint(input, &c))
        error(1, "unexpected EOF");
    fseek(input, 0L, 0);

    if (c == BESM6_MAGIC)
        return 0;       /* regular file */

    return 1;           /* archive */
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
            error(2, "symbol table overflow");
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
            error(2, "out of memory");
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

/*
 * Check whether the object file provides any symbols we need.
 */
int need_this_obj(obj_image_t *obj)
{
    //TODO
    return 1;
}

/*
 * Append object structure to the single-linked list.
 */
void append_to_obj_list(obj_image_t *obj)
{
    if (!obj)
        error(2, "out of memory");

    obj->next = 0;

    if (obj_tail) {
        obj_tail->next = obj;
    } else {
        obj_head = obj;
    }
    obj_tail = obj;
}

/*
 * Pass 1: load one object file.
 * The file can be opened as file descriptor fd,
 * or supplied as byte array of given size.
 * Return 1 in case any names have been resolved,
 * otherwise return 0.
 */
int load1obj(FILE *fd, char *data, unsigned nbytes)
{
    obj_image_t obj = {0};

    if (fd) {
        if (obj_read_fd(fd, &obj) < 0)
            error(2, "bad format");
    } else {
        if (obj_read_data(data, nbytes, &obj) < 0)
            error(2, "bad format");

        /* Does this component have anything useful for us? */
        if (!need_this_obj(&obj)) {
            return 0;
        }
    }

    /* Link in the object file. */
    cur_text_rel = 0;
    cur_data_rel = - obj.cmd_len;
    cur_bss_rel = - obj.cmd_len - obj.const_len;

    cur_text_rel += text_size;
    cur_data_rel += data_size;
    cur_bss_rel += bss_size;

    text_size += obj.cmd_len;
    data_size += obj.const_len;
    bss_size += obj.data_len;

    /* Merge symbols into a common symbol table. */
    merge_symbols(&obj);

    /* Reallocate the object image and add to the list. */
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

/*
 * Read callback for archive.
 */
ssize_t myread(struct archive *a, void *fd, const void **pbuf)
{
    static char buf[4096];

    *pbuf = buf;
    return fread(buf, 1, sizeof(buf), (FILE*)fd);
}

/*
 * Scan file to find defined symbols.
 */
void load1name(char *fname)
{
    if (open_input(fname) == 0) {
        /*
         * Regular file.
         */
        if (trace > 1)
            printf("%s\n", fname);
        load1obj(input, NULL, 0);
    } else {
        /*
         * Archive.
         */
        struct archive *a = archive_read_new();
        struct archive_entry *entry;

        archive_read_support_filter_all(a);
        archive_read_support_format_all(a);
        archive_read_open(a, input, NULL, myread, NULL);
        while (archive_read_next_header(a, &entry) == ARCHIVE_OK) {
            const char *name = archive_entry_pathname(entry);
            unsigned nbytes = archive_entry_size(entry);
            static char data[MAXSZ*6];

            if (trace > 1)
                printf("%s(%s):\n", fname, name);

            if (nbytes > sizeof(data)) {
                error(2, "too long array entry");
            }
            if (archive_read_data(a, data, nbytes) != nbytes) {
                error(2, "read error");
            }
            load1obj(NULL, data, nbytes);
        }
    }
    fclose(input);
}

/*
 * scan files once to find symdefs
 */
void pass1(int argc, char **argv)
{
    int c, i;
    long num;
    char *ap, **p;
    char save;

    p = argv + 1;
    for (c=1; c<argc; ++c) {
        filname = 0;
        ap = *p++;

        if (*ap != '-') {
            load1name(ap);
            continue;
        }
        for (i=1; ap[i]; i++) {
            switch (ap[i]) {

                /* output file name */
            case 'o':
                if (++c >= argc)
                    error(2, "-o: argument missing");
                ofilename = *p++;
                ofilfnd++;
                continue;

                /* 'use' */
            case 'u':
                if (++c >= argc)
                    error(2, "-u: argument missing");
                enter(slookup(*p++));
                continue;

                /* 'entry' */
            case 'e':
                if (++c >= argc)
                    error (2, "-e: argument missing");
                enter(slookup(*p++));
                entrypt = lastsym;
                continue;

                /* set data size in words */
            case 'D':
                if (++c >= argc)
                    error(2, "-D: argument missing");
                num = atoi(*p++);
                if (data_size > num)
                    error(2, "-D: too small");
                data_size = num;
                continue;

                /* base address of loading */
            case 'T':
                basaddr = strtoul(ap+i+1, 0, 0);
                break;

                /* library */
            case 'l':
                save = ap[--i];
                ap[i] = '-';
                load1name(&ap[i]);
                ap[i] = save;
                break;

                /* discard local symbols */
            case 'x':
                xflag++;
                continue;

                /* discard locals starting with LOCSYM */
            case 'X':
                Xflag++;
                continue;

                /* discard all except locals and globals*/
            case 'S':
                Sflag++;
                continue;

                /* preserve rel. bits, don't define common */
            case 'r':
                rflag++;
                arflag++;
                continue;

                /* discard all symbols */
            case 's':
                sflag++;
                xflag++;
                continue;

                /* define common even with rflag */
            case 'd':
                dflag++;
                continue;

                /* tracing */
            case 't':
                trace++;
                continue;

            case 'k':
                alflag++;
                continue;

            default:
                error(2, "unknown flag");
            }
            break;
        }
    }
}

void ldrsym(struct nlist *sp, long val, int type)
{
    if (sp == 0) return;
    if (sp->n_type != N_EXT+N_UNDF) {
        printf("%s: ", sp->n_name);
        error(1, "name redefined");
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
    int nund;
    long cmorigin;

    p_etext = *slookup("_etext");
    p_edata = *slookup("_edata");
    p_end = *slookup("_end");

    /*
     * If there are any undefined symbols, save the relocation bits.
     */
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
    if (rflag) alflag = sflag = 0;

    /*
     * Assign common locations.
     */
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

    /*
     * Now set symbols to their final value
     */
    torigin = basaddr;
    if (alflag)
        torigin = ALIGN(torigin, 1024);
    dorigin = torigin + text_size;
    if (alflag)
        dorigin = ALIGN(dorigin, 1024);
    cmorigin = dorigin + data_size;
    borigin = cmorigin + cmsize;
    nund = 0;
    for (sp=symtab; sp<symp; sp++) {
        switch (sp->n_type) {
        case N_EXT+N_UNDF:
            if (!arflag) errlev |= 01;
            if (!arflag)   {
                if (!nund)
                    printf("Undefined:\n");
                nund++;
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
            error(1, "long address: %s=0%lo", sp->n_name, sp->n_value);
    }
    bss_size += cmsize;
}

void tcreat(FILE **buf, int tempflg)
{
    *buf = fopen(tempflg ? tfname : ofilename, "w+");
    if (! *buf)
        error(2, tempflg ?
            "cannot create temporary file" :
            "cannot create output file");
    if (tempflg)
        unlink(tfname);
}

void setupout()
{
    int fd = mkstemp(tfname);
    if (fd == -1) {
        error(2, "cannot create temporary file %s", tfname);
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
    filhdr.a_magic = alflag ? AMAGIC : FMAGIC;
    filhdr.a_text = text_size;
    filhdr.a_data = data_size;
    filhdr.a_bss = bss_size;
    filhdr.a_syms = ALIGN(symtab_size, WSZ);
    if (entrypt) {
        if (entrypt->n_type != N_EXT+N_TEXT &&
            entrypt->n_type != N_EXT+N_UNDF)
            error(1, "entry out of text");
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

struct nlist *lookloc(struct local *lp, int sn)
{
    struct local *clp;

    for (clp=local; clp<lp; clp++)
        if (clp->locindex == sn)
            return clp->locsymbol;
    if (trace) {
        fprintf(stderr, "*** %d ***\n", sn);
        for (clp=local; clp<lp; clp++)
            fprintf(stderr, "%ld, ", clp->locindex);
        fprintf(stderr, "\n");
    }
    error(2, "bad symbol reference");
    /* NOTREACHED */
    return 0;
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

void relhalf(struct local *lp, long t, long *pt)
{
    long a, ad;
    struct nlist *sp;

    if (trace > 2)
        printf("%08lx", t);

    /* extract address from command */

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

    /* compute address shift `ad' */
    /* update relocation word */

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
    case REXT:
        sp = lookloc(lp, (int) RGETIX(t));
        t &= RSHORT;
        if (sp->n_type == N_EXT+N_UNDF ||
            sp->n_type == N_EXT+N_COMM)
        {
            //t |= REXT | RPUTIX(nsym + (sp - symtab));
            break;
        }
        t |= reltype(sp->n_type);
        ad = sp->n_value;
        break;
    }

    /* add updated address to command */

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

void relocate(struct local *lp, FILE *b1, FILE *b2, long len)
{
    long t;

    len /= WSZ/2;
    while (len--) {
        t = fgeth(input);
        relhalf(lp, t, &t);
        fputh(t, b1);
    }
}

void load2obj(obj_image_t *obj)
{
    struct nlist *sp;
    struct local *lp;
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
        printf("cur_text_rel=%lxh, cur_data_rel=%lxh, cur_bss_rel=%lxh\n",
            cur_text_rel, cur_data_rel, cur_bss_rel);
    /*
     * Re-read the symbol table, recording the numbering
     * of symbols for fixing external references.
     */
    lp = local;
    symno = -1;
    for (;;) {
        symno++;
        count = fgetsym(input, &cursym);
        if (count == 0)
            error(2, "out of memory");
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
            error(2, "internal error: symbol not found");
        free(cursym.n_name);
        if (cursym.n_type == N_EXT+N_UNDF ||
            cursym.n_type == N_EXT+N_COMM)
        {
            if (lp >= &local[NSYMPR])
                error(2, "local symbol table overflow");
            lp->locindex = symno;
            lp++->locsymbol = sp;
            continue;
        }
        if (cursym.n_type != sp->n_type ||
            cursym.n_value != sp->n_value)
        {
            printf("%s: ", cursym.n_name);
            error(1, "name redefined");
        }
    }

    if (trace > 1)
        printf("** TEXT **\n");
    relocate(lp, toutb, troutb, obj->cmd_len);

    if (trace > 1)
        printf("** DATA **\n");
    relocate(lp, doutb, droutb, obj->const_len);

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
    long n;
    struct nlist *p;

    if (alflag) {
        /* now torigin points to the end of text */
        n = torigin;
        while (n & 01777) {
            n++;
            fputh(0L, toutb);
            fputh(0L, toutb);
            if (rflag) {
                fputh(0L, troutb);
                fputh(0L, troutb);
            }
        }
    }
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
    if (argc == 1) {
        printf("Usage: %s [-xXsSrndt] [-lname] [-D num] [-u name] [-e name] [-o file] file...\n",
            argv[0]);
        exit(4);
    }
    if (signal(SIGINT, SIG_IGN) != SIG_IGN)
        signal(SIGINT, delexit);
    if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
        signal(SIGTERM, delexit);

    /*
     * Первый проход: вычисление длин сегментов и таблицы имен,
     * а также адреса входа.
     */
    pass1(argc, argv);
    filname = 0;

    /*
     * Обработка таблицы имен.
     */
    middle();

    /*
     * Создание буферных файлов и запись заголовка
     */
    setupout();

    /*
     * Второй проход: настройка связей.
     */
    pass2(argc, argv);

    /*
     * Сброс буферов.
     */
    finishout();

    if (!ofilfnd) {
        unlink("a.out");
        link("l.out", "a.out");
        ofilename = "a.out";
    }
    delarg = errlev;
    delexit();
    return 0;
}
