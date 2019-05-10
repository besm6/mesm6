/*
 * A straightforward conversion of the Pascal-Monitor compiler
 * to C++. The compiler must be called with two arguments, infile and outfile.
 * Infile is the Pascal source in ASCII/UTF-8; outfile is a big-endian
 * bytestream representation of the object module.
 * The original compiler was forming the object module in an
 * "unpacked" form: the section lengths occupied one word each.
 * It was the job of the monitor system to pack it before putting
 * into the temporary/personal library.
 */
#include <cstdio>
#include <string>
#include <vector>
#include <map>
#include <cstdlib>
#include <stdint.h>
#include <cmath>
#include <cstring>
#include <sstream>
#include <wctype.h>

FILE * pasinput = stdin;
unsigned char PASINPUT;

const char * boilerplate = " Pascal-Monitor in C++ (17.05.2019)";

const int64_t
fnSQRT  = 0,  fnSIN  = 1,  fnCOS  = 2,  fnATAN  = 3,  fnASIN = 4,
    fnLN    = 5,  fnEXP  = 6,  fnABS = 7,  fnTRUNC = 8,  fnODD  = 9,
    fnORD   = 10, fnCHR  = 11, fnSUCC = 12, fnPRED  = 13, fnEOF  = 14,
    fnREF   = 15, fnEOLN = 16, fnSQR = 17, fnROUND = 18, fnCARD = 19,
    fnMINEL = 20, fnPTR  = 21, fnABSI = 22, fnSQRI  = 23, fn24 = 24, fn29 = 29;

const int64_t
S3 = 0,
    S4 = 1,
    S5 = 2,
    S6 = 3,
    NoPtrCheck = 4,
    NoStackCheck = 5,
    DebugInteractive = 44,
    DebugCode  = 45,
    DebugPrint = 46,
    DebugEntry = 47;

const int64_t
errBooleanNeeded = 0,
    errIdentAlreadyDefined = 2,
    errNoIdent = 3,
    errNotAType = 4,
    errNoConstant = 6,
    errConstOfOtherTypeNeeded = 7,
    errTypeMustNotBeFile = 9,
    errNotDefined = 11,
    errBadSymbol = 12,
    errNeedOtherTypesOfOperands = 21,
    errWrongVarTypeBefore = 22,
    errUsingVarAfterIndexingPackedArray = 28,
    errNoSimpleVarForLoop = 30,
    errTooManyArguments = 38,
    errNoCommaOrParenOrTooFewArgs = 41,
    errNumberTooLarge = 43,
    errVarTooComplex = 48,
    errEOFEncountered = 52,
    errFirstDigitInCharLiteralGreaterThan3 = 60;

const int64_t
macro = 0100000000,
    mcACC2ADDR = 6,
    mcPOP = 4,
    mcPUSH = 5,
    mcMULTI = 7,
    mcADDSTK2REG = 8,
    mcADDACC2REG = 9,
    mcODD = 10,
    mcSQRR = 12,
    mcROUND = 11,
    mcMINEL = 15,
    mcSQRI = 13,
    mcPOP2ADDR = 19,
    mcCARD = 23;

const int64_t    ASN64 = 0360100,

    ASCII0 =    04000007,
    E1 =        04000010,
    ZERO =      04000011,
    MULTMASK =  04000012,
    MANTISSA =  04000014,
    MINUS1 =    04000017,
    PLUS1 =     04000021,
    BITS15 =    04000022,
    REAL05 =    04000023,
    ALLONES =   04000024,
    HEAPPTR =   04000027,

    KATX =      0000000,
    KXTS =      0030000,
    KADD =      0040000,
    KSUB =      0050000,
    KRSUB =     0060000,
    KAMX =      0070000,
    KXTA =      0100000,
    KAAX =      0110000,
    KAEX =      0120000,
    KARX =      0130000,
    KAVX =      0140000,
    KAOX =      0150000,
//    KDIV =      0160000,
    KMUL =      0170000,
    KAPX =      0200000,
    KAUX =      0210000,
    KACX =      0220000,
    KANX =      0230000,
    KYTA =      0310000,
//    KASN =      0360000,
    KNTR =      0370000,
    KATI =      0400000,
//    KSTI =      0410000,
    KITA =      0420000,
    KITS =      0430000,
    KMTJ =      0440000,
    KJADDM =    0450000,
    KE74 =      0740000,
    KUTC =      02200000,
//    CUTC =      02200000,
    KWTC =      02300000,
//    CWTC =      02300000,
    KVTM =      02400000,
    KUTM =      02500000,
//    KUZA =      02600000,
//    \KU1A =      02700000,
    KUJ =       03000000,
    KVJM =      03100000,
    KVZM =      03400000,
//    KV1M =      03500000,
    KVLM =      03700000,

    I7 =        034000000,      /* frame pointer */
    I8 =        040000000,      /* const pointer */
    I9 =        044000000,      /* temp register */
    I10 =       050000000,      /* temp register */
    I11 =       054000000,      /* temp register */
    I12 =       060000000,      /* temp register */
    I13 =       064000000,      /* link register */
    I14 =       070000000,      /* temp register */
    SP =        074000000;      /* stack pointer, reg 15 */

enum Symbol {
/*0B*/  IDENT,      INTCONST,   REALCONST,  CHARCONST,
        LTSY,       GTSY,       NOTSY,      LPAREN,
/*10B*/ LBRACK,     MULOP,      ADDOP,      RELOP,
        RPAREN,     RBRACK,     COMMA,      SEMICOLON,
/*20B*/ PERIOD,     ARROW,      COLON,      BECOMES,
        LABELSY,    CONSTSY,    TYPESY,     VARSY,
/*30B*/ FUNCSY,     PROCSY,     SETSY,      PACKEDSY,
        ARRAYSY,    RECORDSY,   FILESY,     BEGINSY,
/*40B*/ IFSY,       CASESY,     REPEATSY,   WHILESY,
        FORSY,      WITHSY,     GOTOSY,     ENDSY,
/*50B*/ ELSESY,     UNTILSY,    OFSY,       DOSY,
        TOSY,       DOWNTOSY,   THENSY,     SELECTSY,
/*60B*/ PROGRAMSY,  OTHERSY,    LBRACE,     RBRACE,
        NOSY
};

enum IdClass {
        TYPEID,     ENUMID,     ROUTINEID,  VARID,
        FORMALID,   FIELDID
};

enum Insn {
/*000*/ ATX,   STX,   OP2,   XTS,   ADD,   SUB,   RSUB,  AMX,
/*010*/ XTA,   AAX,   AEX,   ARX,   AVX,   AOX,   ADIVX, AMULX,
/*020*/ APX,   AUX,   ACX,   ANX,   EADD,  ESUB,  ASX,   XTR,
/*030*/ RTE,   YTA,   OP32,  OP33,  EADDI, ESUBI, ASN,   NTR,
/*040*/ ATI,   STI,   ITA,   ITS,   MTJ,   JADDM, ELFUN,
/*047*/ UTC,   WTC,   VTM,   UTM,   UZA,   U1A,   UJ,    VJM
};


enum Operator {
    MUL,        RDIVOP,     AMPERS,     IDIVOP,     IMODOP,
    PLUSOP,     MINUSOP,    OROP,       NEOP,       EQOP,
    LTOP,       GEOP,       GTOP,       LEOP,       INOP,
    IMULOP,     IDIVROP,    SETAND,     SETXOR,     SETOR,
    SETSUB,     INTPLUS,    INTMINUS,   badop27,    badop30,
    badop31,    MKRANGE,    ASSIGNOP,   GETELT,     GETVAR,
    op36,       op37,       GETENUM,    GETFIELD,   DEREF,
    FILEPTR,    op44,       ALNUM,      PCALL,      FCALL,
    BOUNDS,     TOREAL,     NOTOP,      INEGOP,     RNEGOP,
    STANDPROC,  NOOP
};

enum OpGen {
    gen0,  STORE, LOAD,  gen3,  SETREG,
    gen5,  gen6,  gen7,  gen8,  gen9,
    gen10, gen11, gen12, FILEACCESS, gen14,
    gen15, gen16, LITINSN
};

// Flags for ops that can potentially be optimized if one operand is a constant
enum OpFlg {
    opfCOMM, opfHELP, opfAND, opfOR, opfDIV, opfMOD, opfMULMSK, opfASSN, opfINV
};

enum Kind {
    kindReal, kindScalar, kindRange, kindPtr,
    kindSet, kindArray, kindRecord, kindFile,
    kindCases
};

struct Bitset {
    uint64_t val:48;
    Bitset operator *(Bitset x) const { x.val &= val; return x; }
    Bitset operator +(Bitset x) const { x.val |= val; return x; }
    Bitset operator -(Bitset x) const { x.val = val & ~x.val; return x; }
    Bitset operator ^(Bitset x) const { x.val ^= val; return x; }
    Bitset operator <<(int x) const {
        Bitset ret;
        ret.val = (val << x) & ((1L<<48)-1);
        return ret;
    }
    Bitset operator >>(int x) const {
        Bitset ret;
        ret.val = (val >> x)  & ((1L<<48)-1);
        return ret;
    }
    bool operator==(Bitset x) const { return val == x.val; }
    bool operator!=(Bitset x) const { return !(*this == x); }
    operator bool() const { return val != 0; }
    bool has(int64_t b) const {
            return b < 48 && (val >> (47-b)) & 1;
    }
    bool operator <=(Bitset x) const {
        return !(val & ~x.val);
    }
};

Bitset mkbs() {
    Bitset ret;
    ret.val = 0;
    return ret;
}

Bitset mkbs(int64_t bit) {
    Bitset ret;
    ret.val = (1L << (47-bit)) & ((1L<<48)-1);
    return ret;
}
Bitset mkbs(int64_t bit1, int64_t bit2) {
    return mkbs(bit1)+mkbs(bit2);
}
Bitset mkbs(int64_t bit1, int64_t bit2, int64_t bit3) {
    return mkbs(bit1)+mkbs(bit2)+mkbs(bit3);
}
Bitset mkbs(int64_t bit1, int64_t bit2, int64_t bit3, int64_t bit4) {
    return mkbs(bit1, bit2)+mkbs(bit3, bit4);
}

Bitset mkbsr(int64_t bit1, int64_t bit2) {
    Bitset ret;
    ret.val = 0;
    for (; bit1 <= bit2; ++bit1) ret = ret + mkbs(bit1);
    return ret;
}

typedef Bitset SetOfSYs; // set of ident .. selectsy;

struct Integer {
    union { uint64_t val:48;
        struct { int64_t ival:41; unsigned exp:7; };
    };
    int64_t operator=(int64_t i);
    operator int64_t() const { return ival; }
};

int64_t Integer::operator=(int64_t i) {
    val = i;
    if (exp != 0 && exp != 127 && exp != 104)
        fprintf(stderr, "Data corruption!\n");
    exp = 104;
    return i;
}

struct Real {
    int64_t mantissa:41;
    unsigned exponent:7;
    void operator=(int64_t i) {
        mantissa = i  & ((1L<<48)-1); exponent = 104;
        if (mantissa == 0)
            exponent = 0;
        else
            while ((mantissa >> 39) == 0 || (mantissa >> 39) == -1) { exponent--; mantissa <<= 1; }
    }
    void operator=(Integer i) { (*this) = i.ival; }
    operator double() const {
        return ldexp(mantissa, exponent-104);
    }
    std::string print() const;
    void operator=(double d) {
        int exp;
        double mant = frexp(d, &exp);
        mantissa = ldexp(mant, 40);
        exponent = exp + 64;
    }
};

std::string Real::print() const {
    std::ostringstream ostr;
    ostr << double(*this);
    return ostr.str();
}

int64_t heap[32768];
int64_t avail = 100;

void * besm6_alloc(size_t s) {
    s = (s + 7) & ~7;
    s /= sizeof(int64_t);
    if (avail + s > 32768) {
        fprintf(stderr, "Out of memory: avail = %ld, wants %lu words\n", avail, s);
        throw std::bad_alloc();
    }
    avail += s;
    return heap + avail - s;
}

template<class T> void setup(T * &p) {
    p = reinterpret_cast<T*>(heap + avail);
}

template<typename T> void succ(T & v) {
    v = (T)(int(v)+1);
}

void rollup(void * p) {
    if (p < heap || p > heap + avail) {
        fprintf(stderr, "Cannot rollup from %p to %p\n", (void*)(heap + avail), p);
        exit(1);
    }
    avail = reinterpret_cast<int64_t*>(p) - heap;
    if (heap + avail != p) {
        fprintf(stderr, "Cannot rollup to unaligned pointer %p\n", p);
        exit(1);
    }
}

// We need to be able to produce NULL, which must not be equal to ptr(0).
// In the BESM-6, NIL was equal to 074000.
void * ptr(int64_t x) {
    if (x == 074000) return NULL;
    if (x < 0 || x >= avail) {
        fprintf(stderr, "Cannot convert %ld to a pointer, avail = %ld\n", x, avail);
        exit(1);
    }
    return heap + x;
}

int64_t ord(void * p) {
    int64_t ret = reinterpret_cast<int64_t>(p);
    if (p == NULL) return 074000;
    if (ret < avail || ret <= 100) return ret;
    if (p < heap || p >= heap + avail) {
        fprintf(stderr, "Invalid pointer to integer conversion, %p is outside of valid heap range %p-%p\n",
                p, (void*)heap, (void*)(heap + avail));
        exit(1);
    }
    if (heap + (reinterpret_cast<int64_t*>(p) - heap) != p) {
        fprintf(stderr, "Unaligned pointer to integer conversion: %p\n", p);
        exit(1);
    }
    return reinterpret_cast<int64_t*>(p) - heap;
}

typedef struct Expr * ExprPtr;
typedef struct Types * TypesPtr;
typedef struct IdentRec * IdentRecPtr;

struct Alfa {
    uint64_t val:48;
    unsigned char operator[](int64_t i) const { return (val >> (48-8*i)) & 0xFF; }
    void put(int64_t i, unsigned char c) {
        c ^= (*this)[i];
        val = (val ^ (uint64_t(c) << (48-8*i))) & 0xFFFFFFFFFFFFL;
    }
    // Mimics BESM-6 exactly, but is not transitive: the list of literals can have repetitions.
    bool operator<(const Alfa & x) const {
        uint64_t tmp = val + (x.val ^ 0xFFFFFFFFFFFFL);
        tmp = (tmp + (tmp >> 48)) & 0xFFFFFFFFFFFFL;
        return tmp >> 47;
    }
    // Better use
    // bool operator<(const Alfa & x) const { return val < x.val; }

    std::string print() const;
};

std::string Alfa::print() const {
    std::string ret;
    for (int i = 1; i <= 6; ++i)
        ret += (*this)[i];
    return ret;
}

void unpck(unsigned char & to, Alfa & from) {
    unsigned char * p = &to;
    for (int i = 0; i < 6; ++i) {
        p[i] = from[i+1];
    }
}

void pck(unsigned char & from, Alfa & to) {
    unsigned char * p = &from;
    for (int i = 0; i < 6; ++i) {
        to.put(i+1, p[i]);
    }
}

struct Word {
    union {
        int64_t ii;
        Integer i;
        Real r;
        bool b;
        Alfa a;
        int64_t  c;
        IdClass cl;
        Bitset m;
    };
    bool operator==(const Word & x) const { return i == x.i; }
    bool operator!=(const Word & x) const { return i != x.i; }
};
typedef struct OneInsn * OneInsnPtr;

struct OneInsn {
    void * operator new(size_t s) {
        return besm6_alloc(s);
    }
    OneInsnPtr next;
    int64_t mode, code, offset;
};

enum ilmode { ilCONST, il1, il2, il3 };
enum state {st0, st1, st2};


struct InsnList {
    void * operator new(size_t s) {
        return besm6_alloc(s);
    }

    OneInsnPtr next, next2;
    TypesPtr typ;
    Bitset regsused;
    ilmode ilm;
    Word ilf5;
    int64_t ilf6;
    int64_t ilf7;
    state st;
    int64_t width, shift;
};

typedef InsnList * InsnListPtr;

struct Types {
    void * operator new(size_t s) {
        return besm6_alloc(s);
    }

    int64_t size,
    bits;
    Kind k;
    union {
//    kindReal:   ();

//    kindRange:
        struct {
            TypesPtr base;
            int64_t checker, left, right;
        };
//    kindArray:
        struct {
            TypesPtr abase, range;
            bool pck;
            int64_t perWord, pcksize;
        };
//    kindScalar:
        struct {
            IdentRecPtr enums;
            int64_t numen, start;
        };
//    kindSet, kindPtr:
        struct {
            TypesPtr sbase;
        };
//    kindFile:
        struct {
            TypesPtr fbase;
            int64_t elsize;
        };
//    kindRecord:
        struct {
            IdentRecPtr ptr1, ptr2;
            bool flag, pckrec;
        };
// kindCases:
        struct {
            Word sel;
            TypesPtr first, next, r6;
        };
    };
    Types(int64_t s_, int64_t b_, Kind k_) :
        size(s_), bits(b_), k(k_) { }
    Types(int64_t s_, int64_t b_, Kind k_, TypesPtr _sbase) :
        size(s_), bits(b_), k(k_), sbase(_sbase) { }
    Types(int64_t s_, int64_t b_, Kind k_, Word sel_, TypesPtr f_, TypesPtr n_, TypesPtr r_) :
        size(s_), bits(b_), k(k_), sel(sel_), first(f_), next(n_), r6(r_) { }
    Types() { }
    std::string p() const;
};

std::string Types::p() const {
    std::ostringstream ostr;
    switch (k) {
    case kindReal:
        return "REAL";
    case kindRange:
        ostr << "base: " << base->p() << " range: " << left << ".." << right;
        return ostr.str();
    case kindArray:
        if (pck) ostr << "packed ";
        ostr << "array [" << range->p() << "] of " << base->p();
        return ostr.str();
    case kindSet:
        ostr << "set of " << sbase->p();
        return ostr.str();
    case kindPtr:
        ostr << "ptr to " << sbase->p();
        return ostr.str();
    case kindFile:
        ostr << "file of " << fbase->p();
        return ostr.str();
    case kindScalar:
        return "scalar ";
    default:
        return "complex type";
    }
}

struct TypeChain {
    void * operator new(size_t s) {
        return besm6_alloc(s);
    }
    TypeChain * next;
    TypesPtr type1, type2;
    TypeChain(TypeChain * n, TypesPtr t1, TypesPtr t2) : next(n), type1(t1), type2(t2) { }
};

typedef char charmap[128];
typedef char textmap[128];

typedef int64_t four[5]; // [1..4]
typedef Bitset Entries[43]; // [1..42]

struct Expr {
    void * operator new(size_t s) {
        return besm6_alloc(s);
    }
    union {
//    NOOP:
        struct {
            Word val;
            Operator op;
            Word d1, d2;
        };
//    MUL:
        struct {
            TypesPtr typ;
            Word d3;
            ExprPtr expr1, expr2;
        };
//    BOUNDS:
        struct {
            Word d4, d5;
            TypesPtr typ1, typ2;
        };
//    NOTOP:
        struct {
            Word d6, d7;
            IdentRecPtr id1, id2;
        };
//    STANDPROC:
        struct {
            Word d8, d9;
            int64_t num1, num2;
        };
    };
};

struct KeyWord {
    void * operator new(size_t s) {
        return besm6_alloc(s);
    }
    Word w;
    Symbol sym;
    Operator op;
    KeyWord * next;
};

struct StrLabel {
    /*
    void * operator new(size_t s) {
        return besm6_alloc(s);
    }
    */
    StrLabel * next;
    Word ident;
    int64_t offset;
    int64_t exitTarget;
};

struct NumLabel {
    void * operator new(size_t s) {
        return besm6_alloc(s);
    }
    Word id;
    int64_t line, frame, offset;
    NumLabel * next;
    bool defined;
};

std::string toAscii(Bitset val) {
    std::string ret;
    for (int i = 0; i < 8; ++i) {
        int c = (val.val >> (42-(i*6))) & 077;
        if (c == 0) ret += ' ';
        else if (020 <= c && c <= 031) ret += char(c-020+'0');
        else if (041 <= c && c <= 072) ret += char (c-041+'A');
        else if (c == 012) ret += '*';
        else if (c == 017) ret += '/';
        else ret += '?';
    }
    return ret;
}

struct IdentRec {
    void * operator new(size_t s) {
        return besm6_alloc(s);
    }
    Word id;
    int64_t offset;
    IdentRecPtr next;
    TypesPtr typ;
    IdClass cl;
    union {
        // TYPEID,    VARID:  ();
        // ENUMID, FORMALID:
        struct {
            IdentRecPtr list;
            int64_t value;
        };
        // FIELDID:
        struct {
            int64_t maybeUnused;
            TypesPtr uptype;
            bool pckfield;
            int64_t shift, width;
        };

        // ROUTINEID:
        struct {
            int64_t low, high;
            IdentRecPtr argList, preDefLink;
            int64_t level, pos;
            Bitset flags;
        };
    };
    std::string p() const {
        std::string ret;
        char * strp;
        switch (cl) {
        default: return  std::string();
        case ROUTINEID:
            ret = toAscii(id.m) + ": routine";
            asprintf(&strp, "low: %ld high: %ld argl: %ld predef: %ld level: %ld pos: %ld flags: %lx",
                     low, high, ord(argList), ord(preDefLink), level, pos, flags.val);
            ret += strp;
            free(strp);
        }
        return ret;
    }
    IdentRec(Word id_, int64_t o_, IdentRecPtr n_, TypesPtr t_, IdClass cl_) :
        id(id_), offset(o_), next(n_), typ(t_), cl(cl_) { }
    IdentRec(Word id_, int64_t o_, IdentRecPtr n_, TypesPtr t_, IdClass cl_, IdentRecPtr l_, int64_t v_) :
        id(id_), offset(o_), next(n_), typ(t_), cl(cl_), list(l_), value(v_) { }
    IdentRec() { }
};

struct ExtFileRec {
    void * operator new(size_t s) {
        return besm6_alloc(s);
    }
    Word id;
    Word offset;
    ExtFileRec * next;
    int64_t location, line;
};

enum numberSuffix { noSuffix, suffixB, suffixT, suffixC};


// Globals

numberSuffix suffix;
SetOfSYs   bigSkipSet, statEndSys, blockBegSys, statBegSys,
    skipToSet, lvalOpSet;

bool   bool47z, bool48z, bool49z;
bool   dataCheck;
int64_t    jumpType, jumpTarget, int53z;
Operator charClass;
Symbol   SY, prevSY;
int64_t savedObjIdx,
    FcstCnt,
    symTabPos,
    entryPtCnt,
    fileBufSize;

ExprPtr   expr62z, expr63z;
int64_t curInsnTemplate,
    maxLineLen,
    linePos,
    prevErrPos,
    errsInLine,
    moduleOffset,
    lineStartOffset,
    curFrameRegTemplate,
    curProcNesting,
    totalErrors,
    lineCnt,
    bucket,
    strLen,
    heapCallsCnt,
    heapSize,
    arithMode;

std::string stmtName;
KeyWord * keyWordHashPtr;
Kind curVarKind;
ExtFileRec * curExternFile;
char commentModeCH;
unsigned char CH;
int64_t debugLine,
    lineNesting,
    FcstCountTo500,
    objBufIdx,
    int92z, int93z, int94z,
    prevOpcode,
    charEncoding,
    int97z;

bool atEOL,
    checkTypes,
    isDefined, putLeft, bool102z,
    errors,
    declExternal,
    rangeMismatch,
    doPMD,
    checkBounds,
    fuzzReals,
    fixMult,
    bool110z,
    pseudoZ,
    allowCompat,
    checkFortran;
IdentRecPtr outputFile,
    inputFile,
    programObj,
    hashTravPtr,
    uProcPtr;

ExtFileRec * externFileList;
TypesPtr
typ120z, typ121z,
    pointerType,
    setType,
    BooleanType,
    textType,
    IntegerType,
    RealType,
    CharType,
    AlfaType,
    arg1Type,
    arg2Type;
NumLabel *  numLabList;
TypeChain * chain;
Word curToken, curVal;
Bitset O77777, intZero, extSymMask, halfWord, leftInsn;
Word  hashMask, curIdent;
Bitset toAlloc, set145z, set146z, set147z, set148z;
Word optSflags, litOct, litExternal, litForward, litFortran;
ExprPtr uVarPtr, curExpr;
InsnList *  insnList;
ExtFileRec * fileForOutput, * fileForInput;
int64_t maxSmallString, extSymAdornment;

TypesPtr smallStringType[7]; // [2..6]
int64_t symTabCnt;

Word symTabArray[81]; // array [1..80] of Word;
int64_t symTabIndex[81]; // array [1..80] of Integer;
Operator iMulOpMap[48]; // array [MUL..IMODOP] of Operator;
Operator setOpMap[48]; // array [MUL..MINUSOP] of Operator;
Operator iAddOpMap[48]; // array [PLUSOP..MINUSOP] of Operator;
Entries entryPtTable;
four frameRestore[7]; // array [3..6] of four;
int64_t indexreg[16]; // array [1..15] of Integer;
int64_t opToInsn[48]; // array [MUL..op44] of Integer;
int64_t opToMode[48]; // array [MUL..op44] of Integer;
OpFlg opFlags[48]; // array [MUL..op44] of OpFlg;
int64_t funcInsn[24]; // array [0..23] of Integer;
int64_t InsnTemp[48]; // array [Insn] of Integer;

int64_t frameRegTemplate = 04000000,
    constRegTemplate = I8,
    disNormTemplate = KNTR+7;

char lineBufBase[132]; // array [1..130] of char;
int64_t errMapBase[10]; // array [0..9] of Integer;
Operator chrClassTabBase[256]; // array ['_000'..'_177'] of Operator;
KeyWord * KeyWordHashTabBase[128]; // array [0..127] of @KeyWord;
Symbol charSymTabBase[256]; // array ['_000'..'_177'] of Symbol;
IdentRecPtr symHashTabBase[128]; // array [0..127] of IdentRecPtr;
IdentRecPtr typeHashTabBase[128]; //array [0..127] of IdentRecPtr;
int64_t helperMap[100]; // array [1..99] of Integer;
extern Bitset helperNames[100]; // array [1..99] of Bitset;

Bitset symTab[075501]; // array [74000B..75500B] of Bitset;
extern int64_t systemProcNames[30]; // array [0..29] of Integer;
extern int64_t resWordNameBase[30]; // array [0..29] of Integer;
int64_t longSymCnt;
int64_t longSymTabBase[91]; // array [1..90] of Integer;
Bitset longSyms[91]; // array [1..90] of Bitset;
Word constVals[501]; // array [1..500] of Alfa;
int64_t constNums[501]; // array [1..500] of Integer;
Bitset objBuffer[1025]; // array [1..1024] of Bitset;
char koi2text[256];
std::vector<Bitset> FCST; // file of Bitset; /* last */

std::vector<Bitset> CHILD; // file of Bitset;

struct PasInfor {
    int64_t listMode;
    int64_t startOffset;
} PASINFOR;

struct programme {
    programme(int64_t & l2arg1z, IdentRecPtr l2idr2z_);
// label 22420, 22421, 23301;

    IdentRecPtr l2idr2z;
    IdentRecPtr preDefHead, typelist, scopeBound, l2var4z, curIdRec, workidr;
    bool isPredefined, l2bool8z, inTypeDef;
    ExprPtr l2var10z;
    int64_t l2int11z;
    Word l2var12z;
    TypesPtr l2typ13z, l2typ14z;
    NumLabel * l2var15z, * l2var16z;
    StrLabel * strLabList;

    int64_t l2int18z, ii, localSize, l2int21z, jj;
    static std::vector<programme *> super;
    programme();
    ~programme() {
        super.pop_back();
    }
};

std::vector<programme *> programme::super;

const char * pasmitxt(int64_t errNo) {
    switch (errNo) {
    case errBooleanNeeded: return "Boolean required";
    case errIdentAlreadyDefined: return "Identifier already defined";
    case errNoIdent: return "Missing identifier";
    case errNotAType: return "Not a type";
    case errNoConstant: return "Missing constant";
    case errConstOfOtherTypeNeeded: return "Constant of other type required";
    case errTypeMustNotBeFile: return "Type must not be a file type";
    case errNotDefined: return "Unknown identifier";
    case errBadSymbol: return "Bad symbol";
    case errNeedOtherTypesOfOperands: return "Other types of operands required";
    case errNumberTooLarge: return "Number too large";
    case errNoCommaOrParenOrTooFewArgs: return "No comma or parenthesis, or too few args";
//    errWrongVarTypeBefore = 22,
//    errUsingVarAfterIndexingPackedArray = 28,
    case errNoSimpleVarForLoop: return "Undefined variable in FOR loop";
//    errTooManyArguments = 38,
//    errVarTooComplex = 48,
//    errFirstDigitInCharLiteralGreaterThan3 = 60;
    case 1: return "No comma nor semicolon";
    case 16: return "Label not defined in block";
    case 29: return "Index out of bounds";
    case 33: return "Illegal types for assignment";
    case 37: return "Missing INPUT file in program header";
    case 49: return "Too many instructions in a block";
    case 50: return "Symbol table overflow";
    case 51: return "Long symbol overflow";
    case 52: return "EOF encountered";
    case 54: return "Error in pseudo-comment";
    case 55: return "More than 16 digits in a number";
    case 61: return "Empty string";
    case 62: return "Integer needed";
    case 63: return "Bad base type for set";
    case 77: return "Missing OUTPUT file in program header";
    case 79: return "Unknown identifier in type definition";
    case 81: return "Procedure nesting is too deep";
    case 82: return "Previous declaration was not FORWARD";
    case 84: return "Error in declarations";
    case 85: return "Routines left undefined";
    case 86: return "Required token not found: ";
    case 88: return "Different types of case labels and expression";
    case 89: return "integer";
    case 95: return "LPAREN";
    case 96: return "LBRACK";
    case 100: return "RPAREN";
    case 101: return "RBRACK";
    case 102: return "COMMA";
    case 103: return "SEMICOLON";
    case 104: return "PERIOD";
    case 105: return "ARROW";
    case 106: return "COLON";
    case 107: return "ASSIGN";
    case 136: return "PROGRAM";
    }
    return "Dunno";
}

void printErrMsg(int64_t errNo)
{ /* PrintErrMsg */
    putchar(' ');
    if (errNo >= 200)
        printf("Internal error %ld", errNo);
    else {
        if (errNo > 88)
            printErrMsg(86);
        else if (errNo == 20)
            errNo = (SY == IDENT)*2 + 1;
        else if (16 <= errNo && errNo <= 18)
            printf("%ld ", int64_t(curToken.i));
        printf("%s ", pasmitxt(errNo));
        if (errNo == 17)
            printf("%ld", int97z);
        else if (errNo == 22)
            printf("%6s", stmtName.c_str());
    }
    if (errNo != 86 && errNo != 78 && errNo != 79)
        putchar('\n');
} /* PrintErrMsg */

void printTextWord(Word val)
{
    const char *s = toAscii(val.m).c_str();
    while (*s == ' ')
        s++;
    fputs(s, stdout);
}

int64_t toText(const char * str) {
    int64_t ret;
    ret = 0;
    for (; *str; ++str)
        ret = ret << 6 | koi2text[*str & 0xFF];
    return ret;
}

void makeStringType(TypesPtr & res)
{
    TypesPtr span;
    if (maxSmallString >= strLen)
        res = smallStringType[strLen];
    else {
        span = new Types;
        res = new Types;
        /* with span@ do */ {
            span->size = 1;
            span->checker = 0;
            span->bits = 12;
            span->k = kindRange;
            span->base = IntegerType;
            span->left = 1;
            span->right = strLen;
        };
        /* with res@ do */ {
            res->size = (strLen + 5) / 6;
            if (res->size == 1)
                res->bits = strLen * 8;
            else
                res->bits = 0;
            res->k = kindArray;
            res->base = CharType;
            res->range = span;
            res->pck = true;
            res->perWord = 6;
            res->pcksize = 8;
        }
    }
} /* makeStringType */

void addToHashTab(IdentRecPtr arg)
{
    // curVal.m := arg->id.m * hashMask.m;
    // mapai(curVal.a, curVal.i);
    curVal.i = (arg->id.m.val % 65535) % 128;
    arg->next = symHashTabBase[curVal.i];
    symHashTabBase[curVal.i] = arg;
} /* addToHashTab */

void error(int64_t errNo);

void storeObjWord(Bitset insn)
{
    objBuffer[objBufIdx] = insn;
    moduleOffset = moduleOffset + 1;
    if (objBufIdx == 1024) {
        error(49); /* errTooManyInsnsInBlock */
        objBufIdx = 1;
    } else
        objBufIdx = objBufIdx + 1;
} /* storeObjWord */

void form1Insn(int64_t arg)
{
    Word Insn, opcode;
    Bitset half1, half2;
    int64_t pos;
    Insn.i = arg;
    opcode.i = Insn.i & ~077777;
    if (opcode.i == InsnTemp[UJ]) {
        if (prevOpcode == opcode.i)
            return;
        if (putLeft and (prevOpcode == 1)) {
            pos = objBufIdx - 1;
            if (objBuffer[pos] * mkbsr(0, 8) == mkbs(0, 1, 8)+mkbsr(3,5)) {
                prevOpcode = opcode.i;
                half1 = (Insn.m * mkbsr(33,47)) << 24;
                half2 = (objBuffer[pos] * mkbsr(9,23)) >> 24;
                objBuffer[pos] = mkbs(0, 1, 3, 4)+mkbs(6, 28, 29) +
                    half1 + half2;
                return;
            }
        }
    };
    prevOpcode = opcode.i;
    if (putLeft) {
        leftInsn = (Insn.m * halfWord) << 24;
        putLeft = false;
    } else {
        putLeft = true;
        storeObjWord(leftInsn + (Insn.m * halfWord));
    }
} /* form1Insn */

void form2Insn(int64_t i1, int64_t i2)
{
    form1Insn(i1);
    form1Insn(i2);
} /* form2Insn */

void form3Insn(int64_t i1, int64_t i2, int64_t i3)
{
    form2Insn(i1, i2);
    form1Insn(i3);
} /* form3Insn */

void disableNorm()
{
    if (arithMode != 1) {
        form1Insn(disNormTemplate);
        arithMode = 1;
    }
} /* disableNorm */

int64_t getObjBufIdxPlus()
{
    if (putLeft)
        return objBufIdx + 4096;
   else
       return objBufIdx;
} /* getObjBufIdxPlus */


void formJump(int64_t & arg)
{
    int64_t pos;
    bool isLeft;
    if (prevOpcode != InsnTemp[UJ]) {
        pos = getObjBufIdxPlus();
        isLeft = putLeft;
        form1Insn(jumpType + arg);
        if (putLeft == isLeft)
            pos = pos - 1;
        arg = pos;
    }
} /* formJump */

void padToLeft()
{
    if (not putLeft)
        form1Insn(InsnTemp[UTC]);
    prevOpcode = 0;
} /* padToLeft */

void formAndAlign(int64_t arg)
{
    form1Insn(arg);
    padToLeft();
    prevOpcode = 1;
} /* formAndAlign */

void putToSymTab(Bitset arg)
{
    symTab[symTabPos] = arg;
    if (symTabPos == 075500) {
        error(50); /* errSymbolTableOverflow */
        symTabPos = 074000;
    } else
        symTabPos = symTabPos + 1;
} /* putToSymTab */

int64_t allocExtSymbol(Bitset l3arg1z)
{
//    Word l3var1z;
    int64_t l3var2z;
    int64_t ret = symTabPos;
    if (curVal.m * halfWord) {
        for (l3var2z = 1; l3var2z <= longSymCnt; ++l3var2z)
            if (curVal.m == longSyms[l3var2z]) {
                ret = longSymTabBase[l3var2z];
                return ret;
            };
        longSymCnt = longSymCnt + 1;
        if (longSymCnt >= 90) {
            error(51); /* errLongSymbolOverflow */
            longSymCnt = 1;
        };
        longSymTabBase[longSymCnt] = symTabPos;
        longSyms[longSymCnt] = curVal.m;
        l3arg1z = l3arg1z + mkbs(25);
    } else
        l3arg1z = l3arg1z + curVal.m;
    putToSymTab(l3arg1z);
    return ret;
} /* allocExtSymbol */

int64_t getHelperProc(int64_t l3arg1z)
{
    if (helperMap[l3arg1z] == 0)  {
        curVal.m = helperNames[l3arg1z];
        helperMap[l3arg1z] = allocExtSymbol(extSymMask);
    };
    return helperMap[l3arg1z] + (KVJM+I13);
} /*getHelperProc */

void toFCST()
{
    FCST.push_back(curVal.m);
    FcstCnt = FcstCnt + 1;
} /* toFCST */

int64_t addCurValToFCST()
{
    int64_t ret;
    int64_t low, high, mid;
    low = 1;
    if (FcstCountTo500 == 0) {
        ret = FcstCnt;
        FcstCountTo500 = 1;
        constVals[1] = curVal;
        constNums[1] = FcstCnt;
        toFCST();
    } else {
        high = FcstCountTo500;
        do {
            mid = (low + high) / 2;
            if (curVal.m == constVals[mid].m) {
              return constNums[mid];
            }
            if (curVal.a < constVals[mid].a)
                high = mid - 1;
            else
                low = mid + 1;
        } while (low <= high);
        ret = FcstCnt;
        if (FcstCountTo500 != 500) {
            if (curVal.a < constVals[mid].a)
                high = mid;
            else
                high = mid + 1;
            for (mid = FcstCountTo500; mid >= high; --mid) {
                low = mid + 1;
                constVals[low] = constVals[mid];
                constNums[low] = constNums[mid];
            }
            FcstCountTo500 = FcstCountTo500 + 1;
            constVals[high] = curVal;
            constNums[high] = FcstCnt;
        };
        toFCST();
    }
    return ret;
} /* addCurValToFCST */

int64_t allocSymtab(Bitset l3arg1z)
{
    int64_t ret;
    int64_t low, high, mid;
    Word value;
    low = 1;
    value.m = l3arg1z;
    if (symTabCnt == 0) {
        ret = symTabPos;
        symTabCnt = 1;
        symTabArray[1].m = l3arg1z;
        symTabIndex[1] = symTabPos;
    } else {
        high = symTabCnt;
        do {
            mid = (low + high) / 2;
            if (value.m == symTabArray[mid].m) {
                return symTabIndex[mid];
            }
            if (value.m.val < symTabArray[mid].m.val)
                high = mid - 1;
            else
                low = mid + 1;
        } while (high >= low);
        ret = symTabPos;
        if (symTabCnt != 80) {
            if (value.m.val < symTabArray[mid].m.val)
                high = mid;
            else
                high = mid + 1;
            for (mid = symTabCnt; mid >= high; --mid) {
                low = mid + 1;
                symTabArray[low] = symTabArray[mid];
                symTabIndex[low] = symTabIndex[mid];
            }
            symTabCnt = symTabCnt + 1;
            symTabArray[high] = value;
            symTabIndex[high] = symTabPos;
        }
    }
    putToSymTab(value.m);
    return ret;
} /* allocSymtab */

int64_t getFCSToffset()
{
    int64_t ret;
    Word offset;
    ret = addCurValToFCST();
    offset.i = ret;
    if (offset.i < 2048) {
        /* empty */
    } else if (offset.i >= 4096)
        error(204);
    else {
        ret = allocSymtab(offset.m + mkbs(24)) - 070000;
    }
    return ret;
} /* getFCSToffset */

int64_t minel(Bitset b) {
    if (!b.val) return -1;
    int64_t ret = 0;
    uint64_t t = b.val;
    while (((t >> 47) & 1) == 0) {
        ret++;
        t <<= 1;
    }
    return ret;
}

int64_t card(Bitset b) {
    int64_t val = b.val, ret = 0;
    while (val) {
        ++ret;
        val &= val-1;
    }
    return ret;
}

int64_t nrOfBits(Integer value)
{
    curVal.ii = value;
    curVal.m = curVal.m * mkbsr(7, 47);
    return 48-minel(curVal.m);
} /* nrOfBits */

int64_t nrOfBits(int64_t value)
{
    Bitset b;
    b.val = value & ((1L<<48)-1);
    return 48-minel(b);
} /* nrOfBits */

void defineRange(TypesPtr & res, int64_t l, int64_t r)
{
    TypesPtr temp;
    temp = new Types;
    /* with temp@ do */ {
        temp->size = 1;
        temp->bits = 48;
        temp->base = res;
        temp->checker = 0;
        temp->k = kindRange;
        curVal.i = l;
        // curVal.m = curVal.m + intZero;
        temp->left = curVal.i;
        curVal.i = r;
        // curVal.m = curVal.m + intZero;
        temp->right = curVal.i;
        if (temp->left >= 0)
            temp->bits = nrOfBits(curVal.i);
      res = temp;
    }
} /* defineRange */

int64_t getValueOrAllocSymtab(int64_t value)
{
    curVal.i = value;
    curVal.i = curVal.i & 077777;
    if (040000 >= curVal.i)
        return curVal.i;
    else
        return
            allocSymtab((curVal.m + mkbs(24)) * halfWord);
} /* getValueOrAllocSymtab */

void P0715(int64_t mode, int64_t arg)
{
    Bitset addr, insn, leftHalf;
    bool isLarge;
    int64_t work, offset;
    if (mode == 0) {
        padToLeft();
        curVal.i = moduleOffset;
      L1:     addr = curVal.m * mkbsr(33, 47);
        leftHalf = curVal.m << 24;
        while (arg != 0) {
            if (4096 < arg)  {
                isLarge = true;
                arg = arg - 4096;
            } else isLarge = false;
            insn = objBuffer[arg];
            if (isLarge) {
                curVal.m = insn * mkbsr(9, 23);
                curVal.m = curVal.m >> 24;
                // curVal.m = curVal.m + intZero;
                insn = insn * (mkbsr(0,8)+mkbsr(24,47)) + leftHalf;
            } else {
                curVal.m = /* intZero + */ insn * mkbsr(33, 47);
                insn = insn * mkbsr(0, 32) + addr;
            };
            objBuffer[arg] = insn;
            arg = curVal.i;
        };
        return;
    } else if (mode == 2) {
        form1Insn(KVTM+I14 + curVal.i);
        if (curVal.i == 074001)
            form1Insn(KUTM+I14 + FcstCnt);
        form3Insn(KITA+14, InsnTemp[ASN] + arg, KAOX+I7+1);
        form1Insn(KATX+I7+1);
        return;
    } else if ((mode == 1) or (mode < -2)) {
        arg = arg - curVal.i;
        offset = getFCSToffset();
        if (mode == 1)
            work = getHelperProc(68) + (-064200000); /* P/DA */
        else
            work = -mode;
        // A trick here: if curVal had no integer exponent, it should not have it after the assignment.
        if (curVal.i.exp == 104)
            curVal.i = arg;
        else
            curVal.ii = arg;
        arg = getFCSToffset();
        form3Insn(KATX+SP+1, KSUB+I8 + offset, work);
        form3Insn(KRSUB+I8 + arg, work, KXTA+SP+1);
        return;
    } else if (mode == -1) {
        form1Insn(KVTM+I14 + lineCnt);
        formAndAlign(getHelperProc(arg));
        return;
    };
    curVal.i = mode;
    goto L1;
} /* P0715 */

void prInsn(int insn) {
    if ((insn >> 19) & 1)
        printf("%02o %02o %05o", insn >> 20, (insn >> 15) & 037, insn & 077777);
    else
        printf("%02o %03o %04o", insn >> 20, (insn >> 12) & 0177, insn & 07777);
}

void OBPROG(Bitset & start, Bitset & fin) {
    for (Bitset * p = &start; p <= &fin; ++p) {
        if (p != &start && (p - &start) % 4 == 0) putchar('\n');
        prInsn(int(p->val >> 24)); putchar(' '); prInsn(p->val & 0xFFFFFF); printf("     ");
    }
    putchar('\n');
}

//
// Encode the symbol from KOI-8 to UTF-8, and output to stdout.
//
static void kputc(uint8_t c)
{
    if (c >= 0300) {
        static const char *koi2utf[64] = {
            "ю","а","б","ц","д","е","ф","г","х","и","й","к","л","м","н","о",
            "п","я","р","с","т","у","ж","в","ь","ы","з","ш","э","щ","ч","ъ",
            "Ю","А","Б","Ц","Д","Е","Ф","Г","Х","И","Й","К","Л","М","Н","О",
            "П","Я","Р","С","Т","У","Ж","В","Ь","Ы","З","Ш","Э","Щ","Ч","Ъ",
        };
        fputs(koi2utf[c - 0300], stdout);
        return;
    }
    if (c < 040) {
        static const char *extra2utf[32] = {
            0,  0,  0,  0,  0,  0,  "×",0,  0,  0,  0,  0,  0,  0,  "≤","≥",
            0,  0,  0,  0,  0,  0,  0,  "≡","#",0,  "÷",0,  0,  0,  "∨","~",
        };
        const char *u = extra2utf[c];
        if (u) {
            fputs(u, stdout);
            return;
        }
    }
    putchar(c);
}

void endOfLine()
{
    int64_t err, errPos, prevPos, listMode,
    startPos, lastErr;

    listMode = PASINFOR.listMode;
    if ((listMode != 0) or (errsInLine != 0)) {
        printf(" %05lo%5ld%3ld%c", (lineStartOffset + PASINFOR.startOffset),
               lineCnt, lineNesting, commentModeCH);
        startPos = 12;
        if (optSflags.m.has(S4)
            and (maxLineLen == 72)
            and (linePos >= 80)) {
            for (err = 73; err <= 80; ++err)
                putchar(lineBufBase[err]);
            putchar(' ');
            linePos = 73;
            startPos += 9;
        }; /* 1106 */
        do
            linePos = linePos-1;
        while ((lineBufBase[linePos]  == ' ') and (linePos != 0));
        for (err = 1; err <= linePos; ++err) {
            kputc(lineBufBase[err]);
        };
        putchar('\n');
        if (errsInLine != 0)  {
            printf("%*s %*c0", int(startPos), "^^^^^", int(errMapBase[0]), ' ');
            lastErr = errsInLine - 1;
            for (err = 1; err <= lastErr; ++err) {
                errPos = errMapBase[err];
                prevPos = errMapBase[err-1];
                if (errPos != prevPos) {
                    if (prevPos + 1 != errPos)
                        printf("%*c", int(errPos-prevPos-1), ' ');
                    putchar(char(err + 48));
                }
            }
            putchar('\n');
            errsInLine = 0;
            prevErrPos = 0;
        }
    } /* 1160 */
    if ((listMode == 2) and (moduleOffset != lineStartOffset)) {
        OBPROG(objBuffer[objBufIdx - moduleOffset + lineStartOffset],
               objBuffer[objBufIdx-1]);
    } /* 1174 */
    lineStartOffset = moduleOffset;
    linePos = 0;
    lineCnt = lineCnt + 1;
    // One EOF is OK when the file doesn't have any extra characters after "END."
    static int eofs;
    if (feof(pasinput) && eofs++) {
        error(errEOFEncountered);
        throw 9999;
    }
} /* endOfLine */

void requiredSymErr(Symbol sym)
{
    if (linePos != prevErrPos)
        error(sym + 88);
} /* requiredSymErr */

static unsigned char
unicode_to_koi8(int val)
{
    static std::map<int, unsigned char> uni2koi8;
    if (uni2koi8.empty()) {
        static wchar_t cyr[] = L"юабцдефгхийклмнопярстужвьызшэщчъ"
                               L"ЮАБЦДЕФГХИЙКЛМНОПЯРСТУЖВЬЫЗШЭЩЧЪ";
        for (int i = 0; cyr[i]; ++i)
            uni2koi8[cyr[i]] = (unsigned char)(i + 0300);
        uni2koi8[L'×'] = 6;
        uni2koi8[L'#'] = uni2koi8[L'≠'] = 030;
        uni2koi8[L'≤'] = 016;
        uni2koi8[L'≥'] = 017;
        uni2koi8[L'≡'] = 027;
        uni2koi8[L'÷'] = 032;
        uni2koi8[L'∨'] = 036;
        uni2koi8[L'~'] = 037;
    }
    if (uni2koi8.count(val))
        return uni2koi8[val];
    else if (val < 0177)
        return (unsigned char)val;
    else return ' ';
}

static int utf8_getc(FILE *fin)
{
    int c1, c2, c3;
    c1 = getc (fin);
    if (c1 < 0 || ! (c1 & 0x80))
        return c1;
    c2 = getc (fin);
    if (! (c1 & 0x20))
        return (c1 & 0x1f) << 6 | (c2 & 0x3f);
    c3 = getc (fin);
    return (c1 & 0x0f) << 12 | (c2 & 0x3f) << 6 | (c3 & 0x3f);

}

static unsigned char ugetc(FILE * fin) {
    return unicode_to_koi8(utf8_getc(fin));
}

void readToPos80()
{
    while (!feof(pasinput) && linePos < 81 && PASINPUT != '\n') {
        linePos = linePos + 1;
        lineBufBase[linePos] = PASINPUT;
        if (linePos != 81) PASINPUT = ugetc(pasinput);
    }
    endOfLine();
} /* readToPos80 */

struct inSymbol {
    unsigned char localBuf[131];
    int64_t tokenLen, tokenIdx;
    bool expSign;
    IdentRecPtr l3var135z;
    Real expMultiple, expValue;
    char curChar;
    int64_t numstr[17];
    int64_t expLiteral;
    int64_t expMagnitude;
    int64_t l3int162z;
    int64_t chord;
    int64_t l3var164z;
    inSymbol();
};

void nextCH()
{
    do {
        atEOL = PASINPUT == '\n' || feof(pasinput);
        CH = PASINPUT;
        PASINPUT = ugetc(pasinput);
        linePos = linePos + 1;
        lineBufBase[linePos] = CH;
    } while (not ((maxLineLen >= linePos) or atEOL));
} /* nextCH */

struct parseComment {
    // non-recursive, no need for a super stack
    static parseComment * super;
    bool badOpt, flag;
    char c;
    parseComment();
};
parseComment * parseComment::super;

int64_t readOptVal(int64_t limit)
{
    nextCH();
    int64_t res = 0;
    while (('9' >= CH) and (CH >= '0')) {
        res = 10 * res + CH - '0';
        nextCH();
        parseComment::super->badOpt = false;
    }
    if (limit < res) parseComment::super->badOpt = true;
    return res;
} /* readOptVal */

void readOptFlag(bool & res)
{
    nextCH();
    if ((CH == '-') or (CH == '+')) {
        res = CH == '+';
        parseComment::super->badOpt = false;
    }
    nextCH();
} /* readOptFlag */

parseComment::parseComment()
{ /* parseComment */
    super = this;
    nextCH();
    if (CH == '=') {
        do {
            nextCH();
            badOpt = true;
            switch (CH) {
            case 'D': case 'd': {
                curVal.i = readOptVal(15);
                optSflags.m = optSflags.m * mkbsr(0, 40) + curVal.m * mkbsr(41, 47);
            } break;
            case 'Y': case 'y':
                readOptFlag(allowCompat);
                break;
            case 'E': case 'e':
                readOptFlag(declExternal);
                break;
            case 'U': case 'u': {
                readOptFlag(flag);
                if (flag) maxLineLen = 72; else maxLineLen = 120;
            }; break;
            case 'S': case 's': {
                curVal.i = readOptVal(9);
                if (curVal.i == 3)
                    lineCnt = 1;
                else if (4 <= curVal.i && curVal.i <= 9)
                  optSflags.m = optSflags.m + mkbs(curVal.i - 3);
                else {
                    extSymAdornment = curVal.i;
                }
            } break;
            case 'F': case 'f':
                readOptFlag(checkFortran);
                break;
            case 'L': case 'l':
                PASINFOR.listMode = readOptVal(3);
                break;
            case 'P': case 'p':
                readOptFlag(doPMD);
                break;
            case 'T': case 't':
                readOptFlag(checkBounds);
                break;
            case 'A': case 'a':
                charEncoding = readOptVal(3);
                break;
            case 'C': case 'c':
                readOptFlag(checkTypes);
                break;
            case 'R': case 'r':
                readOptFlag(fuzzReals);
                break;
            case 'M': case 'm':
                readOptFlag(fixMult);
                break;
            case 'B': case 'b':
                fileBufSize = readOptVal(4);
                break;
            case 'K': case 'k':
                heapSize = readOptVal(23);
                break;
            case 'Z': case 'z':
                readOptFlag(pseudoZ);
                break;
            }
            if (badOpt)
                error(54); /* errErrorInPseudoComment */
        } while (CH == ',');
    }; /* 1446 */
    bool brace;
    do {
        while (CH != '*' && CH != '}') {
            c = commentModeCH;
            commentModeCH = '*';
            if (atEOL)
                endOfLine();
            nextCH();
            commentModeCH = c;
        };
        brace = (CH == '}');
        nextCH();
    } while (!brace && CH != ')');
    if (!brace)
        nextCH();
} /* parseComment */

inSymbol::inSymbol() {
again: {
        if (dataCheck) {
            error(errEOFEncountered);
            readToPos80();
            throw 9999;
        }
L1473:
        while ((CH == ' ') and not atEOL)
            nextCH();
        /*
        if ('\200' < CH) {
            lineBufBase[linePos] = ' ';
            chord = CH;
            for (int64_t jj = 130; jj <= chord; ++jj) {
                linePos = linePos + 1;
                lineBufBase[linePos] = ' ';
            }
            nextCH();
            goto L1473;
        }
        */
        if (atEOL) {
            endOfLine();
            nextCH();
            if (CH == '%') while (not atEOL) nextCH();
            goto L1473;
        }
        hashTravPtr = NULL;
        SY = charSymTabBase[CH];
        charClass = chrClassTabBase[CH];
//      lexer:
        if (SY != NOSY) {
            switch (SY) {
            case IDENT: {
L1:             curToken.m.val = 0;
                tokenLen = 1;
                do {
                    curVal.c = koi2text[CH];
                    nextCH();
                    if (8 >= tokenLen) {
                        tokenLen = tokenLen + 1;
                        curToken.m = curToken.m << 6;
                        curToken.m = curToken.m + curVal.m;
                    }
                } while (chrClassTabBase[CH] == ALNUM);
                // curVal.m = curToken.m * hashMask.m;
                // mapAI(curVal.a, bucket);
                bucket = curToken.m.val % 65535 % 128;
                curIdent = curToken;
                keyWordHashPtr = KeyWordHashTabBase[bucket];
                while (keyWordHashPtr != NULL) {
                    if (keyWordHashPtr->w.m == curToken.m) {
                        SY = keyWordHashPtr->sym;
                        charClass = keyWordHashPtr->op;
                        goto exitLexer;
                    }
                    keyWordHashPtr = keyWordHashPtr->next;
                }
                isDefined = false;
                SY = IDENT;
                switch (int93z) {
                case 0: {
                    hashTravPtr = symHashTabBase[bucket];
                    while (hashTravPtr != NULL) {
                        if (hashTravPtr->offset == curFrameRegTemplate)
                        {
                            if (hashTravPtr->id.m != curIdent.m)
                                hashTravPtr = hashTravPtr->next;
                            else {
                                isDefined = true;
                                goto exitLexer;
                            }
                        } else
                            goto exitLexer;
                    }
                } break;
                case 1: {
L2:                 hashTravPtr = symHashTabBase[bucket];
                    while (hashTravPtr != NULL) {
                        if (hashTravPtr->id.m != curIdent.m)
                            hashTravPtr = hashTravPtr->next;
                        else
                            goto exitLexer;
                    }
                } break;
                case 2: {
                    if (expr63z == NULL)
                        goto L2;
                    expr62z = expr63z;
                    l3var135z = typeHashTabBase[bucket];
                    if (l3var135z != NULL) {
                        while (expr62z != NULL) {
                            l3int162z = expr62z->typ2->size;
                            hashTravPtr = l3var135z;
                            while (hashTravPtr != NULL) {
                                if ((hashTravPtr->id == curIdent)
                                    and (hashTravPtr->value == l3int162z))
                                    goto exitLexer;
                                hashTravPtr = hashTravPtr->next;
                            }
                            expr62z = expr62z->expr1;
                        }
                    }
                    goto L2;
                } break;
                case 3: {
                    hashTravPtr = typeHashTabBase[bucket];
                    while (hashTravPtr != NULL) {
                        /* with hashTravPtr@ do */ {
                            if ((hashTravPtr->id == curIdent) and
                                (typ121z == hashTravPtr->uptype))
                                goto exitLexer;
                            hashTravPtr = hashTravPtr->next;
                        }
                    }
                    }
                }
            } break; /* IDENT */
            case REALCONST: {
                nextCH();
                if (charSymTabBase[CH] == IDENT)
                    goto L1;
                if (CH == '(')
                    SY = BEGINSY;
                else if (CH == ')')
                    SY = ENDSY;
                else {
                    SY = NOSY;
                    return;
                }
                nextCH();
            } break; /* REALCONST */
            case INTCONST: { /*=m-*/
                SY = INTCONST;
                tokenLen = 0;
                do {
                    tokenLen = tokenLen + 1;
                    if (16 >= tokenLen)
                        numstr[tokenLen] = CH - '0';
                    else {
                        error(55); /* errMoreThan16DigitsInNumber */
                        tokenLen = 1;
                    }
                    nextCH();
                } while (charSymTabBase[CH] == INTCONST);
                {
                    if (CH == 'B')
                        suffix = suffixB;
                    else if (CH == 'C')
                        suffix = suffixC;
                    else if (CH == 'T')
                        suffix = suffixT;
                    else {
                        suffix = noSuffix;
                        goto exitOctdec;
                    }
                    nextCH();
                    curToken.c = '\0';
                    for (tokenIdx = 1; tokenIdx <= tokenLen; ++tokenIdx) {
                        if (7 < numstr[tokenIdx])
                            error(20); /* errDigitGreaterThan7 */
                        curToken.m = curToken.m << 3;
                        curToken.m.val = numstr[tokenIdx] | curToken.m.val;
                    }
                    if (suffix == suffixB) {
                        if (curToken.m * mkbsr(0, 6) != mkbs()) {
                            error(errNumberTooLarge);
                            curToken.i = 1;
                        } else
                            curToken.m = curToken.m + intZero;
                    } else if (suffix == suffixT) {
                        l3var164z = 16 - tokenLen;
                        for (expMagnitude = 1; expMagnitude <= l3var164z; ++expMagnitude) {
                          curToken.m = curToken.m << 3;
                        }
                    }
                    goto exitLexer;
                } exitOctdec:
                curToken.i = 0;
                for (tokenIdx = 1; tokenIdx <= tokenLen; ++tokenIdx) {
                    if (109951162777L >= curToken.i)
                        curToken.i = 10 * curToken.i +
                            numstr[tokenIdx];
                    else {
                        error(errNumberTooLarge);
                        curToken.i = 1;
                    }
                }
                expMagnitude = 0;
                if (CH == '.') {
                    nextCH();
                    if (CH == '.') {
                        CH = ':';
                        goto exitLexer;
                    } else if (CH == ')') {
                        CH = ']';
                        goto exitLexer;
                    }
                    curToken.r = curToken.i;
                    SY = REALCONST;
                    if (charSymTabBase[CH] != INTCONST)
                        error(56); /* errNeedMantissaAfterDecimal */
                    else
                        do {
                            if (expMagnitude > -18) {
                                curToken.r = 10.0*curToken.r + CH - 48;
                                expMagnitude = expMagnitude-1;
                            }
                            nextCH();
                        } while (charSymTabBase[CH] == INTCONST);
                } /*2062*/
                if (CH == 'E' || CH == 'e') {
                    if (expMagnitude == 0) {
                        curToken.r = curToken.i;
                        SY = REALCONST;
                    }
                    expSign = false;
                    nextCH();
                    if (CH == '+')
                        nextCH();
                    else if (CH == '-') {
                        expSign = true;
                        nextCH();
                    }
                    expLiteral = 0;
                    if (charSymTabBase[CH] != INTCONST)
                        error(57); /* errNeedExponentAfterE */
                    else
                        do {
                            expLiteral = 10 * expLiteral + CH - 48;
                            nextCH();
                        } while (charSymTabBase[CH] == INTCONST);
                    if (expSign)
                        expMagnitude = expMagnitude - expLiteral;
                    else
                        expMagnitude = expMagnitude + expLiteral;
                }; /* 2122 */
                if (expMagnitude != 0) {
                    expValue = 1.0;
                    expSign = expMagnitude < 0;
                    expMagnitude = std::abs(expMagnitude);
                    expMultiple = 10.0;
                    if (18 < expMagnitude) {
                        expMagnitude = 1;
                        error(58); /* errExponentGreaterThan18 */
                    }
                    do {
                        if (expMagnitude & 1)
                            expValue = expValue * expMultiple;
                        expMagnitude = expMagnitude / 2;
                        if (expMagnitude != 0)
                            expMultiple = expMultiple*expMultiple;
                    } while (expMagnitude != 0);
                    if (expSign)
                        curToken.r = curToken.r / expValue;
                    else
                        curToken.r = curToken.r * expValue;
                }
                goto exitLexer;
            } break; /* INTCONST */ /*=m+*/
            case CHARCONST: {
                {
                    for (tokenIdx = 6; tokenIdx <= 130; ++tokenIdx) {
                        nextCH();
                        if (charSymTabBase[CH] == CHARCONST) {
                            nextCH();
                            if (charSymTabBase[CH] != CHARCONST)
                                goto exitLoop;
                            else
                                goto L2233;
                        }
                        if (atEOL) {
                          L2175:
                            error(59); /* errEOLNInStringLiteral */
                            goto exitLoop;
                        } else if (((CH == '\035') or /* ≡ */
                                    (charSymTabBase[CH] == REALCONST))
                                   and (charSymTabBase[PASINPUT] == INTCONST)) {
                            expLiteral = 0;
                            for (tokenLen = 1; tokenLen <= 3; ++tokenLen) {
                                nextCH();
                                if ('7' < CH)
                                    error(
                                        errFirstDigitInCharLiteralGreaterThan3
                                        );
                                expLiteral = 8*expLiteral + CH - 48;
                            }
                            if (255 < expLiteral)
                                error(errFirstDigitInCharLiteralGreaterThan3);
                            localBuf[tokenIdx] = (unsigned char)expLiteral;
                        } else {
L2233:                      curChar = CH;
                            localBuf[tokenIdx] = curChar;
                        }
                    }
                    goto L2175;
                } exitLoop: ;
                strLen = tokenIdx - 6;
                if (strLen == 0) {
                   error(61); /* errEmptyString */
                }
                if (strLen <= 1) {
                    SY = CHARCONST;
                    tokenLen = 1;
                    curToken.c = '\0';
                    unpck(localBuf[0], curToken.a);
                    pck(localBuf[tokenLen], curToken.a);
                    goto exitLexer;
                } else {
                    curVal.m.val = 0x202020202020L;
                    SY = LTSY;
                    unpck(localBuf[tokenIdx], curVal.a);
                    pck(localBuf[6], curToken.a);
                    curVal = curToken;
                   if (6 >= strLen)
                       goto exitLexer;
                   else {
                       curToken.i = FcstCnt;
                       tokenLen = 6;
                     loop: {
                           toFCST();
                           tokenLen = tokenLen + 6;
                           if (tokenIdx < tokenLen)
                               goto exitLexer;
                           pck(localBuf[tokenLen], curVal.a);
                           goto loop;
                       }
                   }
                };
                } break; /* CHARCONST */
            case LTSY: {
                SY = RELOP;
                nextCH();
                if (CH == '>') {
                    charClass = NEOP;
                    nextCH();
                } else if (CH == '=') {
                    charClass = LEOP;
                    nextCH();
                }
            } break; /* LTOP */
            case GTSY: {
                SY = RELOP;
                nextCH();
                if (CH == '=') {
                    charClass = GEOP;
                    nextCH();
                }
            } break; /* GTOP */
            case LPAREN: {
                nextCH();
                if (CH == '*') {
                    parseComment();
                    goto L1473;
                } else if (CH == '.') {
                    nextCH();
                    SY = LBRACK;
                    charClass = NOOP;
                }
            } break;
            case LBRACE: {
                parseComment();
                goto L1473;
            } break;
            case COLON: {
                nextCH();
                if (CH == '=') {
                    nextCH();
                    SY = BECOMES;
                    charClass = NOOP;
                }
            } break;
            case NOTSY: case LBRACK: case MULOP: case ADDOP:
            case RELOP: case RPAREN: case RBRACK: case COMMA: case SEMICOLON: case ARROW: {
                nextCH();
            } break;
            case PERIOD: {
                nextCH();
                if (CH == '.') {
                    nextCH();
                    SY = COLON;
                    charClass = NOOP;
                } else if (CH == ')') {
                    nextCH();
                    SY = RBRACK;
                    charClass = NOOP;
                } else {
                    if (prevSY == ENDSY)
                        dataCheck = true;
                }
            } break;
            case RBRACE:
                error(errBadSymbol);
                nextCH();
                break;
            default: break;
            } /* switch */
        } else { /* 2444 */
            nextCH();
        }
      exitLexer:
        prevSY = SY;
    if (not pseudoZ and not (optSflags.m.has(DebugCode))) {
            commentModeCH = '=';
            goto again;
        };
        commentModeCH = ' ';
        int93z = int92z;
    }
} /* inSymbol */

void skipToEnd()
{
    Symbol sym;
    sym = SY;
    while ((sym != ENDSY) or (SY != PERIOD)) {
        sym = SY;
        inSymbol();
    }
    if (CH == 'D')
        while (SY != ENDSY)
            inSymbol();
    throw 9999;
}

void error(int64_t errNo)
{
    errors = true;
    bool110z = true;
    if (((linePos != prevErrPos) and (9 >= errsInLine))
        or (errNo == 52)) {
        totalErrors = totalErrors + 1;
        errMapBase[errsInLine] = linePos;
        errsInLine = errsInLine + 1;
        prevErrPos = linePos;
        printf("Error %ld:", errNo);
        printErrMsg(errNo);
        if (60 < totalErrors) {
            putchar('\n');
            endOfLine();
            printErrMsg(53);
            skipToEnd();
        }
    }
}

void skip(Bitset toset)
{
    while (not toset.has(SY))
        inSymbol();
} /* skip */

void test1(Symbol sym, Bitset toset)
{
    if (SY != sym) {
        requiredSymErr(sym);
        skip(toset);
    } else
        inSymbol();
} /* test1 */

void errAndSkip(int64_t errNo, Bitset toset)
{
    error(errNo);
    skip(toset);
} /* errAndSkip */

void parseLiteral(TypesPtr & litType, Word & litValue,
    bool allowSign)
{
    Operator l3var1z;
    litValue = curToken;
    if (GTSY < SY) {
        if (allowSign and (charClass == PLUSOP || charClass == MINUSOP))  {
            l3var1z = charClass;
            inSymbol();
            parseLiteral(litType, litValue, false);
            if (litType != IntegerType && litType != RealType) {
                error(62); /* errIntegerNeeded */
                litType = IntegerType;
                litValue.i = 1;
            } else if (l3var1z == MINUSOP) {
                if (litType == IntegerType)
                    litValue.i = -litValue.i;
                else
                    litValue.r = -litValue.r;
            }
        } else
L99:    {
            litType = NULL;
            error(errNoConstant);
        }
    } else
        switch (SY) {
        case IDENT: {
            if ((hashTravPtr == NULL) or
                (hashTravPtr->cl != ENUMID))
                goto L99;
            litType = hashTravPtr->typ;
            litValue.ii = hashTravPtr->value;
        } break;
        case INTCONST:
            litType = IntegerType;
            break;
        case REALCONST:
            litType = RealType;
            break;
        case CHARCONST:
            litType = CharType;
            break;
        case LTSY:
            makeStringType(litType);
            break;
        case GTSY: {
            litType = pointerType;
            litValue.ii = 074000;
            } break;
        default: break;
        } /* case */
} /* parseLiteral */

void P2672(IdentRecPtr & l3arg1z, IdentRecPtr l3arg2z)
{
    bool l3var1z;
    int64_t l3var2z = 0;
    IdentRecPtr l3var3z, l3var4z;
    if (l3arg1z == NULL) {
        // curVal.m = l3arg2z->id.m * hashMask.m;
        // mapAI(curVal.a, l3var2z);
        l3var2z = (l3arg2z->id.m.val % 65535) % 128;
        l3var1z = true;
        l3arg1z = symHashTabBase[l3var2z];
    } else {
        l3var1z = false;
    }
    if (l3arg1z == l3arg2z) {
        if (l3var1z) {
            symHashTabBase[l3var2z] =
                symHashTabBase[l3var2z]->next;
        } else {
            l3arg1z = l3arg2z->next;
        };
    } else {
        l3var3z = l3arg1z;
        while (l3var3z != l3arg2z) {
            l3var4z = l3var3z;
            if (l3var3z != NULL) {
                l3var3z = l3var3z->next;
            } else {
                return;
            }
        };
        l3var4z->next = l3arg2z->next;
    }
} /* P2672 */

bool isFileType(TypesPtr typtr)
{
    return (typtr->k == kindFile) or
        ((typtr->k == kindRecord) and typtr->flag);
} /* isFileType */

bool knownInType(IdentRecPtr & rec)
{
    if (programme::super.back()->typelist != NULL) {
        rec = programme::super.back()->typelist;
        while (rec != NULL) {
            if (rec->id == curIdent) {
                return true;
            }
            rec = rec->next;
        }
    }
    return false;
} /* knownInType */

void checkSymAndRead(Symbol sym)
{
    if (SY != sym)
        requiredSymErr(sym);
    else
        inSymbol();
} /* checkSymAndRead */

struct typeCheck {
    bool ret;
    typeCheck(TypesPtr type1, TypesPtr type2);
    operator bool() const { return ret; }

    bool baseMatch;
    Kind kind1, kind2;
    TypeChain * link;
    TypesPtr basetyp1, basetyp2;
    IdentRecPtr enums1, enums2;
    int64_t span1, span2;
    static std::vector<typeCheck*> super;
    ~typeCheck() { super.pop_back(); }

    void allocWithTypeCheck() {
        link = new TypeChain(chain, basetyp1, basetyp2);
        chain = link;
        ret = typeCheck(basetyp1, basetyp2);
    }
};
std::vector<typeCheck*> typeCheck::super;

bool checkRecord(TypesPtr l4arg1z, TypesPtr l4arg2z)
{
    bool l4var1z = (l4arg1z == NULL) or (l4arg2z == NULL);
    if (l4var1z) {
        return l4arg1z == l4arg2z;
    } else {
        return typeCheck(l4arg1z->base, l4arg2z->base) and
                 checkRecord(l4arg1z->next, l4arg2z->next);
    }
} /* checkRecord */

typeCheck::typeCheck(TypesPtr type1, TypesPtr type2)
{
    super.push_back(this);
    rangeMismatch = false;
    if (type1->k == kindRange) {
        typ120z = type1->base;
    } else {
        typ120z = type1;
    }
    if (not checkTypes or (type1 == type2))
      L1:      ret = true;
    else
        /* with type1@ do */ {
        kind1 = type1->k;
        kind2 = type2->k;
        if (kind1 == kind2) {
            switch (kind1) {
            case kindReal:
                /* empty */ break;
            case kindScalar: {
                /*(chain)*/
                if (type1->numen == type2->numen) {
                    enums1 = type1->enums;
                    enums2 = type2->enums;
                    while ((enums1 != NULL) and (enums2 != NULL)) {
                        if (enums1->id != enums2->id)
                            break; // exit chain;
                        enums1 = enums1->list;
                        enums2 = enums2->list;
                    };
                    if ((enums1 == NULL) and (enums2 == NULL))
                        goto L1;
                }
            } break;
            case kindRange: {
                baseMatch = (type1->base == type2->base);
                typ120z = type1->base;
                rangeMismatch = (type1->left != type2->left) or
                    (type1->right != type2->right);
                ret = baseMatch;
                return;
            } break;
            case kindPtr: {
                if ((type1 == pointerType) or (type2 == pointerType))
                    goto L1;
                basetyp1 = type1->base;
                basetyp2 = type2->base;
                if (chain != NULL) {
                    link = chain;
                    while (link != NULL) /* with link@ do */ {
                        if (((link->type1 == basetyp1) and
                             (link->type2 == basetyp2)) or
                            ((link->type2 == basetyp1) and
                             (link->type1 == basetyp2)))
                            goto L1;
                        link = link->next;
                    }
                    allocWithTypeCheck();
                } else {
                    setup(type1);
                    allocWithTypeCheck();
                    chain = NULL;
                    rollup(type1);
                    return;
                }
            }; break;
            case kindSet:
                goto L1;
            case kindArray: {
                /* with type1->range@ do */
                span1 = type1->range->right - type1->range->left;
                /* with type2->range@ do */
                span2 = type2->range->right - type2->range->left;
                if (typeCheck(type1->base, type2->base) and
                    (span1 == span2) and
                    (type1->pck == type2->pck) and
                    not rangeMismatch) {
                    if (type1->pck) {
                        if (type1->pcksize == type2->pcksize)
                            goto L1;
                    } else
                        goto L1;
                }
            } break;
            case kindFile: {
                if (typeCheck(type1->base, type2->base))
                    goto L1;
            } break;
            case kindRecord: {
                if (checkRecord(type1->first, type2->first))
                    goto L1;
            } break;
            case kindCases:
                break; // not checked
            } /* case */
        } else {
            if (kind1 == kindRange) {
                    rangeMismatch = true;
                    typ120z = type2;
                    if (type1->base == type2)
                        goto L1;
                } else if ((kind2 == kindRange) and
                           (type1 == type2->base))
                goto L1;
        }
        ret = false;
    }
} /* typeCheck */

int64_t F3307(IdentRecPtr l3arg1z)
{
    int64_t l3var1z;
    IdentRecPtr l3var2z;
    l3var2z = l3arg1z->argList;
    l3var1z = 0;
    if (l3var2z != NULL)
        while (l3var2z != l3arg1z) {
            l3var1z = l3var1z + 1;
            l3var2z = l3var2z->list;
        }
    return l3var1z;
} /* F3307 */

Bitset makeNameWithStars(bool isProc)
{
    bool wantBoth = not isProc and (extSymAdornment == 0);
    if (curVal.m * mkbsr(0, 5) == mkbs()) {
        curVal.m = curVal.m << 6;
        if (wantBoth or (extSymAdornment == 1))
            curVal.m = curVal.m + mkbs(44, 46);
        while (curVal.m * mkbsr(0, 11) == mkbs()) {
            curVal.m = curVal.m << 6;
        }
        if (curVal.m * mkbsr(0, 5) == mkbs()) {
            if (wantBoth)
                curVal.m = mkbs(2, 4) + curVal.m;
            else {
                curVal.m = curVal.m << 6;
            }
        }
    }
    return curVal.m;
} /* makeNameWithStars */

struct formOperator {
    static std::vector<formOperator*> super;
    formOperator(OpGen l3arg1z);
    ~formOperator() { super.pop_back(); }

    int64_t l3int1z, l3int2z, l3int3z;
    int64_t nextInsn;
    ExprPtr l3var5z;
    OpFlg flags;
    Word l3var7z, l3var8z;
    bool l3bool9z;
    Word l3var10z, l3var11z;
    InsnList * saved;
    bool l3bool13z;
};
std::vector<formOperator*> formOperator::super;

struct genOneOp {
    int64_t insnBufIdx;
    int64_t l4var2z, l4var3z, l4var4z;
    Word l4var5z;
    OneInsnPtr l4inl6z, l4inl7z, l4inl8z;
    int64_t l4var9z;
    Word insnBuf[201]; // array [1..200] of Word;
    Word curInsn;
    Word tempInsn;
    OneInsnPtr l4oi212z;
    bool l4var213z;

    void P3363() {
        if (l4var213z)
            form1Insn(InsnTemp[XTA]);
        else
            form1Insn(KXTA+E1);
    }; /* P3363 */

    void addInsnToBuf(int64_t insn) {
        insnBuf[insnBufIdx].i = insn;
        insnBufIdx = insnBufIdx + 1;
    }; /* addInsnToBuf */

    void add2InsnsToBuf(int64_t insn1, int64_t insn2) {
        insnBuf[insnBufIdx].i = insn1;
        insnBuf[insnBufIdx+1].i = insn2;
        insnBufIdx = insnBufIdx + 2;
    }; /* add2InsnsToBuf */

    bool F3413() {
        bool ret;
        l4inl7z = l4inl6z;
        while (l4inl7z != NULL) {
            if (l4inl7z->mode == curInsn.i) {
                ret = true;
                while (l4inl7z->code == macro) {
                    l4inl7z = reinterpret_cast<OneInsn*>(ptr(l4inl7z->offset));
                }
                return ret;
            } else {
                l4inl7z = l4inl7z->next;
            }
        }
        return false;
    }; /* F3413 */

    void addJumpInsn(int64_t opcode) {
        if (not F3413()) {
            l4inl7z = new OneInsn;
            l4inl7z->next = l4inl6z;
            l4inl7z->mode = curInsn.i;
            l4inl7z->code = 0;
            l4inl7z->offset = 0;
            l4inl6z = l4inl7z;
        };
        addInsnToBuf(macro + opcode + ord(l4inl7z));
    }; /* addJumpInsn */

    genOneOp() {
        if (insnList == NULL)
            return;
        set145z = set145z + insnList->regsused;
        l4oi212z = insnList->next2;
        l4var9z = KNTR+7;
        insnBufIdx = 1;
        if (l4oi212z == NULL)
            return;
        l4inl6z = NULL;

        while (l4oi212z != NULL) {
            tempInsn.i = l4oi212z->code;
            l4var4z = tempInsn.i -  macro;
            curInsn.i = l4oi212z->offset;
            switch (l4oi212z->mode) {
            case 0: break;
            case 1: if (arithMode != 1) {
                    addInsnToBuf(KNTR+7);
                    arithMode = 1;
                } break;
            case 2:
                arithMode = 1;
                break;
            case 3: if (arithMode != 2) {
                    addInsnToBuf(InsnTemp[NTR]);
                    arithMode = 2;
                } break;
            case 4:
                arithMode = 2;
                break;
            }; /* case */
            l4oi212z = l4oi212z->next;
            if (l4var4z >= 0) {
                switch (l4var4z) {
                case mcCARD: {
                    add2InsnsToBuf(KACX, KAEX+ZERO);
                } break;
                case 21:
                    goto L3556;
                case 0:
                    addJumpInsn(InsnTemp[UZA]);
                    break;
                case 1:
                    addJumpInsn(InsnTemp[U1A]);
                    break;
                case 2: {
                      tempInsn.i = curInsn.i % 4096;
                      curInsn.i = curInsn.i / 4096;
                      addJumpInsn(InsnTemp[UJ]);
                      curInsn.i = tempInsn.i;
                      L3556:
                      if (F3413())
                          addInsnToBuf(2*macro+ord(l4inl7z));
                      else
                          error(206);
                } break;
                case 3: {
                      tempInsn.i = curInsn.i % 4096;
                      curInsn.i = curInsn.i / 4096;
                      l4var213z =  F3413();
                      l4inl8z = l4inl7z;
                      curInsn.i = tempInsn.i;
                      l4var213z = l4var213z && F3413();
                      if (l4var213z)
                          /* with l4inl7z@ do */ {
                          l4inl7z->code = macro;
                          l4inl7z->offset = ord(l4inl8z);
                      }
                      else
                          error(207);
                } break;
                case 20:
                    addInsnToBuf(3*macro + curInsn.i);
                    break;
                case 4: {
                    if (insnBuf[insnBufIdx-1].m * (mkbsr(21,23)+mkbsr(28,35)) == mkbs())
                        insnBuf[insnBufIdx-1].m = insnBuf[insnBufIdx-1].m + mkbs(35);
                    else
                        addInsnToBuf(KXTA+SP);
                } break;
                case 5:
                    /*blk*/ {
                    if (l4oi212z != NULL) {
                        tempInsn.i = l4oi212z->code;
                        if (tempInsn.m * (mkbsr(21,23)+mkbsr(28,35)) == mkbs(32)) {
                            l4oi212z->code =
                                tempInsn.i - InsnTemp[XTA] + InsnTemp[XTS];
                            break; // exit blk
                        }
                    };
                    addInsnToBuf(KATX+SP);
                } break;
                case mcACC2ADDR:
                    add2InsnsToBuf(KATI+14, KUTC+I14);
                    break;
                case mcMULTI: {
                    addInsnToBuf(getHelperProc(12));        /* P/MI */
                } break;
                case mcADDSTK2REG:
                    add2InsnsToBuf(KWTC+SP, KUTM+indexreg[curInsn.i]);
                    break;
                case mcADDACC2REG:
                    add2InsnsToBuf(KATI+14, KJADDM+I14 + curInsn.i);
                    break;
                case mcODD: {
                    add2InsnsToBuf(KAAX+E1, KAEX+ZERO);
                } break;
                case mcROUND: {
                    addInsnToBuf(KADD+REAL05);                /* round */
                    add2InsnsToBuf(KNTR+7, KADD+ZERO);
                } break;
                case mcSQRR: {
                    add2InsnsToBuf(KATX+SP, KMUL+SP);   /* sqr */
                } break;
                case mcSQRI: {
                    add2InsnsToBuf(KATX+SP, KAEX+MULTMASK);   /* sqrint */
                    add2InsnsToBuf(KMUL+SP, KYTA+64);
                } break;
                case 14:
                    add2InsnsToBuf(indexreg[curInsn.i] + KVTM, KITA + curInsn.i);
                    break;
                case mcMINEL: {
                    add2InsnsToBuf(KANX+ZERO, KSUB+PLUS1);   /* minel */
                } break;
                case 16:
                    add2InsnsToBuf(InsnTemp[XTA], KATX+SP + curInsn.i);
                    break;
                case 17: {
                    addInsnToBuf(KXTS);
                    add2InsnsToBuf(KATX+SP+1, KUTM+SP + curInsn.i);
                } break;
                case 18:
                    add2InsnsToBuf(KVTM+I10, getHelperProc(65)); /* P/B7 */
                    break;
                case mcPOP2ADDR: {
                    addInsnToBuf(KVTM+I14);
                    add2InsnsToBuf(KXTA+SP, KATX+I14);
                } break;
                case 22: {
                    add2InsnsToBuf(KVTM+I14, KXTA+I14);
                      curVal.ii = 040077777;
                      add2InsnsToBuf(allocSymtab(curVal.m) + (KXTS+SP),
                                     KAAX+I8 + curInsn.i);
                      add2InsnsToBuf(KAEX+SP, KATX+I14);
                } break;
                }; /* case */
            } else { /* 4003 */
                if (tempInsn.m.has(28)) {
                    addInsnToBuf(getValueOrAllocSymtab(curInsn.i)+tempInsn.i);
                } else {
                    curVal.i = curInsn.i & 077777;
                    if (curVal.i < 2048)
                        addInsnToBuf(tempInsn.i + curInsn.i);
                    else
                        if ((curVal.i >= 28672) or (curVal.i < 4096)) {
                            addInsnToBuf(
                                allocSymtab((curVal.m + mkbs(24))*halfWord)
                                + tempInsn.i - 28672);
                        } else {
                            add2InsnsToBuf(getValueOrAllocSymtab(curVal.i)
                                           + InsnTemp[UTC], tempInsn.i);
                        }
                }
            }
        }; /* 4037 */
        insnBufIdx = insnBufIdx-1;

        for (l4var4z = insnBufIdx; l4var4z >= 1; --l4var4z) {
            curInsn = insnBuf[l4var4z];
            if ((curInsn.i == InsnTemp[NTR]) or
                (curInsn.i == KNTR+7)) {
                l4var3z = l4var4z - 1;
                l4var213z = false;
                while (l4var3z >= 1) {
                    tempInsn.m = insnBuf[l4var3z].m * mkbsr(28,32);
                    if ((tempInsn.ii != KUTC) and (tempInsn.ii != KWTC))
                        break;
                    l4var3z = l4var3z-1;
                };

                l4var3z = l4var3z + 1;
                if (l4var3z != l4var4z) {
                    for (l4var2z = l4var4z-1;  l4var2z >= l4var3z; --l4var2z) {
                        insnBuf[l4var2z+1] = insnBuf[l4var2z];
                    }
                }
                insnBuf[l4var3z] = curInsn;
            } /* 4103 */
        }
        for (l4var4z = 1; l4var4z <= insnBufIdx; ++l4var4z)
            /*iter*/  {
            curInsn = insnBuf[l4var4z];
            tempInsn.m = curInsn.m * (mkbs(0, 1, 3)+mkbsr(23,32));
            if (tempInsn.i == KATX+SP) {
                l4var2z = l4var4z + 1;
                while (insnBufIdx + 1 != l4var2z) {
                    curVal.m = insnBuf[l4var2z].m * (mkbs(0, 1, 3, 23)+mkbsr(28,35));
                    tempInsn.m = curVal.m * (mkbs(0, 1, 3, 23)+mkbsr(28,32));
                    if (curVal.i == InsnTemp[XTA]) {
                        insnBuf[l4var2z].m =
                            insnBuf[l4var2z].m ^ mkbs(32, 34, 35);
                        goto exit_iter;
                    } else if (curVal.i == InsnTemp[ITA]) {
                        insnBuf[l4var2z].m = insnBuf[l4var2z].m + mkbs(35);
                        goto exit_iter;
                    } else if ((curVal.i == InsnTemp[NTR]) or
                               (tempInsn.i == InsnTemp[UTC]) or
                               (tempInsn.i == InsnTemp[WTC]) or
                               (tempInsn.i == InsnTemp[VTM]))
                        l4var2z = l4var2z + 1;
                    else
                        l4var2z = insnBufIdx + 1;
                }
            } /* 4150 */
            if (curInsn.i == InsnTemp[UTC])
                continue; // exit iter
            if (curInsn.i < macro) {
                form1Insn(curInsn.i);
                tempInsn.m = curInsn.m * mkbsr(28,32);
                if ((tempInsn.i == 03100000) or /* VJM */
                    (tempInsn.i == 00500000))    /* ELFUN */
                    {
                        padToLeft();
                        prevOpcode = 1;
                    };
                continue; // exit iter
            }
            if (curInsn.i >= 3*macro) {
                curInsn.i = curInsn.i - (3*macro);
                if (curInsn.i >= 4096) {
                    l4var213z = true;
                    curInsn.i = curInsn.i - 4096;
                } else {
                  l4var213z = false;
                }
                if (curInsn.i == 0)
                    form1Insn(InsnTemp[UZA] + moduleOffset + 2);
                P3363();
                form1Insn(InsnTemp[UJ] + 2 + moduleOffset);
                padToLeft();
                if (curInsn.i != 0) {
                    if (not F3413())
                        error(211);
                    P0715(0, l4inl7z->code);
                };
                l4var213z = not l4var213z;
                P3363();
                padToLeft();
                continue;
            }; /* 4230 */
            if (curInsn.i >= 2*macro) {
                l4inl7z = reinterpret_cast<OneInsn*>(ptr(curInsn.i - (2*macro)));
                P0715(0, l4inl7z->code);
                l4inl7z->offset = moduleOffset;
            } else {
                curInsn.i = curInsn.i - macro;
                curVal.m = curInsn.m * (mkbs(0, 1, 3)+mkbsr(28,32));
                jumpType = curVal.i;
                curVal.m = (mkbs(0, 1, 3)+mkbsr(33,47)) * curInsn.m;
                l4inl7z = reinterpret_cast<OneInsn*>(ptr(curVal.i));
                formJump(l4inl7z->code);
                jumpType = InsnTemp[UJ];
                continue;
            }
          exit_iter:;
        } /* loop */

        insnList = NULL;
        while (l4inl6z != NULL) {
            /* with l4inl6z@ do */
            if (l4inl6z->offset == 0) {
                jumpTarget = l4inl6z->code;
                return;
            } else
                l4inl6z = l4inl6z->next;
        }
        set146z = set146z - set145z;
    }
}; /* genOneOp */


void addToInsnList(int64_t insn)
{
    OneInsnPtr elt = new OneInsn;
    /* with elt@ do */ {
        elt->next = NULL;
        elt->mode = 0;
        elt->code = insn;
        elt->offset = 0;
    };
    /* with insnList@ do */ {
        if (insnList->next == NULL)
            insnList->next2 = elt;
        else
            insnList->next->next = elt;
        insnList->next = elt;
    }
} /* addToInsnList */


void addInsnAndOffset(int64_t insn, int64_t l4arg2z)
{
    addToInsnList(insn);
    insnList->next->offset = l4arg2z;
} /* addInsnAndOffset */

void addxToInsnList(int64_t insn)
{
    OneInsnPtr elt = new OneInsn;
    /* with elt@ do */ {
        elt->next = insnList->next2;
        elt->mode = 0;
        elt->code = insn;
        elt->offset = 0;
    }
    if (insnList->next2 == NULL)  {
        insnList->next = elt;
    }
    insnList->next2 = elt;
} /* addxToInsnList */

void prepLoad()
{
    int64_t helper, l4int2z, l4int3z;
    TypesPtr l4typ4z;
    Kind l4var5z;
    state l4st6z;
    bool l4bool7z, l4bool8z, l4bool9z;

    l4typ4z = insnList->typ;
    /* with insnList@ do */ {
        switch (insnList->ilm) {
        case ilCONST: {
            curVal = insnList->ilf5;
            if (l4typ4z->size == 1)
              curVal.i = getFCSToffset();
            addToInsnList(constRegTemplate + curInsnTemplate + curVal.i);
        } break;
        case il1: {
            helper = insnList->ilf7;
            l4int2z = insnList->ilf5.i;
            l4int3z = insnList->ilf6;
            if (15 < helper) {
                /* empty */
            } else {
                if (helper == 15) { /* P/CP */
                    addToInsnList(macro + mcACC2ADDR);
                } else {
                    helper = indexreg[insnList->ilf7];
                    if ((l4int2z == 0) and (insnList->st == st0)) {
                        addInsnAndOffset(helper + curInsnTemplate,
                                         l4int3z);
                        goto L4602;
                    } else {
                        addToInsnList(helper + InsnTemp[UTC]);
                    }
                }
            }
            l4st6z = insnList->st;
            if (l4st6z == st0) {
                addInsnAndOffset(l4int2z + curInsnTemplate, l4int3z);
            } else {
                l4var5z = l4typ4z->k;
                if (l4var5z < kindSet or
                    (l4var5z == kindRecord and optSflags.m.has(S6))) {
                    l4bool7z = true;
                    l4bool8z = typeCheck(l4typ4z, IntegerType);
                } else {
                    l4bool7z = false;
                    l4bool8z = false;
                }
                if (l4st6z == st1) {
                    if ((l4int3z != l4int2z) or
                        (helper != 18) or /* P/RC */
                        (l4int2z != 0))
                        addInsnAndOffset(l4int2z + InsnTemp[XTA],
                                         l4int3z);
                    l4int3z = insnList->shift;
                    l4int2z = insnList->width;
                    l4bool9z = true;
                    helper = l4int3z + l4int2z;
                    if (l4bool7z) {
                        if (30 < l4int3z) {
                            addToInsnList(ASN64-48 + l4int3z);
                            addToInsnList(InsnTemp[YTA]);
                            if (helper == 48) /* P/RDR */
                                l4bool9z = false;
                        } else {
                            if (l4int3z != 0)
                                addToInsnList(ASN64 + l4int3z);
                        }; /* 4477 */
                        if (l4bool9z) {
                            curVal.m = mkbsr(48 - l4int2z, 47);
                            addToInsnList(KAAX+I8 + getFCSToffset());
                        }
                    } else { /* 4511 */
                        if (helper != 48)
                            addToInsnList(ASN64-48 + helper);
                        curVal.m = mkbsr(0, l4int2z-1);
                        addToInsnList(KAAX+I8 + getFCSToffset());
                    }; /* 4525 */
                    if (l4bool8z)
                        addToInsnList(KAEX+ZERO);
                } else { /* 4531 */
                    if (l4bool7z)
                        helper = l4bool8z+74; /* P/LDAR[IN] */
                    else
                        helper = 56; /* P/RR */
                    addToInsnList(getHelperProc(helper));
                    insnList->next->mode = 1;
                }
            }
            goto L4545;
        } break;
        case il2: {
              L4545:
            if (bool49z and (l4typ4z == BooleanType) and
                insnList->regsused.has(16))
                addToInsnList(KAEX+E1);
        } break;
        case il3: { /* 4555 */
            if (bool49z)
                addInsnAndOffset(macro+20,
                                 insnList->regsused.has(16)*010000 + insnList->ilf5.i);
        } break;
        } /* case */
      L4602:;
    } /* with */
    /* with insnList@ do */ {
        insnList->ilm = il2;
        insnList->regsused = insnList->regsused + mkbs(0L);
    }
} /* prepLoad */

void P4606()
{
    prepLoad();
    addToInsnList(macro + mcPUSH);
} /* P4606 */

struct setAddrTo {
    Word l4var1z;
    int64_t l4int2z, opCode, l4var4z, l4var5z,
        l4var6z, regField;

    void P4613() {
        l4var1z.i = insnList->ilf6;
        l4var1z.i = l4var1z.i & 077777;
        l4var6z = l4var1z.i;
    }; /* P4613 */

    setAddrTo(int64_t reg) {
        /* with insnList@ do */ {
            l4int2z = insnList->ilf7;
            opCode = InsnTemp[VTM];
            regField = indexreg[reg];
            l4var4z = insnList->ilf5.i;
            insnList->regsused = insnList->regsused + mkbs(reg);
            if (insnList->ilm == ilCONST) {
                curVal = insnList->ilf5;
                if (insnList->typ->size == 1)
                    curVal.i = addCurValToFCST();
                l4var6z = curVal.i;
                l4var5z = 074001;
                goto L4654;
            } else if (l4int2z == 18) {
              L4650:
                P4613();
                if (l4var4z == indexreg[1]) {
                    l4var5z = 074003;
                  L4654:
                    l4var1z.i = macro * l4var5z + l4var6z;
                    l4var6z = allocSymtab(l4var1z.m * mkbsr(12,47));
                    addToInsnList(regField + opCode + l4var6z);
                } else if (l4var4z != 0) {
                    addInsnAndOffset(l4var4z + InsnTemp[UTC], l4var6z);
                    addToInsnList(regField + opCode);
                } else {
                    addInsnAndOffset(regField + opCode, l4var6z);
                }
            } else if (l4int2z == 17) {
                P4613();
                l4var4z = insnList->ilf6;
                l4var5z = insnList->next->code - InsnTemp[UTC];
                if (l4var4z != 0) {
                    l4var1z.i = macro * l4var5z + l4var4z;
                    l4var5z = allocSymtab(l4var1z.m * mkbsr(12,47));
                }
                insnList->next->code = regField + l4var5z + opCode;
            } else if (l4int2z == 16) {
                P4613();
                if (l4var4z != 0)
                    addToInsnList(l4var4z + InsnTemp[UTC]);
                addInsnAndOffset(regField + opCode, l4var6z);
            } else if (l4int2z == 15) {
                addToInsnList(InsnTemp[ATI] + reg);
                opCode = InsnTemp[UTM];
                goto L4650;
            } else {
                addToInsnList(indexreg[l4int2z] + InsnTemp[UTC]);
                goto L4650;
            }
        } /* with */
        insnList->ilm = il1;
        insnList->ilf7 = reg;
        insnList->ilf6 = 0;
        insnList->ilf5.i = 0;
    } /* setAddrTo */
};

void prepStore()
{
    int64_t l4int1z, l4int2z, l4int3z;
    bool l4bool4z, l4bool5z;
    state l4st6z;
    Kind l4var7z;

    // with insnList@ do
        l4int1z = insnList->ilf7;
    if (15 < l4int1z) {
        /* nothing? */
    } else if (l4int1z == 15)  {
        addToInsnList(macro + mcACC2ADDR);
    } else {
        addToInsnList(indexreg[l4int1z] + InsnTemp[UTC]);
    }
    l4bool4z = insnList->regsused.has(0);
    l4st6z = insnList->st;
    if ((l4st6z != st0) or l4bool4z)
        addxToInsnList(macro + mcPUSH);
    if (l4st6z == st0) {
        if (l4bool4z)  {
            addInsnAndOffset(insnList->ilf5.i + InsnTemp[UTC],
                             insnList->ilf6);
            addToInsnList(macro+mcPOP2ADDR);
        } else {
            addInsnAndOffset(insnList->ilf5.i, insnList->ilf6);
        }
    } else {
        l4var7z = insnList->typ->k;
        l4int1z = insnList->typ->bits;
        l4bool5z = (l4var7z < kindSet) or
            ((l4var7z == kindRecord) and optSflags.m.has(S6));
        if (l4st6z == st1) {
            l4int2z = insnList->shift;
            l4int3z = l4int2z + insnList->width;
            if (l4bool5z)  {
                if (l4int2z != 0)
                    addxToInsnList(ASN64 - l4int2z);
            } else {
                if (l4int3z != 48)
                    addxToInsnList(ASN64 + 48 - l4int3z);
            }
            addInsnAndOffset(InsnTemp[UTC] + insnList->ilf5.i,
                             insnList->ilf6);
            curVal.m = mkbsr(0,47) - mkbsr((48-l4int3z),(47 -l4int2z));
            addInsnAndOffset(macro+22, getFCSToffset());
        } else {
            if (not l4bool5z) {
                l4int2z = (insnList->width - l4int1z);
                if (l4int2z != 0)
                    addxToInsnList(ASN64 - l4int2z);
                addxToInsnList(InsnTemp[YTA]);
                addxToInsnList(ASN64 - l4int1z);
            }
            addToInsnList(getHelperProc(77)); /* "P/STAR" */
            insnList->next->mode = 1;
        }
    }
} /* prepStore */


void P5117(Operator op)
{
    int64_t & localSize = programme::super.back()->localSize;
    int64_t & l2int21z = programme::super.back()->l2int21z;

    addInsnAndOffset(curFrameRegTemplate, localSize);
    curExpr = new Expr;
    /* with curExpr@ do */
    curExpr->typ = insnList->typ;
    genOneOp();
    curExpr->op = op;
    curExpr->num1 = localSize;
    localSize = localSize + 1;
    if (l2int21z < localSize)
        l2int21z = localSize;
} /* P5117 */

int64_t insnCount()
{
    int64_t cnt;
    OneInsnPtr cur;
    cnt = 0;
    cur = insnList->next2;
    while (cur != NULL) {
        cur = cur->next;
        cnt = cnt + 1;
    }
    return cnt;
} /* insnCount */

struct genFullExpr {
    static std::vector<genFullExpr*> super;
    genFullExpr(ExprPtr exprToGen_);
    ~genFullExpr() { super.pop_back(); }
// label 7567, 7760, 10075, 10122;
    ExprPtr & exprToGen;
    bool arg1Const, arg2Const;
    InsnList * otherIns;
    Word arg1Val, arg2Val;
    Operator curOP;
    int64_t work;

    void P5155() {
        prepLoad();
        insnList->ilm = il1;
        insnList->st = st0;
        insnList->ilf6 = 0;
        insnList->ilf5.i = 0;
        insnList->ilf7 = 18;
    }; /* P5155 */

    void genDeref() {
        Word l5var1z, l5var2z;
        bool doPtrCheck;

        doPtrCheck = checkBounds and not (optSflags.m.has(NoPtrCheck))
            and (curOP == DEREF);
        if (not doPtrCheck and (
                (insnList->st == st0) or
                (insnList->st == st1 and
                 insnList->shift == 0))) {
            l5var1z.i = insnList->ilf7;
            l5var2z.i = insnList->ilf6;
            if (l5var1z.i == 18 or l5var1z.i == 16) {
              L5220:
                addInsnAndOffset((insnList->ilf5.i + InsnTemp[WTC]), l5var2z.i);
            } else {
                if (l5var1z.i == 17) {
                    if (l5var2z.i == 0) {
                        insnList->next->code = insnList->next->code +
                            InsnTemp[XTA];
                    } else
                        goto L5220;
                } else if (l5var1z.i == 15) {
                    addToInsnList(macro + mcACC2ADDR);
                    goto L5220;
                } else {
                    addInsnAndOffset((indexreg[l5var1z.i] + InsnTemp[WTC]),
                                     l5var2z.i);
                }
            }
        } else {
            P5155();
            if (doPtrCheck) {
                addToInsnList(KVTM+I14 + lineCnt);
                addToInsnList(getHelperProc(7)); /* "P/CA"*/
                insnList->next->mode = 1;
            }
            addToInsnList(macro + mcACC2ADDR);
        }
        insnList->ilf6 = 0;
        insnList->ilf5.i = 0;
        insnList->ilf7 = 16;
    }; /* genDeref */

    void genHelper() {
        InsnList * &saved = formOperator::super.back()->saved;
        P4606();
        saved = insnList;
        insnList = otherIns;
        prepLoad();
        addToInsnList(getHelperProc(formOperator::super.back()->nextInsn));
        insnList->regsused = insnList->regsused + saved->regsused + mkbsr(11,14);
        saved->next->next = insnList->next2;
        insnList->next2 = saved->next2;
    }; /* genHelper */

    void prepMultiWord() {
        bool l5var1z;
        InsnList * l5var2z;

        l5var1z = otherIns->regsused.has(12);
        setAddrTo(12);
        if (l5var1z) {
            addToInsnList(KITA+12);
            addToInsnList(macro + mcPUSH);
        }
        l5var2z = insnList;
        insnList = otherIns;
        setAddrTo(14);
        if (l5var1z) {
            addToInsnList(macro + mcPOP);
            addToInsnList(KATI+12);
        }
        l5var2z->regsused = insnList->regsused + l5var2z->regsused;
        l5var2z->next->next = insnList->next2;
        l5var2z->next = insnList->next;
        insnList = l5var2z;
    }; /* prepMultiWord */

    void genCheckBounds(TypesPtr l5arg1z) {
        int64_t l5var1z;
        Word /* l5var2z, l5var3z, */ l5var4z;

        l5var1z = l5arg1z->checker;
        if (l5var1z == 0) {
            curVal.i = l5arg1z->left;
            l5var4z.i = l5arg1z->right;
            if (l5arg1z->base != IntegerType) {
                curVal.m = curVal.m * mkbsr(7,47);
                l5var4z.m = l5var4z.m * mkbsr(7,47);
            }
            prevOpcode = 0;
            formAndAlign(KUJ+5 + moduleOffset);
            l5arg1z->checker = moduleOffset;
            l5var1z = moduleOffset;
            P0715(1, l5var4z.i);
            formAndAlign(KUJ+I13);
        }
        prepLoad();
        addToInsnList(KVTM+I14 + lineCnt);
        addToInsnList(KVJM+I13 + l5var1z);
        insnList->next->mode = 1;
    }; /* genCheckBounds */


    void negateCond () {
        if (insnList->ilm == ilCONST) {
            insnList->ilf5.b = not insnList->ilf5.b;
        } else {
            insnList->regsused = insnList->regsused ^ mkbs(16);
        }
    }; /* negateCond */

    void tryFlip(bool commutes) {
        int64_t l5var1z;
        InsnList * l5var2z;
        InsnList * &saved = formOperator::super.back()->saved;
        int64_t &nextInsn = formOperator::super.back()->nextInsn;

        if (not otherIns->regsused.has(0)) {
            l5var1z = 0;
        } else if (not insnList->regsused.has(0)) {
            l5var1z = commutes + 1;
        } else {
            l5var1z = 3;
        }
        switch (l5var1z) {
        case 0: L100:
        {
            prepLoad();
          saved = insnList;
          insnList = otherIns;
          curInsnTemplate = nextInsn;
          prepLoad();
          curInsnTemplate = InsnTemp[XTA];
        } break;
        case 1:
            if (nextInsn == InsnTemp[SUB]) {
                nextInsn = InsnTemp[RSUB];
                goto L22;
            } else
                goto L33;
            break;
        case 2: L22: {
            saved = insnList;
            insnList = otherIns;
            otherIns = saved;
            goto L100;
        } break;
        case 3: L33: {
            prepLoad();
            addToInsnList(indexreg[15] + nextInsn);
            l5var2z = insnList;
            insnList = otherIns;
            P4606();
              saved = insnList;
              insnList = l5var2z;
        } break;
        }; /* case */
        insnList->next->mode = 0;
        saved->next->next = insnList->next2;
        insnList->next2 = saved->next2;
        insnList->regsused = insnList->regsused + mkbs(0L);
    }; /* tryFlip */

    void genBoolAnd() {
        bool l5var1z, l5var2z;
        int64_t l5var3z, l5var4z, l5var5z, l5var6z, l5var7z;
        InsnList * l5ins8z;
        Word l5var9z;

        if (arg1Const) {
            if (arg1Val.b)
              insnList = otherIns;
        } else if (arg2Const) {
            if (not arg2Val.b)
                insnList = otherIns;
        } else {
            l5var1z = insnList->regsused.has(16);
            l5var2z = otherIns->regsused.has(16);
            l5var5z = int94z;
            int94z = int94z + 1;
            bool49z = false;
            l5var6z = l5var1z + macro;
            l5var7z = l5var2z + macro;
            if (insnList->ilm == il3) {
                l5var3z = insnList->ilf5.i;
            } else {
                l5var3z = 0;
                prepLoad();
            }
            if (otherIns->ilm == il3) {
                l5var4z = otherIns->ilf5.i;
            } else {
                l5var4z = 0;
            }
            l5var9z.m = (insnList->regsused + otherIns->regsused);
            if (l5var3z == 0) {
                if (l5var4z == 0) {
                    addInsnAndOffset(l5var6z, l5var5z);
                    l5ins8z = insnList;
                    insnList = otherIns;
                    prepLoad();
                    addInsnAndOffset(l5var7z, l5var5z);
                } else {
                    if (l5var2z) {
                        addInsnAndOffset(l5var6z, l5var5z);
                        l5ins8z = insnList;
                        insnList = otherIns;
                        addInsnAndOffset(macro + 2,
                                         010000 * l5var5z + l5var4z);
                    } else {
                        addInsnAndOffset(l5var6z, l5var4z);
                        l5var5z = l5var4z;
                        l5ins8z = insnList;
                        insnList = otherIns;
                    }
                }
            } else {
                if (l5var4z == 0) {
                    if (l5var1z) {
                        addInsnAndOffset(macro + 2,
                                         010000 * l5var5z + l5var3z);
                        l5ins8z = insnList;
                        insnList = otherIns;
                        prepLoad();
                        addInsnAndOffset(l5var7z, l5var5z);
                    } else {
                        l5ins8z = insnList;
                        insnList = otherIns;
                        prepLoad();
                        addInsnAndOffset(l5var7z, l5var3z);
                        l5var5z = l5var3z;
                    }
                } else {
                    if (l5var1z) {
                        if (l5var2z) {
                            addInsnAndOffset(macro + 2,
                                             010000 * l5var5z + l5var3z);
                            l5ins8z = insnList;
                            insnList = otherIns;
                            addInsnAndOffset(macro + 2,
                                             010000 * l5var5z + l5var4z);
                        } else {
                            addInsnAndOffset(macro + 2,
                                             010000 * l5var4z + l5var3z);
                            l5ins8z = insnList;
                            insnList = otherIns;
                            l5var5z = l5var4z;
                        }
                    } else {
                        l5ins8z = insnList;
                        insnList = otherIns;
                        l5var5z = l5var3z;
                        if (l5var2z)
                            addInsnAndOffset(macro + 2,
                                             010000 * l5var3z + l5var4z);
                        else
                            addInsnAndOffset(macro + 3,
                                             010000 * l5var3z + l5var4z);
                    }
                }
            }
            insnList->regsused = l5var9z.m - mkbs(16);
            l5ins8z->next->next = insnList->next2;
            insnList->next2 = l5ins8z->next2;
            insnList->ilm = il3;
            insnList->ilf5.i = l5var5z;
            bool49z = true;
        }
    } /* genBoolAnd */

    void genConstDiv() {
        // TODO correctly
        curVal.ii = (1L<<47)|(1L << 40)/arg2Val.i; // PASDIV(1/arg2Val.i);
        ++curVal.ii;
        addToInsnList(KMUL+I8 + getFCSToffset());
    }; /* genConstDiv */

};
std::vector<genFullExpr*> genFullExpr::super;

void genGetElt() {
    int64_t l5var1z, dimCnt, curDim, l5var4z, l5var5z, l5var6z,
        l5var7z, l5var8z;
    InsnList insnCopy;
    InsnListPtr copyPtr, l5ins21z;
    Word l5var22z, l5var23z;
    bool l5var24z, l5var25z;
    TypesPtr l5var26z, l5var27z;
    ilmode l5ilm28z;
    ExprPtr l5var29z;
    InsnListPtr getEltInsns[11]; // array [1..10] of InsnListPtr;
    ExprPtr & exprToGen = genFullExpr::super.back()->exprToGen;
    InsnList * &saved = formOperator::super.back()->saved;

    dimCnt = 0;
    l5var29z = exprToGen;
    while (l5var29z->op == GETELT) {
        genFullExpr(l5var29z->expr2);
        dimCnt = dimCnt + 1;
        getEltInsns[dimCnt] = insnList;
        l5var29z = l5var29z->expr1;
    }
    (void) genFullExpr(l5var29z);
    l5ins21z = insnList;
    insnCopy = *insnList;
    copyPtr = &insnCopy;
    l5var22z.m = set147z;
    for (curDim = 1; curDim <= dimCnt; ++curDim)
        l5var22z.m = l5var22z.m - getEltInsns[curDim]->regsused;
    for (curDim = dimCnt; curDim >= 1; curDim--) {
        l5var26z = insnCopy.typ->base;
        l5var27z = insnCopy.typ->range;
        l5var25z = insnCopy.typ->pck;
        l5var7z = l5var27z->left;
        l5var8z = l5var26z->size;
        if (not l5var25z)
            insnCopy.ilf6 = insnCopy.ilf6 - l5var8z * l5var7z;
        insnList = getEltInsns[curDim];
        l5ilm28z = insnList->ilm;
        if (l5ilm28z == ilCONST) {
            curVal = insnList->ilf5;
            // curVal.m = curVal.m +  intZero;
            if (curVal.i < l5var7z or
                l5var27z->right < curVal.i)
                error(29); /* errIndexOutOfBounds */
            if (l5var25z) {
                l5var4z = curVal.i - l5var7z;
                l5var5z = insnCopy.typ->perWord;
                insnCopy.regsused = insnCopy.regsused + mkbs(0L);
                insnCopy.ilf6 = l5var4z / l5var5z + insnCopy.ilf6;
                l5var6z = (l5var5z-1-l5var4z % l5var5z) *
                    insnCopy.typ->pcksize;
                switch (insnCopy.st) {
                case st0: insnCopy.shift = l5var6z;
                    break;
                case st1: insnCopy.shift = insnCopy.shift + l5var6z +
                        insnCopy.typ->bits - 48;
                    break;
                case st2: error(errUsingVarAfterIndexingPackedArray);
                    break;
                } /* case */
                insnCopy.width = insnCopy.typ->pcksize;
                insnCopy.st = st1;
            } /* 6116 */ else {
                insnCopy.ilf6 = curVal.i  * l5var26z->size +
                    insnCopy.ilf6;
            }
        } else { /* 6123*/
            if (checkBounds) {
                l5var24z = typeCheck(l5var27z, insnList->typ);
                if (rangeMismatch)
                    genFullExpr::super.back()->genCheckBounds(l5var27z);
            }
            if (l5var8z != 1) {
                prepLoad();
                if (l5var27z->base == IntegerType) {
                    l5var4z = KYTA+64;
                } else {
                    l5var4z = KYTA+64-40;
                }
                addToInsnList(insnCopy.typ->perWord);
                insnList->next->mode = 1;
                if (l5var7z >= 0)
                    addToInsnList(l5var4z);
                else
                    addToInsnList(macro + mcMULTI);
           }
            if (l5ilm28z == il3 or
                (l5ilm28z == il1 and
                 insnList->st != st0))
                prepLoad();
           l5var23z.m = insnCopy.regsused + insnList->regsused;
           if (not l5var25z) {
               if (insnCopy.ilf7 == 18) {
                    if (insnList->ilm == il2) {
                        insnCopy.ilf7 = 15;
                    } else { /* 6200 */
                        insnCopy.ilf7 = 16;
                        curInsnTemplate = InsnTemp[WTC];
                        prepLoad();
                        curInsnTemplate = InsnTemp[XTA];
                    }; /* 6205 */
                    insnCopy.next = insnList->next;
                    insnCopy.next2 = insnList->next2;
                } else { /* 6211 */
                    if (insnCopy.ilf7 >= 15) {
                        l5var1z = minel(l5var22z.m);
                        if (0 >= l5var1z) {
                            l5var1z = minel(set147z - insnCopy.regsused);
                            if (0 >= l5var1z)
                                l5var1z = 9;
                        }
                        saved = insnList;
                        insnList = copyPtr;
                        l5var23z.m = l5var23z.m + mkbs(l5var1z);
                        if (insnCopy.ilf7 == 15) {
                            addToInsnList(InsnTemp[ATI] + l5var1z);
                        } else {
                            addToInsnList(indexreg[l5var1z] + InsnTemp[VTM]);
                        }
                        insnCopy.ilf7 = l5var1z;
                        insnCopy.regsused = insnCopy.regsused + mkbs(l5var1z);
                        insnList = saved;
                    } else {
                            l5var1z = insnCopy.ilf7;
                    } /* 6251 */
                    if (insnList->regsused.has(l5var1z)) {
                        P4606();
                        insnList->next->next = insnCopy.next2;
                        insnCopy.next2 = insnList->next2;
                        insnList = copyPtr;
                        addInsnAndOffset(macro+mcADDSTK2REG, l5var1z);
                    } else {
                         if (insnList->ilm == il2) {
                             addInsnAndOffset(macro+mcADDACC2REG, l5var1z);
                         } else {
                             curInsnTemplate = InsnTemp[WTC];
                             prepLoad();
                             curInsnTemplate = InsnTemp[XTA];
                             addToInsnList(indexreg[l5var1z] + InsnTemp[UTM]);
                         }
                         insnCopy.next->next = insnList->next2;
                         insnCopy.next = insnList->next;
                     }
                } /* 6305 */
           } else { /* 6306 */
                if (insnCopy.st == st0) {
                    prepLoad();
                    if (l5var7z != 0) {
                        curVal.i = 0 - l5var7z;
                        if (not typeCheck(insnList->typ, IntegerType))
                            curVal.m = curVal.m - intZero;
                        addToInsnList(KADD+I8 + getFCSToffset());
                        insnList->next->mode = 1;
                    }
                    l5var24z = insnCopy.regsused.has(0);
                    if (l5var24z)
                        addToInsnList(macro + mcPUSH);
                    saved = insnList;
                    insnList = copyPtr;
                    setAddrTo(14);
                    if (l5var24z)
                        addToInsnList(macro + mcPOP);
                    l5var23z.m = l5var23z.m + mkbs(0, 10, 11, 13)+mkbs(14);
                    insnCopy.st = st2;
                    insnCopy.ilf6 = 0;
                    insnCopy.ilf5.i = 0;
                    insnCopy.width = insnCopy.typ->pcksize;
                    curVal.i = insnCopy.width;
                    if (curVal.i == 24)
                        curVal.i = 7;
                    curVal.m = curVal.m << 24;
                    addToInsnList(allocSymtab(  /* P/00C */
                        helperNames[76] + curVal.m)+(KVTM+I11));
                    insnCopy.ilf7 = 16;
                    insnCopy.shift = 0;
                    saved->next->next = insnCopy.next2;
                    insnCopy.next2 = saved->next2;
                } else {
                    error(errUsingVarAfterIndexingPackedArray);
                }
            } /* 6403 */
            insnCopy.regsused = l5var23z.m;
        }
        insnCopy.typ = l5var26z;
    } /* 6406 */
    insnList = l5ins21z;
    *insnList = insnCopy;
} /* genGetElt */

struct genEntry {
    genEntry();

    ExprPtr l5exp1z, l5exp2z;
    IdentRecPtr l5idr3z, l5idr4z, l5idr5z, l5idr6z;
    bool l5bool7z, l5bool8z, l5bool9z, l5bool10z, l5bool11z;
    Word l5var12z, l5var13z, l5var14z;
    int64_t l5var15z, l5var16z;
    Word l5var17z, l5var18z, l5var19z;
    InsnListPtr l5inl20z;
    Operator l5op21z;
    IdClass l5idc22z;
    void traceEntry(bool isEntry);
};


int64_t allocGlobalObject(IdentRecPtr l6arg1z)
{
    if (l6arg1z->pos == 0) {
        if (l6arg1z->flags * mkbs(20, 21) != mkbs()) {
            curVal = l6arg1z->id;
            curVal.m = makeNameWithStars(true);
            l6arg1z->pos = allocExtSymbol(extSymMask);
        } else {
            l6arg1z->pos = symTabPos;
            putToSymTab(mkbs());
        }
    }
    return l6arg1z->pos;
} /* allocGlobalObject */


void genEntry::traceEntry(bool isEntry)
{
    if (not optSflags.m.has(DebugEntry))
        return;
    curVal = l5idr5z->id;
    addToInsnList(KVTM+I10 + addCurValToFCST());
    if (isEntry)
        addToInsnList(KVTM+I11 + lineCnt);
    addToInsnList(getHelperProc(isEntry * 22 + 57)); /* P/C(E|I) */
} /* traceEntry */

genEntry::genEntry()
{
    ExprPtr & exprToGen = genFullExpr::super.back()->exprToGen;
    l5exp1z = exprToGen->expr1;
    l5idr5z = exprToGen->id2;
    l5bool7z = (l5idr5z->typ == NULL);
    l5bool9z = (l5idr5z->list == NULL);
    if (l5bool7z)
        l5var13z.i = 3;
    else
        l5var13z.i = 4;
    l5var12z.m = l5idr5z->flags;
    l5bool10z = (l5var12z.m.has(21));
    l5bool11z = (l5var12z.m.has(24));
    if (l5bool9z) {
        l5var14z.i = F3307(l5idr5z);
        l5idr6z = l5idr5z->argList;
    } else {
        l5var13z.i = l5var13z.i + 2;
    }
    insnList = new InsnList;
    insnList->next2 = NULL;
    insnList->next = NULL;
    insnList->typ = l5idr5z->typ;
    insnList->regsused = (l5idr5z->flags + mkbsr(7,15)) * (mkbsr(0,8)+mkbsr(10,15));
    insnList->ilm = il2;
    if (l5bool10z) {
        l5bool8z = not l5bool7z;
        if (checkFortran) {
            addToInsnList(getHelperProc(92)); /* "P/MF" */
        }
    } else {
        l5bool8z = true;
        if (((not l5bool9z) and (l5exp1z != NULL))
            or ((l5bool9z) and (l5var14z.i >= 2))) {
            addToInsnList(KUTM+SP + l5var13z.i);
        }
    }
    l5var14z.i = 0;
// (loop)
    while (l5exp1z != NULL) { /* 6574 */
        l5exp2z = l5exp1z->expr2;
        l5exp1z = l5exp1z->expr1;
        l5op21z = l5exp2z->op;
        l5var14z.i = l5var14z.i + 1;
        l5inl20z = insnList;
        if ((l5op21z == PCALL) or (l5op21z == FCALL)) {
            l5idr4z = l5exp2z->id2;
            insnList = new InsnList;
            insnList->next2 = NULL;
            insnList->next = NULL;
            insnList->regsused = mkbs();
            set145z = set145z + l5idr4z->flags;
            if (l5idr4z->list != NULL) {
                addToInsnList(l5idr4z->offset + InsnTemp[XTA] +
                              l5idr4z->value);
                if (l5bool10z)
                    addToInsnList(getHelperProc(19)); /* "P/EA" */
            } else
                /*(a) */         { /* 6636 */
                if (l5idr4z->value == 0) {
                    if ((l5bool10z) and (l5idr4z->flags.has(21))) {
                        addToInsnList(allocGlobalObject(l5idr4z) +
                                      (KVTM+I14));
                        addToInsnList(KITA+14);
                        goto exit_a;
                    } else { /* 6651 */
                        l5var16z = 0;
                        formJump(l5var16z);
                        padToLeft();
                        l5idr4z->value = moduleOffset;
                        l5idr3z = l5idr4z->argList;
                        l5var15z = l5idr4z->typ != NULL;
                        l5var17z.i = F3307(l5idr4z);
                        form3Insn(KVTM+I10+ 4+moduleOffset,
                                  KVTM+I9 + l5var15z,
                                  KVTM+I8 + 074001);
                        formAndAlign(getHelperProc(62)); /* "P/BP" */
                        l5var15z = l5var17z.i + 2 + l5var15z;
                        form1Insn(KXTA+SP + l5var15z);
                        if ((1) < l5var17z.i)
                            form1Insn(KUTM+SP + l5var15z);
                        else
                            form1Insn(0);
                        form2Insn(
                            getHelperProc(63/*P/B6*/) - 0500000,
                            allocGlobalObject(l5idr4z) + KUJ);
                        if (l5idr3z != NULL) {
                            do {
                                l5idc22z = l5idr3z->cl;
                                if ((l5idc22z == ROUTINEID) and
                                    (l5idr3z->typ != NULL))
                                    l5idc22z = ENUMID;
                                form2Insn(0, l5idc22z);
                                l5idr3z = l5idr3z->list;
                            } while (l5idr4z != l5idr3z);
                        } /* 6745 */
                        storeObjWord(mkbs());
                        P0715(0, l5var16z);
                    }
                } /* 6752 */
                addToInsnList(KVTM+I14 + l5idr4z->value);
                if (l5idr4z->flags.has(21))
                    addToInsnList(KITA+14);
                else
                    addToInsnList(getHelperProc(64)); /* "P/PB" */
              exit_a:;
            }; /* 6765 */
            if (l5op21z == PCALL)
                l5idc22z = ROUTINEID;
            else
                l5idc22z = ENUMID;
        } else { /* 6772 */
            (void) genFullExpr(l5exp2z);
            if (insnList->ilm == il1)
              l5idc22z = FORMALID;
            else
                l5idc22z = VARID;
        } /* 7001 */
        if (not (not l5bool9z or (l5idc22z != FORMALID) or
                 (l5idr6z->cl != VARID)))
            l5idc22z = VARID;
          loop:
        if ((l5idc22z == FORMALID) or (l5bool11z)) {
            setAddrTo(14);
            addToInsnList(KITA+14);
        } else if (l5idc22z == VARID) {
            if (insnList->typ->size != 1) {
                l5idc22z = FORMALID;
                goto loop;
            } else {
                prepLoad();
            }
        } /* 7027 */
        if (not l5bool8z)
            addxToInsnList(macro + mcPUSH);
        l5bool8z = false;
        if (l5inl20z->next != NULL) {
            l5inl20z->next->next = insnList->next2;
            insnList->next2 = l5inl20z->next2;
        }
        insnList->regsused = insnList->regsused + l5inl20z->regsused;
        if (not l5bool9z) {
            curVal.cl = l5idc22z;
            addToInsnList(KXTS+I8 + getFCSToffset());
        }
        if (l5bool9z and not l5bool11z)
            l5idr6z = l5idr6z->list;
    }; /* while -> 7061 */
    traceEntry(true);
    if (l5bool10z) {
        addToInsnList(KNTR+2);
        insnList->next->mode = 4;
    }
    if (l5bool9z) {
        addToInsnList(allocGlobalObject(l5idr5z) + (KVJM+I13));
        if (l5idr5z->flags.has(20)) {
            l5var17z.i = 1;
        } else {
            l5var17z.i = l5idr5z->offset / 04000000;
        } /* 7102 */
    } else { /* 7103 */
        l5var15z = 0;
        if (l5var14z.i == 0) {
            l5var17z.i = l5var13z.i + 1;
        } else {
            l5var17z.i = -(2 * l5var14z.i + l5var13z.i);
            l5var15z = 1;
        } /* 7115 */
        addInsnAndOffset(macro+16 + l5var15z,
                         getValueOrAllocSymtab(l5var17z.i));
        addToInsnList(l5idr5z->offset + InsnTemp[UTC] + l5idr5z->value);
        addToInsnList(macro+18);
        l5var17z.i = 1;
    } /* 7132 */
    insnList->next->mode = 2;
    if (curProcNesting != l5var17z.i) {
        if (not l5bool10z) {
            if (l5var17z.i + 1 == curProcNesting) {
                addToInsnList(KMTJ+I7 + curProcNesting);
            } else {
                l5var15z = frameRestore[curProcNesting][l5var17z.i];
                if (l5var15z == (0)) {
                    curVal.ii = 06017L << 36; /* P/ */
                    l5var19z.ii = (curProcNesting + 16) << 30;
                    l5var18z.ii = (l5var17z.i + 16) << 24;
                    curVal.m = curVal.m + l5var19z.m + l5var18z.m;
                    l5var15z = allocExtSymbol(extSymMask);
                    frameRestore[curProcNesting][l5var17z.i] = l5var15z;
                }
                addToInsnList(KVJM+I13 + l5var15z);
            }
        }
    } /* 7176 */
    if (not l5bool9z or (mkbs(20, 21) * l5var12z.m != mkbs())) {
        addToInsnList(KVTM+040074001);
    }
    set145z = (set145z + l5var12z.m) * mkbsr(1,15);
    traceEntry(false);
    if (l5bool10z) {
        if (not checkFortran)
            addToInsnList(KNTR+7);
        else
            addToInsnList(getHelperProc(93));    /* "P/FM" */
        insnList->next->mode = 2;
    } else {
        if (not l5bool7z)
            addToInsnList(KXTA+SP + l5var13z.i - 1);
    } /* 7226 */
    if (not l5bool7z) {
        insnList->typ = l5idr5z->typ;
        insnList->regsused = insnList->regsused + mkbs(0L);
        insnList->ilm = il2;
        set146z = set146z - l5var12z.m;
    }
    /* 7237 */
} /* genEntry */

void startInsnList(ilmode l5arg1z)
{
    ExprPtr & exprToGen = genFullExpr::super.back()->exprToGen;
    insnList = new InsnList;
    insnList->next = NULL;
    insnList->next2 = NULL;
    insnList->typ = exprToGen->typ;
    insnList->regsused = mkbs();
    insnList->ilm = l5arg1z;
    if (l5arg1z == ilCONST) {
        insnList->ilf5.ii = exprToGen->num1;
        insnList->ilf7 = exprToGen->num2;
    } else {
        insnList->st = st0;
        insnList->ilf7 = 18;
        insnList->ilf5.ii = curFrameRegTemplate;
        insnList->ilf6 = exprToGen->num1;
    }
}

void genCopy()
{
    int64_t size;
    InsnList * &saved = formOperator::super.back()->saved;
    int64_t &work = genFullExpr::super.back()->work;
    InsnList * &otherIns = genFullExpr::super.back()->otherIns;

    size = insnList->typ->size;
    if (size == 1) {
        saved = insnList;
        insnList = otherIns;
        prepLoad();
        genOneOp();
        insnList = saved;
        prepStore();
        genOneOp();
    } else {
        genFullExpr::super.back()->prepMultiWord();
        genOneOp();
        size = size - 1;
        formAndAlign(KVTM+I13 + getValueOrAllocSymtab(-size));
        work = moduleOffset;
        form2Insn(KUTC+I14 + size, KXTA+I13);
        form3Insn(KUTC+I12 + size, KATX+I13,
                  KVLM+I13 + work);
        set145z = set145z + mkbsr(12,14);
    }
} /* genCopy */

void genComparison() {
    bool hasEq;
    Bitset l5set2z;
    int64_t mode, size;

    int64_t &l3int3z = formOperator::super.back()->l3int3z;
    Operator &curOP = genFullExpr::super.back()->curOP;
    bool &arg1Const = genFullExpr::super.back()->arg1Const;
    bool &arg2Const = genFullExpr::super.back()->arg2Const;
    Word &arg1Val = genFullExpr::super.back()->arg1Val;
    Word &arg2Val = genFullExpr::super.back()->arg2Val;
    InsnList * &otherIns = genFullExpr::super.back()->otherIns;
    InsnList * &saved = formOperator::super.back()->saved;
    int64_t &nextInsn = formOperator::super.back()->nextInsn;
    int64_t &work = genFullExpr::super.back()->work;
    TypesPtr &l2typ13z = programme::super.back()->l2typ13z;

    l3int3z = curOP - NEOP;
    hasEq = l3int3z & 1;
    if (l3int3z == 6) {     /* IN */
        if (arg1Const) {
            if (arg2Const) {
                insnList->ilf5.b = arg2Val.m.has(arg1Val.i);
            } else {
                l5set2z = mkbs(arg1Val.i);
                if (l5set2z == mkbs()) {
                    insnList->ilf5.b = false;
                } else {
                    insnList = otherIns;
                    prepLoad();
                    curVal.m = l5set2z;
                    addToInsnList(KAAX+I8 + getFCSToffset());
                    insnList->ilf5.i = 0;
                    insnList->ilm = il3;
                }
            } /* 7412 */
        } else { /* 7413 */
            saved = insnList;
            insnList = otherIns;
            otherIns = saved;
            nextInsn = 66;      /* P/IN */
            genFullExpr::super.back()->genHelper();
            insnList->ilm = il2;
        }
    } else { /* 7423 */
        if (hasEq)
            l3int3z = l3int3z - 1;
        l2typ13z = insnList->typ;
        curVarKind = l2typ13z->k;
        size = l2typ13z->size;
        if (l2typ13z == RealType) {
            if (fuzzReals)
                work = 0;
            else
                work = 1;
        } else if (curVarKind == kindSet)
            work = 2;
        else if (curVarKind == kindScalar || curVarKind == kindRange)
            work = 3;
        else {
            work = 4;
        }
        if (size != 1) {
            genFullExpr::super.back()->prepMultiWord();
            addInsnAndOffset(KVTM+I11, 1 - size);
            addToInsnList(getHelperProc(89 + l3int3z)); /* P/EQ */
            insnList->ilm = il2;
            hasEq = not hasEq;
        } else if (l3int3z == 0) {
            if (work == 0) {
                nextInsn = 15;         /* P/CP */
              L7475:
                genFullExpr::super.back()->genHelper();
                insnList->ilm = il2;
            } else { /* 7501 */
                nextInsn = InsnTemp[AEX];
                genFullExpr::super.back()->tryFlip(true);
              L7504:
                insnList->ilm = il3;
                insnList->ilf5.i = 0;
            }
        } else { /* 7510 */
            switch (work) {
            case 0: { /*7511*/
                nextInsn = 16;         /* P/AB */
                goto L7475;
            } break;
            case 1: { /*7513*/
                mode = 3;
                  L7514:
                nextInsn = InsnTemp[SUB];
                genFullExpr::super.back()->tryFlip(false);
                insnList->next->mode = mode;
                if (mode == 3) {
                    addToInsnList(KNTR+023);
                    insnList->next->mode = 2;
                }
                goto L7504;
            } break;
            case  2: { /*7527*/
                nextInsn = InsnTemp[AAX];
                  L7530:
                prepLoad();
                addToInsnList(KAEX+ALLONES);
                genFullExpr::super.back()->tryFlip(true);
                goto L7504;
            } break;
            case 3: { /*7536*/
                mode = 1;
                goto L7514;
            } break;
            case 4: { /*7540*/
                nextInsn = InsnTemp[ARX];
                goto L7530;
            } break;
            }; /* case */
        }; /* 7554 */
        insnList->regsused = insnList->regsused - mkbs(16);
        if (hasEq)
            genFullExpr::super.back()->negateCond();
    } /* 7562 */
} /* genComparison */

genFullExpr::genFullExpr(ExprPtr exprToGen_) : exprToGen(exprToGen_) {
    int64_t &l3int3z = formOperator::super.back()->l3int3z;
    bool &l3bool13z = formOperator::super.back()->l3bool13z;
    int64_t &nextInsn = formOperator::super.back()->nextInsn;
    OpFlg &flags = formOperator::super.back()->flags;
    InsnList * &saved = formOperator::super.back()->saved;
    IdentRecPtr &curIdRec = programme::super.back()->curIdRec;

    super.push_back(this);

    if (exprToGen == NULL)
        return;
L7567:
    curOP = exprToGen->op;
    if (curOP < GETELT) {
        genFullExpr(exprToGen->expr2);
        otherIns = insnList;
        if (curOP == ASSIGNOP)
            l3bool13z = false;
        genFullExpr(exprToGen->expr1);
        if (curOP == ASSIGNOP)
            l3bool13z = true;
        if (insnList->ilm == ilCONST) {
            arg1Const = true;
            arg1Val = insnList->ilf5;
        } else
            arg1Const = false;
        if (otherIns->ilm == ilCONST) {
            arg2Const = true;
            arg2Val = otherIns->ilf5;
        } else
            arg2Const = false;
        if ((mkbs(NEOP)+mkbs(EQOP)+mkbs(LTOP)+mkbs(GEOP)+mkbs(GTOP)+mkbs(LEOP)+mkbs(INOP)).has(curOP)) {
            genComparison();
        } else { /* 7625 */
            if (arg1Const and arg2Const) {
                switch (curOP) {
                case MUL:        arg1Val.r = arg1Val.r * arg2Val.r;
                    break;
                case RDIVOP:     arg1Val.r = arg1Val.r / arg2Val.r;
                    break;
                case AMPERS:     arg1Val.b = arg1Val.b and arg2Val.b;
                    break;
                case IDIVOP:     arg1Val.i = arg1Val.i / arg2Val.i;
                    break;
                case IMODOP:     arg1Val.i = arg1Val.i % arg2Val.i;
                    break;
                case PLUSOP:     arg1Val.r = arg1Val.r + arg2Val.r;
                    break;
                case MINUSOP:    arg1Val.r = arg1Val.r - arg2Val.r;
                    break;
                case OROP:       arg1Val.b = arg1Val.b or arg2Val.b;
                    break;
                case IMULOP:     arg1Val.i = arg1Val.i * arg2Val.i;
                    break;
                case SETAND:     arg1Val.m = arg1Val.m * arg2Val.m;
                    break;
                case SETXOR:     arg1Val.m = arg1Val.m ^ arg2Val.m;
                    break;
                case INTPLUS:    arg1Val.i = arg1Val.i + arg2Val.i;
                    break;
                case INTMINUS:   arg1Val.i = arg1Val.i - arg2Val.i;
                    break;
                case SETOR:      arg1Val.m = arg1Val.m + arg2Val.m;
                    break;
                case IDIVROP:    arg1Val.r = (double)arg1Val.i / (double)arg2Val.i;
                    break;
                case SETSUB:
                    goto L10075;
                case NEOP: case EQOP: case LTOP: case GEOP: case GTOP: case LEOP: case INOP:
                case badop27: case badop30: case badop31: case MKRANGE: case ASSIGNOP:
                    error(200);
                    break;
                default:
                    break;
                } /* case 7750 */
                insnList->ilf5 = arg1Val;
            } else { /*7752*/
                l3int3z = opToMode[curOP];
                flags = opFlags[curOP];
                nextInsn = opToInsn[curOP];
                switch (flags) {
                case opfCOMM:
                L7760:
                    tryFlip(curOP==MUL||curOP==PLUSOP||curOP==SETAND||curOP==INTPLUS);
                    break;
                case opfHELP:
                    genHelper();
                    break;
                case opfASSN: {
                    genCopy();
                    return;
                }
                case opfAND: {
                    genBoolAnd();
                    return;
                }
                case opfOR: {
                    negateCond();
                    saved = insnList;
                    insnList = otherIns;
                    negateCond();
                    otherIns = insnList;
                    insnList = saved;
                    genBoolAnd();
                    negateCond();
                    return;
                }
                case opfMOD:
                    if (arg2Const) {
                        prepLoad();
                        if (card(arg2Val.m) == 4) { // check for integer with 1 bit set, incl. the exponent
                            // compute the mask
                            curVal.m = arg2Val.m;
                            curVal.i = curVal.i - 1;
                            addToInsnList(KAAX+I8 +getFCSToffset());
                            l3int3z = 0;
                        } else { /* 10016 */
                            addToInsnList(macro + mcPUSH);
                            genConstDiv();
                            insnList->next->mode = 1;
                            curVal.m = arg2Val.m - mkbs(1, 3);
                            addToInsnList(KMUL+I8 + getFCSToffset());
                            addToInsnList(KYTA+64);
                            addToInsnList(KRSUB+SP);
                            l3int3z = 1;
                        } /* 10036 */
                    } else { /* 10037 */
                        genHelper();
                    };
                    break;
                case opfDIV: {
                    if (arg2Const) {
                        prepLoad();
                        genConstDiv();
                        l3int3z = 1;
                    } else
                        genHelper();
                } break;
                case opfMULMSK: {
                    if (arg1Const) {
                        insnList->ilf5.m = arg1Val.m ^ mkbs(1, 3);
                    } else {
                        if (arg2Const) {
                            otherIns->ilf5.m = arg2Val.m ^ mkbs(1, 3);
                        } else {
                            prepLoad();
                            addToInsnList(KAEX+MULTMASK);
                        }
                    }
                    tryFlip(true);
                    insnList->next->mode = 1;
                    if (fixMult)
                        addToInsnList(macro + mcMULTI);
                    else
                        addToInsnList(KYTA+64);
                } break;
                case opfINV: {
                      L10075:
                    saved = insnList;
                    insnList = otherIns;
                    otherIns = saved;
                    prepLoad();
                    addToInsnList(KAEX+ALLONES);
                    goto L7760;
                } break;
                }; /* case 10122 */
              L10122:
                insnList->next->mode = l3int3z;
            }
        }
    } else { /* 10125 */
        if (FILEPTR >= curOP) {
            if (curOP == GETVAR) {
                insnList = new InsnList;
                curIdRec = exprToGen->id1;
                /* with insnList@ do */ {
                    insnList->next = NULL;
                    insnList->next2 = NULL;
                    insnList->regsused = mkbs();
                    insnList->ilm = il1;
                    insnList->ilf5.i = curIdRec->offset;
                    insnList->ilf6 = curIdRec->high;
                    insnList->st = st0;
                    insnList->ilf7 = 18;
                };
                if (curIdRec->cl == FORMALID) {
                    genDeref();
                } else if (curIdRec->cl == ROUTINEID) {
                    insnList->ilf6 = 3;
                    insnList->ilf5.i = (insnList->ilf5.i + frameRegTemplate);
                } else if (insnList->ilf6 >= 074000) {
                    addToInsnList(InsnTemp[UTC] + insnList->ilf6);
                    insnList->ilf6 = 0;
                    insnList->ilf7 = 17;
                    insnList->ilf5.i = 0;
                }
            } else /* 10171 */
            if (curOP == GETFIELD) {
                genFullExpr(exprToGen->expr1);
                curIdRec = exprToGen->id2;
                /* with insnList@ do */ {
                    insnList->ilf6 = insnList->ilf6 + curIdRec->offset;
                    if (curIdRec->pckfield) {
                        switch (insnList->st) {
                        case st0:
                            insnList->shift = curIdRec->shift;
                            break;
                        case st1: {
                            insnList->shift = insnList->shift + curIdRec->shift;
                            if (not optSflags.m.has(S6))
                                insnList->shift = insnList->shift + curIdRec->uptype->bits - 48;
                        } break;
                        case st2:
                            if (not l3bool13z)
                                error(errUsingVarAfterIndexingPackedArray);
                            else {
                                P5155();
                                insnList->shift = curIdRec->shift;
                            }
                            break;
                        } /* 10235*/
                        insnList->width = curIdRec->width;
                        insnList->st = st1;
                        insnList->regsused = insnList->regsused + mkbs(0L);
                    }
                }
            } else /* 10244 */
            if (curOP == GETELT)
                genGetElt();
            else if ((curOP == DEREF) or (curOP == FILEPTR)) {
                genFullExpr(exprToGen->expr1);
                genDeref();
            } else if (curOP == op36) {
                startInsnList(il1);
            } else if (curOP == op37) {
                startInsnList(il1);
                genDeref();
            } else if (curOP == GETENUM)
                startInsnList(ilCONST);
        } else if (curOP == ALNUM)
            genEntry();
        else if (BOUNDS <= curOP && curOP <= RNEGOP) {
            genFullExpr(exprToGen->expr1);
            if (insnList->ilm == ilCONST) {
                arg1Val = insnList->ilf5;
                switch (curOP) {
                case BOUNDS: {
                      arg2Val.m = mkbs(0,1,3) + arg1Val.m;
                    /* with exprToGen->typ2@ do */ {
                            if ((arg2Val.i < exprToGen->typ2->left) or
                                (exprToGen->typ2->right < arg2Val.i))
                                error(errNeedOtherTypesOfOperands);
                        }
                } break;
                case TOREAL: arg1Val.r = arg1Val.i;
                    break;
                case NOTOP: arg1Val.b = not arg1Val.b;
                    break;
                case RNEGOP: arg1Val.r = -arg1Val.r;
                    break;
                case INEGOP: arg1Val.i = -arg1Val.i;
                    break;
                default:
                    break;
                }; /* case 10345 */
                insnList->ilf5 = arg1Val;
            } else if (curOP == NOTOP) {
                negateCond();
            } else {
                prepLoad();
                if (curOP == BOUNDS) {
                    if (checkBounds)
                        genCheckBounds(exprToGen->typ2);
                } else if (curOP == TOREAL) {
                    addToInsnList(InsnTemp[AVX]);
                    l3int3z = 3;
                    goto L10122;
                } else {
                    addToInsnList(KAVX+MINUS1);
                    if (curOP == RNEGOP)
                        l3int3z = 3;
                    else
                        l3int3z = 1;
                    goto L10122;
                }
            }
        } else /* 10376 */
        if (curOP == STANDPROC) {
            genFullExpr(exprToGen->expr1);
            work = exprToGen->num2;
            if (100 < work) {
                prepLoad();
                addToInsnList(getHelperProc(work - 100));
            } else {
                if (insnList->ilm == ilCONST) {
                    arg1Const = true;
                    arg1Val = insnList->ilf5;
                } else
                    arg1Const = false;
                arg2Const = (insnList->typ == RealType);
                if (arg1Const) {
                    switch (work) {
                    case fnSQRT:  arg1Val.r = sqrt(arg1Val.r);
                        break;
                    case fnSIN:   arg1Val.r = sin(arg1Val.r);
                        break;
                    case fnCOS:   arg1Val.r = cos(arg1Val.r);
                        break;
                    case fnATAN:  arg1Val.r = atan(arg1Val.r);
                        break;
                    case fnASIN:  arg1Val.r = asin(arg1Val.r);
                        break;
                    case fnLN:    arg1Val.r = log(arg1Val.r);
                        break;
                    case fnEXP:   arg1Val.r = exp(arg1Val.r);
                        break;
                    case fnABS:   arg1Val.r = fabs(arg1Val.r);
                        break;
                    case fnTRUNC: arg1Val.i = int64_t(trunc(arg1Val.r));
                        break;
                    case fnODD:   arg1Val.b = arg1Val.i & 1;
                        break;
                    case fnORD:   arg1Val.m = arg1Val.m + mkbs(0,1,3); // adding integer exponent
                        break;
                    case fnCHR:   arg1Val.m = arg1Val.m - mkbs(0,1,3); // dropping integer exponent
                        break;
                    case fnSUCC:  arg1Val.m.val = (arg1Val.m.val + 1) & ((1L<<48)-1);
                        break;
                    case fnPRED:  arg1Val.m.val = (arg1Val.m.val - 1)  & ((1L<<48)-1);
                        break;
                    case fnPTR:   arg1Val.m = arg1Val.m - mkbs(0,1,3); // bitwise the same as CHR
                        break;
                    case fnSQR:   arg1Val.r = arg1Val.r*arg1Val.r;
                        break;
                    case fnROUND: arg1Val.i = int64_t(round(arg1Val.r));
                        break;
                    case fnCARD:  arg1Val.i = card(arg1Val.m);
                        break;
                    case fnMINEL: arg1Val.i = minel(arg1Val.m);
                        break;
                    case fnABSI:  arg1Val.i = labs(arg1Val.i);
                        break;
                    case fnSQRI:  arg1Val.i = arg1Val.i*arg1Val.i;
                        break;
                    case fnEOF:
                    case fnREF:
                    case fnEOLN:
                        error(201);
                        break;
                    } /* 10546 */
                    insnList->ilf5 = arg1Val;
                } else if ((work >= fnEOF) and (fnEOLN >= work)) {
                    if (work == fnREF) {
                        setAddrTo(14);
                        addToInsnList(KITA+14);
                    } else {
                        setAddrTo(12);
                        addToInsnList(getHelperProc(work - 6));
                    }
                    /* with insnList@ do */ {
                        insnList->ilm = il2;
                        insnList->regsused = insnList->regsused + mkbs(0L);
                    }
                } else {
                    prepLoad();
                    if (work == fnTRUNC) {
                        l3int3z = 2;
                        addToInsnList(getHelperProc(58)); /*"P/TR"*/
                        goto L10122;
                    };
                    if ((fnSQRT<=work&&work<=fnEXP)||
                        (fnODD<=work&&work<=fnSUCC)||work==fnCARD||work==fnPTR) {
                        l3int3z = 0;
                    } else if (work ==fnABS || work == fnSQR)
                        l3int3z = 3;
                    else {
                        l3int3z = 1;
                    }
                    addToInsnList(funcInsn[work]);
                    goto L10122;
                }
            }
        } else { /* 10621 */
            if (curOP == NOOP) {
                curVal = exprToGen->val;
                if (set146z.has(curVal.i)) {
                    insnList = new InsnList;
                    /* with insnList@ do */ {
                        insnList->typ = exprToGen->expr2->typ;
                        insnList->next = NULL;
                        insnList->next2 = NULL;
                        insnList->regsused = mkbs();
                        insnList->ilm = il1;
                        insnList->ilf7 = 18;
                        insnList->ilf5.i = indexreg[curVal.i];
                        insnList->ilf6 = 0;
                        insnList->st = st0;
                    }
                } else {
                    curVal.i = 14;
                    exprToGen->val = curVal;
                    exprToGen = exprToGen->expr2;
                    goto L7567;
                };
                return;
            } else {
                error(220);
            }
        }
    } /* 10654 */
    insnList->typ = exprToGen->typ;
    /* 10656 */
} /* genFullExpr */

void formFileInit()
{
    ExtFileRec * l4exf1z;
    TypesPtr l4var2z;
    IdentRecPtr l4var3z;
    int64_t l4int4z, l4int5z;

    if (optSflags.m.has(S5) ||
        (externFileList == NULL && inputFile == NULL && outputFile == NULL)) {
        formAndAlign(KUJ+I13);
        return;
    }
    form2Insn(KITS+13, KATX+SP);
    while (curExpr != NULL) {
        l4exf1z = reinterpret_cast<ExtFileRec*>(curExpr->typ);
        l4var3z = curExpr->id2;
        l4int4z = l4var3z->value;
        l4var2z = l4var3z->typ->base;
        l4int5z = l4var3z->typ->elsize;
        if (l4int4z < 074000) {
            form1Insn(getValueOrAllocSymtab(l4int4z) +
                      InsnTemp[UTC] + I7);
            l4int4z = 0;
        }
        form3Insn(KVTM+I12 + l4int4z, KVTM+I10 + fileBufSize,
                  KVTM+I9 + l4int5z);
        form1Insn(KVTM+I11 + l4var2z->size);
        if (l4exf1z == NULL) {
            form1Insn(InsnTemp[XTA]);
        } else {
            curVal.i = l4exf1z->location;
            if (curVal.i == 512)
                curVal = l4exf1z->offset;
            form1Insn(KXTA+I8 + getFCSToffset());
        }
        formAndAlign(getHelperProc(69)); /*"P/CO"*/
        curVal = l4var3z->id;
        form2Insn(KXTA+I8+getFCSToffset(), KATX+I12+26);
        if ((l4int5z != 0) and
            typeCheck(l4var2z, IntegerType))
            form2Insn(KXTA+ZERO, KATX+I12+25);
        curExpr = curExpr->expr1;
    }
    form1Insn(getHelperProc(70)/*"P/IT"*/ + (-I13-0100000));
    padToLeft();
} /* formFileInit */

formOperator::formOperator(OpGen l3arg1z)
{ /* formOperator */
    super.push_back(this);
    l3bool13z = true;
    if ((errors and (l3arg1z != SETREG)) or curExpr == NULL)
        return;
    if (l3arg1z!=gen3&&l3arg1z!=gen6&&l3arg1z!=gen9&&l3arg1z!=gen14&&l3arg1z!=gen16)
        (void) genFullExpr(curExpr);
    switch (l3arg1z) {
    case gen7: genOneOp();
        break;
    case SETREG: {
        /* with insnList@ do */
        {
            l3int3z = insnCount();
            l3var5z = new Expr;
            l3var5z->expr1 = expr63z;
            expr63z = l3var5z;
            l3var5z->op = NOOP;
            switch (insnList->st) {
            case st0: {
                if (l3int3z == 0)  {
                    l3int2z = 14;
                } else {
                    l3var10z.m = set148z * set147z;
                    if (l3var10z.m != mkbs()) {
                        l3int2z = minel(l3var10z.m);
                    } else {
                        l3int2z = 14;
                    }
                    if (l3int3z != 1) {
                        (void) setAddrTo(l3int2z);
                        addToInsnList(KITA + l3int2z);
                        P5117(op37);
                    } else if (l3int2z != 14) {
                        (void) setAddrTo(l3int2z);
                        genOneOp();
                    }
                    l3var11z.m = mkbs(l3int2z) - mkbs(14);
                    set145z = set145z - l3var11z.m;
                    set147z = set147z - l3var11z.m;
                    set146z = set146z + l3var11z.m;
                }
                curVal.i = l3int2z;
                l3var5z->val = curVal;
            } break;
            case st1: {
                curVal.i = 14;
                l3var5z->val = curVal;
            } break;
            case st2:
                error(errVarTooComplex);
                break;
            } /* case */
        } /* with */
        l3var5z->expr2 = curExpr;
    } break; /* SETREG */
    case gen0: {
        prepLoad();
        if (insnCount() > 1)
            P5117(op36);
    } break;
    case STORE: {
        prepStore();
        genOneOp();
    } break;
    case gen3: {
        curInsnTemplate = curVal.i;
        (void) formOperator(LOAD);
        curInsnTemplate = InsnTemp[XTA];
    } break;
    case gen5: {
        if (insnList->st != st0)
            error(errVarTooComplex);
        setAddrTo(9);
        genOneOp();
    } break;
    case gen6: {
        l3int1z = curVal.i;
        (void) genFullExpr(curExpr);
        prepLoad();
        if (insnList->regsused.has(9))
            error(errVarTooComplex);
        genOneOp();
        form1Insn(KATX+I9 + l3int1z);
    } break;
    case gen8: {
        (void) setAddrTo(12);
        genOneOp();
    } break;
    case gen9: {
        curVal.m = curVal.m + intZero;
        form1Insn(KXTA+I8 + getFCSToffset());
    } break;
    case gen10: {
        prepLoad();
        addxToInsnList(macro + mcPUSH);
        genOneOp();
    } break;
    case gen11: case gen12: {
        setAddrTo(11);
        if (l3arg1z == gen12)
            addxToInsnList(macro + mcPUSH);
        genOneOp();
          set145z = set145z + mkbs(12);
    } break;
    case FILEACCESS: {
        setAddrTo(12);
        genOneOp();
        formAndAlign(jumpTarget);
    } break;
    case gen14:
        formFileInit();
        break;
    case LOAD: {
        prepLoad();
        genOneOp();
    } break;
    case gen15:
        /* with insnList@ do */ {
            l3bool9z = jumpTarget == 0;
            l3int3z = jumpTarget;
            if (insnList->ilm == ilCONST) {
                if (insnList->ilf5.b) {
                    jumpTarget = 0;
                } else {
                    if (l3bool9z) {
                        formJump(jumpTarget);
                    } else {
                        form1Insn(InsnTemp[UJ] + jumpTarget);
                    }
                }
            } else {
                l3var8z.b = (insnList->regsused.has(16));
                if ((insnList->ilm == il3) and
                    (insnList->ilf5.i != 0)) {
                    genOneOp();
                    if (l3var8z.b) {
                        if (l3bool9z)
                            formJump(l3int3z);
                        else
                            form1Insn(InsnTemp[UJ] + l3int3z);
                        P0715(0, jumpTarget);
                        jumpTarget = l3int3z;
                    } else {
                        if (not l3bool9z) {
                            if (not putLeft)
                                padToLeft();
                            P0715(l3int3z, jumpTarget);
                        }
                    }
                } else {
                    if (insnList->ilm == il1) {
                        bool49z = false;
                        prepLoad();
                        bool49z = true;
                    }
                    genOneOp();
                    if (l3var8z.b)
                        nextInsn = InsnTemp[U1A];
                    else
                        nextInsn = InsnTemp[UZA];
                    if (l3bool9z) {
                        jumpType = nextInsn;
                        formJump(l3int3z);
                        jumpType = InsnTemp[UJ];
                        jumpTarget = l3int3z;
                    } else {
                        form1Insn(nextInsn + l3int3z);
                    }
                }
            }
        } break; /* gen15 */
    case gen16: {
        l3var5z = curExpr;
        curExpr = curExpr->expr1;
        (void) formOperator(gen11);
        genFullExpr(l3var5z->expr2);
        if (insnList->regsused.has(11))
            error(44); /* errIncorrectUsageOfStandProcOrFunc */
        setAddrTo(12);
        genOneOp();
        arg1Type = l3var5z->expr2->typ;
        /* with arg1Type->range@ do */
            l3int3z = arg1Type->range->right - arg1Type->range->left + 1;
        form2Insn((KVTM+I14) + l3int3z,
                  (KVTM+I10+64) - arg1Type->pcksize);
        l3int3z = ord(l3var5z->typ);
        l3int1z = arg1Type->perWord;
        if (l3int3z == 72)          /* P/KC */
            l3int1z = 1 - l3int1z;
        form1Insn(getValueOrAllocSymtab(l3int1z) + (KVTM+I9));
        if (typeCheck(curExpr->typ, IntegerType)) {
            l3int1z = KXTA+ZERO;
        } else {
            l3int1z = InsnTemp[XTA];
        };
        form1Insn(l3int1z);
        formAndAlign(getHelperProc(l3int3z));
   } break;
   case LITINSN: {
       /* with insnList@ do */ {
            if (insnList->ilm != ilCONST)
                error(errNoConstant);
            if (insnList->typ->size != 1)
                error(errConstOfOtherTypeNeeded);
            curVal = insnList->ilf5;
        }
    } break;
    } /* case */
} /* formOperator */

struct parseTypeRef {
    static std::vector<parseTypeRef*> super;
    parseTypeRef(TypesPtr & newType, Bitset skipTarget_);
    ~parseTypeRef() { super.pop_back(); }
    typedef std::pair<int64_t, int64_t> pair;
    typedef pair pair7[8]; // array [1..7] of pair;
    typedef struct {
            int64_t size, count;
            pair7 pairs;
    } caserec;

    Bitset skipTarget;
    bool isPacked;
    bool cond;
    caserec cases;
    Word leftBound, rightBound;
    int64_t numBits, l3int22z, span;
    IdentRecPtr curEnum, curField;
    TypesPtr l3typ26z, nestedType, tempType, curType;
    Word l3unu30z;
    IdentRecPtr l3idr31z;

    void definExprPtrType(TypesPtr toType) {
        IdentRecPtr & typelist = programme::super.back()->typelist;
        curType = new Types(1, 15, kindPtr, toType);
        curEnum = new IdentRec(curIdent, lineCnt, typelist, curType, TYPEID);
        typelist = curEnum;
    } /* definExprPtrType */
};
std::vector<parseTypeRef*> parseTypeRef::super;

struct parseRecordDecl {
    static std::vector<parseRecordDecl*> super;
    parseRecordDecl(TypesPtr rectype, bool isOuterDecl_);
    ~parseRecordDecl() { super.pop_back(); }

    bool isOuterDecl;
    TypesPtr l4typ1z, selType, l4var3z, l4var4z, l4var5z;
    IdentRecPtr l4var6z;
    Word l4var7z, l4var8z;
    int64_t l4var9z;
    parseTypeRef::caserec cases1, cases2;

    void addFieldToHash() {
        IdentRecPtr &curEnum = parseTypeRef::super.back()->curEnum;
        TypesPtr &curType = parseTypeRef::super.back()->curType;
        bool &isPacked = parseTypeRef::super.back()->isPacked;
        curEnum->id = curIdent;
        curEnum->next = typeHashTabBase[bucket];
        curEnum->cl = FIELDID;
        curEnum->uptype = curType;
        curEnum->pckfield = isPacked;
        typeHashTabBase[bucket] = curEnum;
    }
};
std::vector<parseRecordDecl*> parseRecordDecl::super;

void packFields()
{
    int64_t l5var1z, pairIdx, l5var3z, l5var4z, l5var5z;
    parseTypeRef::pair * l5var6z;

    bool &cond = parseTypeRef::super.back()->cond;
    TypesPtr &curType = parseTypeRef::super.back()->curType;
    IdentRecPtr &curField = parseTypeRef::super.back()->curField;
    IdentRecPtr &l3idr31z = parseTypeRef::super.back()->l3idr31z;
    TypesPtr &selType = parseRecordDecl::super.back()->selType;
    Bitset &skipTarget = parseTypeRef::super.back()->skipTarget;
    parseTypeRef::caserec &cases = parseTypeRef::super.back()->cases;
    bool &isOuterDecl = parseRecordDecl::super.back()->isOuterDecl;
    IdentRecPtr &curEnum = parseTypeRef::super.back()->curEnum;
    bool &isPacked = parseTypeRef::super.back()->isPacked;

    parseTypeRef(selType, skipTarget + mkbs(CASESY));
    if (curType->ptr2 == NULL) {
        curType->ptr2 = curField;
    } else {
        l3idr31z->list = curField;
    }
    cond = isFileType(selType);
    if (not isOuterDecl and cond)
        error(errTypeMustNotBeFile);
    curType->flag = cond or curType->flag;
    l3idr31z = curEnum;
    do {
        curField->typ = selType;
        if (isPacked) {
            l5var1z = selType->bits;
            curField->width = l5var1z;
            if (l5var1z != 48) {
                for (pairIdx = 1; pairIdx <= cases.count; ++pairIdx)
                  L11523:
                    {
                        l5var6z = &cases.pairs[pairIdx];
                        if (l5var6z->first >= l5var1z) {
                            curField->shift = 48 - l5var6z->first;
                            curField->offset = l5var6z->second;
                            if (not optSflags.m.has(S6))
                                curField->shift = 48 - curField->width - curField->shift;
                            l5var6z->first = l5var6z->first - l5var1z;
                            if (l5var6z->first == 0) {
                                cases.pairs[pairIdx] = cases.pairs[cases.count];
                                cases.count = cases.count - 1;
                            } /* 11562 */
                            goto L11622;
                        }
                    } /* 11564 */
                if (cases.count != 7) {
                    cases.count = cases.count + 1;
                    pairIdx = cases.count;
                } else {
                    l5var3z = 48;
                    for (l5var4z = 1; l5var4z <= 7; ++l5var4z) {
                        l5var5z = cases.pairs[l5var4z].first;
                        if (l5var5z < l5var3z) {
                            l5var3z = l5var5z;
                            pairIdx = l5var4z;
                        }
                    } /* for */
                } /* 11606 */
                cases.pairs[pairIdx] = std::make_pair(48, cases.size);
                cases.size = cases.size + 1;
                goto L11523;
            }
        } /* 11615 */
        curField->pckfield = false;
        curField->offset = cases.size;
        cases.size = cases.size + selType->size;
      L11622:
        if (PASINFOR.listMode == 3) {
            printf("%16c", ' ');
            if (curField->pckfield)
                printf("PACKED");
            printf(" FIELD ");
            printTextWord(curField->id);
            printf(".OFFSET=%05loB", curField->offset);
            if (curField->pckfield) {
                printf(".<<=SHIFT=%2ld. WIDTH=%2ld BITS", curField->shift,
                       curField->width);
            } else {
                printf(".WORDS=%ld", selType->size);
            }
            putchar('\n');
        }
        cond = (curField == curEnum);
        curField = curField->list;
    } while (!cond);
} /* packFields */

parseRecordDecl::parseRecordDecl(TypesPtr rectype, bool isOuterDecl_) : isOuterDecl(isOuterDecl_)
{
    bool &cond = parseTypeRef::super.back()->cond;
    TypesPtr &curType = parseTypeRef::super.back()->curType;
    TypesPtr &tempType = parseTypeRef::super.back()->tempType;
    IdentRecPtr &curField = parseTypeRef::super.back()->curField;
    IdentRecPtr &curEnum = parseTypeRef::super.back()->curEnum;
    bool &isPacked = parseTypeRef::super.back()->isPacked;
    parseTypeRef::caserec &cases = parseTypeRef::super.back()->cases;
    Bitset &skipTarget = parseTypeRef::super.back()->skipTarget;

    super.push_back(this);

    int93z = 3;
    inSymbol();
    while (SY == IDENT) {
        l4var6z = NULL;
        do {
            if (SY != IDENT) {
                error(errNoIdent);
            } else {
                if (hashTravPtr != NULL)
                    error(errIdentAlreadyDefined);
                curEnum = new IdentRec;
                addFieldToHash();
                if (l4var6z == NULL) {
                    curField = curEnum;
                } else {
                    l4var6z->list = curEnum;
                }
                l4var6z = curEnum;
                int93z = 3;
                inSymbol();
            }
            cond = (SY != COMMA);
            if (not cond) {
                int93z = 3;
                inSymbol();
            }
        } while (!cond);
        checkSymAndRead(COLON);
        packFields();
        if (SY == SEMICOLON) {
            int93z = 3;
            inSymbol();
        }
    } /*11752*/
    if (SY == CASESY) {
        int93z = 3;
        inSymbol();
        selType = IntegerType;
/*(identif)*/
        if (SY != IDENT) {
            error(3);
            skip(skipTarget + mkbs(OFSY));
        } else { /* 11766 */
            l4var8z = curIdent;
            l4var9z = bucket;
            curEnum = hashTravPtr;
            inSymbol();
            if (SY == COLON) {
                if (curEnum != NULL)
                    error(errIdentAlreadyDefined);
                curEnum = new IdentRec;
                curIdent = l4var8z;
                bucket = l4var9z;
                addFieldToHash();
                inSymbol();
                curField = curEnum;
                packFields();
            } else {
                curEnum = symHashTabBase[l4var9z];
                while (curEnum != NULL) {
                    if (curEnum->id != l4var8z) {
                        curEnum = curEnum->next;
                    } else {
                        if (curEnum->cl != TYPEID) {
                            error(errNotAType);
                            selType = IntegerType;
                        } else {
                            selType = curEnum->typ;
                        }
                        goto exit_identif;
                    }
                }
                error(errNotDefined);
            }
        } exit_identif:; /* 12035 */
        if (selType->k == kindRange)
            selType = selType->base;
        checkSymAndRead(OFSY);
        cases1 = cases;
        cases2 = cases;
        l4typ1z = NULL;
        do {
            l4var3z = NULL;
            do {
                parseLiteral(l4var4z, l4var7z, false);
                if (l4var4z == NULL)
                    error(errNoConstant);
                else if (not typeCheck(l4var4z, selType))
                    error(errConstOfOtherTypeNeeded);
                l4var5z = new Types(cases.size, 48, kindCases, l4var7z, NULL, NULL, NULL);
                if (l4var3z == NULL) {
                    tempType = l4var5z;
                } else {
                    l4var3z->r6 = l4var5z;
                }
                l4var3z = l4var5z;
                inSymbol();
                cond = (SY != COMMA);
                if (not cond)
                    inSymbol();
            } while (!cond);
            if (l4typ1z == NULL) {
                if (curType->base == NULL) {
                    curType->base = tempType;
                } else {
                    rectype->first = tempType;
                }
            } else {
                l4typ1z->next = tempType;
            }
            l4typ1z = tempType;
            checkSymAndRead(COLON);
            if (SY != LPAREN)
                requiredSymErr(LPAREN);
            parseRecordDecl(tempType, false);
            if ((cases2.size < cases.size) or
                (isPacked and (cases.size = 1) and (cases2.size = 1) and
                 (cases.count = 1) and (cases2.count = 1) and
                 (cases.pairs[1].first < cases2.pairs[1].first))) {
                cases2 = cases;
            } /* 12201 */
            cases = cases1;
            checkSymAndRead(RPAREN);
            cond = SY != SEMICOLON;
            if (not cond)
                inSymbol();
            if (SY == ENDSY)
                cond = true;
        } while (!cond);
        cases = cases2;
    } /* 12232 */
    rectype->size = cases.size;
    if (isPacked and (cases.size == 1) and (cases.count == 1)) {
        rectype->bits = 48 - cases.pairs[1].first;
    }
    /* 12242 */
} /* parseRecordDecl*/

parseTypeRef::parseTypeRef(TypesPtr & newType, Bitset skipTarget_) : skipTarget(skipTarget_)
{
    bool &inTypeDef = programme::super.back()->inTypeDef;
    super.push_back(this);
    isPacked = false;
L12247:
    if (SY == LPAREN) {
        span = 0;
        int93z = 0;
        inSymbol();
        curField = NULL;
        curType = new Types;
        while (SY == IDENT) {
            if (isDefined)
                error(errIdentAlreadyDefined);
            curEnum = new IdentRec(curIdent, curFrameRegTemplate,
                                   symHashTabBase[bucket], curType,
                                   ENUMID, NULL, span);
            symHashTabBase[bucket] = curEnum;
            span = span + 1;
            if (curField == NULL) {
                curType->enums = curEnum;
            } else {
                curField->list = curEnum;
            };
            curField = curEnum;
            inSymbol();
            if (SY == COMMA) {
                int93z = 0;
                inSymbol();
            } else {
                if (SY != RPAREN)
                    requiredSymErr(RPAREN);
            }
        } /* 12324 */
        checkSymAndRead(RPAREN);
        if (curField == NULL) {
            curType = BooleanType;
            error(errNoIdent);
        } else {
            // curType@ := [1, nrOfBits(span - 1), kindScalar, , span, 0];
            curType->size = 1;
            curType->bits = nrOfBits(span - 1);
            curType->k = kindScalar;
            curType->numen = span;
            curType->start = 0;
        }
    } else /* 12344 */
    if (SY == ARROW) {
        inSymbol();
        if (SY != IDENT) {
            error(errNoIdent);
            curType = pointerType;
        } else {
            if (hashTravPtr == NULL) {
                if (inTypeDef) {
                    if (knownInType(curEnum)) {
                        curType = curEnum->typ;
                    } else {
                        definExprPtrType(IntegerType);
                    }
                } else {
                  L12366:
                    error(errNotAType);
                    curType = pointerType;
                }
            } else {
                if (hashTravPtr->cl != TYPEID) {
                    goto L12366;
                }
                curType = new Types(1, 15, kindPtr, hashTravPtr->typ);
            } /* 12405 */
            inSymbol();
        }
    } else /* 12410 */
    if (SY == IDENT) {
        if (hashTravPtr != NULL) {
            if (hashTravPtr->cl == TYPEID) {
                curType = hashTravPtr->typ;
            } else {
                goto L12760;
            }
        } else {
            if (inTypeDef) {
                if (knownInType(curEnum)) {
                    curType = curEnum->typ;
                    curType->base = BooleanType;
                } else {
                    definExprPtrType(BooleanType);
                }
            } else {
                error(errNotAType);
                curType = IntegerType;
            }
        }
        inSymbol();
    } else { /* 12440 */
        if (SY == PACKEDSY) {
            isPacked = true;
            inSymbol();
            goto L12247;
        }
        if (SY == RECORDSY) { /* 12446 */
            curType = new Types();
            typ121z = curType;
            /* with curType@ do */ {
                curType->size = 0;
                curType->bits = 48;
                curType->k = kindRecord;
                curType->ptr1 = NULL;
                curType->first = NULL;
                curType->flag = false;
                curType->pckrec = isPacked;
            }
            cases.size = 0;
            cases.count = 0;
            parseRecordDecl(curType, true);
            checkSymAndRead(ENDSY);
        } else if (SY == ARRAYSY) {
            inSymbol();
            if (SY == LBRACK)
                inSymbol();
            tempType = NULL;
          L12476:
            parseTypeRef(nestedType, skipTarget + mkbs(OFSY));
            curVarKind = nestedType->k;
            if (curVarKind != kindRange) {
                if (curVarKind == kindScalar and
                    nestedType != IntegerType) {
                    span = nestedType->numen;
                } else {
                    error(8); /* errNotAnIndexType */
                    nestedType = IntegerType;
                    span = 10;
                }
                defineRange(nestedType, 0, span - 1);
            } /* 12524 */
            l3typ26z = new Types();
            /* with l3typ26z@ do */ {
                l3typ26z->size = ord(tempType);
                l3typ26z->bits = 48;
                l3typ26z->k = kindArray;
                l3typ26z->range = nestedType;
            }
            if (tempType == NULL)
                curType = l3typ26z;
            else
                tempType->base = l3typ26z;
            tempType = l3typ26z;
            if (SY == COMMA) {
                inSymbol();
                goto L12476;
            }
            if (SY == RBRACK)
                inSymbol();
            checkSymAndRead(OFSY);
            parseTypeRef(nestedType, skipTarget);
            l3typ26z->base = nestedType;
            if (isFileType(nestedType))
                error(errTypeMustNotBeFile);
            /* with l3typ26z@, ptr2@ */ do {
                span = l3typ26z->ptr2->high - l3typ26z->ptr2->low + 1;
                tempType = (Types*)ptr(l3typ26z->size);
                l3int22z = l3typ26z->base->bits;
                // Don't clear t->pck flag for word-sized arrays.
                //if (24 < l3int22z)
                //    isPacked = false;
                l3typ26z->bits = 48;
                if (isPacked) {
                    l3int22z = 48 / l3int22z;
                    if (l3int22z == 9) {
                        l3int22z = 8;
                    } else if (l3int22z == 5) {
                          l3int22z = 4;
                    }
                    l3typ26z->perWord = l3int22z;
                    l3typ26z->pcksize = 48 / l3int22z;
                    l3int22z = span * l3typ26z->pcksize;
                    if (l3int22z % 48 == 0)
                        numBits = 0;
                    else
                        numBits = 1;
                    l3typ26z->size = l3int22z / 48 + numBits;
                    if (l3typ26z->size == 1)
                        l3typ26z->bits = l3int22z;
                } else { /* 12633 */
                    l3typ26z->size = span * l3typ26z->base->size;
                    curVal.i = l3typ26z->base->size;
                    curVal.m = curVal.m * mkbsr(7,47) + mkbs(0);
                    if (l3typ26z->range->base != IntegerType)
                        curVal.m = curVal.m + mkbs(1, 3);
                    l3typ26z->perWord = KMUL+ I8 + getFCSToffset();
                } /* 12652 */
                l3typ26z->pck = isPacked;
                isPacked = false;
                cond = (curType == l3typ26z);
                l3typ26z = tempType;
            } while (!cond);
        } else /* 12663 */
        if (SY == FILESY) {
            inSymbol();
            checkSymAndRead(OFSY);
            parseTypeRef(nestedType, skipTarget);
            if (isFileType(nestedType))
                error(errTypeMustNotBeFile);
            if (isPacked) {
                l3int22z = nestedType->bits;
                if (24 < l3int22z)
                    isPacked = false;
            }
            curType = new Types;
            if (not isPacked)
                l3int22z = 0;
            /* with curType@ do */ {
                curType->size = 30;
                curType->bits = 48;
                curType->k = kindFile;
                curType->base = nestedType;
                curType->elsize = l3int22z;
            }
        } else /* 12721 */
        if (SY == SETSY) {
            inSymbol();
            checkSymAndRead(OFSY);
            parseTypeRef(nestedType, skipTarget);
            /* with nestedType@ do */ {
                if (nestedType->k == kindRange and
                   nestedType->left >= 0 and
                   47 >= nestedType->right)
                    numBits = nestedType->right + 1;
                else if (nestedType->k == kindScalar and
                         48 >= nestedType->numen)
                    numBits = nestedType->numen;
                else {
                    numBits = 48;
                    error(63); /* errBadBaseTypeForSet */
                }
            }
            curType = new Types(1, numBits, kindSet, nestedType);
        } else {
          L12760:
            parseLiteral(tempType, leftBound, true);
            if (tempType != NULL) {
                inSymbol();
                if (SY != COLON) {
                    requiredSymErr(COLON);
                } else {
                    inSymbol();
                }
                parseLiteral(curType, rightBound, true);
                if (curType == tempType and
                    curType->k == kindScalar) {
                    defineRange(curType, leftBound.i, rightBound.i);
                    inSymbol();
                    goto L13020;
                }
            }
            error(64); /* errIncorrectRangeDefinition */
            curType = BooleanType;
        }
    }
  L13020:
    if (errors)
        skip(skipToSet + mkbs(RPAREN, RBRACK, SEMICOLON, OFSY));
    newType = curType;
} /* parseTypeRef */

void dumpEnumNames(TypesPtr l3arg1z)
{
    IdentRecPtr l3var1z;
    if (l3arg1z->start == 0) {
        l3arg1z->start = FcstCnt;
        l3var1z = l3arg1z->enums;
        while (l3var1z != NULL) {
            curVal = l3var1z->id;
            l3var1z = l3var1z->list;
            toFCST();
        }
    }
} /* dumpEnumNames */

void formPMD()
{
    TypesPtr l3typ1z;
    Word l3var2z;
    Bitset l3var3z;
    bool l3var4z;
    Kind l3var5z;
    IdentRecPtr &l2idr2z = programme::super.back()->l2idr2z;
    IdentRecPtr &curIdRec = programme::super.back()->curIdRec;

    for (int bb = 0; bb <= 1; ++bb) {
        l3var4z = bb;
        if (l3var4z) {
            optSflags.m = optSflags.m + mkbs(S3);
            curVal.i = 074001;
            P0715(2, 34); /*"P/DS"*/
            curVal = l2idr2z->id;
            toFCST();
            curVal.i = lineCnt;
            toFCST();
        } /* 13063 */
        for (int jj = 0; jj <= 127; ++jj)  {
            curIdRec = symHashTabBase[jj];
            /*13066*/
            while (curIdRec != NULL and
                   l2idr2z < curIdRec) /* with curIdRec@ do */ {
                if (curIdRec->typ != NULL) // check added; in the BESM-6 dereferencing NULL is OK
                    l3var2z.i = curIdRec->typ->size;
                if ((curIdRec->cl == VARID || curIdRec->cl == FORMALID) and
                    (curIdRec->value < 074000)) {
                    curVal = curIdRec->id;
                    if (l3var4z)
                        toFCST();
                    l3typ1z = curIdRec->typ;
                    l3var5z = l3typ1z->k;
                    l3var3z = mkbs();
                    if (l3var5z == kindPtr) {
                        l3typ1z = l3typ1z->base;
                        l3var5z = l3typ1z->k;
                        l3var3z = mkbs(0);
                    }
                    if (l3typ1z == RealType)
                        curVal.i = 0;
                    else if (typeCheck(l3typ1z, IntegerType))
                        curVal.i = 0100000;
                    else if (typeCheck(l3typ1z, CharType))
                        curVal.i = 0200000;
                    else if (l3var5z == kindArray)
                        curVal.i = 0400000;
                    else if (l3var5z == kindScalar) {
                        dumpEnumNames(l3typ1z);
                        curVal.i = 01000000 * l3typ1z->start + 0300000;
                    } else if (l3var5z == kindFile)
                        curVal.i = 0600000;
                    else {
                        curVal.i = 0500000;
                    }
                    curVal.i = curVal.i + curIdRec->value;
                    l3var2z.m = l3var2z.m << 33;
                    curVal.m = curVal.m * mkbsr(15,47) + l3var2z.m + l3var3z;
                    if (l3var4z)
                        toFCST();
                } /* 13164 */
                curIdRec = curIdRec->next;
            } /* 13166 */
        } /*13167+*/
        curVal.m = mkbs();
        if (l3var4z)
            toFCST();
    }
} /* formPMD */

void parseDecls(int64_t l3arg1z)
{
    int64_t l3int1z;
    Word frame;
    bool l3var3z;

    IdentRecPtr &l2idr2z = programme::super.back()->l2idr2z;
    int64_t &l2int11z = programme::super.back()->l2int11z;

    switch (l3arg1z) {
    case 0: {
        int93z = 0;
        inSymbol();
        if (SY != IDENT)
            errAndSkip(3, skipToSet + mkbs(IDENT));
    } break;
    case 2: {
        padToLeft();
        l3var3z = l2idr2z->flags.has(22);
        l3arg1z = l2idr2z->pos;
        frame.i = moduleOffset - 040000;
        if (l3arg1z != 0)
            symTab[l3arg1z] = mkbs(24, 29) + frame.m * halfWord;
        l2idr2z->pos = moduleOffset;
        l3arg1z = F3307(l2idr2z);
        if (l3var3z) {
            if (41 >= entryPtCnt) {
                curVal = l2idr2z->id;
                entryPtTable[entryPtCnt] = makeNameWithStars(true);
                entryPtTable[entryPtCnt+1] = mkbs(1) + frame.m - mkbs(0, 3);
                entryPtCnt = entryPtCnt + 2;
            } else
                error(87); /* errTooManyEntryProcs */
        };
        if (l2idr2z->typ == NULL) {
            frame.i = 3;
        } else {
            frame.i = 4;
        };
        if (l3var3z)
            form2Insn((KVTM+I14) + l3arg1z + (frame.i - 3) * 01000,
                      getHelperProc(94 /*"P/NN"*/) - 010000000);
        if (1 < l3arg1z) {
            frame.i = getValueOrAllocSymtab(-(frame.i+l3arg1z));
        }
        if (optSflags.m.has(S5) and
            curProcNesting == 1)
            l3int1z = 59;  /* P/LV */
        else
            l3int1z = curProcNesting;
        l3int1z = getHelperProc(l3int1z) - (-04000000);
        if (l3arg1z == 1) {
            form1Insn((KATX+SP) + frame.i);
        } else if (l3arg1z != 0) {
            form2Insn(KATX+SP, (KUTM+SP) + frame.i);
        }
        formAndAlign(l3int1z);
        savedObjIdx = objBufIdx;
        if (curProcNesting != 1)
            form1Insn(0);
        if (l3var3z)
            form1Insn(KVTM+I8+074001);
        if (l2int11z != 0) {
            form1Insn(InsnTemp[XTA]);
            formAndAlign(KVJM+I13 + l2int11z);
            curVal.i = l2int11z;
            P0715(2, 49 /* "P/RDC" */);
        }
        if (curProcNesting == 1) {
            if (heapCallsCnt != 0 and
                heapSize == 0)
                error(65 /*errCannotHaveK0AndNew*/);
            l3var3z = (heapSize == 0) or
                ((heapCallsCnt == 0) and (heapSize == 100));
            if (heapSize == 100)
                heapSize = 4;
            if (not l3var3z) {
                form2Insn(KVTM+I14+getValueOrAllocSymtab(heapSize*02000),
                          getHelperProc(26 /*"P/GD"*/));
                padToLeft();
            }
        }
        if (doPMD)
            formPMD();
    } break;
    } /* case */
} /* parseDecls */


struct Statement {
    static std::vector<Statement*> super;
    Statement();
    ~Statement() { super.pop_back(); }

    ExprPtr boundary;
    NumLabel * l3var2z;
    StrLabel * l3var3z;
    Word l3var4z;
    bool l3bool5z;
    IdClass l3var6z;
    Word l3var7z, l3var8z;
    int64_t startLine;
    int64_t l3var10z;
    int64_t l3var11z;
    IdentRecPtr l3idr12z;
};

std::vector<Statement*> Statement::super;

bool isCharArray(TypesPtr arg)
{
    return (arg->k == kindArray) and (arg->base == CharType);
} /* isCharArray */

void expression();

void parseLval()
{
    ExprPtr l4exp1z, l4exp2z;
    TypesPtr l4typ3z;
    Kind l4var4z;

    if (hashTravPtr->cl == FIELDID) {
        curExpr = expr62z;
        goto L13530;
    } else {
        curExpr = new Expr;
        /* with curExpr@ do */ {
            curExpr->typ = hashTravPtr->typ;
            curExpr->op = GETVAR;
            curExpr->id1 = hashTravPtr;
        }
      L13462:
        inSymbol();
        l4typ3z = curExpr->typ;
        l4var4z = l4typ3z->k;
        if (SY == ARROW) {
            l4exp1z = new Expr;
            /* with l4exp1z@ do */ {
                l4exp1z->expr1 = curExpr;
                if (l4var4z == kindPtr) {
                    l4exp1z->typ = l4typ3z->base;
                    l4exp1z->op = DEREF;
                } else if (l4var4z == kindFile) {
                    l4exp1z->typ = l4typ3z->base;
                    l4exp1z->op = FILEPTR;
                } else {
                    stmtName = "  ^   ";
                    error(errWrongVarTypeBefore);
                    l4exp1z->typ = l4typ3z;
                }
            }
            curExpr = l4exp1z;
        } else if (SY == PERIOD) {
            if (l4var4z == kindRecord) {
                int93z = 3;
                typ121z = l4typ3z;
                inSymbol();
                if (hashTravPtr == NULL) {
                    error(20); /* errDigitGreaterThan7 ??? */
                } else {
                  L13530:
                    l4exp1z = new Expr;
                    /* with l4exp1z@ do */ {
                        l4exp1z->typ = hashTravPtr->typ;
                        l4exp1z->op = GETFIELD;
                        l4exp1z->expr1 = curExpr;
                        l4exp1z->id2 = hashTravPtr;
                    }
                    curExpr = l4exp1z;
                }
            } else {
                stmtName = "  .   ";
                error(errWrongVarTypeBefore);
            }
        } else if (SY == LBRACK) {
            stmtName = "  [   ";
            do {
                l4exp1z = curExpr;
                expression();
                l4typ3z = l4exp1z->typ;
                if (l4typ3z->k != kindArray) {
                    error(errWrongVarTypeBefore);
                } else {
                    if (not typeCheck(l4typ3z->range, curExpr->typ))
                        error(66 /*errOtherIndexTypeNeeded */);
                    l4exp2z = new Expr;
                    /* with l4exp2z@ do */ {
                        l4exp2z->typ = l4typ3z->base;
                        l4exp2z->expr1 = l4exp1z;
                        l4exp2z->expr2 = curExpr;
                        l4exp2z->op = GETELT;
                    }
                    l4exp1z = l4exp2z;
                }
                curExpr = l4exp1z;
                stmtName = "  ,   ";
            } while (SY == COMMA);
            if (SY != RBRACK)
                error(67 /*errNeedBracketAfterIndices*/);
        } else return;
    }
    goto L13462;
} /* parseLval */

void castToReal(ExprPtr & value)
{
    ExprPtr cast;
    cast = new Expr;
    /* with cast@ do */ {
        cast->typ = RealType;
        cast->op = TOREAL;
        cast->expr1 = value;
        value = cast;
    }
}

bool areTypesCompatible(ExprPtr & l4arg1z)
{
    if (arg1Type == RealType) {
        if (typeCheck(IntegerType, arg2Type)) {
            castToReal(l4arg1z);
            return true;
        }
    } else if (arg2Type == RealType and
               typeCheck(IntegerType, arg1Type)) {
        castToReal(curExpr);
        return true;
    }
    return false;
}

void parseCallArgs(IdentRecPtr l4arg1z)
{
    bool l4var1z;
    ExprPtr l4exp2z, l4exp3z, l4exp4z;
    IdentRecPtr l4idr5z = NULL;
    Operator l4op6z;
    IdClass l4idc7z;

    /* with l4arg1z@ do */ {
        if (l4arg1z->typ != NULL)
            set146z = set146z - l4arg1z->flags;
        l4var1z = (l4arg1z->list == NULL) and not (l4arg1z->flags.has(24));
    }
    l4exp3z = new Expr;
    l4exp4z = l4exp3z;
    bool48z = true;
    /* with l4exp3z@ do */ {
        l4exp3z->typ = l4arg1z->typ;
        l4exp3z->op = ALNUM;
        l4exp3z->id2 = l4arg1z;
        l4exp3z->id1 = NULL;
    }
    if (SY == LPAREN) {
        if (l4var1z) {
            l4idr5z = l4arg1z->argList;
            if (l4idr5z == NULL) {
                error(errTooManyArguments);
                throw 8888;
            }
        }
        do {
            if (l4var1z and l4arg1z == l4idr5z) {
                error(errTooManyArguments);
                throw 8888;
            }
            bool47z = true;
            expression();
            l4op6z = curExpr->op;
            /* (a) */
            if (l4var1z) {
                l4idc7z = l4idr5z->cl;
                if (l4op6z == PCALL) {
                    if (l4idc7z != ROUTINEID or
                        l4idr5z->typ != NULL) {
                      L13736:
                        error(39); /*errIncompatibleArgumentKinds*/
                        goto exit_a;
                    }
                } else { /* 13741 */
                    if (l4op6z == FCALL) {
                        if (l4idc7z == ROUTINEID) {
                            if (l4idr5z->typ == NULL)
                                goto L13736;
                        } else if (curExpr->id2->argList == NULL and
                                   l4idc7z == VARID) {
                            curExpr->op = ALNUM;
                            curExpr->expr1 = NULL;
                        } else
                            goto L13736;
                    } else if (lvalOpSet.has(l4op6z)) {
                        if (l4idc7z != VARID and
                            l4idc7z != FORMALID)
                            goto L13736;
                    } else {
                        if (l4idc7z != VARID)
                            goto L13736;
                    }
                }
                arg1Type = curExpr->typ;
                if (arg1Type != NULL) {
                    if (not typeCheck(arg1Type, l4idr5z->typ))
                        error(40); /*errIncompatibleArgumentTypes*/
                }
            } exit_a:; /* 14006 */
            l4exp2z = new Expr;
            /* with l4exp2z@ do */ {
                l4exp2z->typ = NULL;
                l4exp2z->expr1 = NULL;
                l4exp2z->expr2 = curExpr;
            }
            l4exp4z->expr1 = l4exp2z;
            l4exp4z = l4exp2z;
            if (l4var1z)
                l4idr5z = l4idr5z->list;
        } while (SY == COMMA);
        if (SY != RPAREN or
            (l4var1z and l4idr5z != l4arg1z))
            error(errNoCommaOrParenOrTooFewArgs);
        else
            inSymbol();
    } else { /* 14035 */
        if (l4var1z and l4arg1z->argList != NULL)
            error(42); /*errNoArgList*/
    }
    curExpr = l4exp3z;
    /* 14042 */
} /* parseCallArgs */

struct Factor {
    Word l4var1z;
    bool l4var2z;
    Word l4var3z, l4var4z;
    ExprPtr l4exp5z, l4exp6z, l4var7z, l4var8z;
    IdentRecPtr routine;
    Operator l4op10z;
    TypesPtr l4typ11z;
    bool l4var12z;

    void stdCall() {
        const int64_t chkREAL = 0,  chkINT    = 1,  chkCHAR = 2,    chkSCALAR = 3,
            chkPTR  = 4,  chkFILE   = 5,  chkSET  = 6,    chkOTHER  = 7;

        Operator l5op1z;
        TypesPtr l5var2z;
        Kind argKind;
        Bitset asBitset;
        int64_t stProcNo, checkMode;

        curVal.i = routine->low;
        stProcNo = curVal.i;
        if (SY != LPAREN) {
            requiredSymErr(LPAREN);
            throw 8888;
        }
        expression();
        if (stProcNo >= fnEOF and
            fnEOLN >= stProcNo and
            not (GETELT<=curExpr->op && curExpr->op <=FILEPTR)) {
            error(27); /* errExpressionWhereVariableExpected */
            return;
        }
        arg1Type = curExpr->typ;
        if (arg1Type->k == kindRange)
            arg1Type = arg1Type->base;
        argKind = arg1Type->k;
        if (arg1Type == RealType)
            checkMode = chkREAL;
        else if (arg1Type == IntegerType)
            checkMode = chkINT;
        else if (arg1Type == CharType)
            checkMode = chkCHAR;
        else if (argKind == kindScalar)
            checkMode = chkSCALAR;
        else if (argKind == kindPtr)
            checkMode = chkPTR;
        else if (argKind == kindFile)
            checkMode = chkFILE;
        else if (argKind == kindSet)
            checkMode = chkSET;
        else {
            checkMode = chkOTHER;
        }
        asBitset = mkbs(stProcNo);

        if (not ((checkMode == chkREAL and
                 asBitset <= mkbsr(fnSQRT,fnTRUNC)+mkbs(fnREF, fnSQR, fnROUND, fn29))
                 or ((checkMode == chkINT and
                     asBitset <= mkbsr(fnSQRT,fnABS)+mkbs(fnODD, fnCHR, fnREF)+mkbs(fnSQR, fnPTR)))
                 or (mkbs(chkCHAR, chkSCALAR, chkPTR).has(checkMode) and
                     (asBitset <= mkbs(fnORD, fnSUCC, fnPRED, fnREF)))
                 or ((checkMode == chkFILE) and
                     (asBitset <= mkbs(fnEOF, fnREF, fnEOLN)))
                 or ((checkMode == chkSET) and
                     (asBitset <= mkbs(fnREF, fnCARD, fnMINEL)))
                 or ((checkMode == chkOTHER) and
                     (stProcNo == fnREF))))
            error(errNeedOtherTypesOfOperands);

        if (not (asBitset <= mkbs(fnABS, fnSUCC, fnPRED, fnSQR))) {
            arg1Type = routine->typ;
        } else if ((checkMode == chkINT) and (asBitset <= mkbs(fnABS, fnSQR))) {
            if (stProcNo == fnABS)
                stProcNo = fnABSI;
            else
              stProcNo = fnSQRI;
        }
        l4exp6z = new Expr;
        l4exp6z->op = STANDPROC;
        l4exp6z->expr1 = curExpr;
        l4exp6z->num2 = stProcNo;
        if (stProcNo == fn24) {
            if (SY != COMMA) {
                requiredSymErr(COMMA);
                throw 8888;
            }
            expression();
            l5var2z = curExpr->typ;
            l5op1z = badop27;
            if ((l5var2z != RealType) and
                not typeCheck(l5var2z, IntegerType))
                error(errNeedOtherTypesOfOperands);
            if (l5var2z == RealType)
                l5op1z = badop30;
            else if (checkMode == chkREAL)
                l5op1z = badop31;
            l4exp6z->expr2 = curExpr;
            l4exp6z->op = l5op1z;
        }
      curExpr = l4exp6z;
      curExpr->typ = arg1Type;
      checkSymAndRead(RPAREN);
    }; /* stdCall */

    Factor() {
        l4var2z = bool47z;
        bool47z = false;
        if (SY < MULOP) {
            switch (SY) {
            case IDENT: {
                if (hashTravPtr == NULL) {
                    error(errNotDefined);
                    curExpr = uVarPtr;
                } else switch (hashTravPtr->cl) {
                    case TYPEID: {
                        error(23); /* errTypeIdInsteadOfVar */
                          curExpr = uVarPtr;
                    } break;
                    case ENUMID: {
                        curExpr = new Expr;
                        /* with curExpr@ do */ {
                            curExpr->typ = hashTravPtr->typ;
                            curExpr->op = GETENUM;
                            curExpr->num1 = hashTravPtr->value;
                            curExpr->num2 = 0;
                        }
                        inSymbol();
                    } break;
                    case ROUTINEID: {
                        routine = hashTravPtr;
                        inSymbol();
                        if (routine->offset == 0) {
                            if (routine->typ != NULL and
                                SY == LPAREN) {
                                stdCall();
                                return;
                            }
                            error(44); /* errIncorrectUsageOfStandProcOrFunc */
                        } else if (routine->typ == NULL) {
                            if (l4var2z) {
                                l4op10z = PCALL;
                            } else {
                                error(68); /* errUsingProcedureInExpression */
                            }
                        } else /* 14330 */ {
                            if (SY == LPAREN) {
                                parseCallArgs(routine);
                                return;
                            }
                            if (l4var2z) {
                                l4op10z = FCALL;
                            } else {
                                parseCallArgs(routine);
                                return;
                            }
                        } /* 14342 */
                        curExpr = new Expr;
                        if (SY != RPAREN && SY != COMMA) {
                            error(errNoCommaOrParenOrTooFewArgs);
                            throw 8888;
                        }
                        /* with curExpr@ do */ {
                            curExpr->typ = routine->typ;
                            curExpr->op = l4op10z;
                            curExpr->expr1 = NULL;
                            curExpr->id2 = routine;
                        }
                    } break;
                    case VARID: case FORMALID: case FIELDID:
                        parseLval();
                        break;
                    } /* case */
            } break;
            case LPAREN: {
                expression();
                checkSymAndRead(RPAREN);
            } break;
            case INTCONST: case REALCONST: case CHARCONST: case LTSY: case GTSY: {
                curExpr = new Expr;
                parseLiteral(curExpr->typ, curExpr->d1, false);
                curExpr->num2 = suffix;
                curExpr->op = GETENUM;
                inSymbol();
            } break;
            case NOTSY: {
                inSymbol();
                Factor();
                if (curExpr->typ != BooleanType)
                    error(1); /* errNoCommaNorSemicolon */
                l4exp6z = curExpr;
                curExpr = new Expr;
                /* with curExpr@ do */ {
                curExpr->typ = BooleanType;
                curExpr->op = NOTOP;
                curExpr->expr1 = l4exp6z;
                }
            } break;
            case LBRACK: {
                curExpr = new Expr;
                inSymbol();
                l4var8z = curExpr;
                l4var1z.m = mkbs();
                if (SY != RBRACK) {
                    l4var12z = true;
                    bool102z = false;
                    do {
                        l4exp6z = curExpr;
                        expression();
                        if (l4var12z) {
                            l4typ11z = curExpr->typ;
                            if (not (mkbs(l4typ11z->k) <= mkbs(kindScalar, kindRange)))
                                error(23); /* errTypeIdInsteadOfVar */
                        } else {
                            if (not typeCheck(l4typ11z, curExpr->typ))
                                error(24); /*errIncompatibleExprsInSetCtor*/
                        }
                        l4var12z = false;
                        l4exp5z = curExpr;
                        if (SY == COLON) {
                            expression();
                            if (not typeCheck(l4typ11z, curExpr->typ))
                                error(24); /*errIncompatibleExprsInSetCtor*/
                            if (l4exp5z->op == GETENUM and
                                curExpr->op == GETENUM) {
                                l4var4z.i = l4exp5z->num1;
                                l4var3z.i = curExpr->num1;
                                l4var4z.m = l4var4z.m - intZero;
                                l4var3z.m = l4var3z.m - intZero;
                                l4var1z.m = l4var1z.m + mkbsr(l4var4z.i, l4var3z.i);
                                curExpr = l4exp6z;
                                goto L14567;
                            }
                            l4var7z = new Expr;
                            /* with l4var7z@ do */ {
                                l4var7z->typ = setType;
                                l4var7z->op = MKRANGE;
                                l4var7z->expr1 = l4exp5z;
                                l4var7z->expr2 = curExpr;
                            }
                            l4exp5z = l4var7z;
                        } else {/* 14535 */
                            if (l4exp5z->op == GETENUM) {
                                l4var4z.i = l4exp5z->num1;
                                l4var4z.m = l4var4z.m - intZero;
                                l4var1z.m = l4var1z.m + mkbs(l4var4z.i);
                                curExpr = l4exp6z;
                                goto L14567;
                            }
                            l4var7z = new Expr;
                            /* with l4var7z@ do */ {
                                l4var7z->typ = setType;
                                l4var7z->op = STANDPROC;
                                l4var7z->expr1 = l4exp5z;
                                l4var7z->num2 = 109;
                                l4exp5z = l4var7z;
                            }
                        } /* 14560 */
                        curExpr = new Expr;
                        /* with curExpr@ do */ {
                            curExpr->typ = setType;
                            curExpr->op = SETOR;
                            curExpr->expr1 = l4exp6z;
                            curExpr->expr2 = l4exp5z;
                        }
                      L14567:;
                    } while (SY == COMMA);
                } /* 14571 */
                checkSymAndRead(RBRACK);
                /* with l4var8z@ do */ {
                    l4var8z->op = GETENUM;
                    l4var8z->typ = setType;
                    l4var8z->d1 = l4var1z;
                }
            } break;
            default:;
            } /* case */
        } else {
            error(errBadSymbol);
            throw 8888;
        }
    }
};  /* Factor */

void term() {
    Operator l4var1z;
    ExprPtr l4var2z, l4var3z;
    bool l4var4z;

    Factor();
    while (SY == MULOP) {
        l4var1z = charClass;
        inSymbol();
        l4var2z = curExpr;
        Factor();
        arg1Type = curExpr->typ;
        arg2Type = l4var2z->typ;
        l4var4z = typeCheck(arg1Type, arg2Type);
        if (not l4var4z and
            RDIVOP < l4var1z) {
          L14650:
            error(errNeedOtherTypesOfOperands);
        } else {
            switch (l4var1z) {
            case MUL:
            case RDIVOP: {
                if (l4var4z) {
                    if (arg1Type == RealType) {
                        /* empty */
                    } else {
                        if (typ120z == IntegerType) {
                            if (l4var1z == MUL) {
                                arg1Type = IntegerType;
                            } else {
                                arg1Type = RealType;
                            }
                            l4var1z = iMulOpMap[l4var1z];
                        } else {
                            if (arg1Type->k == kindSet) {
                                l4var1z = setOpMap[l4var1z];
                            } else
                                goto L14650;
                        }
                    }
                } else {
                    if (areTypesCompatible(l4var2z)) {
                        arg1Type = RealType;
                    } else
                        goto L14650;
                }
            } break;
            case AMPERS: {
                if (arg1Type != BooleanType)
                    goto L14650;
            } break;
            case IDIVOP: {
                if (typ120z != IntegerType)
                    goto L14650;
                arg1Type = IntegerType;
            } break;
            case IMODOP: {
                if (typ120z == IntegerType) {
                    arg1Type = IntegerType;
                } else {
                    if (arg1Type->k == kindSet)
                        l4var1z = SETXOR;
                    else
                        goto L14650;
                }
            } break;
            default: break;
            }
            l4var3z = new Expr;
            /* with l4var3z@ do */ {
                l4var3z->op = l4var1z;
                l4var3z->expr1 = l4var2z;
                l4var3z->expr2 = curExpr;
                curExpr = l4var3z;
                l4var3z->typ = arg1Type;
            }
        }
    }
} /* term */

void simpleExpression() {
    ExprPtr l4var1z, l4var2z;
    Operator l4var3z;
    Kind argKind;
    bool l4bool5z;

    l4bool5z = false;
    if (charClass == PLUSOP || charClass == MINUSOP) {
        if (charClass == MINUSOP)
            l4bool5z = true;
        inSymbol();
    }
    term();
/* minus */
    if (l4bool5z) {
        arg1Type = curExpr->typ;
        l4var2z = new Expr;
        /* with l4var2z@ do */ {
            l4var2z->typ = arg1Type;
            l4var2z->expr1 = curExpr;
            if (arg1Type == RealType) {
                l4var2z->op = RNEGOP;
            } else if (typeCheck(arg1Type, IntegerType)) {
                l4var2z->op = INEGOP;
                l4var2z->typ = IntegerType;
            } else {
                error(69); /* errUnaryMinusNeedRealOrInteger */
                goto exit_minus;
            }
            curExpr = l4var2z;
        }
    } exit_minus:; /* 15010 */
    while (SY == ADDOP) {
        l4var3z = charClass;
        inSymbol();
        l4var2z = curExpr;
        term();
        arg1Type = curExpr->typ;
        arg2Type = l4var2z->typ;
        l4bool5z = typeCheck(arg1Type, arg2Type);
        argKind = arg2Type->k;
        if (kindSet < argKind) {
          L15031:
            error(errNeedOtherTypesOfOperands);
        } else {
            l4var1z = new Expr;
            /* with l4var1z@ do */ {
                if (l4var3z == OROP) {
                    if (not l4bool5z or
                       arg1Type != BooleanType)
                        goto L15031;
                    l4var1z->typ = BooleanType;
                    l4var1z->op = l4var3z;
                } else /* 15046 */ {
                    if (l4bool5z) {
                        if (arg1Type == RealType) {
                            l4var1z->op = l4var3z;
                            l4var1z->typ = RealType;
                        } else if (typ120z == IntegerType) {
                            l4var1z->op = iAddOpMap[l4var3z];
                            l4var1z->typ = IntegerType;
                        } else if (argKind == kindSet) {
                            l4var1z->op = setOpMap[l4var3z];
                            l4var1z->typ = arg1Type;
                        } else {
                            goto L15031;
                        }
                    } else if (areTypesCompatible(l4var2z)) {
                        l4var1z->typ = RealType;
                        l4var1z->op = l4var3z;
                    } else
                        goto L15031;
                } /* 15077 */
                l4var1z->expr1 = l4var2z;
                l4var1z->expr2 = curExpr;
                curExpr = l4var1z;
            }
        }
    }
} /* simpleExpression */

void expression() {
    Operator oper;
    ExprPtr l4var2z, l4var3z;

    if (bool102z)
        inSymbol();
    else
        bool102z = true;
    simpleExpression();
    if (SY == RELOP) {
        oper = charClass;
        inSymbol();
        l4var3z = curExpr;
        simpleExpression();
        arg1Type = curExpr->typ;
        arg2Type = l4var3z->typ;
        if (typeCheck(arg1Type, arg2Type)) {
            if ((oper == INOP) or
                (arg1Type->k == kindFile) or
                (arg1Type->size != 1 and
                 oper >= LTOP and
                 not isCharArray(arg1Type)))
                error(errNeedOtherTypesOfOperands);
        } else /* 15150 */ {
            if (not areTypesCompatible(l4var3z) and
                ((arg1Type->k != kindSet) or
                 not (arg2Type->k == kindScalar || arg2Type->k == kindRange) or
                 (oper != INOP))) {
                error(errNeedOtherTypesOfOperands);
            }
        } /* 15167 */
        l4var2z = new Expr;
        if ((arg2Type->k == kindSet) and
            (oper == LTOP || oper == GTOP))
            error(errNeedOtherTypesOfOperands);
        /* with l4var2z@ do */ {
            l4var2z->typ = BooleanType;
            if (oper == GTOP || oper == LEOP) {
                l4var2z->expr1 = curExpr;
                l4var2z->expr2 = l4var3z;
                if (oper == GTOP)
                    l4var2z->op = LTOP;
                else
                    l4var2z->op = GEOP;
            } else {
                l4var2z->expr1 = l4var3z;
                l4var2z->expr2 = curExpr;
                l4var2z->op = oper;
            }
            curExpr = l4var2z;
        }
    }
} /* expression */

void forStatement() {
    TypesPtr l4typ1z;
    ExprPtr l4exp2z, l4var3z, l4var4z;
    int64_t l4int5z, l4int6z, l4int7z, l4int8z;
    bool l4var9z;

    inSymbol();
    disableNorm();
    curExpr = NULL;
    if (SY == IDENT) {
        if (hashTravPtr != NULL and hashTravPtr->cl == VARID) {
            parseLval();
            if (curExpr->op != GETVAR)
                error(errNoSimpleVarForLoop);
        } else
            error(errNoSimpleVarForLoop);
    } else {
        errAndSkip(errNoIdent, skipToSet + mkbs(BECOMES, DOSY, TOSY, DOWNTOSY));
    } /* 15251 */
    if (curExpr == NULL)
        curExpr = uVarPtr;
    l4exp2z = curExpr;
    l4typ1z = l4exp2z->typ;
    if (not (l4typ1z->k == kindScalar || l4typ1z->k == kindRange))
        error(25); /* errExprNotOfADiscreteType */
    if (typeCheck(IntegerType, l4typ1z))
      l4int5z = KATX+PLUS1;
    else
      l4int5z = KATX+E1;
    if (SY == BECOMES) {
        expression();
        l4var9z = true;
    } else {
        l4var9z = false;
    }
    l4var3z = curExpr;
    l4int6z = InsnTemp[ADD];
    if (not typeCheck(l4typ1z, l4var3z->typ))
        error(31); /* errIncompatibleTypesOfLoopIndexAndExpr */

    if (SY == TOSY) {
        // Do nothing
    } else if (SY == DOWNTOSY)
        l4int6z = InsnTemp[SUB];
    else {
        error(70); /* errNeitherToNorDownto */
    }
    expression();
    if (not typeCheck(l4typ1z, curExpr->typ))
        error(31); /* errIncompatibleTypesOfLoopIndexAndExpr */
    (void) formOperator(gen0);
    l4var4z = curExpr;
    if (l4var9z) {
        curExpr = l4var3z;
        (void) formOperator(LOAD);
    } else {
        form1Insn(InsnTemp[XTA] + l4int5z);
    }
    l4int7z = 0;
    disableNorm();
    formJump(l4int7z);
    padToLeft();
    l4int8z = moduleOffset;
    checkSymAndRead(DOSY);
    Statement();
    disableNorm();
    curExpr = l4exp2z;
    (void) formOperator(LOAD);
    form1Insn(l4int6z + l4int5z);
    P0715(0, l4int7z);
    (void) formOperator(STORE);
    curExpr = l4var4z;
    if (l4int6z == InsnTemp[SUB])
        curVal.i = l4int6z;
    else
        curVal.i = InsnTemp[RSUB];
    /*15401*/
    (void) formOperator(gen3);
    form1Insn(InsnTemp[UZA] + l4int8z);
} /* forStatement */

void withStatement() {
    ExprPtr l4exp1z;
    Bitset l4var2z, l4var3z;
    int64_t l4var4z;
    int64_t & localSize = programme::super.back()->localSize;

    l4exp1z = expr63z;
    l4var4z = localSize;
    l4var2z = set147z;
    l4var3z = mkbs();
    do {
        inSymbol();
        if (hashTravPtr != NULL and
            hashTravPtr->cl >= VARID) {
            parseLval();
            if (curExpr->typ->k == kindRecord) {
                (void) formOperator(SETREG);
                l4var3z = (l4var3z + mkbs(curVal.i)) * set148z;
            } else {
                error(71); /* errWithOperatorNotOfARecord */
            }
        } else {
            error(72); /* errWithOperatorNotOfAVariable */
        }
    } while (SY == COMMA);
    checkSymAndRead(DOSY);
    Statement();
    expr63z = l4exp1z;
    localSize = l4var4z;
    set147z = l4var2z;
    set145z = set145z + l4var3z;
} /* withStatement */

void reportStmtType(int64_t l4arg1z)
{
    int64_t &startLine = Statement::super.back()->startLine;

    printf(" STATEMENT %s IN %ld LINE\n", stmtName.c_str(), startLine);
} /* reportStmtType */

bool structBranch(bool isGoto) {
    StrLabel * curLab;
    int ii;
    bool ret = true;
    StrLabel * &strLabList = programme::super.back()->strLabList;

    if (SY == IDENT or not isGoto) {
        curLab = strLabList;
        ii = 1;
        while (curLab != NULL) {
            /* with curLab@ do */ {
                if (curLab->ident.m == mkbs()) {
                    ii = ii - 1;
                } else {
                    if (curLab->ident == curIdent) {
                        if (ii == 1) {
                            if (isGoto) {
                                form1Insn(InsnTemp[UJ] + curLab->offset);
                            } else {
                                formJump(curLab->exitTarget);
                            }
                        } else {
                            form1Insn(getValueOrAllocSymtab(ii) +
                                      (KVTM+I13));
                            if (isGoto) {
                                form1Insn(KVTM+I10 + curLab->offset);
                            } else {
                                jumpType = KVTM+I10;
                                formJump(curLab->exitTarget);
                                jumpType = InsnTemp[UJ];
                            };
                            form1Insn(getHelperProc(60) +
                                      06437777713700000); /* P/ZAM */
                        };
                        return ret;
                    }
                }
                curLab = curLab->next;
            }
        }
        if (not isGoto and SY != IDENT) {
            if (ii != 1) {
                form1Insn(getValueOrAllocSymtab(ii) + (KVTM+I13));
                form1Insn(getHelperProc(60)); /* P/ZAM */
            }
            formJump(int53z);
        } else {
            error(errNotDefined);
        }
    } else
        return false;
    return ret;
} /* structBranch */

void caseStatement() {
    typedef struct CaseClause {
        CaseClause * next;
        Word value;
        int64_t offset;
    } * CaseClausePtr;

    CaseClausePtr allClauses, curClause, clause, old = NULL;
    bool isIntCase;
    bool otherSeen;
    int64_t otherOffset = -1;
    bool itemsEnded, goodMode;
    TypesPtr firstType, itemtype, exprtype;
    Word itemvalue;
    int64_t itemSpan;
    Word expected;
    int64_t startLine, l4var17z, endOfStmt;
    Word minValue, maxValue;

    startLine = lineCnt;
    expression();
    exprtype = curExpr->typ;
    otherSeen = false;
    if (exprtype == AlfaType or
        (exprtype->k == kindScalar || exprtype->k == kindRange))
        (void) formOperator(LOAD);
    else
        error(25); /* errExprNotOfADiscreteType */
    disableNorm();
    l4var17z = 0;
    endOfStmt = 0;
    allClauses = NULL;
    formJump(l4var17z);
    checkSymAndRead(OFSY);
    firstType = NULL;
    goodMode = true;
    do {
        if (not (SY == SEMICOLON || SY == ENDSY)) {
            padToLeft();
            arithMode = 1;
            if (SY == OTHERSY) {
                if (otherSeen)
                    error(73); /* errCaseLabelsIdentical */
                inSymbol();
                otherSeen = true;
                otherOffset = moduleOffset;
            } else do {
                    parseLiteral(itemtype, itemvalue, true);
                    if (itemtype != NULL) {
                        if (firstType == NULL) {
                            firstType = itemtype;
                        } else {
                            if (not typeCheck(itemtype, firstType))
                                error(errConstOfOtherTypeNeeded);
                        } /* 15700 */
                        clause = new CaseClause;
                        clause->value = itemvalue;
                        clause->offset = moduleOffset;
                        curClause = allClauses;
                        while (curClause != NULL) {
                            if (itemvalue == curClause->value) {
                                error(73); /* errCaseLabelsIdentical */
                                break;
                            } else if (itemvalue.i < curClause->value.i) {
                                break;
                            } else {
                                old = curClause;
                                curClause = curClause->next;
                            }
                        } /* 15723 */
                        if (curClause == allClauses) {
                            clause->next = allClauses;
                            allClauses = clause;
                        } else {
                            clause->next = curClause;
                            old->next = clause;
                        }
                        inSymbol();
                    } /* 15735 */
                  itemsEnded = (SY != COMMA);
                  if (not itemsEnded)
                      inSymbol();
                } while (!itemsEnded);
            checkSymAndRead(COLON);
            Statement();
            goodMode = goodMode and (arithMode == 1);
            formJump(endOfStmt);
        } /* 15762 */
        itemsEnded = (SY == ENDSY);
        if (not itemsEnded)
            inSymbol();
    } while (!itemsEnded);
    if (SY != ENDSY) {
        requiredSymErr(ENDSY);
        stmtName = "CASE  ";
        reportStmtType(startLine);
    } else
        inSymbol();
    if (not typeCheck(firstType, exprtype)) {
        error(88); /* errDifferentTypesOfLabelsAndExpr */;
        return;
    }
    padToLeft();
    isIntCase = typeCheck(exprtype, IntegerType);
    if (allClauses != NULL) {
        expected = allClauses->value;
        minValue = expected;
        curClause = allClauses;
        while (curClause != NULL) {
            if (expected == curClause->value and
                exprtype->k == kindScalar) {
                maxValue = expected;
                if (isIntCase) {
                    expected.i = expected.i + 1; // Integers with exponent
                } else {
                    curVal = expected;
                    curVal.ii = curVal.ii + 1; // Integers without exponent
                    expected = curVal;
                }
                curClause = curClause->next;
            } else {
                itemSpan = 34000;
                P0715(0, l4var17z);
                if (firstType->k == kindRange) {
                    itemSpan = std::max(std::abs(firstType->left),
                                        std::abs(firstType->right));
                } else {
                    if (firstType->k == kindScalar)
                        itemSpan = firstType->numen;
                }
                itemsEnded = (itemSpan < 32000);
                if (itemsEnded) {
                    form1Insn(KATI+14);
                } else {
                    form1Insn(KATX+SP+1);
                }
                minValue.i = (minValue.i - minValue.i); /* WTF? */
                while (allClauses != NULL) {
                    if (itemsEnded) {
                        curVal.i = (minValue.i - allClauses->value.i);
                        curVal.m = (curVal.m + intZero);
                        form1Insn(getValueOrAllocSymtab(curVal.i) +
                                  (KUTM+I14));
                        form1Insn(KVZM+I14 + allClauses->offset);
                        minValue = allClauses->value;
                    } else {
                        form1Insn(KXTA+SP+1);
                        curVal = allClauses->value;
                        form2Insn(KAEX + I8 + getFCSToffset(),
                                  InsnTemp[UZA] + allClauses->offset);
                    }
                    allClauses = allClauses->next;
                }
                if (otherSeen)
                    form1Insn(InsnTemp[UJ] + otherOffset);
                goto L16211;
            } /* if 16141 */
        } /* while 16142 */
        if (not otherSeen) {
            otherOffset = moduleOffset;
            formJump(endOfStmt);
        }
        P0715(0, l4var17z);
        curVal = minValue;
        P0715(-(InsnTemp[U1A]+otherOffset), maxValue.i);
        curVal = minValue;
        curVal.m = (curVal.m + intZero);
        form1Insn(KATI+14);
        curVal.i = ((moduleOffset + (1)) - curVal.i);
        if (curVal.i < 040000) {
            curVal.i = (curVal.i - 040000);
            curVal.i = allocSymtab(mkbs(24, 29) +
                        (curVal.m * O77777));
        }
        form1Insn(KUJ+I14 + curVal.i);
        while (allClauses != NULL) {
            padToLeft();
            form1Insn(InsnTemp[UJ] + allClauses->offset);
            allClauses = allClauses->next;
        }
      L16211:
        P0715(0, endOfStmt);
        if (not goodMode)
            disableNorm();
        /* 16217 */
    }
} /* caseStatement */

void assignStatement(bool doLHS) {
    ExprPtr lhsExpr, assnExpr;
    int64_t indCnt;
    TypesPtr srcType, targType;
    bool &l3bool5z = Statement::super.back()->l3bool5z;

    if (doLHS)
        parseLval();
    else {
        curExpr = new Expr;
        /* with curExpr@ do */ {
            curExpr->typ = hashTravPtr->typ;
            curExpr->op = GETVAR;
            curExpr->id1 = hashTravPtr;
        }
        inSymbol();
    }
    checkSymAndRead(BECOMES);
    bool102z = false;
    targType = curExpr->typ;
    if (targType->k == kindRecord and
        SY == LBRACK) {
        (void) formOperator(gen5);
        indCnt = 0;
        inSymbol();
        l3bool5z = false;

        do {
            if (SY == COMMA) {
                indCnt = indCnt + 1;
                inSymbol();
            } else if (SY == RBRACK) {
                inSymbol();
                break;
            } else {
                bool102z = false;
                expression();
                curVal.i = indCnt;
                (void) formOperator(gen6);
            } /* 16270 */
        } while(true);
        curExpr = NULL;
    } else if (SY == SEMICOLON and allowCompat) {
        (void) formOperator(STORE);
        bool102z = true;
        curExpr = NULL;
    } else /* 16303 */ {
        lhsExpr = curExpr;
        expression();
        srcType = curExpr->typ;
        if (typeCheck(targType, srcType)) {
            if (srcType->k == kindFile)
                error(75); /*errCannotAssignFiles*/
            else {
                if (rangeMismatch and targType->k == kindRange) {
                    assnExpr = new Expr;
                    /* with assnExpr@ do */ {
                        assnExpr->typ = srcType;
                        assnExpr->op = BOUNDS;
                        assnExpr->expr1 = curExpr;
                        assnExpr->typ2 = targType;
                    }
                    curExpr = assnExpr;
                }
              L16332:
                assnExpr = new Expr;
                /* with assnExpr@ do */ {
                    assnExpr->typ = targType;
                    assnExpr->op = ASSIGNOP;
                    assnExpr->expr1 = lhsExpr;
                    assnExpr->expr2 = curExpr;
                }
                curExpr = assnExpr;
            }
        } else if (targType == RealType and
                   typeCheck(IntegerType, srcType)) {
            castToReal(curExpr);
            goto L16332;
        } else {
            error(33); /*errIllegalTypesForAssignment*/
        }
    }
    /* 16356 */
} /* assignStatement */

void compoundStatement()
{
    do {
        Statement();
        if (SY == SEMICOLON) {
            inSymbol();
        } else
            break;
    } while(true);
} /* compoundStatement */

void ifWhileStatement(Symbol delim)
{
    int64_t &l3var10z = Statement::super.back()->l3var10z;

    disableNorm();
    expression();
    jumpTarget = 0;
    if (curExpr->typ != BooleanType)
        error(errBooleanNeeded);
    else {
        (void) formOperator(gen15);
    }
    l3var10z = jumpTarget;
    checkSymAndRead(delim);
    Statement();
} /* ifWhileStatement */

struct ParseData {
    struct DATAREC {
        Bitset b;
        unsigned operator[](int i) {
            return (b.val >> (12*(3-i))) & 4095;
        }
        void assn(int i, int64_t val) {
            val &= 4095;
            val ^= (*this)[i];
            b.val = (b.val ^ (val << (12*(3-i)))) & 0xFFFFFFFFFFFFL;
        }
    };

    int64_t dsize, setcount;
    Word l4var3z, l4var4z, l4var5z;
    ExprPtr boundary;
    Word l4var7z, l4var8z, l4var9z;
    std::vector<DATAREC> F;

    int64_t allocDataRef(int64_t l6arg1z) {
        if (l6arg1z >= 2048) {
            curVal.i = l6arg1z;
            return allocSymtab((curVal.m + mkbs(24)) * halfWord);
        } else {
            return l6arg1z;
        }
    } /* allocDataRef */

    void P16432(int64_t l5arg1z) {
        DATAREC l5var1z;

        l5var1z.assn(0, allocDataRef(l4var4z.i));
        if (FcstCnt == l4var3z.i) {
            curVal = l4var8z;
            curVal.i = addCurValToFCST();
        } else {
            curVal = l4var3z;
        }
        l5var1z.assn(1, allocSymtab(mkbs(12,23) + curVal.m * halfWord));
        l5var1z.assn(2, allocDataRef(l5arg1z));
        if (l4var9z.i == 0) {
            curVal.m = l4var7z.m >> 24;
        } else {
            curVal.i = allocSymtab(l4var7z.m + l4var9z.m * halfWord);
        }
        l5var1z.assn(3, curVal.i);
        l4var9z.i = l5arg1z * l4var4z.i + l4var9z.i;
        F.push_back(l5var1z);
        setcount = setcount + 1;
        l4var4z.i = 0;
        l4var3z.i = FcstCnt;
    } /* P16432 */

    ParseData() {
        dsize = FcstCnt;
        inSymbol();
        setcount = 0;
/*(loop)*/
        do { /* 16530 */
            inSymbol();
            setup(boundary);
            if (SY != IDENT) {
                if (SY == ENDSY)
                    break;
                error(errNoIdent);
                curExpr = uVarPtr;
            } else /* 16543 */ {
                if (hashTravPtr == NULL) {
                  L16545:
                    error(errNotDefined);
                    curExpr = uVarPtr;
                    inSymbol();
                } else {
                    if (hashTravPtr->cl == VARID) {
                        parseLval();
                    } else goto L16545;
                }
            } /* 16557 */
            putLeft = true;
            objBufIdx = 1;
            (void) formOperator(gen5);
            if (objBufIdx != 1)
                error(errVarTooComplex);
            l4var7z.m = (leftInsn * mkbsr(12,23));
            l4var3z.i = FcstCnt;
            l4var4z.i = 0;
            l4var9z.i = 0;
            do { /* 16574 */
                expression();
                (void) formOperator(LITINSN);
                l4var8z = curVal;
                if (SY == COLON) {
                    inSymbol();
                    l4var5z = curToken;
                    if (SY != INTCONST) {
                        error(62); /* errIntegerNeeded */
                        l4var5z.i = 0;
                    } else
                        inSymbol();
                } else
                    l4var5z.i = 1;
                if (l4var5z.i != 1) {
                    if (l4var4z.i != 0)
                        P16432(1);
                    l4var4z.i = 1;
                    P16432(l4var5z.i);
                } else {
                    l4var4z.i = l4var4z.i + 1;
                    if (SY == COMMA) {
                        curVal = l4var8z;
                        toFCST();
                    } else {
                        if (l4var4z.i != 1) {
                            curVal = l4var8z;
                            toFCST();
                        }
                        P16432(1);
                    }
                } /* 16641 */
            } while (SY == COMMA);
            rollup(boundary);
        } while (SY == SEMICOLON); /* 16645 */
        if (SY != ENDSY)
            error(errBadSymbol);
        /*
          reset(F);
          while not eof(F) do {
          write(FCST, F->b);
          get(F);
          };
        */
        for (size_t s = 0; s < F.size(); ++s) FCST.push_back(F[s].b);
        int92z = FcstCnt - dsize;
        FcstCnt = dsize;
        int93z = setcount;
    }
}; /* parseData */

struct standProc {

    TypesPtr l4typ1z, l4typ2z, l4typ3z;
    ExprPtr l4var4z, l4var5z;
    ExprPtr l4exp6z;
    ExprPtr l4exp7z, l4exp8z, l4exp9z;
    bool l4bool10z,
    l4bool11z, l4bool12z;
    Word l4var13z, l4var14z, l4var15z;
    int64_t procNo;
    int64_t helperNo;
    OpGen l4var18z;

    void verifyType(TypesPtr l5arg1z) {
        if (hashTravPtr != NULL and
            hashTravPtr->cl >= VARID) {
            parseLval();
            if (l5arg1z != NULL and
                not typeCheck(l5arg1z, curExpr->typ))
                error(errNeedOtherTypesOfOperands);
        } else {
            error(errNotDefined);
            curExpr = uVarPtr;
        }
    } /* verifyType */

    void startReadOrWrite(bool l5arg1z) {
        expression();
        l4typ3z = curExpr->typ;
        l4exp7z = curExpr;
        if (not l5arg1z) {
            if (not lvalOpSet.has(curExpr->op))
                error(27); /* errExpressionWhereVariableExpected */
        }
        if (l4exp9z == NULL) {
            if (l4typ3z && l4typ3z->k == kindFile) {
                l4exp9z = curExpr;
            } else {
                l4exp9z = new Expr;
                l4exp9z->typ = textType;
                l4exp9z->op = GETVAR;
                if (l5arg1z) {
                    if (outputFile != NULL)
                        l4exp9z->id1 = outputFile;
                    else
                        error(77); /* errNoOutput */
                } else {
                    if (inputFile != NULL)
                        l4exp9z->id1 = inputFile;
                    else {
                        error(37); /* errInputMissingInProgramHeader */
                    }
                }
            }
            arg2Type = l4exp9z->typ;
            l4var13z.b = typeCheck(arg2Type->base, CharType);
            l4bool12z = true;
            l4exp8z = new Expr;
            l4exp8z->typ = arg2Type->base;
            l4exp8z->op = FILEPTR;
            l4exp8z->expr1 = l4exp9z;
            l4exp6z = new Expr;
            l4exp6z->typ = l4exp8z->typ;
            l4exp6z->op = ASSIGNOP;
            if (l5arg1z)
                l4exp6z->expr1 = l4exp8z;
            else
                l4exp6z->expr2 = l4exp8z;
        } /* 17002 */
    } /* startReadOrWrite */

    void parseWidthSpecifier(ExprPtr &l5arg1z) {
        expression();
        if (not typeCheck(IntegerType, curExpr->typ)) {
            error(14); /* errExprIsNotInteger */
            curExpr = uVarPtr;
        }
        l5arg1z = curExpr;
    } /* parseWidthSpecifier */

    void callHelperWithArg() {
        if (set145z.has(12) or l4bool12z) {
            curExpr = l4exp9z;
            (void) formOperator(gen8);
        }
        l4bool12z= false;
        formAndAlign(getHelperProc(helperNo));
        disableNorm();
    }; /* callHelperWithArg */

    void P17037() {
        set145z = set145z - mkbs(12);
        if (helperNo != 49 and             /* P/RDC */
            not typeCheck(l4exp8z->typ, l4exp7z->typ))
            error(34); /* errTypeIsNotAFileElementType */
        else {
            if (helperNo == 29) {       /* P/PF */
                l4exp6z->expr2 = l4exp7z;
            } else {
                if (helperNo == 49)
                    helperNo = 30;         /* P/GF */
                l4exp6z->expr1 = l4exp7z;
            }
            curExpr = l4exp6z;
            (void) formOperator(gen7);
            callHelperWithArg();
        }
    } /* P17037 */

    void checkElementForReadWrite() {
        TypesPtr l5typ1z;

        set145z = set145z - mkbs(12);
        if (l4typ3z->k == kindRange)
            l4typ3z = l4typ3z->base;
        curVarKind = l4typ3z->k;
        helperNo = 36;                   /* P/WI */
        if (l4typ3z == IntegerType)
            l4var15z.i = 10;
        else if (l4typ3z == RealType) {
            helperNo = 37;               /* P/WR */
            l4var15z.i = 14;
        } else if (l4typ3z == CharType) {
            helperNo = 38;               /* P/WC */
            l4var15z.i = 1;
        } else if (curVarKind == kindScalar) {
            helperNo = 41;               /* P/WX */
            dumpEnumNames(l4typ3z);
            l4var15z.i = 8;
        } else if (isCharArray(l4typ3z)) {
            l5typ1z = l4typ3z->range;
            l4var15z.i = l5typ1z->right - l5typ1z->left + 1;
            if (not l4typ3z->pck)
                helperNo = 81;            /* P/WA */
            else if (6 >= l4var15z.i)
                helperNo = 39;            /* P/A6 */
            else
                helperNo = 40;           /* P/A7 */
        } else if (l4typ3z->size == 1) {
            helperNo = 42;               /* P/WO */
            l4var15z.i = 17;
        } else {
            error(34); /* errTypeIsNotAFileElementType */
        }
    } /* checkElementForReadWrite */

    void writeProc() {
        l4exp9z = NULL;
        l4var13z.b = true;
        do {
            startReadOrWrite(true);
            if (l4exp7z != l4exp9z) {
                if (not l4var13z.b) {
                  helperNo = 29;         /* P/PF */
                  P17037();
                } else {
                    checkElementForReadWrite();
                    l4var5z = NULL;
                    l4var4z = NULL;
                    if (SY == COLON)
                        parseWidthSpecifier(l4var4z);
                    if (SY == COLON) {
                        parseWidthSpecifier(l4var5z);
                    if (helperNo != 37)    /* P/WR */
                        error(35); /* errSecondSpecifierForWriteOnlyForReal */
                    } else {
                        if (curToken == litOct) {
                            helperNo = 42; /* P/WO */
                            l4var15z.i = 17;
                            if (l4typ3z->size != 1)
                                error(34); /* errTypeIsNotAFileElementType */
                            inSymbol();
                        }
                    }
                    l4bool11z = false;
                    if (l4var4z == NULL and
                        mkbsr(38,40).has(helperNo)) {  /* WC,A6,A7 */
                        helperNo = helperNo + 5;       /* CW,6A,7A */
                        l4bool11z = true;
                    } else {
                        if (l4var4z == NULL) {
                            curVal = l4var15z;
                            (void) formOperator(gen9);
                        } else {
                            curExpr = l4var4z;
                            (void) formOperator(LOAD);
                        }
                    }
                    if (helperNo == 37) {       /* P/WR */
                        if (l4var5z == NULL) {
                            curVal.i = 4;
                            form1Insn(KXTS+I8 + getFCSToffset());
                        } else {
                            curExpr = l4var5z;
                            (void) formOperator(gen10);
                        }
                    }
                    curExpr = l4exp7z;
                    if (l4bool11z) {
                        if (helperNo == 45)     /* P/7A */
                            l4var18z = gen11;
                        else
                            l4var18z = LOAD;
                    } else {
                        if (helperNo == 40 or       /* P/A7 */
                            helperNo == 81)     /* P/WA */
                            l4var18z = gen12;
                        else
                            l4var18z = gen10;
                    }
                    (void) formOperator(l4var18z);
                    if (mkbs(39,40,44,45).has(helperNo) or /* A6,A7,6A,7A */
                        helperNo == 81)
                        form1Insn(KVTM+I10 + l4var15z.i);
                    else {
                        if (helperNo == 41) /* P/WX */
                            form1Insn(KVTM+I11 + l4typ3z->start);
                    }
                    callHelperWithArg();
                }
            }
        } while (SY == COMMA);
        if (procNo == 11) {
            helperNo = 46;                 /* P/WL */
            callHelperWithArg();
        }
        set145z = set145z + mkbs(12);
        if (l4var14z.i == moduleOffset)
            error(36); /*errTooFewArguments */
    } /* writeProc */

    void readProc() {
        l4exp9z = NULL;
        l4var13z.b = true;
        l4var14z.i = moduleOffset;
        do {
            startReadOrWrite(false);
            if (l4exp7z != l4exp9z) {
                if (not l4var13z.b) {
                    helperNo = 30;         /* P/GF */
                  L17346:
                    P17037();
                } else {
                    checkElementForReadWrite();
                    if (helperNo == 38) {       /* P/WC */
                        helperNo = 49;             /* P/RDC */
                        goto L17346;
                    }
                    if (helperNo == 39 or           /* A6,A7 */
                        helperNo == 40) {
                        helperNo = 51;             /* P/RA7 */
                      L17362:
                        curExpr = l4exp7z;
                        (void) formOperator(gen5);
                        form1Insn(KVTM+I10 + l4var15z.i);
                        callHelperWithArg();
                    } else {
                        if (helperNo == 81) {   /* P/WA */
                            helperNo = 90;         /* P/RA */
                            goto L17362;
                        }
                        helperNo = helperNo + 11;
                        callHelperWithArg();
                        curExpr = l4exp7z;
                        (void) formOperator(STORE);
                    }
                }
            }
        } while (SY == COMMA);
        set145z = set145z + mkbs(12);
        if (procNo == 13) {
            helperNo = 53;                 /* P/RL */
            callHelperWithArg();
        }
        if (l4var14z.i == moduleOffset)
            error(36); /* errTooFewArguments */
    } /* readProc */

    void checkArrayArg() {
        verifyType(NULL);
        l4exp9z = curExpr;
        l4typ1z = curExpr->typ;
        if (l4typ1z->pck or
            l4typ1z->k != kindArray)
            error(errNeedOtherTypesOfOperands);
        checkSymAndRead(COMMA);
        bool102z = false;
        expression();
        l4exp8z = curExpr;
        if (l4typ1z->k == kindArray && not typeCheck(l4typ1z->range, l4exp8z->typ))
            error(errNeedOtherTypesOfOperands);
    } /* checkArrayArg */

    void doPackUnpack() {
        TypesPtr t;

        l4exp7z = new Expr;
        l4exp7z->typ = l4typ1z->base;
        l4exp7z->op = GETELT;
        l4exp7z->expr1 = l4exp9z;
        l4exp7z->expr2 = l4exp8z;
        t = l4exp6z->typ;
        if ((t->k != kindArray) or
            not t->pck or
            not typeCheck(t->base, l4typ1z->base) or
            not typeCheck(l4typ1z->range, t->range))
            error(errNeedOtherTypesOfOperands);
        curExpr = new Expr;
        curExpr->val.c = char(procNo + 50);
        curExpr->expr1 = l4exp7z;
        curExpr->expr2 = l4exp6z;
        (void) formOperator(gen16);
    } /* doPackUnpack */

    standProc() { /* standProc */
        IdentRecPtr &l3idr12z = Statement::super.back()->l3idr12z;
        TypesPtr &l2typ13z = programme::super.back()->l2typ13z;
        bool &l3bool5z = Statement::super.back()->l3bool5z;
        int64_t ii;

        curVal.i = l3idr12z->low;
        procNo = curVal.i;
        l4bool10z = (SY == LPAREN);
        l4var14z.i = moduleOffset;
        if (not l4bool10z and
            (mkbsr(0,5)+mkbsr(8,10)+mkbs(12)+mkbsr(16,28)).has(procNo))
            error(45); /* errNoOpenParenForStandProc */
        if ((mkbsr(0,9)-mkbs(6,7)).has(procNo)) {
            inSymbol();
            if (hashTravPtr == NULL || hashTravPtr->cl < VARID) {
                error(46); /* errNoVarForStandProc */
                if (hashTravPtr == NULL)
                    throw 8888;
            }
            parseLval();
            arg1Type = curExpr->typ;
            curVarKind = arg1Type->k;
        }
        if (procNo <= 6)
            jumpTarget = getHelperProc(29 + procNo); /* P/PF */
        switch (procNo) {
        case 0: case 1: case 2: case 3: { /* put, get, rewrite, reset */
            if (curVarKind != kindFile)
                error(47); /* errNoVarOfFileType */
            if (procNo == 3 and SY == COMMA) {
                (void) formOperator(gen8);
                expression();
                if (not typeCheck(IntegerType, curExpr->typ))
                    error(14); /* errExprIsNotInteger */
                (void) formOperator(LOAD);
                formAndAlign(getHelperProc(97)); /*"P/RE"*/
            } else {
                (void) formOperator(FILEACCESS);
            }
        } break;
        case 4: case 5: { /* new, dispose */
            if (curVarKind != kindPtr)
                error(13); /* errVarIsNotPointer */
            heapCallsCnt = heapCallsCnt + 1;
              l4exp9z = curExpr;
              if (procNo == 5)
                  (void) formOperator(gen5);
              l2typ13z = arg1Type->base;
              ii = l2typ13z->size;
              if (charClass == EQOP) {
                  expression();
                  if (not typeCheck(IntegerType, curExpr->typ))
                      error(14); /* errExprIsNotInteger */
                  (void) formOperator(LOAD);
                  form1Insn(KATI+14);
              } else {
                  if (arg1Type->base->k == kindRecord) {
                      l4typ1z = l2typ13z->base;
                      /*loop*/ while (SY == COMMA and l4typ1z != NULL) {
                          inSymbol();
                          parseLiteral(l4typ2z, curVal, true);
                          if (l4typ2z == NULL)
                              break; // exit loop
                          else {
                              inSymbol();
                              /*loop2*/ while (l4typ1z != NULL) {
                                  l4typ2z = l4typ1z;
                                  while (l4typ2z != NULL) {
                                      if (l4typ2z->sel == curVal) {
                                          ii = l4typ1z->size;
                                          goto exit_loop2;
                                      }
                                      l4typ2z = l4typ2z->r6;
                                  };
                                  l4typ1z = l4typ1z->next;
                              } exit_loop2:;
                          }
                      }
                  }
                  form1Insn(KVTM+I14+getValueOrAllocSymtab(ii));
              }
              formAndAlign(jumpTarget);
              if (procNo == 4) {
                  curExpr = l4exp9z;
                  (void) formOperator(STORE);
              }
        } break;
        case 6: { /* halt */
            formAndAlign(jumpTarget);
            return;
        } break;
        case 7: { /* stop */
            form1Insn(KE74);
            return;
        } break;
        case 8: case 9: { /* setup, rollup */
            if (curVarKind != kindPtr)
                error(13); /* errVarIsNotPointer */
            if (procNo == 8) {
                form1Insn(KXTA+HEAPPTR);
                (void) formOperator(STORE);
            } else {
                (void) formOperator(LOAD);
                form1Insn(KATX+HEAPPTR);
            }
        } break;
        case 10: { /* write */
            writeProc();
        } break;
        case 11:
        L17753:
        { /* writeln */
            if (SY == LPAREN) {
                writeProc();
            } else {
                formAndAlign(getHelperProc(54)); /*"P/WOLN"*/
                return;
            }
        } break;
        case 12: { /* read */
            readProc();
        } break;
        case 13: { /* readln */
            if (SY == LPAREN) {
                readProc();
            } else {
                formAndAlign(getHelperProc(55)); /*"P/RILN"*/
                return;
            }
        } break;
        case 14: { /* exit */
            l4bool10z = (SY == LPAREN);
            if (l4bool10z)
                inSymbol();
            if (SY == IDENT) {
                if (not structBranch(false))
                    error(1); /* errCommaOrSemicolonNeeded */
                inSymbol();
            } else {
                formJump(int53z);
            }
            if (not l4bool10z)
                return;
        } break;
        case 15: { /* debug */
            if (optSflags.m.has(DebugPrint)) {
                procNo = 11;
                goto L17753;
            }
            while (SY != RPAREN)
                inSymbol();
        } break;
        case 16: { /* besm */
            expression();
            (void) formOperator(LITINSN);
            formAndAlign(curVal.i);
        } break;
        case 17: { /* mapia */
            l4typ1z = IntegerType;
            l4typ2z = AlfaType;
          L20041:
            expression();
            if (not typeCheck(curExpr->typ, l4typ1z))
                error(errNeedOtherTypesOfOperands);
            checkSymAndRead(COMMA);
            (void) formOperator(LOAD);
            if (procNo == 17) {
                form3Insn(ASN64-33, KAUX+BITS15, KAEX+ASCII0);
            } else {
                form3Insn(KAPX+BITS15, ASN64+33, KAEX+ZERO);
            };
            verifyType(l4typ2z);
            (void) formOperator(STORE);
        } break;
        case 18: { /* mapai */
            l4typ1z = AlfaType;
            l4typ2z = IntegerType;
            goto L20041;
        } break;
        case 19: case 20: { /* pck, unpck */
            inSymbol();
            verifyType(CharType);
            checkSymAndRead(COMMA);
            (void) formOperator(gen8);
            verifyType(AlfaType);
            if (procNo == 20) {
                (void) formOperator(LOAD);
            }
            formAndAlign(getHelperProc(procNo - 6));
            if (procNo == 19)
                (void) formOperator(STORE);
        } break;
        case 21: { /* pack */
            inSymbol();
            checkArrayArg();
            checkSymAndRead(COMMA);
            verifyType(NULL);
            l4exp6z = curExpr;
            doPackUnpack();
        } break;
        case 22: { /* unpack */
            inSymbol();
            verifyType(NULL);
            l4exp6z = curExpr;
            checkSymAndRead(COMMA);
            checkArrayArg();
            doPackUnpack();
        } break;
        case 23: case 24: case 25: case 26: case 27: case 28: { /* MARS procedures */
            l3bool5z = 24 < procNo;
            do {
                expression();
                if (curExpr->typ->size != 1)
                    error(5); /*errSimpleTypeReq*/
                (void) formOperator(LOAD);
                if (l3bool5z) {
                    checkSymAndRead(COMMA);
                    verifyType(NULL);
                    l4exp9z = curExpr;
                    if (SY == COLON) {
                        expression();
                        (void) formOperator(gen10);
                    } else {
                        form2Insn(KVTM + I14 + l4exp9z->typ->size,
                                  KITS + 14);
                    }
                    curExpr = l4exp9z;
                    (void) formOperator(gen12);
                } else {
                    form2Insn(InsnTemp[XTS], InsnTemp[XTS]);
                }
                form1Insn(KWTC + I14 + 077751 + procNo);
                formAndAlign(getHelperProc(80)); /*"PAIB"*/
            } while (SY == COMMA);
        } break;
        }; /* 20257 */
        if ((mkbsr(0,3)+mkbsr(10,13)+mkbs(5,21,22)).has(procNo))
            arithMode = 1;
        checkSymAndRead(RPAREN);
    }
}; /* standProc */

Statement::Statement()
{ /* Statement */
    NumLabel * &l2var16z = programme::super.back()->l2var16z;
    StrLabel * &strLabList = programme::super.back()->strLabList;

    super.push_back(this);
    setup(boundary);
    bool110z = false;
    startLine = lineCnt;
    if (set147z == halfWord)
        ParseData();
    else {
        if (SY == INTCONST) {
            set146z = mkbs();
            l3var2z = numLabList;
            disableNorm();
            l3bool5z = true;
            padToLeft();
            while (l3var2z != l2var16z) { /* with l3var2z@ do */
                if (l3var2z->id != curToken) {
                    l3var2z = l3var2z->next;
                } else {
                    l3bool5z = false;
                    if (l3var2z->defined) {
                        curVal.i = l3var2z->line;
                        error(17); /* errLblAlreadyDefinedInLine */;
                    } else {
                        l3var2z->line = lineCnt;
                        l3var2z->defined = true;
                        padToLeft();
                        if (l3var2z->offset == 0) {
                            /* empty */
                        } else if (l3var2z->offset >= 074000) {
                            curVal.i = (moduleOffset - 040000);
                            symTab[l3var2z->offset] = mkbs(24,29) +
                                                         curVal.m * O77777;
                        } else {
                            P0715(0, l3var2z->offset);
                        } /* 20342 */
                        l3var2z->offset = moduleOffset;
                    }
                    l3var2z = l2var16z;
                }
            } /* while 20346 */
            if (l3bool5z)
                error(16); /* errLblNotDefinedInBlock */;
            inSymbol();
            checkSymAndRead(COLON);
        } /* 20355*/
        if (optSflags.m.has(DebugInteractive) and
            debugLine != lineCnt) {
            P0715(-1, 96 /* "P/DD" */);
            debugLine = lineCnt;
            arithMode = 1;
        }
        l3var4z.b = mkbs(BEGINSY,CASESY,REPEATSY,SELECTSY).has(SY);
        if (l3var4z.b)
            lineNesting = lineNesting + 1;
/*(ident)*/
        if (SY == IDENT) {
            if (hashTravPtr != NULL) try {
                l3var6z = hashTravPtr->cl;
                if (l3var6z >= VARID) {
                    assignStatement(true);
                } else {
                    if (l3var6z == ROUTINEID) {
                        if (hashTravPtr->typ == NULL) {
                            l3idr12z = hashTravPtr;
                            inSymbol();
                            if (l3idr12z->offset == 0) {
                                standProc();
                                goto exit_ident;
                            }
                            parseCallArgs(l3idr12z);
                        } else {
                            assignStatement(false);
                        }
                    } else {
                        error(32); /* errWrongStartOfOperator */
                        goto L8888;
                    }
                }
                (void) formOperator(gen7);
                } catch (int foo) {
                    if (foo != 8888) throw;
                }
            else {
                error(errNotDefined);
              L8888:
                skip(skipToSet + statEndSys);
            }
        } else if (SY == LPAREN) {
            set146z = mkbs();
            inSymbol();
            if (SY != IDENT) {
                error(errNoIdent);
                goto L8888;
            }
            l3var3z = new StrLabel;
            padToLeft();
            disableNorm();
            /* with l3var3z@ do */ {
                l3var3z->next = strLabList;
                l3var3z->ident = curIdent;
                l3var3z->offset = moduleOffset;
                l3var3z->exitTarget = 0;
            }
            strLabList = l3var3z;
            inSymbol();
            checkSymAndRead(RPAREN);
            Statement();
            P0715(0, l3var3z->exitTarget);
            strLabList = strLabList->next;
        } else if (SY == BEGINSY) {
          rep:
            inSymbol();
          skip:
            {
                compoundStatement();
                if (SY != ENDSY) {
                    stmtName = " BEGIN";
                    requiredSymErr(SEMICOLON);
                    reportStmtType(startLine);
                    skip(bigSkipSet);
                    if (statBegSys.has(SY))
                        goto skip;
                    if (SY != SEMICOLON)
                        goto exit_rep;
                    goto rep;
                }
            }
            inSymbol();
          exit_rep:;
        } else if (SY == GOTOSY) {
            inSymbol();
            if (SY != INTCONST) {
                if (structBranch(true)) {
                    inSymbol();
                    return;
                } else
                    goto L8888;
            }
            disableNorm();
            l3var2z = numLabList;
          loop:
            if (l3var2z != NULL) /* with l3var2z@ do */ {
                if (l3var2z->id != curToken) {
                    l3var2z = l3var2z->next;
                } else {
                    if (curFrameRegTemplate == l3var2z->frame) {
                        if (l3var2z->offset >= 040000) {
                            form1Insn(InsnTemp[UJ] + l3var2z->offset);
                        } else {
                            formJump(l3var2z->offset);
                        }
                    } else {
                        if (l3var2z->offset == 0) {
                            l3var2z->offset = symTabPos;
                            putToSymTab(mkbs());
                        }
                        form3Insn(l3var2z->frame + (KMTJ + 13), KVTM+I14 + l3var2z->offset,
                                  getHelperProc(18/*"P/RC"*/) + (-064100000));
                    }
                    goto exit_loop;
                }
                goto loop;
            } else
                error(18); /* errLblNotDefined */
          exit_loop:;
            inSymbol();
        } else if (SY == IFSY) {
            ifWhileStatement(THENSY);
            if (SY == ELSESY) {
                l3var11z = 0;
                formJump(l3var11z);
                P0715(0, l3var10z);
                l3var8z.i = arithMode;
                arithMode = 1;
                inSymbol();
                Statement();
                P0715(0, l3var11z);
                if (l3var8z.i != arithMode) {
                    arithMode = 2;
                    disableNorm();
                }
            } else {
                P0715(0, l3var10z);
            }
        } else if (SY == WHILESY) {
            set146z = mkbs();
            disableNorm();
            padToLeft();
            l3var8z.i = moduleOffset;
            ifWhileStatement(DOSY);
            disableNorm();
            form1Insn(InsnTemp[UJ] + l3var8z.i);
            P0715(0, l3var10z);
            arithMode = 1;
        } else if (SY == REPEATSY) {
            set146z = mkbs();
            disableNorm();
            padToLeft();
            l3var7z.i = moduleOffset;
            do {
                inSymbol();
                Statement();
            } while (SY == SEMICOLON);
            if (SY != UNTILSY) {
                requiredSymErr(UNTILSY);
                stmtName = "REPEAT";
                reportStmtType(startLine);
                goto L8888;
            }
            disableNorm();
            expression();
            if (curExpr->typ != BooleanType) {
                error(errBooleanNeeded);
            } else {
                jumpTarget = l3var7z.i;
                (void) formOperator(gen15);
            }
        } else if (SY == FORSY) {
            set146z = mkbs();
            forStatement();
        } else if (SY == SELECTSY) {
            disableNorm();
            l3bool5z = true;
            l3var11z = 0;
            /* 20707 */
            do {
                arithMode = 1;
                expression();
                if (curExpr->typ != BooleanType) {
                    error(errBooleanNeeded);
                } else {
                    jumpTarget = 0;
                    (void) formOperator(gen15);
                    l3var10z = jumpTarget;
                }
                checkSymAndRead(COLON);
                Statement();
                formJump(l3var11z);
                l3bool5z = l3bool5z and (arithMode == 1);
                P0715(0, l3var10z);
            } while (SY == SEMICOLON);
            checkSymAndRead(ENDSY);
            P0715(0, l3var11z);
            if (not l3bool5z) {
                arithMode = 2;
                disableNorm();
            }
        } else if (SY == CASESY) {
            caseStatement();
        } else if (SY == WITHSY) {
            withStatement();
        } exit_ident:; /* 20757 */
        if (l3var4z.b)
            lineNesting = lineNesting - 1;
        rollup(boundary);
        if (bool110z) {
            bool110z = false;
            goto L8888;
        }
    }
    /* 20766 */
} /* Statement */

void outputObjFile() {
    int64_t idx;

    padToLeft();
    objBufIdx = objBufIdx - 1;
    for (idx = 1; idx <= objBufIdx; ++idx)
        CHILD.push_back(objBuffer[idx]);
    lineStartOffset = moduleOffset;
    prevOpcode = 0;
} /* outputObjFile */

void defineRoutine() {
    int64_t l3var1z;
    Word l3var2z;
    // Word l3var3z; unused
    int64_t l3int4z;
    IdentRecPtr l3idr5z;
    // Word l3var6z; unused
    Word l3var7z;
    IdentRecPtr &l2idr2z = programme::super.back()->l2idr2z;
    int64_t &l2int11z = programme::super.back()->l2int11z;
    int64_t &l2int21z = programme::super.back()->l2int21z;
    int64_t &jj = programme::super.back()->jj;
    int64_t &localSize = programme::super.back()->localSize;
    bool &l2bool8z = programme::super.back()->l2bool8z;

    objBufIdx = 1;
    objBuffer[objBufIdx] = mkbs();
    curInsnTemplate = InsnTemp[XTA];
    bool48z = l2idr2z->flags.has(22);
    lineStartOffset = moduleOffset;
    l3var1z = moduleOffset;
    int92z = 2;
    expr63z = NULL;
    arithMode = 1;
    set146z = mkbs();
    set147z = mkbsr(curProcNesting+1, 6);
    set148z = set147z - mkbs(minel(set147z));
    l3var7z.m = set147z;
    int53z = 0;
    set145z = mkbsr(1,15) - set147z;
    if (curProcNesting != 1)
        parseDecls(2);
    l2int21z = localSize;
    if (SY != BEGINSY)
        requiredSymErr(BEGINSY);
    if (l2idr2z->flags.has(23)) {
        l3idr5z = l2idr2z->argList;
        l3int4z = 3;
        if (l2idr2z->typ != NULL)
            l3int4z = 4;
        while (l3idr5z != l2idr2z) {
            if (l3idr5z->cl == VARID) {
                l3var2z.i = l3idr5z->typ->size;
                if (l3var2z.i != 1) {
                    form3Insn(KVTM+I14 + l3int4z,
                              KVTM+I12 + l3var2z.i,
                              KVTM+I11 + l3idr5z->value);
                    formAndAlign(getHelperProc(73)); /* "P/LNGPAR" */
                }
            }
            l3int4z = l3int4z + 1;
            l3idr5z = l3idr5z->list;
        }
    } /* 21105 */
    if (checkBounds or not optSflags.m.has(NoStackCheck))
        P0715(-1, 95); /* P/SC */
    l3var2z.i = lineNesting;
    do {
        Statement();
        if (SY == SEMICOLON) {
            if (curProcNesting == 1)
                requiredSymErr(PERIOD);
            inSymbol();
            l2bool8z = blockBegSys.has(SY);
            if (not l2bool8z and not errors)
                error(84); /* errErrorInDeclarations */
        } else {
            if (SY == PERIOD and curProcNesting == 1)
                l2bool8z = true;
            else {
                errAndSkip(errBadSymbol, skipToSet);
                l2bool8z = blockBegSys.has(SY);
            }
        }
    } while (!l2bool8z);
    l2idr2z->flags = (set145z * mkbsr(0,15)) + (l2idr2z->flags - l3var7z.m);
    lineNesting = l3var2z.i - 1;
    if (int53z != 0)
        P0715(0, int53z);
    if (not bool48z and not doPMD and (l2int21z == 3) and
        (curProcNesting != 1) and (set145z * mkbsr(1,15) != mkbsr(1,15))) {
        objBuffer[1] = mkbsr(7,11)+mkbsr(21,23)+mkbs(28,31);
        /* with l2idr2z@ do */
        l2idr2z->flags = l2idr2z->flags + mkbs(25);
        if (objBufIdx == 2) {
            objBuffer[1] = mkbs(0,1)+mkbsr(3,5);
            putLeft = true;
        } else {
            l2idr2z->pos = l3var1z;
            if (set145z.has(13)) {
                curVal.i = minel(mkbsr(1,15) - set145z);
                l3var7z.m = curVal.m << 24;
                objBuffer[2] = objBuffer[2] + mkbs(0,1,3)+mkbs(6,9) + l3var7z.m;
            } else {
                curVal.i = 13;
            }
            form1Insn(InsnTemp[UJ] + indexreg[curVal.i]);
        }
    } else /* 21220 */ {
        if (l2int11z == 0)
            jj = 27;    /* P/E */
        else
            jj = 28;   /* P/EF */
        form1Insn(getHelperProc(jj) + (-I13-0100000));
        if (curProcNesting == 1) {
            parseDecls(2);
            if (optSflags.m.has(S3))
                formAndAlign(getHelperProc(78)); /* "P/PMDSET" */
            form1Insn(InsnTemp[UJ] + l3var1z);
            curVal.i = l2idr2z->pos - 040000;
            symTab[074002] = mkbs(24,29) + (curVal.m * halfWord);
        }
        curVal.i = l2int21z;
        if (curProcNesting != 1) {
            curVal.i = curVal.i - 2;
            l3var7z.m = curVal.m << 24;
            objBuffer[savedObjIdx] = objBuffer[savedObjIdx] +
                l3var7z.m + mkbsr(0,4)+mkbs(6,8);
        }
    } /* 21261 */
    outputObjFile();
} /* defineRoutine */

struct initScalars {
    Word l3var1z, noProgram, l3var3z, l3var4z;
    int64_t l3var5z, l3var6z;
    IdentRecPtr l3var7z;
    int64_t l3var8z, l3var9z;
    TypesPtr temptype;
    Word l3var11z;
    IdentRecPtr &curIdRec;

    void regSysType(int64_t l4arg1z, TypesPtr l4arg2z) {
        curIdRec = new IdentRec;
        // curIdRec@ := [l4arg1z, 0, , l4arg2z, TYPEID];
        curIdRec->id.ii = l4arg1z;
        curIdRec->offset = 0;
        curIdRec->typ = l4arg2z;
        curIdRec->cl = TYPEID;
        addToHashTab(curIdRec);
    } /* regSysType */

    void regSysEnum(int64_t l4arg1z, int64_t l4arg2z) {
        curIdRec = new IdentRec;
        // curIdRec@ := [l4arg1z, 48, , temptype, ENUMID, NULL, l4arg2z];
        curIdRec->id.ii = l4arg1z;
        curIdRec->offset = 48;
        curIdRec->typ = temptype;
        curIdRec->cl = ENUMID;
        curIdRec->list = NULL;
        curIdRec->value = l4arg2z;
        addToHashTab(curIdRec);
    } /* regSysEnum */

    void regSysProc(int64_t l4arg1z) {
        curIdRec = new IdentRec;
        // curIdRec@ := [l4arg1z, 0, , temptype, ROUTINEID, l3var9z];
        curIdRec->id.ii = l4arg1z;
        curIdRec->offset = 0;
        curIdRec->typ = temptype;
        curIdRec->cl = ROUTINEID;
        curIdRec->maybeUnused = l3var9z;
        l3var9z = l3var9z + 1;
        addToHashTab(curIdRec);
    } /* registerSysProc */

    initScalars();
};

initScalars::initScalars() :
    curIdRec(programme::super.back()->curIdRec)
{ /* initScalars */
    BooleanType = new Types(1, 1, kindScalar);
    BooleanType->numen = 2;
    BooleanType->start = 0;

    IntegerType = new Types(1, 48, kindScalar);
    IntegerType->numen = 100000;
    IntegerType->start = -1;
    IntegerType->enums = NULL;

    CharType = new Types(1, 8, kindScalar);
    CharType->numen = 256;
    CharType->start = -1;
    CharType->enums = NULL;

    RealType = new Types(1, 48, kindReal);

    setType = new Types(1, 48, kindSet, IntegerType);

    pointerType = new Types(1, 48, kindPtr);
    pointerType->sbase = pointerType;

    textType = new Types(30, 48, kindFile, CharType);
    textType->elsize = 8;

    AlfaType = new Types(1,48,kindArray,CharType);
    AlfaType->range = temptype;
    AlfaType->pck = true;
    AlfaType->perWord = 6;
    AlfaType->pcksize = 8;

    smallStringType[6] = AlfaType;
    regSysType(051566445474562L/*" INTEGER"*/, IntegerType);
    temptype = IntegerType;
    regSysEnum(toText("MAXINT"), 0xD0FFFFFFFFFFL);
    regSysType(042575754454156L/*" BOOLEAN"*/, BooleanType);
    regSysType(043504162L/*"    CHAR"*/, CharType);
    regSysType(062454154L/*"    REAL"*/, RealType);
    regSysType(041544641L/*"    ALFA"*/, AlfaType);
    regSysType(064457064L/*"    TEXT"*/, textType);
    temptype = BooleanType;
    regSysEnum(064626545L/*"    TRUE"*/, 1);
    hashTravPtr = curIdRec;
    regSysEnum(04641546345L/*"   FALSE"*/, 0);
    curIdRec->list = hashTravPtr;
    BooleanType->enums = curIdRec;
    maxSmallString = 0;
    for (strLen = 2; strLen <= 5; ++strLen)
        makeStringType(smallStringType[strLen]);
    maxSmallString = 6;

    curIdRec = new IdentRec;
    curIdRec->offset = 0;
    curIdRec->typ = IntegerType;
    curIdRec->cl = VARID;
    curIdRec->list = NULL;
    curIdRec->value = 7;

    uVarPtr = new Expr;
    uVarPtr->typ = IntegerType;
    uVarPtr->op = GETVAR;
    uVarPtr->id1 = curIdRec;

    uProcPtr = new IdentRec;
    uProcPtr->typ = NULL;
    uProcPtr->list = NULL;
    uProcPtr->argList = NULL;
    uProcPtr->preDefLink = NULL;
    uProcPtr->pos = 0;

    temptype = NULL;
    l3var9z = 0;
    for (l3var5z = 0; l3var5z <= 28; ++l3var5z)
        regSysProc(systemProcNames[l3var5z]);
    l3var9z = 0;
    temptype = RealType;

    regSysProc(063616264L/*"    SQRT"*/);
    regSysProc(0635156L/*"     SIN"*/);
    regSysProc(0435763L/*"     COS"*/);
    regSysProc(0416243644156L/*"  ARCTAN"*/);
    regSysProc(0416243635156L/*"  ARCSIN"*/);
    regSysProc(05456L/*"      LN"*/);
    regSysProc(0457060L/*"     EXP"*/);
    regSysProc(0414263L/*"     ABS"*/);
    temptype = IntegerType;
    regSysProc(06462655643L/*"   TRUNC"*/);
    temptype = BooleanType;
    regSysProc(0574444L/*"     ODD"*/);
    temptype = IntegerType;
    regSysProc(0576244L/*"     ORD"*/);
    temptype = CharType;
    regSysProc(0435062L/*"     CHR"*/);
    regSysProc(063654343L/*"    SUCC"*/);
    regSysProc(060624544L/*"    PRED"*/);
    temptype = BooleanType;
    regSysProc(0455746L/*"     EOF"*/);
    temptype = pointerType;
    regSysProc(0624546L/*"     REF"*/);
    temptype = BooleanType;
    regSysProc(045575456L/*"    EOLN"*/);
    temptype = IntegerType;
    regSysProc(0636162L/*"     SQR"*/);
    regSysProc(06257655644L/*"   ROUND"*/);
    regSysProc(043416244L/*"    CARD"*/);
    regSysProc(05551564554L/*"   MINEL"*/);
    temptype = pointerType;
    regSysProc(0606462L/*"     PTR"*/);
    l3var11z.i = 30;
    l3var11z.m = l3var11z.m * halfWord + mkbs(24,27,28,29);
    programObj = new IdentRec;
    curVal.ii = 0576564606564L/*"  OUTPUT"*/;
    l3var3z = curVal;
    curVal.ii = 05156606564L/*"   INPUT"*/;
    l3var4z = curVal;
    curVal.ii = 05657606257476241L/*"NOPROGRA"*/;
    noProgram = curVal;
    test1(PROGRAMSY, (skipToSet + mkbs(IDENT,LPAREN)));
    symTabPos = 074004;
    /* with programObj@ do */ {
        if (SY == IDENT) {
            curVal = curIdent;
            programObj->id = curVal;
            programObj->pos = 0;
            symTab[074000] = makeNameWithStars(true);
        } else {
            programObj->id.m = mkbs(3);
            error(errNoIdent);
            skip(skipToSet + mkbs(LPAREN));
        }
    }
    if (curIdent != noProgram) {
        entryPtTable[1] = symTab[074000];
        entryPtTable[3].val = 06062574762415500L; /*"PROGRAM "*/
        entryPtTable[2] = mkbs(1);
        entryPtTable[4] = mkbs(1);
        entryPtCnt = 5;
        Bitset foo;
        foo.val = (010L<<20|02474001L)<<24|003074002L; /*10 24 74001 00 30 74002*/
        CHILD.push_back(foo);
        moduleOffset = 040001;
    } else {
        entryPtCnt = 1;
        moduleOffset = 040000;
    }
    programObj->argList = NULL;
    programObj->flags = mkbs();
    objBufIdx = 1;
    temptype = IntegerType;
    defineRange(temptype, 1, 6);
    AlfaType->range = temptype;
    int93z = 0;
    inSymbol();
    outputObjFile();
    outputFile = NULL;
    inputFile = NULL;
    externFileList = NULL;
    lineStartOffset = moduleOffset;
    if (SY == SEMICOLON) {
        goto noFiles;
    }
    test1(LPAREN, skipToSet + mkbs(IDENT));
    l3var7z = new IdentRec;
    /* with l3var7z@ do */ {
        l3var7z->id = l3var3z;
        l3var7z->offset = 0;
        l3var7z->typ = textType;
        l3var7z->cl = VARID;
        l3var7z->list = NULL;
    }
    curVal.ii = 01257656460656412L/*"*OUTPUT*"*/;
    l3var7z->value = allocExtSymbol(l3var11z.m);
    addToHashTab(l3var7z);
    l3var5z = 1;
    while (SY == IDENT) {
        l3var8z = 0;
        curVal = curIdent;
        l3var1z.m = makeNameWithStars(false);
        if (curIdent == l3var4z) {
            inputFile = new IdentRec;
            /* with inputFile@ do */ {
                inputFile->id = curIdent;
                inputFile->offset = 0;
                inputFile->typ = textType;
                inputFile->cl = VARID;
                inputFile->list = NULL;
            }
            curVal = l3var1z;
            inputFile->value = allocExtSymbol(l3var11z.m);
            addToHashTab(inputFile);
            l3var8z = lineCnt;
        } else if (curIdent == l3var3z) {
            outputFile = l3var7z;
            l3var8z = lineCnt;
        } /* 21745 */
        curExternFile = externFileList;
        while (curExternFile != NULL) {
            if (curExternFile->id == curIdent) {
                curExternFile = NULL;
                error(errIdentAlreadyDefined);
            } else {
                curExternFile = curExternFile->next;
            }
        } /* 21760 */
        curExternFile = new ExtFileRec;
        /* with curExternFile@ do */ {
            curExternFile->id = curIdent;
            curExternFile->next = externFileList;
            curExternFile->line = l3var8z;
            curExternFile->offset = l3var1z;
        }
        if (l3var8z != 0) {
            if (curIdent == l3var3z) {
                fileForOutput = curExternFile;
            } else {
                fileForInput = curExternFile;
            }
        }
        externFileList = curExternFile;
        l3var6z = l3var5z;
        l3var5z = l3var5z + 1;
        inSymbol();
        if (charClass == MUL) {
            l3var6z = l3var6z + 64;
            inSymbol();
        }
        if (SY == INTCONST) {
          l3var6z = 01000 * curToken.i + l3var6z;
            if (suffix == noSuffix and
                1 < curToken.i and
                curToken.i < 127) {
                l3var6z = l3var6z + 128;
            } else if (suffix == suffixB and
                       01000000 < curToken.i and
                       curToken.i < 01743671743) {
                l3var6z = l3var6z + 256;
            } else {
                error(76); /* errWrongNumberForExternalFile */
            }
            inSymbol();
        } else {
            l3var6z = 512;
        }
        curExternFile->location = l3var6z;
        if (SY == COMMA)
            inSymbol();
    } /* 22042 */
    checkSymAndRead(RPAREN);
  noFiles:
    checkSymAndRead(SEMICOLON);
    l3var6z = 40;
    do
        programme(l3var6z, programObj);
    while (SY != PERIOD);
    if (CH != 'D') {
        int92z = 0;
        int93z = 0;
    } else {
        set147z = halfWord;
        dataCheck = false;
        Statement();
    }
    readToPos80();
    curVal.i = l3var6z;
    symTab[074003] = (helperNames[25] + mkbs(24,27,28,29)) +
        (curVal.m * halfWord);
} /* initScalars */

void makeExtFile() {
    ExprPtr &l2var10z = programme::super.back()->l2var10z;
    IdentRecPtr &workidr = programme::super.back()->workidr;
    l2var10z = new Expr;
    /* with l2var10z@ do */ {
        l2var10z->typ = reinterpret_cast<TypesPtr>(curExternFile);
        l2var10z->id2 = workidr;
        l2var10z->expr1 = curExpr;
    }
    curExpr = l2var10z;
} /* makeExtFile */

void parseParameters() {
    IdentRecPtr l3var1z, l3var2z, l3var3z;
    IdClass parClass;
    int64_t l3var5z, l3var6z;
    Symbol l3sym7z;
    bool noComma;
    TypesPtr expType;
    IdentRecPtr &curIdRec = programme::super.back()->curIdRec;
    int64_t &l2int18z = programme::super.back()->l2int18z;

    int92z = 0;
    l3var5z = 0;
    int93z = 0;
    inSymbol();
    l3var2z = NULL;
    if (not mkbs(IDENT,VARSY,FUNCSY,PROCSY).has(SY))
        errAndSkip(errBadSymbol, (skipToSet + mkbs(IDENT,RPAREN)));
    int92z = 1;
    while (mkbs(IDENT,VARSY,FUNCSY,PROCSY).has(SY)) {
        l3sym7z = SY;
        if (SY == IDENT)
            parClass = VARID;
        else if (SY == VARSY)
            parClass = FORMALID;
        else {
            parClass = ROUTINEID;
        }
        l3var3z = NULL;
        if (SY == PROCSY)
            expType = NULL;
        else
            expType = IntegerType;
        l3var6z = 0;
        if (SY != IDENT) {
            int93z = 0;
            inSymbol();
        }
        do {
            if (SY == IDENT) {
                if (isDefined)
                    error(errIdentAlreadyDefined);
                l3var6z = l3var6z + 1;
                l3var1z = new IdentRec;
                /* with l3var1z@ do */ {
                    l3var1z->id = curIdent;
                    l3var1z->offset = curFrameRegTemplate;
                    l3var1z->cl = parClass;
                    l3var1z->next = symHashTabBase[bucket];
                    l3var1z->typ = NULL;
                    l3var1z->list = curIdRec;
                    l3var1z->value = l2int18z;
                }
                symHashTabBase[bucket] = l3var1z;
                l2int18z = l2int18z + 1;
                if (l3var2z == NULL)
                    curIdRec->argList = l3var1z;
                else
                    l3var2z->list = l3var1z;
                l3var2z = l3var1z;
                if (l3var3z == NULL)
                    l3var3z = l3var1z;
                inSymbol();
            } else
                errAndSkip(errNoIdent, skipToSet + mkbs(RPAREN,COMMA,COLON));
            noComma = (SY != COMMA);
            if (not noComma) {
                int93z = 0;
                inSymbol();
            }
        } while (!noComma);
        if (l3sym7z != PROCSY) {
            checkSymAndRead(COLON);
            parseTypeRef(expType, (skipToSet + mkbs(IDENT,RPAREN)));
            if (l3sym7z != VARSY) {
                if (isFileType(expType))
                    error(5); /*errSimpleTypeReq */
                else if (expType->size != 1)
                     l3var5z = l3var6z * expType->size + l3var5z;
            }
            if (l3var3z != NULL) {
                while (l3var3z != curIdRec) /* do with l3var3z@ do */ {
                    l3var3z->typ = expType;
                    l3var3z = l3var3z->list;
                }
            }
        }

        if (SY == SEMICOLON) {
            int93z = 0;
            inSymbol();
            if (not (skipToSet + mkbs(IDENT,VARSY,FUNCSY,PROCSY)).has(SY))
                errAndSkip(errBadSymbol, (skipToSet + mkbs(IDENT,RPAREN)));
        }
    }
    /* 22276 */
    if (l3var5z != 0) {
        curIdRec->flags = (curIdRec->flags + mkbs(23));
        l3var6z = l2int18z;
        l2int18z = l2int18z + l3var5z;
        l3var2z = curIdRec->argList;
        /* 22306 */
        while (l3var2z != curIdRec) {
            if (l3var2z->cl == VARID) {
                l3var5z = l3var2z->typ->size;
                if (l3var5z != 1) {
                    l3var2z->value = l3var6z;
                    l3var6z = l3var6z + l3var5z;
                }
            }
            l3var2z = l3var2z->list;
        }
    }
    /* 22322 */
    checkSymAndRead (RPAREN);
} /* parseParameters */

void exitScope(IdentRecPtr arg[128]) {
    IdentRecPtr &workidr = programme::super.back()->workidr;
    IdentRecPtr &scopeBound = programme::super.back()->scopeBound;

    for (int ii = 0; ii <= 127; ++ii) {
        workidr = arg[ii];
        while (workidr != NULL and
              workidr >= scopeBound)
            workidr = workidr->next;
        arg[ii] = workidr;
    }
} /* exitScope */

programme::programme(int64_t & l2arg1z, IdentRecPtr const l2idr2z_) : l2idr2z(l2idr2z_)
{
    super.push_back(this);
    localSize = l2arg1z;
    if (localSize == 0) {
        inSymbol();
        initScalars();
        return;
    }
    preDefHead = reinterpret_cast<IdentRec*>(ptr(0));
    inTypeDef = false;
    l2int11z = 0;
    strLabList = NULL;
    lineNesting = lineNesting + 1;
    l2var16z = numLabList;
    do {
        if (SY == LABELSY) {
            do {
                inSymbol();
                if (SY != INTCONST) {
                    requiredSymErr(INTCONST);
                    goto L22421;
                }
                l2var15z = numLabList;
                while (l2var15z != l2var16z) {
                    if (l2var15z->id != curToken) {
                        l2var15z = l2var15z->next;
                    } else {
                        int97z = l2var15z->line;
                        error(17); /* errLblAlreadyDefinedInLine */
                        goto L22420;
                    }
                }
                l2var15z = new NumLabel;
                /* with l2var15z@ do */ {
                    l2var15z->id = curToken;
                    l2var15z->frame = curFrameRegTemplate;
                    l2var15z->offset = 0;
                    l2var15z->line = lineCnt;
                    l2var15z->defined = false;
                    l2var15z->next = numLabList;
                }
                numLabList = l2var15z;
              L22420:
                inSymbol();
              L22421:
                if (SY != COMMA && SY != SEMICOLON)
                    errAndSkip(1, skipToSet + mkbs(COMMA,SEMICOLON));
            } while(SY == COMMA);
            if (SY == SEMICOLON)
                inSymbol();
        } /* 22432 */
        if (SY == CONSTSY) {
            parseDecls(0);
            while  (SY == IDENT) {
                if (isDefined)
                    error(errIdentAlreadyDefined);
                // workidr@ := [curIdent, curFrameRegTemplate, symHashTabBase[bucket], , ENUMID, NULL];
                workidr =
                    new IdentRec(curIdent, curFrameRegTemplate, symHashTabBase[bucket], NULL, ENUMID, NULL, 0L);
                symHashTabBase[bucket] = workidr;
                inSymbol();
                if (charClass != EQOP)
                    error(errBadSymbol);
                else
                    inSymbol();
                /* with workidr@ do */
                parseLiteral(workidr->typ, *reinterpret_cast<Word*>(&workidr->value), true);
                if (workidr->typ == NULL) {
                    error(errNoConstant);
                    workidr->typ = IntegerType;
                    workidr->value = 1;
                } else
                    inSymbol();
                if (SY == SEMICOLON) {
                    int93z = 0;
                    inSymbol();
                    if (!(skipToSet + mkbs(IDENT)).has(SY)) {
                        errAndSkip(errBadSymbol, skipToSet + mkbs(IDENT));
                    }
                } else {
                    requiredSymErr(SEMICOLON);
                }
            }
        } /* 22511 */
        objBufIdx = 1;
        if (SY == TYPESY) {
            inTypeDef = true;
            typelist = NULL;
            parseDecls(0);
            while (SY == IDENT) {
                if (isDefined)
                    error(errIdentAlreadyDefined);
                ii = bucket;
                l2var12z = curIdent;
                inSymbol();
                if (charClass != EQOP)
                    error(errBadSymbol);
                else
                    inSymbol();
                parseTypeRef(l2typ13z, skipToSet + mkbs(SEMICOLON));
                curIdent = l2var12z;
                if (knownInType(curIdRec)) {
                    l2typ14z = curIdRec->typ;
                    if (l2typ14z->base == BooleanType) {
                        if (l2typ13z->k != kindPtr) {
                            prevErrPos = 0;
                            error(78); /* errPredefinedAsPointer */
                            printf(": ");
                            printTextWord(l2var12z);
                            printf(" in line %ld\n", curIdRec->offset);
                        }
                        l2typ14z->base = l2typ13z->base;
                    } else {
                        l2typ14z->base = l2typ13z;
                        curIdRec->typ = l2typ13z;
                    }
                    P2672(typelist, curIdRec);
                } else {
                    curIdRec = new IdentRec;
                    /* with curIdRec@ do */ {
                        curIdRec->id = l2var12z;
                        curIdRec->offset = curFrameRegTemplate;
                        curIdRec->typ = l2typ13z;
                        curIdRec->cl = TYPEID;
                    }
                } /* 22574 */
                curIdRec->next = symHashTabBase[ii];
                symHashTabBase[ii] = curIdRec;
                int93z = 0;
                checkSymAndRead(SEMICOLON);
            } /* 22602 */
            while (typelist != NULL) {
                l2var12z = typelist->id;
                curIdRec = typelist;
                prevErrPos = 0;
                error(79); /* errNotFullyDefined */
                printf(": ");
                printTextWord(l2var12z);
                printf(" in line %ld\n", curIdRec->offset);
                typelist = typelist->next;
            }
        } /* TYPESY -> 22612 */
        inTypeDef = false;
        curExpr = NULL;
    if (SY == VARSY) {
        parseDecls(0);
        /*22617*/
        do {
            workidr = NULL;
            /*22620*/
            do {
                if (SY == IDENT) {
                    curIdRec = new IdentRec;
                    if (isDefined)
                        error(errIdentAlreadyDefined);
                    /* with curIdRec@ do */ {
                        curIdRec->id = curIdent;
                        curIdRec->offset = curFrameRegTemplate;
                        curIdRec->next = symHashTabBase[bucket];
                        curIdRec->cl = VARID;
                        curIdRec->list = NULL;
                    }
                    symHashTabBase[bucket] = curIdRec;
                    inSymbol();
                    if (workidr == NULL)
                        workidr = curIdRec;
                    else
                        l2var4z->list = curIdRec;
                    l2var4z = curIdRec;
                } else
                    error(errNoIdent);
                if (SY != COMMA && SY != COLON)
                    errAndSkip(1, skipToSet + mkbs(IDENT,COMMA));
                l2bool8z = SY != COMMA;
                if (not l2bool8z) {
                    int93z = 0;
                    inSymbol();
                };
            } while (!l2bool8z);
            checkSymAndRead(COLON);
            parseTypeRef(l2typ13z, skipToSet + mkbs(IDENT,SEMICOLON));
            jj = l2typ13z->size;
            while (workidr != NULL) /* do with workidr@ do */ {
                curIdRec = workidr->list;
                workidr->typ = l2typ13z;
                workidr->list = NULL;
                l2bool8z = true;
                if (curProcNesting == 1) {
                    curExternFile = externFileList;
                    l2var12z = workidr->id;
                    curVal.i = jj;
                    toAlloc = curVal.m * halfWord + mkbs(24,27,28,29);
                    while (l2bool8z and curExternFile != NULL) {
                        if (curExternFile->id == l2var12z) {
                            l2bool8z = false;
                            if (curExternFile->line == 0) {
                                curVal = curExternFile->offset;
                                workidr->value = allocExtSymbol(toAlloc);
                                curExternFile->line = lineCnt;
                            }
                        } else {
                            curExternFile = curExternFile->next;
                        }
                    }
                } /* 22731 */
                if (l2bool8z) {
                    workidr->value = localSize;
                    if (PASINFOR.listMode == 3) {
                        printf("%25s", "VARIABLE ");
                        printTextWord(workidr->id);
                        printf(" OFFSET (%ld) %05loB. WORDS=%05loB\n", curProcNesting,
                                localSize, jj);
                    }
                    localSize = localSize + jj;
                    curExternFile = NULL;
                } /*22764*/
                if (isFileType(l2typ13z))
                    makeExtFile();
                workidr = curIdRec;
            } /* 22771 */
            int93z = 0;
            checkSymAndRead(SEMICOLON);
            if (SY != IDENT and not skipToSet.has(SY))
                errAndSkip(errBadSymbol, skipToSet + mkbs(IDENT));
        } while (SY == IDENT);
    } /* VARSY -> 23003 */
    if (curProcNesting == 1) {
        if (outputFile != NULL) {
            workidr = outputFile;
            curExternFile = fileForOutput;
            makeExtFile();
        }
        if (inputFile != NULL) {
            workidr = inputFile;
            curExternFile = fileForInput;
            makeExtFile();
        }
    }
    if (curExpr != NULL) {
        l2int11z = moduleOffset;
        (void) formOperator(gen14);
    } else
        l2int11z = 0;
    if (curProcNesting == 1) {
        curExternFile = externFileList;
        while (curExternFile != NULL) {
            if (curExternFile->line == 0) {
                error(80); /* errUndefinedExternFile */
                printTextWord(curExternFile->id);
                putchar('\n');
            }
            curExternFile = curExternFile->next;
        }
    } /*23035*/
    outputObjFile();
    while (SY == PROCSY or SY == FUNCSY) {
        l2bool8z = SY == PROCSY;
        if (curFrameRegTemplate == 7) {
            error(81); /* errProcNestingTooDeep */
        }
        int93z = 0;
        inSymbol();
        if (SY != IDENT) {
            error(errNoIdent);
            curIdRec = uProcPtr;
            isPredefined = false;
        } else {
            if (isDefined) /* with hashTravPtr@ do */ {
                if (hashTravPtr->cl == ROUTINEID and
                    hashTravPtr->list == NULL and
                    hashTravPtr->preDefLink != NULL and
                    (hashTravPtr->typ == NULL) == l2bool8z) {
                    isPredefined = true;
                } else {
                    isPredefined = false;
                    error(errIdentAlreadyDefined);
                    printErrMsg(82); /* errPrevDeclWasNotForward */
                }
            } else
                isPredefined = false;
        } /* 23103 */
        if (not isPredefined) {
            curIdRec = new IdentRec;
            /* with curIdRec@ do */ {
                curIdRec->id = curIdent;
                curIdRec->offset = curFrameRegTemplate;
                curIdRec->next = symHashTabBase[bucket];
                curIdRec->typ = NULL;
                symHashTabBase[bucket] = curIdRec;
                curIdRec->cl = ROUTINEID;
                curIdRec->list = NULL;
                curIdRec->value = 0;
                curIdRec->argList = NULL;
                curIdRec->preDefLink = NULL;
                if (declExternal)
                    curIdRec->flags = mkbsr(0,15)+mkbs(22);
                else
                    curIdRec->flags = mkbsr(0,15);
                curIdRec->pos = 0;
                curFrameRegTemplate = curFrameRegTemplate + frameRegTemplate;
                if (l2bool8z)
                    l2int18z = 3;
                else
                    l2int18z = 4;
            }
            curProcNesting = curProcNesting + 1;
            inSymbol();
            if (6 < curProcNesting)
                error(81); /* errProcNestingTooDeep */
            if (not mkbs(LPAREN,SEMICOLON,COLON).has(SY))
                errAndSkip(errBadSymbol, skipToSet + mkbs(LPAREN,SEMICOLON,COLON));
            if (SY == LPAREN)
                parseParameters();
            if (not l2bool8z) {
                if (SY != COLON)
                    errAndSkip(106 /*:*/, skipToSet + mkbs(SEMICOLON));
                else {
                    inSymbol();
                    parseTypeRef(curIdRec->typ, skipToSet + mkbs(SEMICOLON));
                    if (curIdRec->typ->size != 1)
                        error(errTypeMustNotBeFile);
                }
            }
        } else /*23167*/ {
            /* with hashTravPtr@ do */ {
                l2int18z = hashTravPtr->level;
                curFrameRegTemplate = curFrameRegTemplate + indexreg[1];
                curProcNesting = curProcNesting + 1;
                if (preDefHead == hashTravPtr) {
                    preDefHead = hashTravPtr->preDefLink;
                } else {
                    curIdRec = preDefHead;
                    while (hashTravPtr != curIdRec) {
                        workidr = curIdRec;
                        curIdRec = curIdRec->preDefLink;
                    }
                    workidr->preDefLink = hashTravPtr->preDefLink;
                }
            }
            hashTravPtr->preDefLink = NULL;
            curIdRec = hashTravPtr->argList;
            if (curIdRec != NULL) {
                while (curIdRec != hashTravPtr) {
                    addToHashTab(curIdRec);
                    curIdRec = curIdRec->list;
                }
            }
            curIdRec = hashTravPtr;
            setup(scopeBound);
            inSymbol();
        } /* 23224 */
        checkSymAndRead(SEMICOLON);
        /* with curIdRec@ do */
        if (curIdent == litForward) {
            if (isPredefined)
                error(83); /* errRepeatedPredefinition */
            curIdRec->level = l2int18z;
            curIdRec->preDefLink = preDefHead;
            preDefHead = curIdRec;
        } else if (curIdent == litExternal or
                   curIdent == litFortran) {
            if (curIdent == litExternal) {
                curVal.m = mkbs(20);
            } else if (checkFortran) {
                curVal.m = mkbs(21,24);
                checkFortran = false;
            } else {
                curVal.m = mkbs(21);
            }
            curIdRec->flags = curIdRec->flags + curVal.m;
        } else /* 23257 */ {
            do {
                setup(scopeBound);
                programme(l2int18z, curIdRec);
                if (SY != FUNCSY && SY != PROCSY && SY != BEGINSY)
                    errAndSkip(errBadSymbol, skipToSet);
            } while (SY != FUNCSY && SY != PROCSY && SY != BEGINSY);
            rollup(scopeBound);
            exitScope(symHashTabBase);
            exitScope(typeHashTabBase);
            goto L23301;
        } /* 23277 */
        inSymbol();
        checkSymAndRead(SEMICOLON);
      L23301:
        workidr = curIdRec->argList;
        if (workidr != NULL) {
            while (workidr != curIdRec) {
                scopeBound = NULL;
                P2672(scopeBound, workidr);
                workidr = workidr->list;
            }
        } /* 23314 */
        curFrameRegTemplate = curFrameRegTemplate - indexreg[1];
        curProcNesting = curProcNesting - 1;
    } /* 23320 */
    if (SY != BEGINSY and
        (not allowCompat or not blockBegSys.has(SY)))
        errAndSkip(84 /* errErrorInDeclarations */, skipToSet);
    } while (!statBegSys.has(SY));
    if (preDefHead != ptr(0))  {
        error(85); /* errNotFullyDefinedProcedures */
        while (preDefHead != NULL) {
            printTextWord(preDefHead->id);
            preDefHead = preDefHead->preDefLink;
        }
        putchar('\n');
    }
    defineRoutine();
    while (numLabList != l2var16z) {
        if (not numLabList->defined) {
            printf(" %ld:", int64_t(numLabList->id.i));
            l2bool8z = false;
        }
        numLabList = numLabList->next;
    }
    if (not l2bool8z) {
        printTextWord(l2idr2z->id);
        error(90); /* errLblDefinitionInBlock */
    }
    l2arg1z = l2int21z;
    /* 23364 */
} /* programme */

struct initTables {
    int64_t idx, jdx;
    // l2unu3z, l2unu4z, l2unu5z: Word;

    void initInsnTemplates() {
        Insn l3var1z;
        Operator l3var2z;

        for (l3var1z = ATX; l3var1z <= JADDM; succ(l3var1z))
            InsnTemp[l3var1z] = l3var1z * 010000;
        InsnTemp[ELFUN] = 0500000;
        jdx = KUTC;
        for (l3var1z = UTC; l3var1z <= VJM; succ(l3var1z)) {
            InsnTemp[l3var1z] = jdx;
            jdx = (jdx + 0100000);
        }
        for (idx=1; idx <= 15; ++idx)
            indexreg[idx] = idx * frameRegTemplate;
        jumpType = InsnTemp[UJ];
        for (l3var2z = MUL; l3var2z<=ASSIGNOP; succ(l3var2z)) {
            opFlags[l3var2z] = opfCOMM;
            opToInsn[l3var2z] = 0;
            if (mkbs(MUL, RDIVOP, PLUSOP, MINUSOP).has(l3var2z)) {
                opToMode[l3var2z] = 3;
            } else if (mkbs(IDIVOP, IMODOP).has(l3var2z)) {
                opToMode[l3var2z] = 2;
            } else if (mkbs(IMULOP, INTPLUS, INTMINUS, badop27).has(l3var2z)) {
                opToMode[l3var2z] = 1;
            } else if (mkbs(IDIVROP,badop30,badop31).has(l3var2z)) {
                opToMode[l3var2z] = 4;
            } else {
                opToMode[l3var2z] = 0;
            }
        }
        opToInsn[MUL] = InsnTemp[AMULX];
        opToInsn[RDIVOP] = InsnTemp[ADIVX];
        opToInsn[IDIVOP] = 17; /* P/DI */
        opToInsn[IMODOP] = 11; /* P/MD */
        opToInsn[PLUSOP] = InsnTemp[ADD];
        opToInsn[MINUSOP] = InsnTemp[SUB];
        opToInsn[IMULOP] = InsnTemp[AMULX];
        opToInsn[SETAND] = InsnTemp[AAX];
        opToInsn[SETXOR] = InsnTemp[AEX];
        opToInsn[SETOR] = InsnTemp[AOX];
        opToInsn[INTPLUS] = InsnTemp[ADD];
        opToInsn[INTMINUS] = InsnTemp[SUB];
        opToInsn[IDIVROP] = 67; /* P/IS */
        opToInsn[badop27] = 22; /* P/II unused, undefined */
        opToInsn[badop30] = 23; /* P/RR */
        opToInsn[badop31] = 24; /* P/RI */
        opToInsn[MKRANGE] = 61; /* P/PI */
        opToInsn[SETSUB] = InsnTemp[AAX];
        opFlags[AMPERS] = opfAND;
        opFlags[IDIVOP] = opfDIV;
        opFlags[OROP] = opfOR;
        opFlags[IMULOP] = opfMULMSK;
        opFlags[IMODOP] = opfMOD;
        opFlags[badop27] = opfHELP;
        opFlags[badop30] = opfHELP;
        opFlags[badop31] = opfHELP;
        opFlags[MKRANGE] = opfHELP;
        opFlags[IDIVROP] = opfHELP;
        opFlags[ASSIGNOP] = opfASSN;
        opFlags[SETSUB] = opfINV;
        for (jdx = 0; jdx <= 6; ++jdx) {
            funcInsn[jdx] = (0500000 + jdx);
        }
    } /* initInsnTemplates */


    void regResWord(int64_t l4arg1z) {
        KeyWord * kw;
        Word l4var2z;
        curVal.ii = l4arg1z;
        // curVal.m = curVal.m * hashMask.m;
        // mapai(curVal.a, curVal.i);
        curVal.i = (curVal.m.val % 65535) % 128;
        l4var2z.ii = l4arg1z;
        kw = new KeyWord;
        /* with kw@ do */ {
            kw->w = l4var2z;
            kw->sym = SY;
            kw->op = charClass;
            kw->next = KeyWordHashTabBase[curVal.i];
        }
        KeyWordHashTabBase[curVal.i] = kw;
        if (charClass == NOOP) {
            succ(SY);
        } else {
            succ(charClass);
        }
    } /* regResWord */

    void regKeyWords() {
        SY = MULOP;
        charClass = AMPERS;
        regResWord(0415644L/*"     AND"*/);
        regResWord(0445166L/*"     DIV"*/);
        regResWord(0555744L/*"     MOD"*/);
        SY = GTSY; /* reused as NULLSY */
        charClass = NOOP;
        regResWord(0565154L/*"     NIL"*/);
        SY = ADDOP;
        charClass = OROP;
        regResWord(05762L/*"      OR"*/);
        SY = RELOP;
        charClass = INOP;
        regResWord(05156L/*"      IN"*/);
        SY = NOTSY;
        charClass = NOOP;
        regResWord(0565764L/*"     NOT"*/);
        SY = LABELSY;
        charClass = NOOP;
        for (idx = 0; idx <= 29; ++idx)
            regResWord(resWordNameBase[idx]);
    } /* regKeyWords */

    void initArrays() {
        // int64_t l3var1z;
        int64_t l3var2z;
        FcstCnt = 0;
        FcstCountTo500 = 0;
        for (idx = 3; idx <= 6; ++idx) {
            l3var2z = idx - 2;
            for (jdx=1; jdx <= l3var2z; ++jdx)
                frameRestore[idx][jdx] = 0;
        }
        for (idx=1; idx <= 99; ++idx)
            helperMap[idx] = 0;
    } /* initArrays */

    void initSets() {
        skipToSet = blockBegSys + statBegSys - mkbs(CASESY);
        bigSkipSet = skipToSet + statEndSys;
    } /* initSets */

    initTables () {
        initArrays();
        initInsnTemplates();
        initSets();
        memcpy(&koi2text['*'],
               "\012\036\000\035\000\017" // 052-057 (* + , - . /)
               "\020\021\022\023\024\025\026\027" // 060-067 (0 - 7)
               "\030\031\000\000\000\000\000\000" // 070-077 (8 9 : ; < = > ?)
               "\000\041\042\043\044\045\046\047" // 100-107 (@ - G)
               "\050\051\052\053\054\055\056\057" // 110-117 (H - O)
               "\060\061\062\063\064\065\066\067" // 120-127 (P - W)
               "\070\071\072\000\000\000\000\000" // 130-137 (X Y Z [ \ ] ^ _)
               "\000\041\042\043\044\045\046\047" // 140-147 (` - g)
               "\050\051\052\053\054\055\056\057" // 150-157 (h - o)
               "\060\061\062\063\064\065\066\067" // 160-167 (p - w)
               "\070\071\072\000\000\000\000\000" // 170-177 (x y z { | } ~ )
               , 86);
        memcpy(&koi2text[0300],
               "\077\041\002\003\004\045\005\006" // 300-307 (ю - г)
               "\070\007\013\053\014\055\050\057" // 310-317 (х - о)
               "\034\015\060\043\064\071\016\042" // 320-327 (п - в)
               "\032\037\040\073\074\075\076\000" // 330-337 (ь - ъ)
               "\077\041\002\003\004\045\005\006" // 340-347 (Ю - Г)
               "\070\007\013\053\014\055\050\057" // 350-357 (Х - О)
               "\034\015\060\043\064\071\016\042" // 360-367 (П - В)
               "\032\037\040\073\074\075\076\000" // 370-377 (Ь - Ъ)
               , 64);
        CHILD.clear();
        for (jdx = 1; jdx <= 10; ++jdx)
            CHILD.push_back(Bitset());
        for (idx = 0; idx <= 127; ++idx) {
            symHashTabBase[idx] = NULL;
            typeHashTabBase[idx] = NULL;
            KeyWordHashTabBase[idx] = NULL;
        }
        regKeyWords();
        numLabList = NULL;
        totalErrors = 0;
        heapCallsCnt = 0;
        putLeft = true;
        bool102z = true;
        curFrameRegTemplate = frameRegTemplate;
        curProcNesting = 1;
    } /* initTables */
};

void finalize() {
    int64_t idx, cnt;
    int64_t sizes[11]; // array [1..10] of @Integer;

    sizes[1] = 1;
    sizes[2] = symTabPos - 074000 - 1;
    sizes[5] = longSymCnt;
    sizes[6] = moduleOffset - 040000;
    sizes[8] = FcstCnt;
    sizes[3] = 0;
    sizes[4] = 0;
    sizes[7] = 0;
    sizes[9] = int92z;
    sizes[10] = int93z;
    curVal.i = moduleOffset - 040000;
    symTab[074001] = mkbs(24,29) + curVal.m - intZero;
    // Forming the compact form of the module header.
    CHILD[7].val = sizes[1] | (sizes[2] << 12);
    CHILD[8].val = sizes[5] << 30 | sizes[9] << 15 | sizes[10];
    CHILD[9].val = sizes[8] << 30 | sizes[7] << 15 | sizes[6];
    /*
    reset(FCST);
    while not eof(FCST) do {
        write(CHILD, FCST@);
        get(FCST);
    };
    */
    CHILD.insert(CHILD.end(), FCST.begin(), FCST.end());
    curVal.i = (symTabPos - 070000L) * 0100000000L;
    for (cnt = 1; cnt <= longSymCnt; ++cnt) {
        idx = longSymTabBase[cnt];
        symTab[idx] = (symTab[idx] + (curVal.m * mkbsr(9,23)));
        curVal.i = (curVal.i + 0100000000L);
    }
    symTabPos = symTabPos - 1;
    for (cnt = 074000; cnt <= symTabPos; ++cnt)
        CHILD.push_back(symTab[cnt]);
    for (cnt = 1; cnt <= longSymCnt; ++cnt)
        CHILD.push_back(longSyms[cnt]);
    if (allowCompat) {
        printf("%6ld LINES STRUCTURE ", lineCnt - 1);
        for (idx=1; idx <=10; ++idx)
            printf("%ld ", sizes[idx]);
        putchar('\n');
    }
    entryPtTable[entryPtCnt] = mkbs();

} /* finalize */

void initOptions() {
    PASINFOR.startOffset = PASINFOR.startOffset - 16384;
    commentModeCH = ' ';
    lineNesting = 0;
    maxLineLen = 120;
    CH = ' ';
    PASINPUT = ugetc(pasinput);
    linePos = 0;
    prevErrPos = 0;
    errsInLine = 0;
    lineCnt = 1;
    checkFortran = false;
    bool110z = false;
    int93z = 1;
    int92z = 1;
    moduleOffset = 16384;
    lineStartOffset = 16384;
    int94z = 1;
    bool47z = false;
    dataCheck = false;
    heapSize = 100;
    bool49z = true;
    atEOL = false;
    doPMD = true; // not (42 in curVal.m);
    checkTypes = true;
    fixMult = true;
    fuzzReals = true;
    pseudoZ = true;
    checkBounds = true; // not (44 in curVal.m);
    declExternal = false;
    errors = false;
    allowCompat = false;
    litExternal.ii = 04570644562564154L;
    litForward.ii = 046576267416244L;
    litFortran.ii = 046576264624156L;
    fileBufSize = 1;
    charEncoding = 2;
    chain = NULL;
    litOct.ii = 0574364L;
    longSymCnt = 0;
    extSymAdornment = 0;
    symTabCnt = 0;
} /* initOptions */

int main() {
// Data Initializations moved here
    blockBegSys = mkbs(LABELSY, CONSTSY, TYPESY, VARSY)+mkbs(FUNCSY, PROCSY, BEGINSY);
    statBegSys = mkbs(BEGINSY, IFSY, CASESY, REPEATSY)+mkbs(WHILESY, FORSY, WITHSY)+
        mkbs(GOTOSY, SELECTSY);
    O77777 = mkbsr(33,47);
    intZero = mkbs(0,1,3);
    // unused138z = 063000000L;
    extSymMask.val = 043000000L;
    halfWord = mkbsr(24,47);
    hashMask.m.val = 0203407;
    statEndSys = mkbs(SEMICOLON, ENDSY, ELSESY, UNTILSY);
    lvalOpSet = mkbs(GETELT, GETVAR, op36, op37)+mkbs(GETFIELD, DEREF, FILEPTR);

    funcInsn[fnABS] = KAMX;
    funcInsn[fnTRUNC] = KADD+ZERO;
    funcInsn[fnODD] = KAAX+E1;
    funcInsn[fnORD] = KAOX+ZERO;
    funcInsn[fnCHR] = KAAX+MANTISSA;
    funcInsn[fnSUCC] = KARX+E1;
    funcInsn[fnPRED] = KSUB+E1;
    funcInsn[fnSQR] = macro + mcSQRR;
    funcInsn[fnROUND] = macro + mcROUND;
    funcInsn[fnCARD] = macro + mcCARD;
    funcInsn[fnMINEL] = macro + mcMINEL;
    funcInsn[fnPTR] = KAAX+MANTISSA;
    funcInsn[fnABSI] = KAMX;
    funcInsn[fnSQRI] = macro + mcSQRI;

    for (int i = 0; i < 128; ++i) {
        charSymTabBase[i] = NOSY;
        chrClassTabBase[i] = NOOP;
    }
    for (int i = 0; i < 10; ++i) {
        charSymTabBase[i+'0'] = INTCONST;
        chrClassTabBase[i+'0'] = ALNUM;
    }
    for (int i = 0; i < 26; ++i) {
        charSymTabBase[i+'A'] = IDENT;
        chrClassTabBase[i+'A'] = ALNUM;
        charSymTabBase[i+'a'] = IDENT;
        chrClassTabBase[i+'a'] = ALNUM;
    }

    charSymTabBase['\''] = CHARCONST;
    charSymTabBase['_'] = REALCONST;
    charSymTabBase['<'] = LTSY;
    charSymTabBase['>'] = GTSY;
    chrClassTabBase['+'] = PLUSOP;
    chrClassTabBase['-'] = MINUSOP;
    chrClassTabBase['*'] = MUL;
    chrClassTabBase['/'] = RDIVOP;
    chrClassTabBase['='] = EQOP;
    chrClassTabBase['&'] = AMPERS;
    chrClassTabBase['>'] = GTOP;
    chrClassTabBase['<'] = LTOP;
    chrClassTabBase['#'] = NEOP;
    chrClassTabBase['='] = EQOP;
    charSymTabBase['+'] = ADDOP;
    charSymTabBase['-'] = ADDOP;
    charSymTabBase['*'] = MULOP;
    charSymTabBase['/'] = MULOP;
    charSymTabBase['&'] = MULOP;
    charSymTabBase[','] = COMMA;
    charSymTabBase['.'] = PERIOD;
    charSymTabBase['@'] = ARROW;
    charSymTabBase['^'] = ARROW;
    charSymTabBase['('] = LPAREN;
    charSymTabBase[')'] = RPAREN;
    charSymTabBase[';'] = SEMICOLON;
    charSymTabBase['['] = LBRACK;
    charSymTabBase[']'] = RBRACK;
    charSymTabBase['#'] = RELOP;
    charSymTabBase['='] = RELOP;
    charSymTabBase[':'] = COLON;
    charSymTabBase['~'] = NOTSY;
    charSymTabBase['{'] = LBRACE;
    charSymTabBase['}'] = RBRACE;

    iAddOpMap[PLUSOP] = INTPLUS;
    iAddOpMap[MINUSOP] = INTMINUS;
    setOpMap[PLUSOP] = SETOR;
    setOpMap[MINUSOP] = SETSUB;
    iMulOpMap[MUL] = IMULOP;
    iMulOpMap[RDIVOP] = IDIVROP;
    setOpMap[MUL] = SETAND;
    setOpMap[RDIVOP] = SETXOR;

    // Main program starts here

    // L0 by default: no listing, only errors
    PASINFOR.listMode = 0;
    if (PASINFOR.listMode != 0)
        printf("%s\n", boilerplate);
    initOptions();
    curInsnTemplate = 0;
    initTables();
    try {
        programme(curInsnTemplate, hashTravPtr);
    } catch (int foo) {
        if (foo == 9999) goto L9999;
    }
    if (errors) {
      L9999:   printf(" IN %ld LINES %ld ERRORS\n", lineCnt-1, totalErrors);
        exit(1);
    } else {
        finalize();
        // Dump CHILD here
        if (FILE * f = fopen("temp.obj", "w")) {
            for (size_t i = 7; i < CHILD.size(); ++i) {
                for (int j = 40; j >= 0; j -= 8)
                    fputc((CHILD[i].val >> j) & 0xFF, f);
            }
            fclose(f);
        } else {
            OBPROG(CHILD[7], CHILD.back());
        }
        exit(0);
    }
}
int64_t resWordNameBase[30] = {
        05441424554L             /*"   LABEL"*/,
        04357566364L             /*"   CONST"*/,
        064716045L               /*"    TYPE"*/,
        0664162L                 /*"     VAR"*/,
        04665564364515756L       /*"FUNCTION"*/,
        06062574345446562L       /*"PROCEDUR"*/,
        0634564L                 /*"     SET"*/,
        0604143534544L           /*"  PACKED"*/,
        04162624171L             /*"   ARRAY"*/,
        0624543576244L           /*"  RECORD"*/,
        046515445L               /*"    FILE"*/,
        04245475156L             /*"   BEGIN"*/,
        05146L                   /*"      IF"*/,
        043416345L               /*"    CASE"*/,
        0624560454164L           /*"  REPEAT"*/,
        06750515445L             /*"   WHILE"*/,
        0465762L                 /*"     FOR"*/,
        067516450L               /*"    WITH"*/,
        047576457L               /*"    GOTO"*/,
        0455644L                 /*"     END"*/,
        045546345L               /*"    ELSE"*/,
        06556645154L             /*"   UNTIL"*/,
        05746L                   /*"      OF"*/,
        04457L                   /*"      DO"*/,
        06457L                   /*"      TO"*/,
        0445767566457L           /*"  DOWNTO"*/,
        064504556L               /*"    THEN"*/,
        0634554454364L           /*"  SELECT"*/,
        060625747624155L         /*" PROGRAM"*/,
        0576450456263L           /*"  OTHERS"*/};
#if 0
// Non-ASCII chars ignored so far
    charSymTabBase['Ю'] := IDENT:31;
    chrClassTabBase['Ю'] := ALNUM:31;
    charSymTabBase['ю'] := IDENT:31;
    chrClassTabBase['ю'] := ALNUM:31;
    charSymTabBase[chr(27)] := CHARCONST;
    charSymTabBase[chr(22)] := ARROW;
    chrClassTabBase['÷'] := IDIVOP;
    charSymTabBase['÷'] := MULOP;
    chrClassTabBase['∨'] := OROP;
    charSymTabBase['∨'] := ADDOP;
    chrClassTabBase['×'] := MUL;
    charSymTabBase['×'] := MULOP;
    chrClassTabBase['≤'] := LEOP;
    chrClassTabBase['≥'] := GEOP;
    charSymTabBase['≤'] := RELOP;
    charSymTabBase['≥'] := RELOP;
#endif

Bitset helperNames[100] = { {0L},
       {06017210000000000L}      /*"P/1     "*/,
       {06017220000000000L}     /*"P/2     "*/,
       {06017230000000000L}     /*"P/3     "*/,
       {06017240000000000L}     /*"P/4     "*/,
       {06017250000000000L}     /*"P/5     "*/,
       {06017260000000000L}     /*"P/6     "*/,
       {06017434100000000L}     /*"P/CA    "*/,
       {06017455700000000L}     /*"P/EO    "*/,
       {06017636300000000L}     /*"P/SS    "*/,
/*10*/ {06017455400000000L}     /*"P/EL    "*/,
       {06017554400000000L}     /*"P/MD    "*/,
       {06017555100000000L}     /*"P/MI    "*/,
       {06017604100000000L}     /*"P/PA    "*/,
       {06017655600000000L}     /*"P/UN    "*/,
       {06017436000000000L}     /*"P/CP    "*/,
       {06017414200000000L}     /*"P/AB    "*/,
       {06017445100000000L}     /*"P/DI    "*/,
       {06017624300000000L}     /*"P/RC    "*/,
       {06017454100000000L}     /*"P/EA    "*/,
/*20*/ {06017564100000000L}     /*"P/NA    "*/,
       {06017424100000000L}     /*"P/BA    "*/,
       {06017515100000000L}     /*"P/II   u"*/,
       {06017626200000000L}     /*"P/RR    "*/,
       {06017625100000000L}     /*"P/RI    "*/,
       {06017214400000000L}     /*"P/1D    "*/,
       {06017474400000000L}     /*"P/GD    "*/,
       {06017450000000000L}     /*"P/E     "*/,
       {06017454600000000L}     /*"P/EF    "*/,
       {06017604600000000L}     /*"P/PF    "*/,
/*30*/ {06017474600000000L}     /*"P/GF    "*/,
       {06017644600000000L}     /*"P/TF    "*/,
       {06017624600000000L}     /*"P/RF    "*/,
       {06017566700000000L}     /*"P/NW    "*/,
       {06017446300000000L}     /*"P/DS    "*/,
       {06017506400000000L}     /*"P/HT    "*/,
       {06017675100000000L}     /*"P/WI    "*/,
       {06017676200000000L}     /*"P/WR    "*/,
       {06017674300000000L}     /*"P/WC    "*/,
       {06017412600000000L}     /*"P/A6    "*/,
/*40*/ {06017412700000000L}     /*"P/A7    "*/,
       {06017677000000000L}     /*"P/WX    "*/,
       {06017675700000000L}     /*"P/WO    "*/,
       {06017436700000000L}     /*"P/CW    "*/,
       {06017264100000000L}     /*"P/6A    "*/,
       {06017274100000000L}     /*"P/7A    "*/,
       {06017675400000000L}     /*"P/WL    "*/,
       {06017624451000000L}     /*"P/RDI   "*/,
       {06017624462000000L}     /*"P/RDR   "*/,
       {06017624443000000L}     /*"P/RDC   "*/,
/*50*/ {06017624126000000L}     /*"P/RA6   "*/,
       {06017624127000000L}     /*"P/RA7   "*/,
       {06017627000000000L}     /*"P/RX   u"*/,
       {06017625400000000L}     /*"P/RL    "*/,
       {06017675754560000L}     /*"P/WOLN  "*/,
       {06017625154560000L}     /*"P/RILN  "*/,
       {06017626200000000L}     /*"P/RR    "*/,
       {06017434500000000L}     /*"P/CE    "*/,
       {06017646200000000L}     /*"P/TR    "*/,
       {06017546600000000L}     /*"P/LV    "*/,
/*60*/ {06017724155000000L}     /*"P/ZAM  u"*/,
       {06017605100000000L}     /*"P/PI    "*/,
       {06017426000000000L}     /*"P/BP    "*/,
       {06017422600000000L}     /*"P/B6    "*/,
       {06017604200000000L}     /*"P/PB    "*/,
       {06017422700000000L}     /*"P/B7    "*/,
       {06017515600000000L}     /*"P/IN    "*/,
       {06017516300000000L}     /*"P/IS    "*/,
       {06017444100000000L}     /*"P/DA    "*/,
       {06017435700000000L}     /*"P/CO    "*/,
/*70*/ {06017516400000000L}     /*"P/IT    "*/,
       {06017435300000000L}     /*"P/CK    "*/,
       {06017534300000000L}     /*"P/KC    "*/,
       {06017545647604162L}     /*"P/LNGPAR"*/,
       {06017544441620000L}     /*"P/LDAR  "*/,
       {06017544441625156L}     /*"P/LDARIN"*/,
       {06017202043000000L}     /*"P/00C   "*/,
       {06017636441620000L}     /*"P/STAR  "*/,
       {06017605544634564L}     /*"P/PMDSET"*/,
       {06017435100000000L}     /*"P/CI    "*/,
/*80*/ {06041514200000000L}     /*"PAIB    "*/,
       {06017674100000000L}     /*"P/WA    "*/,
       {06361626412000000L}     /*"SQRT*   "*/,
       {06351561200000000L}     /*"SIN*    "*/,
       {04357631200000000L}     /*"COS*    "*/,
       {04162436441561200L}     /*"ARCTAN* "*/,
       {04162436351561200L}     /*"ARCSIN* "*/,
       {05456120000000000L}     /*"LN*     "*/,
       {04570601200000000L}     /*"EXP*    "*/,
       {06017456100000000L}     /*"P/EQ    "*/,
/*90*/ {06017624100000000L}     /*"P/RA    "*/,
       {06017474500000000L}     /*"P/GE    "*/,
       {06017554600000000L}     /*"P/MF    "*/,
       {06017465500000000L}     /*"P/FM    "*/,
       {06017565600000000L}     /*"P/NN    "*/,
       {06017634300000000L}     /*"P/SC    "*/,
       {06017444400000000L}     /*"P/DD    "*/,
       {06017624500000000L}     /*"P/RE    "*/};

int64_t systemProcNames[30] = {
/*0*/   0606564L                /*"     PUT"*/,
        0474564L                /*"     GET"*/,
        062456762516445L        /*" REWRITE"*/,
        06245634564L            /*"   RESET"*/,
        0564567L                /*"     NEW"*/,
        044516360576345L        /*" DISPOSE"*/,
        050415464L              /*"    HALT"*/,
        063645760L              /*"    STOP"*/,
        06345646560L            /*"   SETUP"*/,
        0625754546560L          /*"  ROLLUP"*/,
/*10*/  06762516445L            /*"   WRITE"*/,
        067625164455456L        /*" WRITELN"*/,
        062454144L              /*"    READ"*/,
        0624541445456L          /*"  READLN"*/,
        045705164L              /*"    EXIT"*/,
        04445426547L            /*"   DEBUG"*/,
        042456355L              /*"    BESM"*/,
        05541605141L            /*"   MAPIA"*/,
        05541604151L            /*"   MAPAI"*/,
        0604353L                /*"     PCK"*/,
/*20*/  06556604353L            /*"   UNPCK"*/,
        060414353L              /*"    PACK"*/,
        0655660414353L          /*"  UNPACK"*/,
        05760455644L            /*"   OPEND"*/,
        044455444L              /*"    DELD"*/,
        056456744L              /*"    NEWD"*/,
        060656444L              /*"    PUTD"*/,
        047456444L              /*"    GETD"*/,
        055574444L              /*"    MODD"*/,
        046515644L              /*"    FIND"*/};
