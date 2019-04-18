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

FILE * pasinput = stdin;
unsigned char PASINPUT;

const char * boilerplate = " Pascal-Monitor in C++ (17.05.2019)";

const int
fnSQRT  = 0,  fnSIN  = 1,  fnCOS  = 2,  fnATAN  = 3,  fnASIN = 4,
    fnLN    = 5,  fnEXP  = 6,  fnABS = 7,  fnTRUNC = 8,  fnODD  = 9,
    fnORD   = 10, fnCHR  = 11, fnSUCC = 12, fnPRED  = 13, fnEOF  = 14,
    fnREF   = 15, fnEOLN = 16, fnSQR = 17, fnROUND = 18, fnCARD = 19,
    fnMINEL = 20, fnPTR  = 21, fnABSI = 22, fnSQRI  = 23, fn24 = 24, fn29 = 29;

const int
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

const int
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

const int
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

const int    ASN64 = 0360100,

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
    KDIV =      0160000,
    KMUL =      0170000,
    KAPX =      0200000,
    KAUX =      0210000,
    KACX =      0220000,
    KANX =      0230000,
    KYTA =      0310000,
    KASN =      0360000,
    KNTR =      0370000,
    KATI =      0400000,
    KSTI =      0410000,
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
    KUZA =      02600000,
    KU1A =      02700000,
    KUJ =       03000000,
    KVJM =      03100000,
    KVZM =      03400000,
    KV1M =      03500000,
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
/*60B*/ PROGRAMSY,  OTHERSY,    NOSY
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
    Bitset operator <<(int x) const {
        Bitset ret;
        ret.val = val << x;
        return ret;
    }
    Bitset operator >>(int x) const {
        Bitset ret;
        ret.val = val >> x;
        return ret;
    }
    bool operator==(Bitset x) const { return val == x.val; }
    bool operator!=(Bitset x) const { return !(*this == x); }
    operator bool() const { return val != 0; }
    bool has(unsigned b) const {
            return b < 48 && (val >> (47-b)) & 1;
    }
};

Bitset mkbs(const char * set) {
    // !!! TODO !!!
    Bitset ret;
    return ret;
}
Bitset mkbs(int bit) {
    // !!! TODO !!!
    Bitset ret;
    return ret;
}

typedef Bitset SetOfSYs; // set of ident .. selectsy;

struct Real { uint64_t val:48; };
struct Integer {
    union { uint64_t val:48;
        struct { int64_t ival:41; unsigned exp:7; };
    };
    void addexp() { exp = 104; } 
};

typedef struct Expr * eptr;
typedef struct Types * tptr;
typedef struct IdentRec * irptr;

typedef unsigned char Alfa[6];

struct Word {
    union {
        int i;
        Integer ii;
        Real r;
        bool b;
        Alfa a;
        char c;
        IdClass cl;
        Bitset m;
    };
};
typedef struct oneInsn * oiptr;

struct oneInsn {
    oiptr next;
    int64_t mode, code, offset;
};

enum ilmode { ilCONST, il1, il2, il3 };
enum state {st0, st1, st2};


struct Insnltyp {
    oiptr next, next2;
    tptr typ;
    Bitset regsused;
    ilmode ilm;
    Word ilf5;
    int64_t ilf6;
    int64_t ilf7;
    state st;
    int64_t width, shift;
};

struct Types {
    int size,
    bits;
    Kind k;
    union {
//    kindReal:   ();
    
//    kindRange:
        struct {
            tptr base;
            int checker, left, right;
        };
//    kindArray:
        struct {
            tptr abase, range;
            bool pck;
            int perWord, pcksize;
        };
//    kindScalar:
        struct {
            irptr enums;
            int numen, start;
        };
//    kindSet, kindPtr:
        struct {
            tptr sbase;
        };
//    kindFile:
        struct {
            tptr fbase;
            int elsize;
        };
//    kindRecord:
        struct {
            irptr ptr1, ptr2;
            bool flag, pckrec;
        };
// kindCases:
        struct {
            Word sel;
            tptr first, next, r6;
        };
    };
};

struct TypeChain {
    TypeChain * next;
    tptr type1, type2;
};

typedef char charmap[128];
typedef char textmap[128];

typedef int four[5]; // [1..4]
typedef Bitset Entries[43]; // [1..42]

struct Expr {
    union {
//    NOOP:
        struct {
            Word val;
            Operator op;
            Word d1, d2;
        };
//    MUL:
        struct {
            tptr typ;
            Word d3;
            eptr expr1, expr2;
        };
//    BOUNDS:
        struct {
            Word d4, d5;
            tptr typ1, typ2;
        };
//    NOTOP:
        struct {
            Word d6, d7;
            irptr id1, id2;
        };
//    STANDPROC:
        struct {
            Word d8, d9;
            Integer num1, num2;
        };
    };
};

struct KeyWord {
    Word w;
    Symbol sym;
    Operator op;
    KeyWord * next;
};

struct StrLabel {
    StrLabel * next;
    Word ident;
    int offset;
    int exitTarget;
};

struct NumLabel {
    Word id;
    int line, frame, offset;
    NumLabel * next;
    bool defined;
};

struct IdentRec {
    Word id;
    int offset;
    irptr next;
    tptr typ;
    IdClass cl;
    union {
        // TYPEID,    VARID:  ();
        // ENUMID, FORMALID:
        struct {
            irptr list;
            Integer value;
        };
        // FIELDID:
        struct {
            Integer maybeUnused;
            tptr uptype;
            bool pckfield;
            int shift, width;
        };
        
        // ROUTINEID:
        struct {
            int low, high;
            irptr argList, preDefLink;
            int level, pos;
            Bitset flags;
        };
    };
};

struct ExtFileRec {
    Word id;
    int offset;
    ExtFileRec * next;
    int location, line;
};

enum numberSuffix { noSuffix, suffixB, suffixT, suffixC};


// Globals

numberSuffix suffix;
SetOfSYs   bigSkipSet, statEndSys, blockBegSys, statBegSys,
    skipToSet, lvalOpSet;

bool   bool47z, bool48z, bool49z;
bool   dataCheck;
int    jumpType, jumpTarget, int53z;
Operator charClass;
Symbol   SY, prevSY;
int savedObjIdx,
    FcstCnt,
    symTabPos,
    entryPtCnt,
    fileBufSize;

eptr   expr62z, expr63z;
int curInsnTemplate,
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

Alfa stmtName;
KeyWord * keyWordHashPtr;
Kind curVarKind;
ExtFileRec * curExternFile;
char commentModeCH;
unsigned char CH;
int debugLine,
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
irptr outputFile,
    inputFile,
    programObj,
    hashTravPtr,
    uProcPtr;

ExtFileRec * externFileList;
tptr 
typ120z, typ121z,
    pointerType,
    setType,
    booleanType,
    textType,
    IntegerType,
    RealType,
    charType,
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
eptr uVarPtr, curExpr;
Insnltyp *  InsnList;
ExtFileRec * fileForOutput, * fileForInput;
int maxSmallString, extSymAdornment;

tptr smallStringType[7]; // [2..6]
int symTabCnt;

Word symTabArray[81]; // array [1..80] of Word;
int symTabIndex[81]; // array [1..80] of Integer;
Operator iMulOpMap[48]; // array [MUL..IMODOP] of Operator;
Operator setOpMap[48]; // array [MUL..MINUSOP] of Operator;
Operator iAddOpMap[48]; // array [PLUSOP..MINUSOP] of Operator;
Entries entryPtTable;
four frameRestore[7]; // array [3..6] of four;
int indexreg[16]; // array [1..15] of Integer;
int opToInsn[48]; // array [MUL..op44] of Integer;
int opToMode[48]; // array [MUL..op44] of Integer;
OpFlg opFlags[48]; // array [MUL..op44] of OpFlg;
int funcInsn[24]; // array [0..23] of Integer;
int InsnTemp[48]; // array [Insn] of Integer;

int frameRegTemplate, constRegTemplate, disNormTemplate;
char lineBufBase[132]; // array [1..130] of char;
int errMapBase[10]; // array [0..9] of Integer;
Operator chrClassTabBase[128]; // array ['_000'..'_177'] of Operator;
KeyWord * KeyWordHashTabBase[128]; // array [0..127] of @KeyWord;
Symbol charSymTabBase[128]; // array ['_000'..'_177'] of Symbol;
irptr symHashTabBase[128]; // array [0..127] of irptr;
irptr typeHashTabBase[128]; //array [0..127] of irptr;
int helperMap[100]; // array [1..99] of Integer;
Bitset helperNames[100]; // array [1..99] of Bitset;

Bitset symTab[075501]; // array [74000B..75500B] of Bitset;
Integer systemProcNames[30]; // array [0..29] of Integer;
Integer resWordNameBase[30]; // array [0..29] of Integer;
int longSymCnt;
int longSymTabBase[91]; // array [1..90] of Integer;
Bitset longSyms[91]; // array [1..90] of Bitset;
Word constVals[501]; // array [1..500] of Alfa;
int constNums[501]; // array [1..500] of Integer;
Bitset objBuffer[1025]; // array [1..1024] of Bitset;
char iso2text[128]; // array ['_052'..'_177'] of '_000'..'_077';
std::vector<Bitset> FCST; // file of Bitset; /* last */

std::vector<Bitset> child; // file of Bitset;

struct PasInfor {
    int listMode;
    bool * errors;
    Entries *  entryptr;
    int startOffset;
    charmap * a0, *a1, *a4;
    textmap * a3;
    int sizes[11]; // array [1..10] of @Integer;
    Bitset flags;
} PASINFOR;

struct programme {
    programme(int & l2arg1z, irptr const l2idr2z);
// label 22420, 22421, 23301;

    irptr preDefHead, typelist, scopeBound, l2var4z, curIdRec, workidr;
    bool isPredefined, l2bool8z, inTypeDef;
    eptr l2var10z;
    Integer l2int11z;
    Word l2var12z;
    tptr l2typ13z, l2typ14z;
    NumLabel * l2var15z, * l2var16z;
    StrLabel * strLabList;

    Integer l2int18z, ii, l2int20z, l2int21z, jj;
};

const char * pasmitxt(int errno) {
    switch (errno) {
    case 49: return "Too many instructions in a block";
    case 50: return "Symbol table overflow";
    case 51: return "Long symbol overflow";
    case 52: return "EOF encountered";
    case 54: return "Error in pseudo-comment";
    case 55: return "More than 16 digits in a number";
    }
    return "Dunno";
}


void printErrMsg(int errno)
{ /* PrintErrMsg */
    putchar(' ');
    if (errno >= 200)
        printf("Internal error %d", errno);
    else {
        if (errno > 88) 
            printErrMsg(86);
        else if (errno == 20)
            errno = (SY == IDENT)*2 + 1;
        else if (16 <= errno && errno <= 18)
            printf("%d ", curToken.i);
        printf("%s ", pasmitxt(errno));
        if (errno == 17) 
            printf("%d", int97z);
        else
            printf("%s", stmtName);
    };
    if (errno != 86)
        putchar('\n');
} /* PrintErrMsg */


void printTextWord(Word val)
{
    printf("<%016lo>", val.m.val);
}

void makeStringType(tptr & res)
{
    tptr span;
    if (maxSmallString >= strLen)
        res = smallStringType[strLen];
    else {
        span = new typeof(*span);
        res = new typeof(*res);
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
            res->base = charType;
            res->range = span;
            res->pck = true;
            res->perWord = 6;
            res->pcksize = 8;
        }
    }
} /* makeStringType */

void addToHashTab(irptr arg)
{
    // curVal.m := arg->id.m * hashMask.m;
    // mapai(curVal.a, curVal.i);
    curVal.i = (curVal.m.val % 65535) % 128;
    arg->next = symHashTabBase[curVal.i];
    symHashTabBase[curVal.i] = arg;
} /* addToHashTab */

void error(int errno);

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

void form1Insn(int arg)
{
    Word Insn, opcode;
    Bitset half1, half2;
    int pos;
    Insn.i = arg;
    opcode.i = Insn.i & ~077777;
    if (opcode.i == InsnTemp[UJ]) {
        if (prevOpcode == opcode.i)
            return;
        if (putLeft and (prevOpcode == 1)) {
            pos = objBufIdx - 1;
            if (objBuffer[pos] * mkbs("0..8") == mkbs("0, 1, 3..5, 8")) {
                prevOpcode = opcode.i;
                half1 = (Insn.m * mkbs("33..47")) << 24;
                half2 = (objBuffer[pos] * mkbs("9..23")) >> 24;
                objBuffer[pos] = mkbs("0, 1, 3, 4, 6, 28, 29") +
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

void form2Insn(int i1, int i2)
{
    form1Insn(i1);
    form1Insn(i2);
} /* form2Insn */

void form3Insn(int i1, int i2, int i3)
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

int getObjBufIdxPlus()
{
    if (putLeft)
        return objBufIdx + 4096;
   else
       return objBufIdx;
} /* getObjBufIdxPlus */


void formJump(int arg)
{
    int pos;
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

void formAndAlign(int arg)
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

int allocExtSymbol(Bitset l3arg1z)
{
//    Word l3var1z;
    int l3var2z;
    int ret = symTabPos;
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
        l3arg1z = l3arg1z + mkbs("25");
    } else
        l3arg1z = l3arg1z + curVal.m;
    putToSymTab(l3arg1z);
    return ret;
} /* allocExtSymbol */

int getHelperProc(int l3arg1z)
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

int addCurValToFCST()
{
    int ret;
    int low, high, mid;
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
            if (curVal.m.val < constVals[mid].m.val)
                high = mid - 1;
            else
                low = mid + 1;
        } while (low <= high);
        ret = FcstCnt;
        if (FcstCountTo500 != 500) {
            if (curVal.m.val < constVals[mid].m.val) 
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

int allocSymtab(Bitset l3arg1z)
{
    int ret;
    int low, high, mid;
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

int getFCSToffset() 
{
    int ret;
    Word offset;
    ret = addCurValToFCST();
    offset.i = ret;
    if (offset.i < 2048) {
        /* empty */
    } else if (offset.i >= 4096)
        error(204);
    else {
        ret = allocSymtab(offset.m + mkbs("24")) - 070000;
    }
    return ret;
} /* getFCSToffset */

int minel(Bitset b) {
    if (!b.val) return -1;
    int ret = 1;
    uint64_t t = b.val;
    while (((t >> 47) & 1) == 0) {
        ret++;
        t <<= 1;
    }
    return ret;
}

int nrOfBits(Integer value)
{
    curVal.ii = value;
    curVal.m = curVal.m * mkbs("7..47");
    return 48-minel(curVal.m);
} /* nrOfBits */

void defineRange(tptr & res, Integer l, Integer r)
{
    tptr temp;
    temp = new typeof(*temp);
    /* with temp@ do */ {
        temp->size = 1;
        temp->bits = 48;
        temp->base = res;
        temp->checker = 0;
        temp->k = kindRange;
        curVal.ii = l;
        curVal.m = curVal.m + intZero;
        temp->left = curVal.i;
        curVal.ii = r;
        curVal.m = curVal.m + intZero;
        temp->right = curVal.i;
        if (temp->left >= 0)
            temp->bits = nrOfBits(curVal.ii);
      res = temp;
    }
} /* defineRange */

int getValueOrAllocSymtab(int value)
{
    curVal.i = value;
    curVal.i = curVal.i % 32768;
    if (040000 >= curVal.i)
        return curVal.i;
    else
        return
            allocSymtab((curVal.m + mkbs("24")) * halfWord);
} /* getValueOrAllocSymtab */

void P0715(int mode, int arg)
{
    Bitset addr, insn, leftHalf;
    bool isLarge;
    int work, offset;
    if (mode == 0) {
        padToLeft();
        curVal.i = moduleOffset;
L1:     addr = curVal.m * mkbs("33..47");
        leftHalf = curVal.m << 24;
        while (arg != 0) {
            if (4096 < arg)  {
                isLarge = true;
                arg = arg - 4096;
            } else isLarge = false;
            insn = objBuffer[arg];
            if (isLarge) {
                curVal.m = insn * mkbs("9..23");
                curVal.m = curVal.m >> 24;
                curVal.m = curVal.m + intZero;
                insn = insn * mkbs("0..8, 24..47") + leftHalf;
            } else {
                curVal.m = intZero + insn * mkbs("33..47");
                insn = insn * mkbs("0..32") + addr;
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
        curVal.i = arg;
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

void endOfLine()
{
    int err, errPos, prevPos, listMode,
    startPos, lastErr;

    extern void OBPROG(Bitset & start, Bitset & fin);

    listMode = PASINFOR.listMode;
    if ((listMode != 0) or (errsInLine != 0)) {
        printf(" %05o%5d%3d%c", (lineStartOffset + PASINFOR.startOffset),
               lineCnt, lineNesting, commentModeCH);
        startPos = 13;
        if (optSflags.m.has(S4)
            and (maxLineLen == 72)
            and (linePos >= 80)) {
            for (err = 73; err <= 80; ++err)
                putchar(lineBufBase[err]);
            putchar(' ');
            linePos = 73;
            startPos = 22;
        }; /* 1106 */
        do
            linePos = linePos-1;
        while ((lineBufBase[linePos]  == ' ') and (linePos != 0));
        for (err = 1; err <= linePos; ++err) {
            putchar(lineBufBase[err]);
        };
        putchar('\n');
        if (errsInLine != 0)  {
            printf("%*s%*c0", startPos, "*****", errMapBase[0], ' ');
            lastErr = errsInLine - 1;
            for (err = 1; err <= lastErr; ++err) {
                errPos = errMapBase[err];
                prevPos = errMapBase[err-1];
                if (errPos != prevPos) {
                    if (prevPos + 1 != errPos) 
                        printf("%*c", (errPos-prevPos-1), ' ');
                    putchar(err + 48);
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
    if (feof(pasinput)) {
        error(errEOFEncountered);
        throw 9999;
    }
} /* endOfLine */

void requiredSymErr(Symbol sym)
{
    if (linePos != prevErrPos)
        error(sym + 88);
} /* requiredSymErr */

void readToPos80()
{
    while (linePos < 81) {
        linePos = linePos + 1;
        lineBufBase[linePos] = PASINPUT;
        if (linePos != 81) PASINPUT = char(getc(pasinput));
    }
    endOfLine();
} /* readToPos80 */

struct inSymbol {
// label    1473, 1, 2, 2175, 2233, 2320;

    unsigned char localBuf[131];
    int tokenLen, tokenIdx;
    bool expSign;
    irptr l3var135z;
    Real expMultiple, expValue;
    char curChar;
    Word numstr[17]; // array [1..16] of Word;
//    l3vars2: array [155..159] of Word;
    int64_t expLiteral;
    int64_t expMagnitude;
    int l3int162z;
    int chord;
    int l3var164z;
    inSymbol();
};

void nextCH()
{
    do {
        atEOL = PASINPUT == '\n';
        CH = PASINPUT;
        PASINPUT = char(getc(pasinput));
        linePos = linePos + 1;
        lineBufBase[linePos] = CH;
    } while (not ((maxLineLen >= linePos) or atEOL));
} /* nextCH */

struct parseComment {
    static parseComment * super;
    bool badOpt, flag;
    char c;
    parseComment();
};

void readOptVal(int & res, int limit)
{
    nextCH();
    res = 0;
    while (('9' >= CH) and (CH >= '0')) {
        res = 10 * res + CH - '0';
        nextCH();
        parseComment::super->badOpt = false;
    }
    if (limit < res) parseComment::super->badOpt = true;
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
            case 'D': {
                readOptVal(curVal.i, 15);
                optSflags.m = optSflags.m * mkbs("0..40") + curVal.m * mkbs("41..47");
            } break;
            case 'Y': readOptFlag(allowCompat);
                break;
            case 'E': readOptFlag(declExternal);
                break;
            case 'U': {
                readOptFlag(flag);
                if (flag) maxLineLen = 72; else maxLineLen = 120;
            }; break;
            case 'S': {
                readOptVal(curVal.i, 9);
                if (curVal.i == 3)
                    lineCnt = 1;
                else if (4 <= curVal.i && curVal.i <= 9)
                  optSflags.m = optSflags.m + mkbs(curVal.i - 3);
                else {
                    extSymAdornment = curVal.i;
                }
            } break;
            case 'F': readOptFlag(checkFortran);
                break;
            case 'L': readOptVal(PASINFOR.listMode, 3);
                break;                
            case 'P': readOptFlag(doPMD);
                break;
            case 'T': readOptFlag(checkBounds);
                break;
            case 'A': readOptVal(charEncoding, 3);
                break;
            case 'C': readOptFlag(checkTypes);
                break;
            case 'R': readOptFlag(fuzzReals);
                break;
            case 'M': readOptFlag(fixMult);
                break;
            case 'B': readOptVal(fileBufSize, 4);
                break;
            case 'K': readOptVal(heapSize, 23);
                break;
            case 'Z': readOptFlag(pseudoZ);
                break;
            }
            if (badOpt)
                error(54); /* errErrorInPseudoComment */
        } while (CH == ',');
    }; /* 1446 */
    do {
        while (CH != '*') {
          c = commentModeCH;
          commentModeCH = '*';
            if (atEOL)
                endOfLine();
            nextCH();
          commentModeCH = c;
        };
        nextCH();
    } while (CH != ')');
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
        if ('\200' < CH) {
            lineBufBase[linePos] = ' ';
            chord = CH;
            for (int jj = 130; jj <= chord; ++jj) {
                linePos = linePos + 1;
                lineBufBase[linePos] = ' ';
            }
            nextCH();
            goto L1473;
        }
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
                    curVal.c = iso2text[CH];
                    nextCH();
                    if (8 >= tokenLen) {
                        tokenLen = tokenLen + 1;
                        curToken.m = curToken.m << 6;
                        curToken.m = curToken.m + curVal.m;
                    }
                } while (chrClassTabBase[CH] == ALNUM);
                // curVal.m = curToken.m * hashMask.m;
                // mapAI(curVal.a, bucket);
                bucket = curVal.m.val % 65535 % 128;
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
                        numstr[tokenLen].i = CH - '0';
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
                        if (7 < numstr[tokenIdx].i)
                            error(20); /* errDigitGreaterThan7 */
                        curToken.m = curToken.m << 3;
                      curToken.m = numstr[tokenIdx].m * mkbs("45..47") +
                        curToken.m;
                    }
                    if (suffix == suffixB) {
                        if (curToken.m * mkbs("0..6") != mkbs("")) {
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
                            numstr[tokenIdx].i;
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
                    }
                    curToken.r = curToken.i;
                    SY = REALCONST;
                    if (charSymTabBase[CH] != INTCONST) 
                        error(56); /* errNeedMantissaAfterDecimal */
                    else
                        do {
                            curToken.r = 10.0*curToken.r + CH - 48;
                            expMagnitude = expMagnitude-1;
                            nextCH();
                        } while (charSymTabBase[CH] == INTCONST);
                } /*2062*/
                if (CH == 'E') {
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
                          L2233:
                            /* with PASINFOR do */ {
/*                                
                                if charEncoding = 3 then {
                                    if (ch < '*') or ('_176' < CH) then
                                        curChar := chr(0)
                                    else {
                                        curChar := iso2text[CH];
                                    }
                                } else if '_176' < CH then {
                                    curChar := CH;
                                } else if charEncoding = 0 then {
                                    curChar := a0@[CH];
                                } else if charEncoding = 1 then {
                                    curChar := a1@[CH];
                                } else if charEncoding = 4 then {
                                    curChar := a4@[CH];
                                } else
*/
                                {                                    
                                    curChar = CH;
                                }
                                localBuf[tokenIdx] = curChar;
                            }
                        }
                    }
                    goto L2175;
                } exitLoop: ;
                strLen = tokenIdx - 6;
                if (strLen == 0) {
                   error(61); /* errEmptyString */
                   strLen = 1;
                   goto L2320;
                } else if (strLen == 1) {
                    SY = CHARCONST;
                    tokenLen = 1;
                    curToken.c = '\0';
                    unpck(localBuf[0], curToken.a);
                    pck(localBuf[tokenLen], curToken.a);
                    goto exitLexer;
                } else {
                  L2320:
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
                }
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
                } else {
                    if (prevSY == ENDSY)
                        dataCheck = true;
                }
            } break;
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

#if 0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure error;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure skipToEnd;
var
    sym: Symbol;
{
    sym := SY;
    while (sym != ENDSY) or (SY != PERIOD) do {
        sym := SY;
        inSymbol
    };
    if CH = 'D' then
        while SY != ENDSY do
            inSymbol;
    goto 9999;
};
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* error */
    errors := true;
    bool110z :=;
    if ((linePos != prevErrPos) and (9 >= errsInLine))
        or (errno = 52)
    then {
        write(' ');
        totalErrors := totalErrors + 1;
        errMapBase[errsInLine] := linePos;
        errsInLine := errsInLine + 1;
        prevErrPos := linePos;
        write('******', errno:0);
        printErrMsg(errno);
        if 60 < totalErrors then {
            writeln;
            endOfLine;
            printErrMsg(53);
            skipToEnd
        }
    }
};
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure skip(toset: setofsys);
{
    while not (SY IN toset) do
        inSymbol;
}; /* skip */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure test1(sym: Symbol; toset: setofsys);
{
    if (SY != sym) then {
        requiredSymErr(sym);
        skip(toset)
    } else
        inSymbol;
}; /* test1 */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure errAndSkip(errno: Integer; toset: setofsys);
{
    error(errno);
    skip(toset)
}; /* errAndSkip */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseLiteral(var litType: tptr; var litValue: Word;
    allowSign: boolean);
label
    99;
var
    l3var1z: operator;
{
    litValue := curToken;
    if (GTSY < SY) then {
        if allowSign and (charClass IN [PLUSOP, MINUSOP]) then {
            l3var1z := charClass;
            inSymbol;
            parseLiteral(litType, litValue, false);
            if (litType != IntegerType) then {
                error(62); /* errIntegerNeeded */
                litType := IntegerType;
                litValue.i := 1;
            } else {
                if (l3var1z = MINUSOP) then
                    litValue.i := -litValue.i;
            };
        } else
99:     {
            litType := NULL;
            error(errNoConstant);
        }
    } else
        case SY of
        IDENT: {
            if (hashTravPtr = NULL) or
               (hashTravPtr->cl != ENUMID) then
                goto 99;
            litType := hashTravPtr->typ;
            litValue.i := hashTravPtr->value;
        };
        INTCONST:
            litType := IntegerType;
        REALCONST:
            litType := realType;
        CHARCONST:
            litType := charType;
        LTSY:
            makeStringType(litType);
        GTSY: {
            litType := pointerType;
            litValue.i := 74000C;
        };
        end /* case */
}; /* parseLiteral */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P2672(var l3arg1z: irptr; l3arg2z: irptr);
var
    l3var1z: boolean;
    l3var2z: Integer;
    l3var3z, l3var4z: irptr;
{
    if l3arg1z = NULL then {
        curVal.m := l3arg2z->id.m * hashMask.m;
        mapAI(curVal.a, l3var2z);
        l3var1z := true;
        l3arg1z := symHashTabBase[l3var2z];
    } else {
        l3var1z := false;
    };
    if (l3arg1z = l3arg2z) then {
        if (l3var1z) then {
            symHashTabBase[l3var2z] :=
                symHashTabBase[l3var2z]->next;
        } else {
            l3arg1z := l3arg2z->next;
        };
    } else {
        l3var3z := l3arg1z;
        while (l3var3z != l3arg2z) do {
            l3var4z := l3var3z;
            if (l3var3z != NULL) then {
                l3var3z := l3var3z->next;
            } else {
                exit
            }
        };
        l3var4z->next := l3arg2z->next;
    }
}; /* P2672 */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function isFileType(typtr: tptr): boolean;
{
    isFileType := (typtr->k = kindFile) or
        (typtr->k = kindRecord) and typtr->flag;
}; /* isFileType */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function knownInType(var rec: irptr): boolean;
{
    if (typelist != NULL) then {
        rec := typelist;
        while (rec != NULL) do {
            if (rec->id = curIdent) then {
                knownInType := true;
                exit
            };
            rec := rec->next;
        }
    };
    knownInType := false;
}; /* knownInType */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure checkSymAndRead(sym: Symbol);
{
    if (SY != sym) then
        requiredSymErr(sym)
    else
        inSymbol
}; /* checkSymAndRead */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function typeCheck(type1, type2: tptr): boolean;
label
    1;
var
    baseMatch: boolean;
    kind1, kind2: kind;
    link: @typechain;
    basetyp1, basetyp2: tptr;
    enums1, enums2: irptr;
    span1, span2: Integer;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure allocWithTypeCheck;
{
    new(link);
    link@ := [chain, basetyp1, basetyp2];
    chain := link;
    typeCheck := typeCheck(basetyp1, basetyp2);
}; /* allocWithTypeCheck */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function checkRecord(l4arg1z, l4arg2z: tptr): boolean;
var
    l4var1z: boolean;
{
    l4var1z := (l4arg1z = NULL) or (l4arg2z = NULL);
    if (l4var1z) then {
        checkRecord := l4arg1z = l4arg2z;
    } else {
        checkRecord := typeCheck(l4arg1z->base, l4arg2z->base) and
                 checkRecord(l4arg1z->next, l4arg2z->next);
    };
}; /* checkRecord */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* typeCheck */
    rangeMismatch := false;
    if (type1->k = kindRange) then {
        typ120z := type1->base;
    } else {
        typ120z := type1;
    };
    if not checkTypes or (type1 = type2) then
1:      typeCheck := true
    else
        with type1@ do {
            kind1 := k;
            kind2 := type2->k;
            if (kind1 = kind2) then {
                case kind1 of
                kindReal:
                    /* empty */;
                kindScalar: {
(chain)             if (type1->numen = type2->numen) then {
                        enums1 := type1->enums;
                        enums2 := type2->enums;
                        while (enums1 != NULL) and (enums2 != NULL) do {
                            if (enums1->id != enums2->id) then
                                exit chain;
                            enums1 := enums1->list;
                            enums2 := enums2->list;
                        };
                        if (enums1 = NULL) and (enums2 = NULL) then
                            goto 1;
                    }
                };
                kindRange: {
                    baseMatch := (type1->base = type2->base);
                    typ120z := type1->base;
                    rangeMismatch := (type1->left != type2->left) or
                                (type1->right != type2->right);
                    typeCheck := baseMatch;
                    exit
                };
                kindPtr: {
                    if (type1 = pointerType) or (type2 = pointerType) then
                        goto 1;
                    basetyp1 := type1->base;
                    basetyp2 := type2->base;
                    if (chain != NULL) then {
                        link := chain;
                        while (link != NULL) do with link@ do {
                            if (type1 = basetyp1) and
                               (type2 = basetyp2) or
                               (type2 = basetyp1) and
                               (type1 = basetyp2) then
                                goto 1;
                            link := next;
                        };
                        allocWithTypeCheck;
                    } else {
                        setup(type1);
                        allocWithTypeCheck;
                        chain := NULL;
                        rollup(type1);
                        exit
                    }
                };
                kindSet:
                    goto 1;
                kindArray: {
                    with type1->range@ do
                        span1 := right - left;
                    with type2->range@ do
                        span2 := right - left;
                    if typeCheck(type1->base, type2->base) and
                       (span1 = span2) and
                       (type1->pck = type2->pck) and
                       not rangeMismatch then {
                        if type1->pck then {
                            if (type1->pcksize = type2->pcksize) then
                                goto 1
                        } else
                            goto 1
                    }
                };
                kindFile: {
                    if typeCheck(type1->base, type2->base) then
                        goto 1;
                };
                kindRecord: {
                    if checkRecord(type1->first, type2->first) then
                        goto 1;
                }
                end /* case */
            } else {
                if (kind1 = kindRange) then {
                    rangeMismatch := true;
                    typ120z := type2;
                    if (type1->base = type2) then
                        goto 1;
                } else if (kind2 = kindRange) and
                          (type1 = type2->base) then
                    goto 1;
            };
            typeCheck := false;
        }
}; /* typeCheck */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function F3307(l3arg1z: irptr): Integer;
var
    l3var1z: Integer;
    l3var2z: irptr;
{
    l3var2z := l3arg1z->argList;
    l3var1z := 0;
    if (l3var2z != NULL) then
        while (l3var2z != l3arg1z) do {
            l3var1z := l3var1z + 1;
            l3var2z := l3var2z->list;
        };
    F3307 := l3var1z;
}; /* F3307 */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function makeNameWithStars(isProc: boolean): Bitset;
var
    wantBoth: boolean;
{
    wantBoth := not isProc and (extSymAdornment = 0);
    if curVal.m * [0..5] = [] then {
        curVal := curVal;
        besm(ASN64-6);
        curVal := ;
        if wantBoth or (extSymAdornment = 1) then
            curVal.m := curVal.m + [44, 46];
        while curVal.m * [0..11] = [] do {
            curVal := curVal;
            besm(ASN64-6);
            curVal := ;
        };
        if curVal.m * [0..5] = [] then {
            if wantBoth then
                curVal.m := [2, 4] + curVal.m
            else {
                curVal := curVal;
                besm(ASN64-6);
                curVal := ;
            }
        }
    };
    makeNameWithStars := curVal.m;
}; /* makeNameWithStars */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure formOperator(l3arg1z: opgen);
var
    l3int1z, l3int2z, l3int3z: Integer;
    nextInsn: Integer;
    l3var5z: eptr;
    flags: opflg;
    l3var7z,
    l3var8z: Word;
    l3bool9z: boolean;
    l3var10z, l3var11z: Word;
    saved: @insnltyp;
    l3bool13z: boolean;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genOneOp;
label
    3556;
var
    insnBufIdx: Integer;
    l4var2z, l4var3z, l4var4z: Integer;
    l4var5z: Word;
    l4inl6z, l4inl7z, l4inl8z: oiptr;
    l4var9z: Integer;
    insnBuf: array [1..200] of Word;
    curInsn: Word;
    tempInsn: Word;
    l4oi212z: oiptr;
    l4var213z: boolean;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P3363;
{
    if l4var213z then
        form1Insn(InsnTemp[XTA])
    else
        form1Insn(KXTA+E1)
}; /* P3363 */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure  addInsnToBuf(insn: Integer);
{
    insnBuf[insnBufIdx].i := insn;
    insnBufIdx := insnBufIdx + 1;
}; /* addInsnToBuf */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure add2InsnsToBuf(insn1, insn2: Integer);
{
    insnBuf[insnBufIdx].i := insn1;
    insnBuf[insnBufIdx+1].i := insn2;
    insnBufIdx := insnBufIdx + 2;
}; /* add2InsnsToBuf */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function F3413: boolean;
{
    l4inl7z := l4inl6z;
    while l4inl7z != NULL do {
        if (l4inl7z->mode = curInsn.i) then {
            F3413 := true;
            while (l4inl7z->code = macro) do {
                l4inl7z := ptr(l4inl7z->offset);
            };
            exit
        } else {
            l4inl7z := l4inl7z->next;
        }
    };
    F3413 := false;
}; /* F3413 */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure addJumpInsn(opcode: Integer);
{
    if not F3413 then {
        new(l4inl7z);
        l4inl7z->next := l4inl6z;
        l4inl7z->mode := curInsn.i;
        l4inl7z->code := 0;
        l4inl7z->offset := 0;
        l4inl6z := l4inl7z;
    };
    addInsnToBuf(macro + opcode + ord(l4inl7z))
}; /* addJumpInsn */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* genOneOp */
    if insnList = NULL
        then exit;
    set145z := set145z + insnList->regsused;
    l4oi212z := insnList->next2;
    l4var9z := 370007B;
    insnBufIdx := 1;
    if l4oi212z = NULL then
        exit;
    l4inl6z := NULL;
    while l4oi212z != NULL do {
        tempInsn.i := l4oi212z->code;
        l4var4z := tempInsn.i -  macro;
        curInsn.i := l4oi212z->offset;
        case l4oi212z->mode of
         0: ;
         1: if arithMode != 1 then {
                addInsnToBuf(370007B);
                arithMode := 1
            };
         2: arithMode := 1;
         3: if arithMode != 2 then {
                addInsnToBuf(InsnTemp[NTR]);
                arithMode := 2;
            };
         4: arithMode := 2;
        end; /* case */
        l4oi212z := l4oi212z->next;
        if l4var4z >= 0 then {
            case l4var4z of
            mcCARD: {
                add2InsnsToBuf(KACX, KAEX+ZERO);
            };
            21: goto 3556;
            0:  addJumpInsn(InsnTemp[UZA]);
            1:  addJumpInsn(InsnTemp[U1A]);
            2: {
                tempInsn.i := curInsn.i mod 4096;
                curInsn.i := curInsn.i div 4096;
                addJumpInsn(InsnTemp[UJ]);
                curInsn.i := tempInsn.i;
3556:           if F3413 then
                    addInsnToBuf(2*macro+ord(l4inl7z))
                else
                    error(206);
            };
            3: {
                 tempInsn.i := curInsn.i mod 4096;
                 curInsn.i := curInsn.i div 4096;
                 l4var213z :=  F3413;
                 l4inl8z := l4inl7z;
                 curInsn.i := tempInsn.i;
                 l4var213z := l4var213z & F3413;
                 if l4var213z then
                    with l4inl7z@ do {
                        code := macro;
                        offset := ord(l4inl8z);
                    }
                else
                    error(207);
            };
            20: addInsnToBuf(3*macro + curInsn.i);
            4: {
                if insnBuf[insnBufIdx-1].m * [21:23, 28:35] = [] then
                    insnBuf[insnBufIdx-1].m := insnBuf[insnBufIdx-1].m + [35]
                else
                    addInsnToBuf(KXTA+SP)
            };
            5:
(blk)       {
                if l4oi212z != NULL then {
                    tempInsn.i := l4oi212z->code;
                    if tempInsn.m * [21:23, 28:35] = [32] then {
                        l4oi212z->code :=
                            tempInsn.i - InsnTemp[XTA] + InsnTemp[XTS];
                        exit blk
                    }
                };
                addInsnToBuf(KATX+SP);
            };
            mcACC2ADDR:  add2InsnsToBuf(KATI+14, KUTC+I14);
            mcMULTI: {
                addInsnToBuf(getHelperProc(12));        /* P/MI */
            };
            mcADDSTK2REG:  add2InsnsToBuf(KWTC+SP, KUTM+
                               indexreg[curInsn.i]);
            mcADDACC2REG:  add2InsnsToBuf(KATI+14, KJADDM+I14 + curInsn.i);
            mcODD: {
                add2InsnsToBuf(KAAX+E1, KAEX+ZERO);
            };
            mcROUND: {
                addInsnToBuf(KADD+REAL05);                /* round */
                add2InsnsToBuf(KNTR+7, KADD+ZERO)
            };
            mcSQRR: {
                add2InsnsToBuf(KATX+SP, KMUL+SP);   /* sqr */
            };
            mcSQRI: {
                add2InsnsToBuf(KATX+SP, KAEX+MULTMASK);   /* sqrint */
                add2InsnsToBuf(KMUL+SP, KYTA+64)
            };
            14: add2InsnsToBuf(indexreg[curInsn.i] + KVTM,
                               KITA + curInsn.i);
            mcMINEL: {
                add2InsnsToBuf(KANX+ZERO, KSUB+PLUS1);   /* minel */
            };
            16: add2InsnsToBuf(InsnTemp[XTA], KATX+SP + curInsn.i);
            17: {
                addInsnToBuf(KXTS);
                add2InsnsToBuf(KATX+SP+1, KUTM+SP + curInsn.i)
            };
            18: add2InsnsToBuf(KVTM+I10, getHelperProc(65)); /* P/B7 */
            mcPOP2ADDR: {
                addInsnToBuf(KVTM+I14);
                add2InsnsToBuf(KXTA+SP, KATX+I14)
            };
            22: {
                add2InsnsToBuf(KVTM+I14, KXTA+I14);
                curVal.i := 40077777C;
                add2InsnsToBuf(allocSymtab(curVal.m) + (KXTS+SP),
                               KAAX+I8 + curInsn.i);
                add2InsnsToBuf(KAEX+SP, KATX+I14)
            };
            end; /* case */
        } else { /* 4003 */
            if 28 in tempInsn.m then {
                addInsnToBuf(getValueOrAllocSymtab(curInsn.i)+tempInsn.i);
            } else {
                curVal.i := curInsn.i mod 32768;
                if curVal.i < 2048 then
                    addInsnToBuf(tempInsn.i + curInsn.i)
                else
(stmt)          if (curVal.i >= 28672) or (curVal.i < 4096) then {
                    addInsnToBuf(
                        allocSymtab((curVal.m + [24])*halfWord)
                        + tempInsn.i - 28672);
                } else {
                    add2InsnsToBuf(getValueOrAllocSymtab(curVal.i)
                                   + InsnTemp[UTC], tempInsn.i);
                }
            }
        }
    }; /* 4037 */
    insnBufIdx := insnBufIdx-1;
    for l4var4z := insnBufIdx downto 1 do {
        curInsn := insnBuf[l4var4z];
        if (curInsn.i = InsnTemp[NTR]) or
           (curInsn.i = 370007B)
        then {
            l4var3z := l4var4z - 1;
            l4var213z := false;
(loop)      if l4var3z < 1 then exit loop else {
                tempInsn.m := insnBuf[l4var3z].m * [28:32];
                if (tempInsn.i = CUTC) or (tempInsn.i = CWTC)
                then {
                    l4var3z := l4var3z-1;
                    goto loop;
                }
            };
/* one Word shorter
(loop)      while l4var3z >= 1 do {
                tempInsn.m := insnBuf[l4var3z].m * [28:32];
                if (tempInsn.i # CUTC) and (tempInsn.i # CWTC)
                then
                    exit loop;
                l4var3z := l4var3z-1;
            };
*/
            l4var3z := l4var3z + 1;
            if (l4var3z != l4var4z) then {
                for l4var2z := l4var4z-1 downto l4var3z do {
                    insnBuf[l4var2z+1] := insnBuf[l4var2z]
                };
            };
            insnBuf[l4var3z] := curInsn;
        }; /* 4103 */
    };
    for l4var4z to insnBufIdx do
(iter)  {
        curInsn := insnBuf[l4var4z];
        tempInsn.m := curInsn.m * [0, 1, 3, 23:32];
        if tempInsn.i = KATX+SP then {
            l4var2z := l4var4z + 1;
            while insnBufIdx + 1 != l4var2z do {
                curVal.m := insnBuf[l4var2z].m * [0, 1, 3, 23, 28:35];
                tempInsn.m := curVal.m * [0, 1, 3, 23, 28:32];
                if curVal.i = InsnTemp[XTA] then {
                    insnBuf[l4var2z].m :=
                        insnBuf[l4var2z].m mod [32, 34, 35];
                    exit iter;
                } else if curVal.i = InsnTemp[ITA] then {
                    insnBuf[l4var2z].m := insnBuf[l4var2z].m + [35];
                    exit iter;
                } else if (curVal.i = InsnTemp[NTR]) or
                    (tempInsn.i = InsnTemp[UTC]) or
                    (tempInsn.i = InsnTemp[WTC]) or
                    (tempInsn.i = InsnTemp[VTM])
                then
                    l4var2z := l4var2z + 1
                else (q) {
                    l4var2z := insnBufIdx + 1;
                }
            };
        }; /* 4150 */
        if curInsn.i = InsnTemp[UTC] then
            exit iter;
        if curInsn.i < macro then {
            form1Insn(curInsn.i);
            tempInsn.m := curInsn.m * [28:32];
            if (tempInsn.i = 3100000C) or /* VJM */
               (tempInsn.i = 0500000C)    /* ELFUN */
            then {
                padToLeft;
                prevOpcode := 1;
            };
            exit iter;
        };
        if (curInsn.i >= 3*macro) then {
            curInsn.i := curInsn.i - (3*macro);
            if curInsn.i >= 4096 then {
                l4var213z := true;
                curInsn.i := curInsn.i - 4096;
            } else {
                l4var213z := false;
            };
            if (curInsn.i = 0) then
                form1Insn(InsnTemp[UZA] + moduleOffset + 2);
            P3363;
            form1Insn(InsnTemp[UJ] + 2 + moduleOffset);
            padToLeft;
            if (curInsn.i != 0) then {
                if (not F3413) then
                    error(211);
                P0715(0, l4inl7z->code);
            };
            l4var213z := not l4var213z;
            P3363;
            padToLeft;
            exit iter
        }; /* 4230 */
        if (curInsn.i >= 2*macro) then {
            l4inl7z := ptr(curInsn.i - (2*macro));
            P0715(0, l4inl7z->code);
            l4inl7z->offset := moduleOffset;
        } else {
            curInsn.i := curInsn.i - macro;
            curVal.m := curInsn.m * [0, 1, 3, 28:32];
            jumpType := curVal.i;
            curVal.m := [0, 1, 3, 33:47] * curInsn.m;
            l4inl7z := ptr(curVal.i);
            formJump(l4inl7z->code);
            jumpType := InsnTemp[UJ];
            exit iter
        }
    }; /* loop */
    insnList := NULL;
    while (l4inl6z != NULL) do {
        with l4inl6z@ do
            if (offset = 0) then {
                jumpTarget := code;
                exit;
            } else
                l4inl6z := next;
    };
    set146z := set146z - set145z;
}; /* genOneOp */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure addToInsnList(insn: Integer);
var elt: oiptr;
{
    new(elt);
    with elt@ do {
        next := NULL;
        mode := 0;
        code := insn;
        offset := 0;
    };
    with insnList@ do {
        if next = NULL then
            next2 := elt
        else
            next->next := elt;
        next := elt
    }
}; /* addToInsnList */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure addInsnAndOffset(insn, l4arg2z: Integer);
{
    addToInsnList(insn);
    insnlist->next->offset := l4arg2z
}; /* addInsnAndOffset */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure addxToInsnList(insn: Integer);
var
    elt: oiptr;
{
    new(elt);
    with elt@ do {
        next := insnList->next2;
        mode := 0;
        code := insn;
        offset := 0;
    };
    if (insnList->next2 = NULL) then {
        insnList->next := elt;
    };
    insnList->next2 := elt;
}; /* addxToInsnList */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure prepLoad;
label
    4545, 4602;
var
    helper, l4int2z, l4int3z: Integer;
    l4typ4z: tptr;
    l4var5z: kind;
    l4st6z: state;
    l4bool7z, l4bool8z, l4bool9z: boolean;
{
    l4typ4z := insnList->typ;
    with insnList@ do {
        case ilm of
        ilCONST: {
            curVal := ilf5;
            if (l4typ4z->size = 1) then
                curVal.i := getFCSToffset;
            addToInsnList(constRegTemplate + curInsnTemplate + curVal.i);
        };
        il1: {
            helper := insnList->ilf7;
            l4int2z := insnList->ilf5.i;
            l4int3z := insnList->ilf6;
            if (15 < helper) then {
                /* empty */
            } else {
                if (helper = 15) then { /* P/CP */
                    addToInsnList(macro + mcACC2ADDR);
                } else {
                    helper := indexreg[insnList->ilf7];
                    if (l4int2z = 0) and (insnList->st = st0) then {
                        addInsnAndOffset(helper + curInsnTemplate,
                                         l4int3z);
                        goto 4602;
                    } else {
                        addToInsnList(helper + InsnTemp[UTC]);
                    }
                }
            };
            l4st6z := insnList->st;
            if l4st6z = st0 then {
                addInsnAndOffset(l4int2z + curInsnTemplate, l4int3z);
            } else {
                l4var5z := l4typ4z->k;
                if (l4var5z < kindSet) or
                   (l4var5z = kindRecord) and (s6 in optSflags.m) then {
                    l4bool7z := true;
                    l4bool8z := typeCheck(l4typ4z, IntegerType);
                } else {
                    l4bool7z := false;
                    l4bool8z := false;
                };
                if l4st6z = st1 then {
                    if (l4int3z != l4int2z) or
                       (helper != 18) or /* P/RC */
                       (l4int2z != 0) then
                        addInsnAndOffset(l4int2z + InsnTemp[XTA],
                                         l4int3z);
                    l4int3z := insnList->shift;
                    l4int2z := insnList->width;
                    l4bool9z := true;
                    helper := l4int3z + l4int2z;
                    if l4bool7z then {
                        if (30 < l4int3z) then {
                            addToInsnList(ASN64-48 + l4int3z);
                            addToInsnList(InsnTemp[YTA]);
                            if (helper = 48) then /* P/RDR */
                                l4bool9z := false;
                        } else {
                            if (l4int3z != 0) then
                                addToInsnList(ASN64 + l4int3z);
                        }; /* 4477 */
                        if l4bool9z then {
                            curVal.m := [(48 - l4int2z)..47];
                            addToInsnList(KAAX+I8 + getFCSToffset);
                        }
                    } else { /* 4511 */
                        if (helper != 48) then
                            addToInsnList(ASN64-48 + helper);
                        curVal.m := [0..(l4int2z-1)];
                        addToInsnList(KAAX+I8 + getFCSToffset);
                    }; /* 4525 */
                    if l4bool8z then
                        addToInsnList(KAEX+ZERO);
                } else { /* 4531 */
                    if l4bool7z then
                        helper := ord(l4bool8z)+74 /* P/LDAR[IN] */
                    else
                        helper := 56; /* P/RR */
                    addToInsnList(getHelperProc(helper));
                    insnList->next->mode := 1;
                }
            };
            goto 4545;
        };
        il2: {
4545:       if bool49z and (l4typ4z = booleanType) and
               (16 in insnList->regsused) then
                addToInsnList(KAEX+E1);
        };
        il3: { /* 4555 */
            if bool49z then
                addInsnAndOffset(macro+20,
                    ord(16 in insnList->regsused)*10000B + insnList->ilf5.i);
        };
        end; /* case */
4602:
    }; /* with */
    with insnList@ do {
        ilm := il2;
        regsused := regsused + [0];
    };
}; /* prepLoad */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P4606;
{
    prepLoad;
    addToInsnList(macro + mcPUSH)
}; /* P4606 */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure setAddrTo(reg: Integer);
label
    4650, 4654;
var
    l4var1z: Word;
    l4int2z, opCode, l4var4z, l4var5z,
    l4var6z, regField: Integer;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P4613;
{
    l4var1z.i := insnList->ilf6;
    l4var1z.i := l4var1z.i mod 32768;
    l4var6z := l4var1z.i
}; /* P4613 */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* setAddrTo */
    with insnList@ do {
        l4int2z := ilf7;
        opCode := InsnTemp[VTM];
        regField := indexreg[reg];
        l4var4z := ilf5.i;
        regsused := regsused + [reg];
        if (ilm = ilCONST) then {
            curVal := ilf5;
            if (typ->size = 1) then
                curVal.i := addCurValToFCST;
            l4var6z := curVal.i;
            l4var5z := 74001B;
            goto 4654;
        } else if (l4int2z = 18) then {
4650:       P4613;
            if (l4var4z = indexreg[1]) then {
                l4var5z := 74003B;
4654:           l4var1z.i := macro * l4var5z + l4var6z;
                l4var6z := allocSymtab(l4var1z.m * [12:47]);
                addToInsnList(regField + opCode + l4var6z);
            } else if (l4var4z != 0) then {
                addInsnAndOffset(l4var4z + InsnTemp[UTC], l4var6z);
                addToInsnList(regField + opCode);
            } else (q) {
                addInsnAndOffset(regField + opCode, l4var6z);
            }
        } else if (l4int2z = 17) then {
            P4613;
            l4var4z := insnList->ilf6;
            l4var5z := insnList->next->code - InsnTemp[UTC];
            if (l4var4z != 0) then {
                l4var1z.i := macro * l4var5z + l4var4z;
                l4var5z := allocSymtab(l4var1z.m * [12:47]);
            };
            insnList->next->code := regField + l4var5z + opCode;
        } else if (l4int2z = 16) then {
            P4613;
            if (l4var4z != 0) then
                addToInsnList(l4var4z + InsnTemp[UTC]);
            addInsnAndOffset(regField + opCode, l4var6z);
        } else if (l4int2z = 15) then {
            addToInsnList(InsnTemp[ATI] + reg);
            opCode := InsnTemp[UTM];
            goto 4650;
        } else {
            addToInsnList(indexreg[l4int2z] + InsnTemp[UTC]);
            goto 4650;
        }
    }; /* with */
    insnList->ilm := il1;
    insnList->ilf7 := reg;
    insnList->ilf6 := 0;
    insnList->ilf5.i := 0;
}; /* setAddrTo */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure prepStore;
var
    l4int1z: Integer;
    l4int2z, l4int3z: Integer;
    l4bool4z, l4bool5z: boolean;
    l4st6z: state;
    l4var7z: kind;
{
    with insnList@ do
        l4int1z := ilf7;
    if (15 < l4int1z) then {
        /* nothing? */
    } else if (l4int1z = 15) then {
        addToInsnList(macro + mcACC2ADDR)
    } else (q) {
        addToInsnList(indexreg[l4int1z] + InsnTemp[UTC]);
        /*=z-*/exit q/*=z+*/
    };
    l4bool4z := 0 in insnList->regsused;
    l4st6z := insnList->st;
    if (l4st6z != st0) or l4bool4z then
        addxToInsnList(macro + mcPUSH);
    if (l4st6z = st0) then {
        if (l4bool4z) then {
            addInsnAndOffset(insnList->ilf5.i + InsnTemp[UTC],
                             insnList->ilf6);
            addToInsnList(macro+mcPOP2ADDR);
        } else {
            addInsnAndOffset(insnList->ilf5.i, insnList->ilf6);
        }
    } else {
        l4var7z := insnList->typ->k;
        l4int1z := insnList->typ->bits;
        l4bool5z := (l4var7z < kindSet) or
                     (l4var7z = kindRecord) and (S6 in optSflags.m);
        if (l4st6z = st1) then {
            l4int2z := insnList->shift;
            l4int3z := l4int2z + insnList->width;
            if l4bool5z then {
                if (l4int2z != 0) then
                    addxToInsnList(ASN64 - l4int2z);
            } else {
                if (l4int3z != 48) then
                    addxToInsnList(ASN64 + 48 - l4int3z);
            };
            addInsnAndOffset(InsnTemp[UTC] + insnList->ilf5.i,
                             insnList->ilf6);
            curVal.m := [0..47] - [(48-l4int3z)..(47 -l4int2z)];
            addInsnAndOffset(macro+22, getFCSToffset);
        } else {
            if not l4bool5z then {
                l4int2z := (insnList->width - l4int1z);
                if (l4int2z != 0) then
                    addxToInsnList(ASN64 - l4int2z);
                addxToInsnList(InsnTemp[YTA]);
                addxToInsnList(ASN64 - l4int1z);
            };
            addToInsnList(getHelperProc(77)); /* "P/STAR" */
            insnList->next->mode := 1;
        }
    }
}; /* prepStore */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P5117(op: operator);
{
    addInsnAndOffset(curFrameRegTemplate, localSize);
    new(curExpr);
    with curExpr@ do
        typ := insnList->typ;
    genOneOp;
    curExpr->op := op;
    curExpr->num1 := localSize;
    localSize := localSize + 1;
    if (l2int21z < localSize) then
        l2int21z := localSize;
}; /* P5117 */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function insnCount: Integer;
var
    cnt: Integer;
    cur: oiptr;
{
    cnt := 0;
    cur := insnList->next2;
    while (cur != NULL) do {
        cur := cur->next;
        cnt := cnt + 1;
    };
    insnCount := cnt;
}; /* insnCount */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genFullExpr(exprToGen: eptr);
label
    7567, 7760, 10075, 10122;
var
    arg1Const, arg2Const: boolean;
    otherIns: @insnltyp;
    arg1Val, arg2Val: Word;
    curOP: operator;
    work: Integer;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P5155;
{
    prepLoad;
    insnList->ilm := il1;
    insnList->st := st0;
    insnList->ilf6 := 0;
    insnList->ilf5.i := 0;
    insnList->ilf7 := 18;
}; /* P5155 */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genDeref;
label
    5220;
var
    l5var1z, l5var2z: Word;
    doPtrCheck: boolean;
{
    doPtrCheck := checkBounds and not (NoPtrCheck in optSflags.m)
               and (curOP = DEREF);
    if not doPtrCheck and (
        (insnList->st = st0) or
        (insnList->st = st1) and
        (insnList->shift = 0))
    then {
        l5var1z.i := insnList->ilf7;
        l5var2z.i := insnList->ilf6;
        if (l5var1z.i = 18) or (l5var1z.i = 16) then {
5220:       addInsnAndOffset((insnList->ilf5.i + InsnTemp[WTC]), l5var2z.i);
        } else {
            if (l5var1z.i = 17) then {
                if (l5var2z.i = 0) then {
                    insnList->next->code := insnList->next->code +
                                                InsnTemp[XTA];
                } else
                    goto 5220;
            } else if (l5var1z.i = 15) then {
                addToInsnList(macro + mcACC2ADDR);
                goto 5220;
            } else (q) {
                addInsnAndOffset((indexreg[l5var1z.i] + InsnTemp[WTC]),
                                 l5var2z.i);
                /*=z-*/exit q/*=z+*/
            }
        }
    } else {
        P5155;
        if (doPtrCheck) then {
            addToInsnList(KVTM+I14 + lineCnt);
            addToInsnList(getHelperProc(7)); /* "P/CA"*/
            insnList->next->mode := 1;
        };
        addToInsnList(macro + mcACC2ADDR);
    };
    insnList->ilf6 := 0;
    insnList->ilf5.i := 0;
    insnList->ilf7 := 16;
}; /* genDeref */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genHelper;
{
    P4606;
    saved := insnList;
    insnList := otherIns;
    prepLoad;
    addToInsnList(getHelperProc(nextInsn));
    insnList->regsused := insnList->regsused + saved->regsused + [11:14];
    saved->next->next := insnList->next2;
    insnList->next2 := saved->next2;
}; /* genHelper */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure prepMultiWord;
var
    l5var1z: boolean;
    l5var2z: @insnltyp;
{
    l5var1z := 12 in otherIns->regsused;
    setAddrTo(12);
    if (l5var1z) then {
        addToInsnList(KITA+12);
        addToInsnList(macro + mcPUSH);
    };
    l5var2z := insnList;
    insnList := otherIns;
    setAddrTo(14);
    if (l5var1z) then {
        addToInsnList(macro + mcPOP);
        addToInsnList(KATI+12);
    };
    l5var2z->regsused := insnList->regsused + l5var2z->regsused;
    l5var2z->next->next := insnList->next2;
    l5var2z->next := insnList->next;
    insnList := l5var2z;
}; /* prepMultiWord */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genCheckBounds(l5arg1z: tptr);
var
    l5var1z: Integer;
    l5var2z, l5var3z, l5var4z: Word;
{
    l5var1z := l5arg1z->checker;
    if (l5var1z = 0) then {
        curVal.i := l5arg1z->left;
        l5var4z.i := l5arg1z->right;
        if (l5arg1z->base != IntegerType) then {
            curVal.m := curVal.m * [7:47];
            l5var4z.m := l5var4z.m * [7:47];
        };
        prevOpcode := 0;
        formAndAlign(KUJ+5 + moduleOffset);
        l5arg1z->checker := moduleOffset;
        l5var1z := moduleOffset;
        P0715(1, l5var4z.i);
        formAndAlign(KUJ+I13);
    };
    prepLoad;
    addToInsnList(KVTM+I14 + lineCnt);
    addToInsnList(KVJM+I13 + l5var1z);
    insnList->next->mode := 1;
}; /* genCheckBounds */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure negateCond;
{
    if (insnList->ilm = ilCONST) then {
        insnList->ilf5.b := not insnList->ilf5.b;
    } else {
        insnList->regsused := insnList->regsused mod [16];
    }
}; /* negateCond */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure tryFlip(commutes: boolean);
label
    100, 22, 33;
var
    l5var1z: Integer;
    l5var2z: @insnltyp;
{
    if not (0 in otherIns->regsused) then {
        l5var1z := 0;
    } else if not (0 in insnList->regsused) then {
        l5var1z := ord(commutes) + 1;
    } else {
        l5var1z := 3;
        /*=z-*/(q) exit q;/*=z+*/
    };
    case l5var1z of
    0:
100: {
        prepLoad;
        saved := insnList;
        insnList := otherIns;
        curInsnTemplate := nextInsn;
        prepLoad;
        curInsnTemplate := InsnTemp[XTA];
    };
    1:
        if (nextInsn = InsnTemp[SUB]) then {
            nextInsn := InsnTemp[RSUB];
            goto 22;
        } else
            goto 33;
   2:
22: {
        saved := insnList;
        insnList := otherIns;
        otherIns := saved;
        goto 100;
    };
    3:
33: {
        prepLoad;
        addToInsnList(indexreg[15] + nextInsn);
        l5var2z := insnList;
        insnList := otherIns;
        P4606;
        saved := insnList;
        insnList := l5var2z;
    };
    end; /* case */
    insnList->next->mode := 0;
    saved->next->next := insnList->next2;
    insnList->next2 := saved->next2;
    insnList->regsused := insnList->regsused + [0];
}; /* tryFlip */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genBoolAnd;
var
    l5var1z, l5var2z: boolean;
    l5var3z, l5var4z, l5var5z, l5var6z, l5var7z: Integer;
    l5ins8z: @insnltyp;
    l5var9z: Word;
{
    if (arg1Const) then {
        if (arg1Val.b) then
            insnList := otherIns;
    } else if (arg2Const) then {
        if (not arg2Val.b) then
            insnList := otherIns;
    } else {
        l5var1z := 16 in insnList->regsused;
        l5var2z := 16 in otherIns->regsused;
        l5var5z := int94z;
        int94z := int94z + 1;
        bool49z := false;
        l5var6z := ord(l5var1z) + macro;
        l5var7z := ord(l5var2z) + macro;
        if (insnList->ilm = il3) then {
            l5var3z := insnList->ilf5.i;
        } else {
            l5var3z := 0;
            prepLoad;
        };
        if (otherIns->ilm = il3) then {
            l5var4z := otherIns->ilf5.i;
        } else {
            l5var4z := 0;
        };
        l5var9z.m := (insnList->regsused + otherIns->regsused);
        if (l5var3z = (0)) then {
            if (l5var4z = (0)) then {
                addInsnAndOffset(l5var6z, l5var5z);
                l5ins8z := insnList;
                insnList := otherIns;
                prepLoad;
                addInsnAndOffset(l5var7z, l5var5z);
            } else {
                if (l5var2z) then {
                    addInsnAndOffset(l5var6z, l5var5z);
                    l5ins8z := insnList;
                    insnList := otherIns;
                    addInsnAndOffset(macro + 2,
                                     10000B * l5var5z + l5var4z);
                } else {
                    addInsnAndOffset(l5var6z, l5var4z);
                    l5var5z := l5var4z;
                    l5ins8z := insnList;
                    insnList := otherIns;
                }
            };
        } else {
            if (l5var4z = (0)) then {
                if (l5var1z) then {
                    addInsnAndOffset(macro + 2,
                                     10000B * l5var5z + l5var3z);
                    l5ins8z := insnList;
                    insnList := otherIns;
                    prepLoad;
                    addInsnAndOffset(l5var7z, l5var5z);
                } else {
                    l5ins8z := insnList;
                    insnList := otherIns;
                    prepLoad;
                    addInsnAndOffset(l5var7z, l5var3z);
                    l5var5z := l5var3z;
                };
            } else {
                if (l5var1z) then {
                    if (l5var2z) then {
                        addInsnAndOffset(macro + 2,
                                         10000B * l5var5z + l5var3z);
                        l5ins8z := insnList;
                        insnList := otherIns;
                        addInsnAndOffset(macro + 2,
                                         10000B * l5var5z + l5var4z);
                    } else {
                        addInsnAndOffset(macro + 2,
                                         10000B * l5var4z + l5var3z);
                        l5ins8z := insnList;
                        insnList := otherIns;
                        l5var5z := l5var4z;
                    }
                } else {
                    l5ins8z := insnList;
                    insnList := otherIns;
                    l5var5z := l5var3z;
                    if (l5var2z) then
                        addInsnAndOffset(macro + 2,
                                         10000B * l5var3z + l5var4z)
                    else
                        addInsnAndOffset(macro + 3,
                                         10000B * l5var3z + l5var4z);
                }
            }
        };
        insnList->regsused := l5var9z.m - [16];
        l5ins8z->next->next := insnList->next2;
        insnList->next2 := l5ins8z->next2;
        insnList->ilm := il3;
        insnList->ilf5.i := l5var5z;
        bool49z := true;
        /*=z-*/exit;/*=z+*/
    }
}; /* genBoolAnd */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genGetElt;
var
    l5var1z, dimCnt, curDim, l5var4z, l5var5z, l5var6z,
        l5var7z, l5var8z: Integer;
    insnCopy: insnltyp;
    copyPtr, l5ins21z: @insnltyp;
    l5var22z, l5var23z: Word;
    l5var24z: boolean;
    l5var25z: boolean;
    l5var26z, l5var27z: tptr;
    l5ilm28z: ilmode;
    l5var29z: eptr;
    getEltInsns: array [1..10] of @insnltyp;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function myminel(l6arg1z: Bitset): Integer;
{
    myminel := minel(l6arg1z);
}; /* myminel */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* genGetElt */
    dimCnt := 0;
    l5var29z := exprToGen;
    while (l5var29z->op = GETELT) do {
        genFullExpr(l5var29z->expr2);
        dimCnt := dimCnt + 1;
        getEltInsns[dimCnt] := insnList;
        l5var29z := l5var29z->expr1;
    };
    genFullExpr(l5var29z);
    l5ins21z := insnList;
    insnCopy := insnList@;
    copyPtr := ref(insnCopy);
    l5var22z.m := set147z;
    for curDim to dimCnt do
       l5var22z.m := l5var22z.m - getEltInsns[curDim]->regsused;
    for curDim := dimCnt downto 1 do {
        l5var26z := insnCopy.typ->base;
        l5var27z := insnCopy.typ->range;
        l5var25z := insnCopy.typ->pck;
        l5var7z := l5var27z->left;
        l5var8z := l5var26z->size;
        if not l5var25z then
            insnCopy.ilf6 := insnCopy.ilf6 - l5var8z * l5var7z;
        insnList := getEltInsns[curDim];
        l5ilm28z := insnList->ilm;
        if (l5ilm28z = ilCONST) then {
            curVal := insnList->ilf5;
            curVal.m := curVal.m +  intZero;
            if (curVal.i < l5var7z) or
               (l5var27z->right < curVal.i) then
                error(29); /* errIndexOutOfBounds */
            if (l5var25z) then {
                l5var4z := curVal.i - l5var7z;
                l5var5z := insnCopy.typ->perWord;
                insnCopy.regsused := insnCopy.regsused + [0];
                insnCopy.ilf6 := l5var4z DIV l5var5z + insnCopy.ilf6;
                l5var6z := (l5var5z-1-l5var4z MOD l5var5z) *
                           insnCopy.typ->pcksize;
                case insnCopy.st of
                st0: insnCopy.shift := l5var6z;
                st1: insnCopy.shift := insnCopy.shift + l5var6z +
                                           insnCopy.typ->bits - 48;
                st2: error(errUsingVarAfterIndexingPackedArray);
                end; /* case */
                insnCopy.width := insnCopy.typ->pcksize;
                insnCopy.st := st1;
            } /* 6116 */ else {
                insnCopy.ilf6 := curVal.i  * l5var26z->size +
                                  insnCopy.ilf6;
            }
        } else { /* 6123*/
            if (checkBounds) then {
                l5var24z := typeCheck(l5var27z, insnList->typ);
                if (rangeMismatch) then
                    genCheckBounds(l5var27z);
            };
            if (l5var8z != 1) then {
                prepLoad;
                if (l5var27z->base = IntegerType) then {
                    l5var4z := KYTA+64;
                } else {
                    l5var4z := KYTA+64-40;
                };
                addToInsnList(insnCopy.typ->perWord);
                insnList->next->mode := 1;
                if (l5var7z >= 0) then
                    addToInsnList(l5var4z)
                else
                    addToInsnList(macro + mcMULTI);
           };
           if (l5ilm28z = il3) or
              (l5ilm28z = il1) and
              (insnList->st != st0) then
               prepLoad;
           l5var23z.m := insnCopy.regsused + insnList->regsused;
           if (not l5var25z) then {
               if (insnCopy.ilf7 = 18) then {
                    if (insnList->ilm = il2) then {
                        insnCopy.ilf7 := 15;
                    } else { /* 6200 */
                        insnCopy.ilf7 := 16;
                        curInsnTemplate := InsnTemp[WTC];
                        prepLoad;
                        curInsnTemplate := InsnTemp[XTA];
                    }; /* 6205 */
                    insnCopy.next := insnList->next;
                    insnCopy.next2 := insnList->next2;
                } else { /* 6211 */
                    if (insnCopy.ilf7 >= 15) then {
                        l5var1z :=  myminel(l5var22z.m);
                        if (0 >= l5var1z) then {
                            l5var1z := myminel(set147z - insnCopy.regsused);
                            if (0 >= l5var1z) then
                                l5var1z := 9;
                        };
                        saved := insnList;
                        insnList := copyPtr;
                        l5var23z.m := l5var23z.m + [l5var1z];
                        if (insnCopy.ilf7 = 15) then {
                            addToInsnList(InsnTemp[ATI] + l5var1z);
                        } else {
                            addToInsnList(indexreg[l5var1z] + InsnTemp[VTM]);
                        };
                        insnCopy.ilf7 := l5var1z;
                        insnCopy.regsused := insnCopy.regsused + [l5var1z];
                        insnList := saved;
                    } else {
                            l5var1z := insnCopy.ilf7;
                    }; /* 6251 */
                    if (l5var1z IN insnList->regsused) then {
                         P4606;
                         insnList->next->next := insnCopy.next2;
                         insnCopy.next2 := insnList->next2;
                         insnList := copyPtr;
                         addInsnAndOffset(macro+mcADDSTK2REG, l5var1z);
                    } else {
                         if (insnList->ilm = il2) then {
                             addInsnAndOffset(macro+mcADDACC2REG, l5var1z);
                         } else {
                             curInsnTemplate := InsnTemp[WTC];
                             prepLoad;
                             curInsnTemplate := InsnTemp[XTA];
                             addToInsnList(indexreg[l5var1z] + InsnTemp[UTM]);
                         };
                         insnCopy.next->next := insnList->next2;
                         insnCopy.next := insnList->next;
                     }
                }; /* 6305 */
           } else { /* 6306 */
                if (insnCopy.st = st0) then {
                    prepLoad;
                    if (l5var7z != 0) then {
                        curVal.i := 0 - l5var7z;
                        if (not typeCheck(insnList->typ, IntegerType)) then
                            curVal.m := curVal.m - intZero;
                        addToInsnList(KADD+I8 + getFCSToffset);
                        insnList->next->mode := 1;
                    };
                    l5var24z := 0 in insnCopy.regsused;
                    if (l5var24z) then
                        addToInsnList(macro + mcPUSH);
                    saved := insnList;
                    insnList := copyPtr;
                    setAddrTo(14);
                    if (l5var24z) then
                        addToInsnList(macro + mcPOP);
                    l5var23z.m := l5var23z.m + [0, 10, 11, 13, 14];
                    insnCopy.st := st2;
                    insnCopy.ilf6 := 0;
                    insnCopy.ilf5.i := 0;
                    insnCopy.width := insnCopy.typ->pcksize;
                    curVal.i := insnCopy.width;
                    if (curVal.i = 24) then
                        curVal.i := 7;
                    curVal := curVal;besm(ASN64-24);curVal:=;
                    addToInsnList(allocSymtab(  /* P/00C */
                        helperNames[76] + curVal.m)+(KVTM+I11));
                    insnCopy.ilf7 := 16;
                    insnCopy.shift := 0;
                    saved->next->next := insnCopy.next2;
                    insnCopy.next2 := saved->next2;
                } else {
                    error(errUsingVarAfterIndexingPackedArray);
                }
            }; /* 6403 */
            insnCopy.regsused := l5var23z.m;
        };
        insnCopy.typ := l5var26z;
    }; /* 6406 */
    insnList := l5ins21z;
    insnList@ := insnCopy;
}; /* genGetElt */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genEntry;
var
    l5exp1z, l5exp2z: eptr;
    l5idr3z, l5idr4z, l5idr5z, l5idr6z: irptr;
    l5bool7z, l5bool8z, l5bool9z, l5bool10z, l5bool11z: boolean;
    l5var12z, l5var13z, l5var14z: Word;
    l5var15z: Integer;
    l5var16z, l5var17z, l5var18z, l5var19z: Word;
    l5inl20z: @insnltyp;
    l5op21z: operator; l5idc22z: idclass;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function allocGlobalObject(l6arg1z: irptr): Integer;
{
    if (l6arg1z->pos = 0) then {
        if (l6arg1z->flags * [20, 21] != []) then {
            curVal := l6arg1z->id;
            curVal.m := makeNameWithStars(true);
            l6arg1z->pos := allocExtSymbol(extSymMask);
        } else {
            l6arg1z->pos := symTabPos;
            putToSymTab([]);
        }
    };
    allocGlobalObject := l6arg1z->pos;
}; /* allocGlobalObject */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure traceEntry(isEntry: boolean);
{
    if not (debugEntry in optSflags.m) then
        exit;
    curVal := l5idr5z->id;
    addToInsnList(KVTM+I10 + addCurValToFCST);
    if (isEntry) then
        addToInsnList(KVTM+I11 + lineCnt);
    addToInsnList(getHelperProc(ord(isEntry) * 22 + 57)); /* P/C(E|I) */
}; /* traceEntry */
%
{ /* genEntry */
    l5exp1z := exprToGen->expr1;
    l5idr5z := exprToGen->id2;
    l5bool7z := (l5idr5z->typ = NULL);
    l5bool9z := (l5idr5z->list = NULL);
    if (l5bool7z) then
        l5var13z.i := 3 else l5var13z.i := 4;
    l5var12z.m := l5idr5z->flags;
    l5bool10z := (21 in l5var12z.m);
    l5bool11z := (24 in l5var12z.m);
    if (l5bool9z) then {
        l5var14z.i := F3307(l5idr5z);
        l5idr6z := l5idr5z->argList;
    } else {
        l5var13z.i := l5var13z.i + 2;
    };
    new(insnList);
    insnList->next2 := NULL;
    insnList->next := NULL;
    insnList->typ := l5idr5z->typ;
    insnList->regsused := (l5idr5z->flags + [7:15]) * [0:8, 10:15];
    insnList->ilm := il2;
    if (l5bool10z) then {
        l5bool8z := not l5bool7z;
        if (checkFortran) then {
            addToInsnList(getHelperProc(92)); /* "P/MF" */
        }
    } else {
        l5bool8z := true;
        if (not l5bool9z) and (l5exp1z != NULL)
            or (l5bool9z) and (l5var14z.i >= 2) then {
            addToInsnList(KUTM+SP + l5var13z.i);
        };
    };
    l5var14z.i := 0;
(loop)
    while l5exp1z != NULL do { /* 6574 */
        l5exp2z := l5exp1z->expr2;
        l5exp1z := l5exp1z->expr1;
        l5op21z := l5exp2z->op;
        l5var14z.i := l5var14z.i + 1;
        l5inl20z := insnList;
        if (l5op21z = PCALL) or (l5op21z = FCALL) then {
            l5idr4z := l5exp2z->id2;
            new(insnList);
            insnList->next2 := NULL;
            insnList->next := NULL;
            insnList->regsused := [];
            set145z := set145z + l5idr4z->flags;
            if (l5idr4z->list != NULL) then {
                addToInsnList(l5idr4z->offset + InsnTemp[XTA] +
                              l5idr4z->value);
                if (l5bool10z) then
                    addToInsnList(getHelperProc(19)); /* "P/EA" */
            } else
(a)         { /* 6636 */
                if (l5idr4z->value = 0) then {
                    if (l5bool10z) and (21 in l5idr4z->flags) then {
                        addToInsnList(allocGlobalObject(l5idr4z) +
                                      (KVTM+I14));
                        addToInsnList(KITA+14);
                        exit a;
                    } else { /* 6651 */
                        l5var16z.i := 0;
                        formJump(l5var16z.i);
                        padToLeft;
                        l5idr4z->value := moduleOffset;
                        l5idr3z := l5idr4z->argList;
                        l5var15z := ord(l5idr4z->typ != NULL);
                        l5var17z.i := F3307(l5idr4z);
                        form3Insn(KVTM+I10+ 4+moduleOffset,
                                  KVTM+I9 + l5var15z,
                                  KVTM+I8 + 74001B);
                        formAndAlign(getHelperProc(62)); /* "P/BP" */
                        l5var15z := l5var17z.i + 2 + l5var15z;
                        form1Insn(KXTA+SP + l5var15z);
                        if ((1) < l5var17z.i) then
                            form1Insn(KUTM+SP + l5var15z)
                        else
                            form1Insn(0);
                        form2Insn(
                            getHelperProc(63/*P/B6*/) + 6437777777300000C,
                            allocGlobalObject(l5idr4z) + KUJ);
                        if (l5idr3z != NULL) then {
                            repeat
                                l5idc22z := l5idr3z->cl;
                                if (l5idc22z = ROUTINEID) and
                                   (l5idr3z->typ != NULL) then
                                    l5idc22z := ENUMID;
                                form2Insn(0, ord(l5idc22z));
                                l5idr3z := l5idr3z->list;
                            until (l5idr4z = l5idr3z);
                        }; /* 6745 */
                        storeObjWord([]);
                        P0715(0, l5var16z.i);
                    }
                }; /* 6752 */
                addToInsnList(KVTM+I14 + l5idr4z->value);
                if 21 in l5idr4z->flags then
                    addToInsnList(KITA+14)
                else
                    addToInsnList(getHelperProc(64)); /* "P/PB" */
            }; /* 6765 */
            if (l5op21z = PCALL) then
                l5idc22z := ROUTINEID
            else
                l5idc22z := ENUMID;
        } else { /* 6772 */
            genFullExpr(l5exp2z);
            if (insnList->ilm = il1) then
                l5idc22z := FORMALID
            else
                l5idc22z := VARID;
        }; /* 7001 */
        if not (not l5bool9z or (l5idc22z != FORMALID) or
               (l5idr6z->cl != VARID)) then
            l5idc22z := VARID;
(loop)      if (l5idc22z = FORMALID) or (l5bool11z) then {
            setAddrTo(14);
            addToInsnList(KITA+14);
        } else if (l5idc22z = VARID) then {
            if (insnList->typ->size != 1) then {
                l5idc22z := FORMALID;
                goto loop;
            } else {
                prepLoad;
                /*=z-*/(q) exit q/*=z+*/
            }
        }; /* 7027 */
        if not l5bool8z then
            addxToInsnList(macro + mcPUSH);
        l5bool8z := false;
        if (l5inl20z->next != NULL) then {
            l5inl20z->next->next := insnList->next2;
            insnList->next2 := l5inl20z->next2;
        };
        insnList->regsused := insnList->regsused + l5inl20z->regsused;
        if not l5bool9z then {
            curVal.cl := l5idc22z;
            addToInsnList(KXTS+I8 + getFCSToffset);
        };
        if l5bool9z and not l5bool11z then
            l5idr6z := l5idr6z->list;
    }; /* while -> 7061 */
    traceEntry(true);
    if l5bool10z then {
        addToInsnList(KNTR+2);
        insnList->next->mode := 4;
    };
    if l5bool9z then {
        addToInsnList(allocGlobalObject(l5idr5z) + (KVJM+I13));
        if (20 in l5idr5z->flags) then {
            l5var17z.i := 1;
        } else {
            l5var17z.i := l5idr5z->offset div 4000000B;
        } /* 7102 */
    } else { /* 7103 */
        l5var15z := 0;
        if (l5var14z.i = 0) then {
            l5var17z.i := l5var13z.i + 1;
        } else {
            l5var17z.i := -(2 * l5var14z.i + l5var13z.i);
            l5var15z := 1;
        }; /* 7115 */
        addInsnAndOffset(macro+16 + l5var15z,
                         getValueOrAllocSymtab(l5var17z.i));
        addToInsnList(l5idr5z->offset + InsnTemp[UTC] + l5idr5z->value);
        addToInsnList(macro+18);
        l5var17z.i := 1;
    }; /* 7132 */
    insnList->next->mode := 2;
    if (curProcNesting != l5var17z.i) then {
        if not l5bool10z then {
            if (l5var17z.i + 1 = curProcNesting) then {
                addToInsnList(KMTJ+I7 + curProcNesting);
            } else {
                l5var15z := frameRestore[curProcNesting][l5var17z.i];
                if (l5var15z = (0)) then {
                    curVal.i := 6017T; /* P/ */
                    l5var19z.i := curProcNesting + 16;
                    besm(ASN64-30);
                    l5var19z := ;
                    l5var18z.i := l5var17z.i + 16;
                    besm(ASN64-24);
                    l5var18z := ;
                    curVal.m := curVal.m + l5var19z.m + l5var18z.m;
                    l5var15z := allocExtSymbol(extSymMask);
                    frameRestore[curProcNesting][l5var17z.i] := l5var15z;
                };
                addToInsnList(KVJM+I13 + l5var15z);
            }
        }
    }; /* 7176 */
    if not l5bool9z or ([20, 21] * l5var12z.m != []) then {
        addToInsnList(KVTM+40074001B);
    };
    set145z := (set145z + l5var12z.m) * [1:15];
    traceEntry(false);
    if l5bool10z then {
        if (not checkFortran) then
            addToInsnList(KNTR+7)
        else
            addToInsnList(getHelperProc(93));    /* "P/FM" */
        insnList->next->mode := 2;
    } else {
        if not l5bool7z then
            addToInsnList(KXTA+SP + l5var13z.i - 1);
    }; /* 7226 */
    if not l5bool7z then {
        insnList->typ := l5idr5z->typ;
        insnList->regsused := insnList->regsused + [0];
        insnList->ilm := il2;
        set146z := set146z - l5var12z.m;
    }
    /* 7237 */
}; /* genEntry */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure startInsnList(l5arg1z: ilmode);
{
    new(insnList);
    insnList->next := NULL;
    insnList->next2 := NULL;
    insnList->typ := exprToGen->typ;
    insnList->regsused := [];
    insnList->ilm := l5arg1z;
    if (l5arg1z = ilCONST) then {
        insnList->ilf5.i := exprToGen->num1;
        insnList->ilf7 := exprToGen->num2;
    } else {
        insnList->st := st0;
        insnList->ilf7 := 18;
        insnList->ilf5.i := curFrameRegTemplate;
        insnList->ilf6 := exprToGen->num1;
    }
}; /* startInsnList */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genCopy;
var
    size: Integer;
{
    size := insnList->typ->size;
    if (size = 1) then {
        saved := insnList;
        insnList := otherIns;
        prepLoad;
        genOneOp;
        insnList := saved;
        prepStore;
        genOneOp;
    } else {
        prepMultiWord;
        genOneOp;
        size := size - 1;
        formAndAlign(KVTM+I13 + getValueOrAllocSymtab(-size));
        work := moduleOffset;
        form2Insn(KUTC+I14 + size, KXTA+I13);
        form3Insn(KUTC+I12 + size, KATX+I13,
                  KVLM+I13 + work);
        set145z := set145z + [12:14];
    }
}; /* genCopy */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genConstDiv;
    function PASDIV(r: real): Word;
        external;
{
    curVal := PASDIV(1/arg2Val.i);
    addToInsnList(KMUL+I8 + getFCSToffset);
}; /* genConstDiv */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genComparison;
label
    7475, 7504, 7514, 7530;
var
    hasEq: boolean;
    l5set2z: Bitset;
    mode, size: Integer;
{
    l3int3z := ord(curOP) - ord(NEOP);
    hasEq := odd(l3int3z);
    if (l3int3z = 6) then {     /* IN */
        if (arg1Const) then {
            if (arg2Const) then {
                insnList->ilf5.b := (arg1Val.i IN arg2Val.m);
            } else {
                l5set2z := [arg1Val.i];
                if (l5set2z = []) then {
                    insnList->ilf5.b := false;
                } else {
                    insnList := otherIns;
                    prepLoad;
                    curVal.m := l5set2z;
                    addToInsnList(KAAX+I8 + getFCSToffset);
                    insnList->ilf5.i := 0;
                    insnList->ilm := il3;
                }
            }; /* 7412 */
        } else { /* 7413 */
            saved := insnList;
            insnList := otherIns;
            otherIns := saved;
            nextInsn := 66;      /* P/IN */
            genHelper;
            insnList->ilm := il2;
        }
    } else { /* 7423 */
        if hasEq then
            l3int3z := l3int3z - 1;
        l2typ13z := insnList->typ;
        curVarKind := l2typ13z->k;
        size := l2typ13z->size;
        if (l2typ13z = realType) then {
            if (fuzzReals) then
                work := 0
            else
                work := 1;
        } else if (curVarKind = kindSet) then
            work := 2
        else if (curVarKind IN [kindScalar, kindRange]) then
            work := 3
        else {
            work := 4;
            /*=z-*/(a) exit a/*=z+*/
        };
        if (size != 1) then {
            prepMultiWord;
            addInsnAndOffset(KVTM+I11, 1 - size);
            addToInsnList(getHelperProc(89 + l3int3z)); /* P/EQ */
            insnList->ilm := il2;
            hasEq := not hasEq;
        } else /* 7471 */ if l3int3z = 0 then {
            if work = 0 then {
                nextInsn := 15;         /* P/CP */
7475:           genHelper;
                insnList->ilm := il2;
            } else { /* 7501 */
                nextInsn := InsnTemp[AEX];
                tryFlip(true);
7504:           insnList->ilm := il3;
                insnList->ilf5.i := 0;
            };
        } else { /* 7510 */
            case work of
            0: { /*7511*/
                nextInsn := 16;         /* P/AB */
                goto 7475;
            };
            1: { /*7513*/
                mode := 3;
7514:           nextInsn := InsnTemp[SUB];
                tryFlip(false);
                insnList->next->mode := mode;
                if mode = 3 then {
                    addToInsnList(KNTR+23B);
                    insnList->next->mode := 2;
                };
                goto 7504;
            };
            2: { /*7527*/
                nextInsn := InsnTemp[AAX];
7530:           prepLoad;
                addToInsnList(KAEX+ALLONES);
                tryFlip(true);
                goto 7504;
            };
            3: { /*7536*/
                mode := 1;
                goto 7514;
            };
            4: { /*7540*/
                nextInsn := InsnTemp[ARX];
                goto 7530;
            };
            end; /* case */
        }; /* 7554 */
        insnList->regsused := insnList->regsused - [16];
        if (hasEq)
            then negateCond;
    }; /* 7562 */
    /* 7562 */
}; /* genComparison */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* genFullExpr */;
    if exprToGen = NULL then
        exit;
7567:
    curOP := exprToGen->op;
    if (curOP < GETELT) then {
        genFullExpr(exprToGen->expr2);
        otherIns := insnList;
        if (curOP = ASSIGNOP) then
            l3bool13z := false;
        genFullExpr(exprToGen->expr1);
        if (curOP = ASSIGNOP) then
            l3bool13z := true;
        if (insnList->ilm = ilCONST) then {
            arg1Const := true;
            arg1Val := insnList->ilf5;
        } else
            arg1Const := false;
        if (otherIns->ilm = ilCONST) then {
            arg2Const := true;
            arg2Val := otherIns->ilf5;
        } else
            arg2Const := false;
        if (curOP IN [NEOP, EQOP, LTOP, GEOP, GTOP, LEOP, INOP]) then {
            genComparison;
        } else { /* 7625 */
            if arg1Const and arg2Const then {
                case curOP of
                MUL:        arg1Val.r := arg1Val.r * arg2Val.r;
                RDIVOP:     arg1Val.r := arg1Val.r / arg2Val.r;
                AMPERS:     arg1Val.b := arg1Val.b and arg2Val.b;
                IDIVOP:     arg1Val.i := arg1Val.i DIV arg2Val.i;
                IMODOP:     arg1Val.i := arg1Val.i MOD arg2Val.i;
                PLUSOP:     arg1Val.r := arg1Val.r + arg2Val.r;
                MINUSOP:    arg1Val.r := arg1Val.r - arg2Val.r;
                OROP:       arg1Val.b := arg1Val.b or arg2Val.b:
                IMULOP:     arg1Val.i := arg1Val.i * arg2Val.i;
                SETAND:     arg1Val.m := arg1Val.m * arg2Val.m;
                SETXOR:     arg1Val.m := arg1Val.m MOD arg2Val.m;
                INTPLUS:    arg1Val.i := arg1Val.i + arg2Val.i;
                INTMINUS:   arg1Val.i := arg1Val.i - arg2Val.i;
                SETOR:      arg1Val.m := arg1Val.m + arg2Val.m;
                IDIVROP:    arg1Val.r := arg1Val.i / arg2Val.i;
                SETSUB:
                    goto 10075;
                NEOP, EQOP, LTOP, GEOP, GTOP, LEOP, INOP,
                badop27, badop30, badop31, MKRANGE, ASSIGNOP:
                    error(200);
                end; /* case 7750 */
                insnList->ilf5 := arg1Val;
            } else { /*7752*/
                l3int3z := opToMode[curOP];
                flags := opFlags[curOP];
                nextInsn := opToInsn[curOP];
                case flags of
                opfCOMM:
7760:               tryFlip(curOP in [MUL, PLUSOP, SETAND, INTPLUS]);
                opfHELP:
                    genHelper;
                opfASSN: {
                    genCopy;
                    exit
                };
                opfAND: {
                    genBoolAnd;
                    exit
                };
                opfOR: {
                    negateCond;
                    saved := insnList;
                    insnList := otherIns;
                    negateCond;
                    otherIns := insnList;
                    insnList := saved;
                    genBoolAnd;
                    negateCond;
                    exit
                };
                opfMOD:
                    if (arg2Const) then {
                        prepLoad;
                        if card(arg2Val.m) = 4 then {
                            curVal.m := [0,1,3,minel(arg2Val.m-intZero)+1..47];
                            addToInsnList(KAAX+I8 +getFCSToffset);
                            l3int3z := 0;
                        } else { /* 10016 */
                            addToInsnList(macro + mcPUSH);
                            genConstDiv;
                            insnList->next->mode := 1;
                            curVal.m := arg2Val.m - [1, 3];
                            addToInsnList(KMUL+I8 + getFCSToffset);
                            addToInsnList(KYTA+64);
                            addToInsnList(KRSUB+SP);
                            l3int3z := 1;
                        } /* 10036 */
                    } else { /* 10037 */
                        genHelper;
                    };
                opfDIV: {
                    if arg2Const then {
                        prepLoad;
                        genConstDiv;
                        l3int3z := 1;
                    } else
                        genHelper;
                };
                opfMULMSK: {
                    if (arg1Const) then {
                        insnList->ilf5.m := arg1Val.m MOD [1, 3];
                    } else {
                        if (arg2Const) then {
                            otherIns->ilf5.m := arg2Val.m MOD [1, 3];
                        } else (q) {
                            prepLoad;
                            addToInsnList(KAEX+MULTMASK);
                            /*=z-*/exit q/*=z+*/
                        }
                    };
                    tryFlip(true);
                    insnList->next->mode := 1;
                    if (fixMult) then
                        addToInsnList(macro + mcMULTI)
                    else
                        addToInsnList(KYTA+64);
                };
                opfINV: {
10075:              saved := insnList;
                    insnList := otherIns;
                    otherIns := saved;
                    prepLoad;
                    addToInsnList(KAEX+ALLONES);
                    goto 7760
                }
                end; /* case 10122 */
10122:          insnList->next->mode := l3int3z;
            }
        }
    } else { /* 10125 */
        if (FILEPTR >= curOP) then {
            if (curOP = GETVAR) then {
                new(insnList);
                curIdRec := exprToGen->id1;
                with insnList@ do {
                    next := NULL;
                    next2 := NULL;
                    regsused := [];
                    ilm := il1;
                    ilf5.i := curIdRec->offset;
                    ilf6 := curIdRec->high.i;
                    st := st0;
                    ilf7 := 18;
                };
                if (curIdRec->cl = FORMALID) then {
                    genDeref;
                } else if (curIdRec->cl = ROUTINEID) then {
                    insnList->ilf6 := 3;
                    insnList->ilf5.i := (insnList->ilf5.i + frameRegTemplate);
                } else if (insnList->ilf6 >= 74000B) then {
                    addToInsnList(InsnTemp[UTC] + insnList->ilf6);
                    insnList->ilf6 := 0;
                    insnList->ilf7 := 17;
                    insnList->ilf5.i := 0;
                }
            } else /* 10171 */
            if (curOP = GETFIELD) then {
                genFullExpr(exprToGen->expr1);
                curIdRec := exprToGen->id2;
                with insnList@ do {
                    ilf6 := ilf6 + curIdRec->offset;
                    if (curIdRec->pckfield) then {
                        case st of
                        st0:
                            shift := curIdRec->shift;
                        st1: {
                            shift := shift + curIdRec->shift;
                            if not (S6 IN optSflags.m) then
                                shift := shift +
                                           curIdRec->uptype->bits - 48;
                        };
                        st2:
                            if (not l3bool13z) then
                                error(errUsingVarAfterIndexingPackedArray)
                            else {
                                P5155;
                                insnList->shift := curIdRec->shift;
                            }
                        end; /* 10235*/
                        insnList->width := curIdRec->width;
                        insnList->st := st1;
                        insnList->regsused := insnList->regsused + [0];
                    }
                };
            } else /* 10244 */
            if (curOP = GETELT) then
                genGetElt
            else
            if (curOP = DEREF) or (curOP = FILEPTR) then {
                genFullExpr(exprToGen->expr1);
                genDeref;
            } else
            if (curOP = op36) then {
                startInsnList(il1);
            } else
            if (curOP = op37) then {
                startInsnList(il1);
                genDeref;
            } else
            if (curOP = GETENUM) then
                startInsnList(ilCONST)
            /*=z-*/else ;/*=z+*/
        } else /* 10272 */
        if (curOP = ALNUM) then
            genEntry
        else if (curOP IN [BOUNDS..RNEGOP]) then {
            genFullExpr(exprToGen->expr1);
            if (insnList->ilm = ilCONST) then {
                arg1Val := insnList->ilf5;
                case curOP of
                BOUNDS: {
                    arg2Val.m := [0,1,3] + arg1Val.m;
                    with exprToGen->typ2@ do {
                        if (arg2Val.i < left) or
                           (right < arg2Val.i) then
                            error(errNeedOtherTypesOfOperands)
                    }
                };
                TOREAL: arg1Val.r := arg1Val.i;
                NOTOP:  arg1Val.b := not arg1Val.b;
                RNEGOP: arg1Val.r := -arg1Val.r;
                INEGOP: arg1Val.i := -arg1Val.i;
                end; /* case 10345 */
                insnList->ilf5 := arg1Val;
            } else /* 10347 */
            if (curOP = NOTOP) then {
                negateCond;
            } else {
                prepLoad;
                if (curOP = BOUNDS) then {
                    if (checkBounds) then
                        genCheckBounds(exprToGen->typ2);
                } else if (curOP = TOREAL) then {
                    addToInsnList(InsnTemp[AVX]);
                    l3int3z := 3;
                    goto 10122;
                } else {
                    addToInsnList(KAVX+MINUS1);
                    if (curOP = RNEGOP) then
                        l3int3z := 3
                    else
                        l3int3z := 1;
                    goto 10122;
                }
            }
        } else /* 10376 */
        if (curOP = STANDPROC) then {
            genFullExpr(exprToGen->expr1);
            work := exprToGen->num2;
            if (100 < work) then {
                prepLoad;
                addToInsnList(getHelperProc(work - 100));
            } else {
                if (insnList->ilm = ilCONST) then {
                    arg1Const := true;
                    arg1Val := insnList->ilf5;
                } else
                    arg1Const := false;
                arg2Const := (insnList->typ = realType);
                if (arg1Const) then {
                    case work of
                    fnSQRT:  arg1Val.r := sqrt(arg1Val.r);
                    fnSIN:   arg1Val.r := sin(arg1Val.r);
                    fnCOS:   arg1Val.r := cos(arg1Val.r);
                    fnATAN:  arg1Val.r := arctan(arg1Val.r);
                    fnASIN:  arg1Val.r := arcsin(arg1Val.r);
                    fnLN:    arg1Val.r := ln(arg1Val.r);
                    fnEXP:   arg1Val.r := exp(arg1Val.r);
                    fnABS:   arg1Val.r := abs(arg1Val.r);
                    fnTRUNC: arg1Val.i := trunc(arg1Val.r);
                    fnODD:   arg1Val.b := odd(arg1Val.i);
                    fnORD:   arg1Val.i := ord(arg1Val.c);
                    fnCHR:   arg1Val.c := chr(arg1Val.i);
                    fnSUCC:  arg1Val.c := succ(arg1Val.c);
                    fnPRED:  arg1Val.c := pred(arg1Val.c);
                    fnPTR:   arg1Val.c := chr(arg1Val.i);
                    fnSQR:   arg1Val.r := arg1Val.r*arg1Val.r;
                    fnROUND: arg1Val.i := round(arg1Val.r);
                    fnCARD:  arg1Val.i := card(arg1Val.m);
                    fnMINEL: arg1Val.i := minel(arg1Val.m);
                    fnABSI:  arg1Val.i := abs(arg1Val.i);
                    fnSQRI:  arg1Val.i := arg1Val.i*arg1Val.i;
                    fnEOF,
                    fnREF,
                    fnEOLN:
                        error(201);
                    end; /* 10546 */
                    insnList->ilf5 := arg1Val;
                } else /* 10550 */
                if (work >= fnEOF) and (fnEOLN >= work) then {
                    if (work = fnREF) then {
                        setAddrTo(14);
                        addToInsnList(KITA+14);
                    } else {
                        setAddrTo(12);
                        addToInsnList(getHelperProc(work - 6));
                    };
                    with insnList@ do {
                        ilm := il2;
                        regsused := regsused + [0];
                    }
                } else {
                    prepLoad;
                    if (work = fnTRUNC) then {
                        l3int3z := 2;
                        addToInsnList(getHelperProc(58)); /*"P/TR"*/
                        goto 10122;
                    };
                    if (work IN [fnSQRT:fnEXP,
                                 fnODD:fnSUCC, fnCARD, fnPTR]) then {
                        l3int3z := 0;
                    } else if (work IN [fnABS, fnSQR]) then
                        l3int3z := 3
                    else {
                        l3int3z := 1;
                        /*=z-*/(q) exit q/*=z+*/
                    };
                    addToInsnList(funcInsn[work]);
                    goto 10122;
                }
            }
        } else { /* 10621 */
            if (curOP = NOOP) then {
                curVal := exprToGen->val;
                if (curVal.i IN set146z) then {
                    new(insnList);
                    with insnList@ do {
                        typ := exprToGen->expr2->typ;
                        next := NULL;
                        next2 := ;
                        regsused := [];
                        ilm := il1;
                        ilf7 := 18;
                        ilf5.i := indexreg[curVal.i];
                        ilf6 := 0;
                        st := st0;
                    }
                } else {
                    curVal.i := 14;
                    exprToGen->val := curVal;
                    exprToGen := exprToGen->expr2;
                    goto 7567;
                };
                exit
            } else (q) {
                error(220);
                /*=z-*/exit q/*=z+*/
            }
        };
    }; /* 10654 */
    insnList->typ := exprToGen->typ;
    /* 10656 */
}; /* genFullExpr */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure formFileInit;
var l4exf1z: @extfilerec;
    l4var2z: tptr;
    l4var3z: irptr;
    l4int4z, l4int5z: Integer;
{
    if (S5 IN optSflags.m) then {
        formAndAlign(KUJ+I13);
        exit
    };
    form2Insn(KITS+13, KATX+SP);
    while (curExpr != NULL) do {
        l4exf1z := ptr(ord(curExpr->typ));
        l4var3z := curExpr->id2;
        l4int4z := l4var3z->value;
        l4var2z := l4var3z->typ->base;
        l4int5z := l4var3z->typ->elsize;
        if (l4int4z < 74000B) then {
            form1Insn(getValueOrAllocSymtab(l4int4z) +
                      InsnTemp[UTC] + I7);
            l4int4z := 0;
        };
        form3Insn(KVTM+I12 + l4int4z, KVTM+I10 + fileBufSize,
                  KVTM+I9 + l4int5z);
        form1Insn(KVTM+I11 + l4var2z->size);
        if (l4exf1z = NULL) then {
            form1Insn(InsnTemp[XTA]);
        } else {
            curVal.i := l4exf1z->location;
            if (curVal.i = 512) then
                curVal.i := l4exf1z->offset;
            form1Insn(KXTA+I8 + getFCSToffset);
        };
        formAndAlign(getHelperProc(69)); /*"P/CO"*/
        curVal := l4var3z->id;
        form2Insn(KXTA+I8+getFCSToffset, KATX+I12+26);
        if (l4int5z != 0) and
           typeCheck(l4var2z, IntegerType) then
            form2Insn(KXTA+ZERO, KATX+I12+25);
        curExpr := curExpr->expr1;
    };
    form1Insn(getHelperProc(70)/*"P/IT"*/ + (-I13-100000B));
    padToLeft;
}; /* formFileInit */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* formOperator */
    l3bool13z := true;
    if (errors and (l3arg1z != SETREG)) or (curExpr = NULL) then
        exit;
    if not (l3arg1z IN [gen3, gen6, gen9, gen14, gen16]) then
        genFullExpr(curExpr);
    case l3arg1z of
    gen7: genOneOp;
    SETREG: {
        with insnList@ do {
            l3int3z := insnCount;
            new(l3var5z);
            l3var5z->expr1 := expr63z;
            expr63z := l3var5z;
            l3var5z->op := NOOP;
            case st of
            st0: {
                if (l3int3z = 0) then {
                    l3int2z := 14;
                } else {
                    l3var10z.m := set148z * set147z;
                    if (l3var10z.m != []) then {
                        l3int2z := minel(l3var10z.m);
                    } else {
                        l3int2z := 14;
                    };
                    if (l3int3z != 1) then {
                        setAddrTo(l3int2z);
                        addToInsnList(KITA + l3int2z);
                        P5117(op37);
                    } else if (l3int2z != 14) then {
                        setAddrTo(l3int2z);
                        genOneOp;
                    };
                    l3var11z.m := [l3int2z] - [14];
                    set145z := set145z - l3var11z.m;
                    set147z := set147z - l3var11z.m;
                    set146z := set146z + l3var11z.m;
                };
                curVal.i := l3int2z;
                l3var5z->val := curVal;
            };
            st1: {
                curVal.i := 14;
                l3var5z->val := curVal;
            };
            st2:
                error(errVarTooComplex);
            end; /* case */
        }; /* with */
        l3var5z->expr2 := curExpr;
    }; /* SETREG */
    gen0: {
        prepLoad;
        if (insnCount > 1) then
            P5117(op36)
    };
    STORE: {
        prepStore;
        genOneOp
    };
    gen3: {
        curInsnTemplate := curVal.i;
        formOperator(LOAD);
        curInsnTemplate := InsnTemp[XTA];
    };
    gen5: {
        if (insnList->st != st0) then
            error(errVarTooComplex);
        setAddrTo(9);
        genOneOp;
    };
    gen6: {
        l3int1z := curVal.i;
        genFullExpr(curExpr);
        prepLoad;
        if (9 IN insnList->regsused) then
            error(errVarTooComplex);
        genOneOp;
        form1Insn(KATX+I9 + l3int1z);
    };
    gen8: {
        setAddrTo(12);
        genOneOp
    };
    gen9: {
        curVal.m := curVal.m + intZero;
        form1Insn(KXTA+I8 + getFCSToffset);
    };
    gen10: {
        prepLoad;
        addxToInsnList(macro + mcPUSH);
        genOneOp;
    };
    gen11, gen12: {
        setAddrTo(11);
        if (l3arg1z = gen12) then
            addxToInsnList(macro + mcPUSH);
        genOneOp;
        set145z := set145z + [12];
    };
    FILEACCESS: {
        setAddrTo(12);
        genOneOp;
        formAndAlign(jumpTarget);
    };
    gen14:
        formFileInit;
    LOAD: {
        prepLoad;
        genOneOp
    };
    gen15:
        with insnList@ do {
            l3bool9z := jumpTarget = 0;
            l3int3z := jumpTarget;
            if (ilm = ilCONST) then {
                if (ilf5.b) then {
                    jumpTarget := 0;
                } else {
                    if (l3bool9z) then {
                        formJump(jumpTarget);
                    } else {
                        form1Insn(InsnTemp[UJ] + jumpTarget);
                    }
                }
            } else {
                l3var8z.b := (16 in insnList->regsused);
                if (insnList->ilm = il3) and
                   (insnList->ilf5.i != 0) then {
                    genOneOp;
                    if (l3var8z.b) then {
                        if (l3bool9z) then
                            formJump(l3int3z)
                        else
                            form1Insn(InsnTemp[UJ] + l3int3z);
                        P0715(0, jumpTarget);
                        jumpTarget := l3int3z;
                    } else {
                        if (not l3bool9z) then {
                            if (not putLeft) then
                                padToLeft;
                            P0715(l3int3z, jumpTarget);
                        }
                    };
                } else {
                    if (insnList->ilm = il1) then {
                        bool49z := false;
                        prepLoad;
                        bool49z := true;
                    };
                    genOneOp;
                    if (l3var8z.b) then
                        nextInsn := InsnTemp[U1A]
                    else
                        nextInsn := InsnTemp[UZA];
                    if (l3bool9z) then {
                        jumpType := nextInsn;
                        formJump(l3int3z);
                        jumpType := InsnTemp[UJ];
                        jumpTarget := l3int3z;
                    } else {
                        form1Insn(nextInsn + l3int3z);
                    }
                }
            }
        }; /* gen15 */
    gen16: {
        l3var5z := curExpr;
        curExpr := curExpr->expr1;
        formOperator(gen11);
        genFullExpr(l3var5z->expr2);
        if (11 IN insnList->regsused) then
            error(44); /* errIncorrectUsageOfStandProcOrFunc */
        setAddrTo(12);
        genOneOp;
        arg1Type := l3var5z->expr2->typ;
        with arg1Type->range@ do
            l3int3z := right - left + 1;
        form2Insn((KVTM+I14) + l3int3z,
                  (KVTM+I10+64) - arg1Type->pcksize);
        l3int3z := ord(l3var5z->typ);
        l3int1z := arg1Type->perWord;
        if (l3int3z = 72) then          /* P/KC */
            l3int1z := 1 - l3int1z;
        form1Insn(getValueOrAllocSymtab(l3int1z) + (KVTM+I9));
        if typeCheck(curExpr->typ, IntegerType) then {
            l3int1z := KXTA+ZERO;
        } else {
            l3int1z := InsnTemp[XTA];
        };
        form1Insn(l3int1z);
        formAndAlign(getHelperProc(l3int3z));
   };
   LITINSN: {
        with insnList@ do {
            if (ilm != ilCONST) then
                error(errNoConstant);
            if (insnList->typ->size != 1) then
                error(errConstOfOtherTypeNeeded);
            curVal := insnList->ilf5;
        }
    };
    end; /* case */
}; /* formOperator */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseTypeRef(var newtype: tptr; skipTarget: setofsys);
label
    12247, 12366, 12476, 12760, 13020;
type
    pair = record
            first, second: Integer
        end;
    pair7 = array [1..7] of pair;
    caserec = record
            size, count: Integer;
            pairs: pair7;
        end;
var
    isPacked: boolean;
    cond: boolean;
    cases: caserec;
    leftBound, rightBound: Word;
    numBits, l3int22z, span: Integer;
    curEnum, curField: irptr;
    l3typ26z, nestedType, tempType, curType: tptr;
    l3unu30z: Word;
    l3idr31z: irptr;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure definePtrType(toType: tptr);
{
    new(curType = 4);
    curType@ := [1, 15, kindPtr, toType];
    new(curEnum = 5);
    curEnum@ := [curIdent, lineCnt, typelist, curType, TYPEID];
    typelist := curEnum;
}; /* definePtrType */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseRecordDecl(rectype: tptr; isOuterDecl: boolean);
var
    l4typ1z, selType, l4var3z, l4var4z, l4var5z: tptr;
    l4var6z: irptr;
    l4var7z, l4var8z: Word;
    l4var9z: Integer;
    cases1, cases2: caserec;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure addFieldToHash;
{
    curEnum@ := [curIdent, , typeHashTabBase[bucket], ,
                    FIELDID, NULL, curType, isPacked];
    typeHashTabBase[bucket] := curEnum;
}; /* addFieldToHash */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure packFields;
label
    11523, 11622;
var
    l5var1z, pairIdx, l5var3z, l5var4z, l5var5z: Integer;
    l5var6z: @pair;
{
    parseTypeRef(selType, skipTarget + [CASESY]);
    if (curType->ptr2 = NULL) then {
        curType->ptr2 := curField;
    } else {
        l3idr31z->list := curField;
    };
    cond := isFileType(selType);
    if (not isOuterDecl) and cond then
        error(errTypeMustNotBeFile);
    curType->flag := cond or curType->flag;
    l3idr31z := curEnum;
    repeat
        curField->typ := selType;
(q)     if (isPacked) then {
            l5var1z := selType->bits;
            curField->width := l5var1z;
            if (l5var1z != 48) then {
                for pairIdx to cases.count do
11523:          {
                    l5var6z := ref(cases.pairs[pairIdx]);
                    if (l5var6z->first >= l5var1z) then {
                        curField->shift := 48 - l5var6z->first;
                        curField->offset := l5var6z->second;
                        if not (S6 IN optSflags.m) then
                            curField->shift := 48 - curField->width -
                                                  curField->shift;
                        l5var6z->first := l5var6z->first - l5var1z;
                        if l5var6z->first = 0 then {
                            cases.pairs[pairIdx] :=
                                cases.pairs[cases.count];
                            cases.count := cases.count - 1;
                        }; /* 11562 */
                        goto 11622;
                    }
                }; /* 11564 */
                if (cases.count != 7) then {
                    cases.count := cases.count + 1;
                    pairIdx := cases.count;
                } else {
                    l5var3z := 48;
                    for l5var4z to 7 do {
                        l5var5z := cases.pairs[l5var4z].first;
                        if (l5var5z < l5var3z) then {
                            l5var3z := l5var5z;
                            pairIdx := l5var4z;
                        }
                    }; /* for */
                }; /* 11606 */
                cases.pairs[pairIdx] := [48, cases.size];
                cases.size := cases.size + 1;
                goto 11523;
            }
        }; /* 11615 */
        curField->pckfield := false;
        curField->offset := cases.size;
        cases.size := cases.size + selType->size;
11622:
        if (PASINFOR.listMode = 3) then {
            write(' ':16);
            if (curField->pckfield) then
                write('PACKED');
            write(' FIELD ');
            printTextWord(curField->id);
            write('.OFFSET=', curField->offset:5 oct, 'B');
            if (curField->pckfield) then {
                write('.<<=SHIFT=', curField->shift:2,
                      '. WIDTH=', curField->width:2, ' BITS');
            } else {
                write('.WORDS=', selType->size:0);
            };
            writeLN;
        };
        cond := (curField = curEnum);
        curField := curField->list;
    until cond;
    /* 11674 */
}; /* packFields */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* parseRecordDecl */
    int93z := 3;
    inSymbol;
    /*11702*/
    while (SY = IDENT) do {
        l4var6z := NULL;
        repeat
            if (SY != IDENT) then {
                error(errNoIdent);
            } else {
                if (hashTravPtr != NULL) then
                    error(errIdentAlreadyDefined);
                new(curEnum = 10);
                addFieldToHash;
                if (l4var6z = NULL) then {
                    curField := curEnum;
                } else {
                    l4var6z->list := curEnum;
                };
                l4var6z := curEnum;
                int93z := 3;
                inSymbol;
            };
            cond := (SY != COMMA);
            if (not cond) then {
                int93z := 3;
                inSymbol;
            }
        until cond;
        checkSymAndRead(COLON);
        packFields;
        if (SY = SEMICOLON) then {
            int93z := 3;
            inSymbol;
        }
    }; /*11752*/
    if (SY = CASESY) then {
        int93z := 3;
        inSymbol;
        selType := IntegerType;
(identif)
        if (SY != IDENT) then {
            error(3);
            skip(skipTarget + [OFSY]);
        } else { /* 11766 */
            l4var8z := curIdent;
            l4var9z := bucket;
            curEnum := hashTravPtr;
            inSymbol;
            if (SY = COLON) then {
                if (curEnum != NULL) then
                    error(errIdentAlreadyDefined);
                new(curEnum = 10);
                curIdent := l4var8z;
                bucket := l4var9z;
                addFieldToHash;
                inSymbol;
                curField := curEnum;
                packFields;
            } else {
                curEnum := symHashTabBase[l4var9z];
                while (curEnum != NULL) do {
                    if (curEnum->id != l4var8z) then {
                        curEnum := curEnum->next;
                    } else {
                        if (curEnum->cl != TYPEID) then {
                            error(errNotAType);
                            selType := IntegerType;
                        } else {
                            selType := curEnum->typ;
                        };
                        exit identif;
                    };
                };
                error(errNotDefined)
            };
        }; /* 12035 */
        if (selType->k = kindRange) then
            selType := selType->base;
        checkSymAndRead(OFSY);
        cases1 := cases;
        cases2 := cases;
        l4typ1z := NULL;
        repeat
            l4var3z := NULL;
            repeat
                parseLiteral(l4var4z, l4var7z, false);
                if (l4var4z = NULL) then
                    error(errNoConstant)
                else if (not typeCheck(l4var4z, selType)) then
                    error(errConstOfOtherTypeNeeded);
                new(l4var5z = 7);
                l4var5z@ := [cases.size, 48, kindCases,
                                    l4var7z, NULL, NULL, NULL];
                if (l4var3z = NULL) then {
                    tempType := l4var5z;
                } else {
                    l4var3z->r6 := l4var5z;
                };
                l4var3z := l4var5z;
                inSymbol;
                cond := (SY != COMMA);
                if (not cond) then
                    inSymbol;
            until cond;
            if (l4typ1z = NULL) then {
                if (curType->base = NULL) then {
                    curType->base := tempType;
                } else {
                    rectype->first := tempType;
                }
            } else {
                l4typ1z->next := tempType;
            };
            l4typ1z := tempType;
            checkSymAndRead(COLON);
            if (SY != LPAREN) then
                requiredSymErr(LPAREN);
            parseRecordDecl(tempType, false);
            if (cases2.size < cases.size) or
               isPacked and (cases.size = 1) and (cases2.size = 1) and
                (cases.count = 1) and (cases2.count = 1) and
                (cases.pairs[1].first < cases2.pairs[1].first) then {
                cases2 := cases;
            }; /* 12201 */
            cases := cases1;
            checkSymAndRead(RPAREN);
            cond := SY != SEMICOLON;
            if (not cond) then
                inSymbol;
            if (SY = ENDSY) then
                cond := true;
        until cond;
        cases := cases2;
    }; /* 12232 */
    rectype->size := cases.size;
    if isPacked and (cases.size = 1) and (cases.count = 1) then {
        rectype->bits := 48 - cases.pairs[1].first;
    }
    /* 12242 */
}; /* parseRecordDecl*/
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* parseTypeRef */
    isPacked := false;
12247:
    if (SY = LPAREN) then {
        span := 0;
        int93z := 0;
        inSymbol;
        curField := NULL;
        new(curType = 6);
        while (SY = IDENT) do {
            if (isDefined) then
                error(errIdentAlreadyDefined);
            new(curEnum = 7);
            curEnum@ := [curIdent, curFrameRegTemplate,
                            symHashTabBase[bucket], curType,
                            ENUMID, NULL, ptr(span)];
            symHashTabBase[bucket] := curEnum;
            span := span + 1;
            if (curField = NULL) then {
                curType->enums := curEnum;
            } else {
                curField->list := curEnum;
            };
            curField := curEnum;
            inSymbol;
            if (SY = COMMA) then {
                int93z := 0;
                inSymbol;
            } else {
                if (SY != RPAREN) then
                    requiredSymErr(RPAREN);
            };
        }; /* 12324 */
        checkSymAndRead(RPAREN);
        if (curField = NULL) then {
            curType := booleanType;
            error(errNoIdent);
        } else {
            curType@ := [1, nrOfBits(span - 1), kindScalar, ,
                          span, 0];
        };
    } else /* 12344 */
    if (SY = ARROW) then {
        inSymbol;
        if (SY != IDENT) then {
            error(errNoIdent);
            curType := pointerType;
        } else {
            if (hashTravPtr = NULL) then {
                if (inTypeDef) then {
                    if (knownInType(curEnum)) then {
                        curType := curEnum->typ;
                    } else {
                        definePtrType(IntegerType);
                    };
                } else {
12366:              error(errNotAType);
                    curType := pointerType;
                };
            } else {
                if (hashTravPtr->cl != TYPEID) then {
                    goto 12366
                };
                new(curType = 4);
                with curType@ do {
                    size := 1;
                    bits := 15;
                    k := kindPtr;
                    base := hashTravPtr->typ;
                }
            }; /* 12405 */
            inSymbol;
        }
    } else /* 12410 */
    if (SY = IDENT) then {
        if (hashTravPtr != NULL) then {
            if (hashTravPtr->cl = TYPEID) then {
                curType := hashTravPtr->typ;
            } else {
                goto 12760;
            }
        } else {
            if (inTypeDef) then {
                if (knownInType(curEnum)) then {
                    curType := curEnum->typ;
                    curType->base := booleanType;
                } else {
                    definePtrType(booleanType);
                };
            } else {
                error(errNotAType);
                curType := IntegerType;
            };
        };
        inSymbol;
    } else { /* 12440 */
        if (SY = PACKEDSY) then {
            isPacked := true;
            inSymbol;
            goto 12247;
        };
        if (SY = RECORDSY) then { /* 12446 */
            new(curType = 7);
            typ121z := curType;
            with curType@ do {
                size := 0;
                bits := 48;
                k := kindRecord;
                ptr1 := NULL;
                first := NULL;
                flag := false;
                pckrec := isPacked;
            };
            cases.size := 0;
            cases.count := 0;
            parseRecordDecl(curType, true);
            checkSymAndRead(ENDSY);
        } else /* 12467 */
        if (SY = ARRAYSY) then {
            inSymbol;
            if (SY = LBRACK) then
                inSymbol;
            tempType := NULL;
12476:      parseTypeRef(nestedType, skipTarget + [OFSY]);
            curVarKind := nestedType->k;
            if (curVarKind != kindRange) then {
                if (curVarKind = kindScalar) and
                   (nestedType != IntegerType) then {
                    span := nestedType->numen;
                } else {
                    error(8); /* errNotAnIndexType */
                    nestedType := IntegerType;
                    span := 10;
                };
                defineRange(nestedType, 0, span - 1);
            }; /* 12524 */
            new(l3typ26z, kindArray);
            with l3typ26z@ do {
                size := ord(tempType);
                bits := 48;
                k := kindArray;
                range := nestedType;
            };
            if (tempType = NULL) then
                curType := l3typ26z
            else
                tempType->base := l3typ26z;
            tempType := l3typ26z;
            if (SY = COMMA) then {
                inSymbol;
                goto 12476;
            };
            if (SY = RBRACK) then
                inSymbol;
            checkSymAndRead(OFSY);
            parseTypeRef(nestedType, skipTarget);
            l3typ26z->base := nestedType;
            if isFileType(nestedType) then
                error(errTypeMustNotBeFile);
            repeat with l3typ26z@, ptr2@ do {
                span := high.i - low + 1;
                tempType := ptr(size);
                l3int22z := base->bits;
                if (24 < l3int22z) then
                    isPacked := false;
                bits := 48;
                if (isPacked) then {
                    l3int22z := 48 DIV l3int22z;
                    if (l3int22z = 9) then {
                        l3int22z := 8;
                    } else if (l3int22z = 5) then {
                        l3int22z := 4
                    } /*=z-*/else/*=z+*/ ;
                    perWord := l3int22z;
                    pcksize := 48 DIV l3int22z;
                    l3int22z := span * pcksize;
                    if l3int22z mod 48 = 0 then
                        numBits := 0
                    else
                        numBits := 1;
                    size := l3int22z div 48 + numBits;
                    if (size = 1) then
                        bits := l3int22z;
                } else { /* 12633 */
                    size := span * base->size;
                    curVal.i := base->size;
                    curVal.m := curVal.m * [7:47] + [0];
                    if (range->base != IntegerType) then
                        curVal.m := curVal.m + [1, 3];
                    l3typ26z->perWord := KMUL+ I8 + getFCSToffset;
                }; /* 12652 */
                l3typ26z->pck := isPacked;
                isPacked := false;
                cond := (curType = l3typ26z);
                l3typ26z := tempType;
            } until cond;
        } else /* 12663 */
        if (SY = FILESY) then {
            inSymbol;
            checkSymAndRead(OFSY);
            parseTypeRef(nestedType, skipTarget);
            if (isFileType(nestedType)) then
                error(errTypeMustNotBeFile);
            if (isPacked) then {
                l3int22z := nestedType->bits;
                if (24 < l3int22z) then
                    isPacked := false;
            };
            new(curType, kindFile);
            if (not isPacked) then
                l3int22z := 0;
            with curType@ do {
                size := 30;
                bits := 48;
                k := kindFile;
                base := nestedType;
                elsize := l3int22z;
            }
        } else /* 12721 */
        if (SY = SETSY) then {
            inSymbol;
            checkSymAndRead(OFSY);
            parseTypeRef(nestedType, skipTarget);
            with nestedType@ do {
                if (k = kindRange) and
                   (left >= 0) and
                   (47 >= right) then
                    numBits := right + 1
                else if (k = kindScalar) and
                        (48 >= numen) then
                    numBits := numen
                else (q) {
                    numBits := 48;
                    error(63); /* errBadBaseTypeForSet */
                    /*=z-*/exit q/*=z+*/
                }
            };
            new(curType, kindSet);
            with curType@ do {
                size := 1;
                bits := numBits;
                k := kindSet;
                base := nestedType;
            }
        } else (q) {
12760:      ;
            parseLiteral(tempType, leftBound, true);
            if (tempType != NULL) then {
                inSymbol;
                if (SY != COLON) then {
                    requiredSymErr(COLON);
                } else {
                    inSymbol;
                };
                parseLiteral(curType, rightBound, true);
                if (curType = tempType) and
                   (curType->k = kindScalar) then {
                    defineRange(curType, leftBound.i, rightBound.i);
                    inSymbol;
                    goto 13020;
                }
            };
            error(64); /* errIncorrectRangeDefinition */
            curType := booleanType;
            /*=z-*/exit q/*=z+*/
        };
    };
13020:
    if (errors) then
        skip(skipToSet + [RPAREN, RBRACK, SEMICOLON, OFSY]);
    newtype := curType;
}; /* parseTypeRef */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure dumpEnumNames(l3arg1z: tptr);
var
    l3var1z: irptr;
{
    if (l3arg1z->start = 0) then {
        l3arg1z->start := FcstCnt;
        l3var1z := l3arg1z->enums;
        while (l3var1z != NULL) do {
            curVal := l3var1z->id;
            l3var1z := l3var1z->list;
            toFCST;
        }
    }
}; /* dumpEnumNames */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure formPMD;
var
    l3typ1z: tptr;
    l3var2z: Word;
    l3var3z: Bitset;
    l3var4z: boolean;
    l3var5z: kind;
{
    for l3var4z := false to true do {
        if l3var4z then {
            optSflags.m := (optSflags.m + [S3]);
            curVal.i := 74001B;
            P0715(2, 34); /*"P/DS"*/
            curVal := l2idr2z->id;
            toFCST;
            curVal.i := lineCnt;
            toFCST;
        }; /* 13063 */
        for jj := 0 to 127 do {
            curIdRec := symHashTabBase[jj];
            /*13066*/
            while (curIdRec != NULL) and
                  (l2idr2z < curIdRec) do with curIdRec@ do {
                l3var2z.i := typ->size;
                if (cl IN [VARID, FORMALID]) and
                  (value < 74000B) then {
                    curVal := id;
                    if (l3var4z) then
                        toFCST;
                    l3typ1z := typ;
                    l3var5z := l3typ1z->k;
                    l3var3z := [];
                    if (l3var5z = kindPtr) then {
                        l3typ1z := l3typ1z->base;
                        l3var5z := l3typ1z->k;
                        l3var3z := [0];
                    };
                    if (l3typ1z = realType) then
                        curVal.i := 0
                    else if typeCheck(l3typ1z, IntegerType) then
                        curVal.i := 100000B
                    else if typeCheck(l3typ1z, charType) then
                        curVal.i := 200000B
                    else if (l3var5z = kindArray) then
                        curVal.i := 400000B
                    else if (l3var5z = kindScalar) then {
                        dumpEnumNames(l3typ1z);
                        curVal.i := 1000000B * l3typ1z->start + 300000B;
                    } else if (l3var5z = kindFile) then
                        curVal.i := 600000B
                    else {
                        curVal.i := 500000B;
                        /*=z-*/(q) exit q/*=z+*/
                    };
                    curVal.i := curVal.i + curIdRec->value;
                    l3var2z := l3var2z;
                    besm(ASN64-33);
                    l3var2z := ;
                    curVal.m := curVal.m * [15:47] + l3var2z.m + l3var3z;
                    if (l3var4z) then
                        toFCST;
                }; /* 13164 */
                curIdRec := curIdRec->next;
            }; /* 13166 */
        }; /*13167+*/
        curVal.m := [];
        if l3var4z then
            toFCST;
    }
}; /* formPMD */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseDecls(l3arg1z: Integer);
var
    l3int1z: Integer;
    frame:   Word;
    l3var3z: boolean;
{
    case l3arg1z of
    0: {
        int93z := 0;
        inSymbol;
        if (SY != IDENT) then
            errAndSkip(3, skipToSet + [IDENT]);
    };
    1: {
        prevErrPos := 0;
        write('IDENT ');
        printTextWord(l2var12z);
        write(' IN LINE ', curIdRec->offset:0);
    };
    2: {
        with l2idr2z@ do
            ; /* useless */
        padToLeft;
        l3var3z := 22 IN l2idr2z->flags;
        l3arg1z := l2idr2z->pos;
        frame.i := moduleOffset - 40000B;
        if (l3arg1z != 0) then
            symTab[l3arg1z] := [24, 29] + frame.m * halfWord;
        l2idr2z->pos := moduleOffset;
        l3arg1z := F3307(l2idr2z);
        if l3var3z then {
            if (41 >= entryPtCnt) then {
                curVal := l2idr2z->id;
                entryPtTable[entryPtCnt] := makeNameWithStars(true);
                entryPtTable[entryPtCnt+1] := [1] + frame.m - [0, 3];
                entryPtCnt := entryPtCnt + 2;
            } else
                error(87); /* errTooManyEntryProcs */
        };
        if (l2idr2z->typ = NULL) then {
            frame.i := 3;
        } else {
            frame.i := 4;
        };
        if l3var3z then
            form2Insn((KVTM+I14) + l3arg1z + (frame.i - 3) * 1000B,
                      getHelperProc(94 /*"P/NN"*/) - 10000000B);
        if 1 < l3arg1z then {
            frame.i := getValueOrAllocSymtab(-(frame.i+l3arg1z));
        };
        if (S5 IN optSflags.m) and
           (curProcNesting = 1) then
            l3int1z := 59  /* P/LV */
        else
            l3int1z := curProcNesting;
        l3int1z := getHelperProc(l3int1z) - (-4000000B);
        if l3arg1z = 1 then {
            form1Insn((KATX+SP) + frame.i);
        } else if (l3arg1z != 0) then {
            form2Insn(KATX+SP, (KUTM+SP) + frame.i);
        } /*=z-*/else/*=z+*/ ;
        formAndAlign(l3int1z);
        savedObjIdx := objBufIdx;
        if (curProcNesting != 1) then
            form1Insn(0);
        if l3var3z then
            form1Insn(KVTM+I8+74001B);
        if (l2int11z != 0) then {
            form1Insn(InsnTemp[XTA]);
            formAndAlign(KVJM+I13 + l2int11z);
            curVal.i := l2int11z;
            P0715(2, 49 /* "P/RDC" */);
        };
        if (curProcNesting = 1) then {
            if (heapCallsCnt != 0) and
               (heapSize = 0) then
                error(65 /*errCannotHaveK0AndNew*/);
            l3var3z := (heapSize = 0) or
                (heapCallsCnt = 0) and (heapSize = 100);
            if (heapSize = 100) then
                heapSize := 4;
            if (not l3var3z) then {
                form2Insn(KVTM+I14+getValueOrAllocSymtab(heapSize*2000B),
                          getHelperProc(26 /*"P/GD"*/));
                padToLeft;
            }
        };
        if (doPMD) then
            formPMD;
    }
    end; /* case */
}; /* parseDecls */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure statement;
label
    8888;
var
    boundary: eptr;
    l3var2z: @numLabel;
    l3var3z: @strLabel;
    l3var4z: Word;
    l3bool5z: boolean;
    l3var6z: idclass;
    l3var7z, l3var8z: Word;
    startLine: Integer;
    l3var10z, l3var11z: Word;
    l3idr12z: irptr;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function isCharArray(arg: tptr): boolean;
{
    with arg@ do
        isCharArray := (k = kindArray) and (base = charType);
}; /* isCharArray */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure expression;
    forward;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseLval;
label
    13462, 13530;
var
    l4exp1z, l4exp2z: eptr;
    l4typ3z: tptr;
    l4var4z: kind;
{
    if (hashTravPtr->cl = FIELDID) then {
        curExpr := expr62z;
        goto 13530;
    } else {
        new(curExpr);
        with curExpr@ do {
            typ := hashTravPtr->typ;
            op := GETVAR;
            id1 := hashTravPtr;
        };
13462:  inSymbol;
        l4typ3z := curExpr->typ;
        l4var4z := l4typ3z->k;
        if (SY = ARROW) then {
            new(l4exp1z);
            with l4exp1z@ do {
                expr1 := curExpr;
                if (l4var4z = kindPtr) then {
                    typ := l4typ3z->base;
                    op := DEREF;
                } else if (l4var4z = kindFile) then {
                    typ := l4typ3z->base;
                    op := FILEPTR;
                } else (q) {
                    stmtName := '  ^   ';
                    error(errWrongVarTypeBefore);
                    l4exp1z->typ := l4typ3z;
                    /*=z-*/exit q/*=z+*/
                }
            };
            curExpr := l4exp1z;
        } else if (SY = PERIOD) then {
            if (l4var4z = kindRecord) then {
                int93z := 3;
                typ121z := l4typ3z;
                inSymbol;
                if (hashTravPtr = NULL) then {
                    error(20); /* errDigitGreaterThan7 ??? */
                } else 13530: {
                    new(l4exp1z);
                    with l4exp1z@ do {
                        typ := hashTravPtr->typ;
                        op := GETFIELD;
                        expr1 := curExpr;
                        id2 := hashTravPtr;
                    };
                    curExpr := l4exp1z;
                }
            } else {
                stmtName := '  .   ';
                error(errWrongVarTypeBefore);
            };
        } else if (SY = LBRACK) then {
            stmtName := '  [   ';
            repeat
                l4exp1z := curExpr;
                expression;
                l4typ3z := l4exp1z->typ;
                if (l4typ3z->k != kindArray) then {
                    error(errWrongVarTypeBefore);
                } else {
                    if (not typeCheck(l4typ3z->range, curExpr->typ)) then
                        error(66 /*errOtherIndexTypeNeeded */);
                    new(l4exp2z);
                    with l4exp2z@ do {
                        typ := l4typ3z->base;
                        expr1 := l4exp1z;
                        expr2 := curExpr;
                        op := GETELT;
                    };
                    l4exp1z := l4exp2z;
                };
                curExpr := l4exp1z;
                stmtName := '  ,   ';
            until (SY != COMMA);
            if (SY != RBRACK) then
                error(67 /*errNeedBracketAfterIndices*/);
        } else exit;
    };
    goto 13462;
}; /* parseLval */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure castToReal(var value: eptr);
var
    cast: eptr;
{
    new(cast);
    with cast@ do {
        typ := realType;
        op := TOREAL;
        expr1 := value;
        value := cast;
    }
}; /* castToReal */
%
function areTypesCompatible(var l4arg1z: eptr): boolean;
{
    if (arg1Type = realType) then {
        if typeCheck(IntegerType, arg2Type) then {
            castToReal(l4arg1z);
            areTypesCompatible := true;
            exit
        };
    } else if (arg2Type = realType) and
               typeCheck(IntegerType, arg1Type) then {
        castToReal(curExpr);
        areTypesCompatible := true;
        exit
    };
    areTypesCompatible := false;
}; /* areTypesCompatible */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseCallArgs(l4arg1z: irptr);
label
    13736;
var
    l4var1z: boolean;
    l4exp2z, l4exp3z, l4exp4z: eptr;
    l4idr5z: irptr;
    l4op6z: operator;
    l4idc7z: idclass;
{
    with l4arg1z@ do {
        if typ != NULL then
            set146z := set146z - flags;
        l4var1z := (list = NULL) and not (24 in flags);
    };
    new(l4exp3z);
    l4exp4z := l4exp3z;
    bool48z := true;
    with l4exp3z@ do {
        typ := l4arg1z->typ;
        op := ALNUM;
        id2 := l4arg1z;
        id1 := NULL;
    };
    if (SY = LPAREN) then {
        if (l4var1z) then {
            l4idr5z := l4arg1z->argList;
            if (l4idr5z = NULL) then {
                error(errTooManyArguments);
                goto 8888;
            }
        };
        repeat
            if (l4var1z) and (l4arg1z = l4idr5z) then {
                error(errTooManyArguments);
                goto 8888;
            };
            bool47z := true;
            expression;
            l4op6z := curExpr->op;
(a)         if l4var1z then {
                l4idc7z := l4idr5z->cl;
                if (l4op6z = PCALL) then {
                    if (l4idc7z != ROUTINEID) or
                       (l4idr5z->typ != NULL) then {
13736:                  error(39); /*errIncompatibleArgumentKinds*/
                        exit a
                    }
                } else { /* 13741 */
                    if (l4op6z = FCALL) then {
                        if (l4idc7z = ROUTINEID) then {
                            if (l4idr5z->typ = NULL) then
                                goto 13736
                        } else /* 13750 */
                        if (curExpr->id2->argList = NULL) and
                           (l4idc7z = VARID) then {
                            curExpr->op := ALNUM;
                            curExpr->expr1 := NULL;
                        } else
                            goto 13736;
                    } else /* 13762 */
                    if (l4op6z IN lvalOpSet) then {
                        if (l4idc7z != VARID) and
                           (l4idc7z != FORMALID) then
                            goto 13736;
                    } else {
                        if (l4idc7z != VARID) then
                            goto 13736;
                        /*=z-*/(q) exit q/*=z+*/
                    }
                };
                arg1Type := curExpr->typ;
                if (arg1Type != NULL) then {
                    if not typeCheck(arg1Type, l4idr5z->typ) then
                        error(40); /*errIncompatibleArgumentTypes*/
                }
            }; /* 14006 */
            new(l4exp2z);
            with l4exp2z@ do {
                typ := NULL;
                expr1 := NULL;
                expr2 := curExpr;
            };
            l4exp4z->expr1 := l4exp2z;
            l4exp4z := l4exp2z;
            if (l4var1z) then
                l4idr5z := l4idr5z->list;
        until (SY != COMMA);
        if (SY != RPAREN) or
           l4var1z and (l4idr5z != l4arg1z) then
            error(errNoCommaOrParenOrTooFewArgs)
        else
            inSymbol;
    } else { /* 14035 */
        if (l4var1z) and (l4arg1z->argList != NULL) then
            error(42); /*errNoArgList*/
    };
    curExpr := l4exp3z;
    /* 14042 */
}; /* parseCallArgs */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure factor;
label
    14567;
var
    l4var1z: Word;
    l4var2z: boolean;
    l4var3z, l4var4z: Word;
    l4exp5z, l4exp6z, l4var7z, l4var8z: eptr;
    routine: irptr;
    l4op10z: operator;
    l4typ11z: tptr;
    l4var12z: boolean;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure stdCall;
const chkREAL = 0;  chkINT    = 1;  chkCHAR = 2;    chkSCALAR = 3;
      chkPTR  = 4;  chkFILE   = 5;  chkSET  = 6;    chkOTHER  = 7;
var
    l5op1z: operator;
    l5var2z: tptr;
    argKind: kind;
    asBitset: Bitset;
    stProcNo, checkMode: Integer;
{
    curVal.i := routine->low;
    stProcNo := curVal.i;
    if (SY != LPAREN) then {
        requiredSymErr(LPAREN);
        goto 8888;
    };
    expression;
    if (stProcNo >= fnEOF) and
       (fnEOLN >= stProcNo) and
       not (curExpr->op IN [GETELT..FILEPTR]) then {
        error(27); /* errExpressionWhereVariableExpected */
        exit;
    };
    arg1Type := curExpr->typ;
    if (arg1Type->k = kindRange) then
        arg1Type := arg1Type->base;
    argKind := arg1Type->k;
    if (arg1Type = realType) then
        checkMode := chkREAL
    else if (arg1Type = IntegerType) then
        checkMode := chkINT
    else if (arg1Type = charType) then
        checkMode := chkCHAR
    else if (argKind = kindScalar) then
        checkMode := chkSCALAR
    else if (argKind = kindPtr) then
        checkMode := chkPTR
    else if (argKind = kindFile) then
        checkMode := chkFILE
    else if (argKind = kindSet) then
        checkMode := chkSET
    else {
        checkMode := chkOTHER;
        /*=z-*/(q) exit q/*=z+*/
    };
    asBitset := [stProcNo];
    if not ((checkMode = chkREAL) and
            (asBitset <= [fnSQRT:fnTRUNC, fnREF, fnSQR, fnROUND, fn29])
           or ((checkMode = chkINT) and
            (asBitset <= [fnSQRT:fnABS, fnODD, fnCHR, fnREF, fnSQR, fnPTR]))
           or ((checkMode IN [chkCHAR, chkSCALAR, chkPTR]) and
            (asBitset <= [fnORD, fnSUCC, fnPRED, fnREF]))
           or ((checkMode = chkFILE) and
            (asBitset <= [fnEOF, fnREF, fnEOLN]))
           or ((checkMode = chkSET) and
            (asBitset <= [fnREF, fnCARD, fnMINEL]))
           or ((checkMode = chkOTHER) and
            (stProcNo = fnREF))) then
        error(errNeedOtherTypesOfOperands);
    if not (asBitset <= [fnABS, fnSUCC, fnPRED, fnSQR]) then {
        arg1Type := routine->typ;
    } else if (checkMode = chkINT) and (asBitset <= [fnABS, fnSQR]) then {
        if stProcNo = fnABS then
            stProcNo := fnABSI
        else
            stProcNo := fnSQRI;
    };
    new(l4exp6z);
    l4exp6z->op := STANDPROC;
    l4exp6z->expr1 := curExpr;
    l4exp6z->num2 := stProcNo;
    if stProcNo = fn24 then {
        if SY != COMMA then {
            requiredSymErr(COMMA);
            goto 8888;
        };
        expression;
        l5var2z := curExpr->typ;
        l5op1z := badop27;
        if (l5var2z != realType) and
            not typeCheck(l5var2z, IntegerType) then
            error(errNeedOtherTypesOfOperands);
        if (l5var2z = realType) then
            l5op1z := badop30
        else if (checkMode = chkREAL) then
            l5op1z := badop31;
        l4exp6z->expr2 := curExpr;
        l4exp6z->op := l5op1z;
    };
    curExpr := l4exp6z;
    curExpr->typ := arg1Type;
    checkSymAndRead(RPAREN);
    /* 14247 */
}; /* stdCall */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* factor */
    l4var2z := bool47z;
    bool47z := false;
    if (SY < MULOP) then {
        case SY of
        IDENT: {
            if (hashTravPtr = NULL) then {
                error(errNotDefined);
                curExpr := uVarPtr;
            } else
                case hashTravPtr->cl of
                TYPEID: {
                    error(23); /* errTypeIdInsteadOfVar */
                    curExpr := uVarPtr;
                };
                ENUMID: {
                    new(curExpr);
                    with curExpr@ do {
                        typ := hashTravPtr->typ;
                        op := GETENUM;
                        num1 := hashTravPtr->value;
                        num2 := 0;
                    };
                    inSymbol;
                };
                ROUTINEID: {
                    routine := hashTravPtr;
                    inSymbol;
                    if (routine->offset = 0) then {
                        if (routine->typ != NULL) and
                           (SY = LPAREN) then {
                            stdCall;
                            exit;
                            /*=z-*/} else {{/*=z+*/
                        };
                        error(44) /* errIncorrectUsageOfStandProcOrFunc */
                        /*=z-*/}/*=z+*/
                    } else if (routine->typ = NULL) then {
                        if (l4var2z) then {
                            l4op10z := PCALL;
                        } else {
                            error(68); /* errUsingProcedureInExpression */
                        }
                   } else /* 14330 */ {
                        if (SY = LPAREN) then {
                            parseCallArgs(routine);
                            exit
                        };
                        if (l4var2z) then {
                            l4op10z := FCALL;
                        } else {
                            parseCallArgs(routine);
                            exit
                        };
                        (q) exit q
                    }; /* 14342 */
                    new(curExpr);
                    if not (SY IN [RPAREN, COMMA]) then {
                        error(errNoCommaOrParenOrTooFewArgs);
                        goto 8888;
                    };
                    with curExpr@ do {
                        typ := routine->typ;
                        op := l4op10z;
                        expr1 := NULL;
                        id2 := routine;
                    }
                };
                VARID, FORMALID, FIELDID:
                    parseLval;
                end /* case */
        };
        LPAREN: {
            expression;
            checkSymAndRead(RPAREN);
        };
        INTCONST, REALCONST, CHARCONST, LTSY, GTSY: {
            new(curExpr);
            parseLiteral(curExpr->typ, curExpr->d1, false);
            curExpr->num2 := ord(suffix);
            curExpr->op := GETENUM;
            inSymbol;
        };
        NOTSY: {
            inSymbol;
            factor;
            if (curExpr->typ != booleanType) then
                error(1); /* errNoCommaNorSemicolon */
            l4exp6z := curExpr;
            new(curExpr);
            with curExpr@ do {
                typ := booleanType;
                op := NOTOP;
                expr1 := l4exp6z;
            }
        };
        LBRACK: {
            new(curExpr);
            inSymbol;
            l4var8z := curExpr;
            l4var1z.m := [];
            if (SY != RBRACK) then {
                l4var12z := true;
                bool102z := false;
                repeat
                    l4exp6z := curExpr;
                    expression;
                    if (l4var12z) then {
                        l4typ11z := curExpr->typ;
                        if not (l4typ11z->k IN [kindScalar, kindRange]) then
                            error(23); /* errTypeIdInsteadOfVar */
                    } else {
                        if not typeCheck(l4typ11z, curExpr->typ) then
                            error(24); /*errIncompatibleExprsInSetCtor*/
                    };
                    l4var12z := false;
                    l4exp5z := curExpr;
                    if (SY = COLON) then {
                        expression;
                        if not typeCheck(l4typ11z, curExpr->typ) then
                            error(24); /*errIncompatibleExprsInSetCtor*/
                        if (l4exp5z->op = GETENUM) and
                           (curExpr->op = GETENUM) then {
                            l4var4z.i := l4exp5z->num1;
                            l4var3z.i := curExpr->num1;
                            l4var4z.m := l4var4z.m - intZero;
                            l4var3z.m := l4var3z.m - intZero;
                            l4var1z.m := l4var1z.m + [l4var4z.i..l4var3z.i];
                            curExpr := l4exp6z;
                            goto 14567;
                        };
                        new(l4var7z);
                        with l4var7z@ do {
                            typ := setType;
                            op := MKRANGE;
                            expr1 := l4exp5z;
                            expr2 := curExpr;
                        };
                        l4exp5z := l4var7z;
                        /*=z-*/(q);/*=z+*/
                   } else {/* 14535 */
                        if (l4exp5z->op = GETENUM) then {
                            l4var4z.i := l4exp5z->num1;
                            l4var4z.m := l4var4z.m - intZero;
                            l4var1z.m := l4var1z.m + [l4var4z.i];
                            curExpr := l4exp6z;
                            goto 14567;
                        };
                        new(l4var7z);
                        with l4var7z@ do {
                            typ := setType;
                            op := STANDPROC;
                            expr1 := l4exp5z;
                            num2 := 109;
                            l4exp5z := l4var7z;
                        }
                    }; /* 14560 */
                    new(curExpr);
                    with curExpr@ do {
                        typ := setType;
                        op := SETOR;
                        expr1 := l4exp6z;
                        expr2 := l4exp5z;
                    };
14567:              ;
                until SY != COMMA;
            }; /* 14571 */
            checkSymAndRead(RBRACK);
            with l4var8z@ do {
                op := GETENUM;
                typ := setType;
                d1 := l4var1z;
            }
        };
        end; /* case */
    } else {
        error(errBadSymbol);
        goto 8888;
    }
    /* 14623 */
}; /* factor */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure term;
label
    14650;
var
    l4var1z: operator;
    l4var2z, l4var3z: eptr;
    l4var4z: boolean;
{
    factor;
    while (SY = MULOP) do {
        l4var1z := charClass;
        inSymbol;
        l4var2z := curExpr;
        factor;
        arg1Type := curExpr->typ;
        arg2Type := l4var2z->typ;
        l4var4z := typeCheck(arg1Type, arg2Type);
        if (not l4var4z) and
           (RDIVOP < l4var1z) then
14650:      error(errNeedOtherTypesOfOperands)
        else {
            case l4var1z of
            MUL, RDIVOP: {
                if (l4var4z) then {
                    if (arg1Type = realType) then {
                        /* empty */
                    } else {
                        if (typ120z = IntegerType) then {
                            if (l4var1z = MUL) then {
                                arg1Type := IntegerType;
                            } else {
                                arg1Type := realType;
                            };
                            l4var1z := imulOpMap[l4var1z];
                        } else {
                            if (arg1Type->k = kindSet) then {
                                l4var1z := setOpMap[l4var1z];
                            } else
                                goto 14650;
                        }
                    }
                } else {
                    if areTypesCompatible(l4var2z) then {
                        arg1Type := realType;
                    } else
                        goto 14650;
                }
            };
            AMPERS: {
                if (arg1Type != booleanType) then
                    goto 14650;
            };
            IDIVOP: {
                if (typ120z != IntegerType) then
                    goto 14650;
                arg1Type := IntegerType;
            };
            IMODOP: {
                if (typ120z = IntegerType) then {
                    arg1Type := IntegerType;
                } else {
                    if (arg1Type->k = kindSet) then
                        l4var1z := SETXOR
                    else
                        goto 14650;
                }
            };
            end;
            new(l4var3z);
            with l4var3z@ do {
                op := l4var1z;
                expr1 := l4var2z;
                expr2 := curExpr;
                curExpr := l4var3z;
                typ := arg1Type;
            }
        }
    }
    /* 14746 */
}; /* term */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure simpleExpression;
label
    15031;
var
    l4var1z, l4var2z: eptr;
    l4var3z: operator;
    argKind: kind;
    l4bool5z: boolean;
{
    l4bool5z := false;
    if (charClass IN [PLUSOP, MINUSOP]) then {
        if (charClass = MINUSOP) then
            l4bool5z := true;
        inSymbol;
    };
    term;
(minus)
    if (l4bool5z) then {
        arg1Type := curExpr->typ;
        new(l4var2z);
        with l4var2z@ do {
            typ := arg1Type;
            expr1 := curExpr;
            if (arg1Type = realType) then {
                op := RNEGOP;
            } else if typeCheck(arg1Type, IntegerType) then {
                l4var2z->op := INEGOP;
                l4var2z->typ := IntegerType;
            } else {
                error(69); /* errUnaryMinusNeedRealOrInteger */
                exit minus
            };
            curExpr := l4var2z;
        }
    }; /* 15010 */
    while (SY = ADDOP) do {
        l4var3z := charClass;
        inSymbol;
        l4var2z := curExpr;
        term;
        arg1Type := curExpr->typ;
        arg2Type := l4var2z->typ;
        l4bool5z := typeCheck(arg1Type, arg2Type);
        argKind := arg2Type->k;
        if (kindSet < argKind) then {
15031:      error(errNeedOtherTypesOfOperands);
        } else {
            new(l4var1z);
            with l4var1z@ do {
                if (l4var3z = OROP) then {
                    if (not l4bool5z) or
                       (arg1Type != booleanType) then
                        goto 15031;
                    typ := booleanType;
                    op := l4var3z;
                } else /* 15046 */ {
                    if (l4bool5z) then {
                        if (arg1Type = realType) then {
                            op := l4var3z;
                            typ := realType;
                        } else if (typ120z = IntegerType) then {
                            op := iAddOpMap[l4var3z];
                            typ := IntegerType;
                        } else if (argKind = kindSet) then {
                            op := setOpMap[l4var3z];
                            typ := arg1Type;
                        } else {
                            goto 15031
                        }
                    } else if areTypesCompatible(l4var2z) then {
                        l4var1z->typ := realType;
                        l4var1z->op := l4var3z;
                    } else
                        goto 15031
                }; /* 15077 */
                l4var1z->expr1 := l4var2z;
                l4var1z->expr2 := curExpr;
                curExpr := l4var1z;
            }
        };
    }
    /* 15104 */
}; /* simpleExpression */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure expression;
var
    oper: operator;
    l4var2z, l4var3z: eptr;
{
    if (bool102z) then
        inSymbol
    else
        bool102z := true;
    simpleExpression;
    if (SY = RELOP) then {
        oper := charClass;
        inSymbol;
        l4var3z := curExpr;
        simpleExpression;
        arg1Type := curExpr->typ;
        arg2Type := l4var3z->typ;
        if typeCheck(arg1Type, arg2Type) then {
            if (oper = INOP) or
               (arg1Type->k = kindFile) or
               (arg1Type->size != 1) and
               (oper >= LTOP) and
               not isCharArray(arg1Type) then
                error(errNeedOtherTypesOfOperands);
        } else /* 15150 */ {
            if not areTypesCompatible(l4var3z) and
               ((arg1Type->k != kindSet) or
               not (arg2Type->k IN [kindScalar, kindRange]) or
               (oper != INOP)) then {
                /*=z-*/besm(2200000B); besm(2200000B);/*=z+*/
                error(errNeedOtherTypesOfOperands);
            } /*=z-*/else;/*=z+*/
        }; /* 15167 */
        new(l4var2z);
        if (arg2Type->k = kindSet) and
           (oper IN [LTOP, GTOP]) then
            error(errNeedOtherTypesOfOperands);
        with l4var2z@ do {
            typ := booleanType;
            if (oper IN [GTOP, LEOP]) then {
                expr1 := curExpr;
                expr2 := l4var3z;
                if (oper = GTOP) then
                    op := LTOP
                else
                    op := GEOP;
            } else {
                expr1 := l4var3z;
                expr2 := curExpr;
                op := oper;
            };
            curExpr := l4var2z;
        }
    }
    /* 15217 */
}; /* expression */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure forStatement;
var
    l4typ1z: tptr;
    l4exp2z, l4var3z, l4var4z: eptr;
    l4int5z, l4int6z, l4int7z, l4int8z: Integer;
    l4var9z: boolean;
{
    inSymbol;
    disableNorm;
    curExpr := NULL;
    if (SY = IDENT) then {
        if (hashTravPtr != NULL) and (hashTravPtr->cl = VARID) then {
            parseLval;
            if (curExpr->op != GETVAR) then
                error(errNoSimpleVarForLoop);
        } else
            error(errNoSimpleVarForLoop);
    } else {
        errAndSkip(errNoIdent, skipToSet + [BECOMES, DOSY, TOSY, DOWNTOSY]);
    }; /* 15251 */
    if (curExpr = NULL) then
        curExpr := uVarPtr;
    l4exp2z := curExpr;
    l4typ1z := l4exp2z->typ;
    if not (l4typ1z->k IN [kindScalar, kindRange]) then
        error(25); /* errExprNotOfADiscreteType */
    if typeCheck(IntegerType, l4typ1z) then
        l4int5z := KATX+PLUS1
    else
        l4int5z := KATX+E1;
    if (SY = BECOMES) then {
        expression;
        l4var9z := true;
    } else {
        l4var9z := false;
    };
    l4var3z := curExpr;
    l4int6z := InsnTemp[ADD];
    if not typeCheck(l4typ1z, l4var3z->typ) then
        error(31); /* errIncompatibleTypesOfLoopIndexAndExpr */
(todownto)
    if (SY = TOSY) then
        exit todownto
    else if (SY = DOWNTOSY) then
        l4int6z := InsnTemp[SUB]
    else (q) {
        error(70); /* errNeitherToNorDownto */
        /*=z-*/exit q/*=z+*/
    };
    expression;
    if not typeCheck(l4typ1z, curExpr->typ) then
        error(31); /* errIncompatibleTypesOfLoopIndexAndExpr */
    formOperator(gen0);
    l4var4z := curExpr;
    if (l4var9z) then {
        curExpr := l4var3z;
        formOperator(LOAD);
    } else {
        form1Insn(InsnTemp[XTA] + l4int5z);
    };
    l4int7z := 0;
    disableNorm;
    formJump(l4int7z);
    padToLeft;
    l4int8z := moduleOffset;
    checkSymAndRead(DOSY);
    statement;
    disableNorm;
    curExpr := l4exp2z;
    formOperator(LOAD);
    form1Insn(l4int6z + l4int5z);
    P0715(0, l4int7z);
    formOperator(STORE);
    curExpr := l4var4z;
    if (l4int6z = InsnTemp[SUB]) then
        curVal.i := l4int6z
    else
        curVal.i := InsnTemp[RSUB];
    /*15401*/
    formOperator(gen3);
    form1Insn(InsnTemp[UZA] + l4int8z);
}; /* forStatement */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure withStatement;
var
    l4exp1z: eptr;
    l4var2z, l4var3z: Bitset;
    l4var4z: Integer;
{
    l4exp1z := expr63z;
    l4var4z := localSize;
    l4var2z := set147z;
    l4var3z := [];
    repeat
        inSymbol;
        if (hashTravPtr != NULL) and
           (hashTravPtr->cl >= VARID) then {
            parseLval;
            if (curExpr->typ->k = kindRecord) then {
                formOperator(SETREG);
                l4var3z := (l4var3z + [curVal.i]) * set148z;
            } else {
                error(71); /* errWithOperatorNotOfARecord */
            };
        } else {
            error(72); /* errWithOperatorNotOfAVariable */
        }
    until (SY != COMMA);
    checkSymAndRead(DOSY);
    statement;
    expr63z := l4exp1z;
    localSize := l4var4z;
    set147z := l4var2z;
    set145z := set145z + l4var3z;
}; /* withStatement */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure reportStmtType(l4arg1z: Integer);
{
    writeln(' STATEMENT ', stmtname:0, ' IN ', startLine:0, ' LINE');
}; /* reportStmtType */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function structBranch(isGoto: boolean): boolean;
var
    curLab: @strLabel;
{
    structBranch := true;
    if (SY = IDENT) or not isGoto then {
        curLab := strLabList;
        ii := 1;
        while (curLab != NULL) do {
            with curLab@ do {
                if (ident.m = []) then {
                    ii := ii - 1;
                } else {
                    if (ident = curIdent) then {
                        if (ii = 1) then {
                            if (isGoto) then {
                                form1Insn(InsnTemp[UJ] + offset);
                            } else {
                                formJump(curLab->exitTarget);
                            };
                        } else {
                            form1Insn(getValueOrAllocSymtab(ii) +
                                      (KVTM+I13));
                            if (isGoto) then {
                                form1Insn(KVTM+I10 + curLab->offset);
                            } else {
                                jumpType := KVTM+I10;
                                formJump(curLab->exitTarget);
                                jumpType := InsnTemp[UJ];
                            };
                            form1Insn(getHelperProc(60) +
                                      6437777713700000C); /* P/ZAM */
                        };
                        exit
                    }
                };
                curLab := curLab->next;
            }
        };
        if not isGoto and (SY != IDENT) then {
            if (ii != 1) then {
                form1Insn(getValueOrAllocSymtab(ii) + (KVTM+I13));
                form1Insn(getHelperProc(60)); /* P/ZAM */
            };
            formJump(int53z);
        } else {
            error(errNotDefined);
        }
    } else
        structBranch := false;
}; /* structBranch */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure caseStatement;
label
    16211;
type
    casechain = record
        next:   @casechain;
        value:  Word;
        offset: Integer;
    end;
var
    allClauses, curClause, clause, unused: @casechain;
    isIntCase: boolean;
    otherSeen: boolean;
    otherOffset: Integer;
    itemsEnded, goodMode: boolean;
    firstType, itemtype, exprtype: tptr;
    itemvalue: Word;
    itemSpan: Integer;
    expected: Word;
    startLine, l4var17z, endOfStmt: Integer;
    minValue, unused2, maxValue: Word;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function max(a, b: Integer): Integer;
{
    if (b < a) then
        max := a
    else
        max := b;
}; /* max */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* caseStatement */
    startLine := lineCnt;
    expression;
    exprtype := curExpr->typ;
    otherSeen := false;
    if (exprtype = alfaType) or
       (exprtype->k IN [kindScalar, kindRange]) then
        formOperator(LOAD)
    else
        error(25); /* errExprNotOfADiscreteType */
    disableNorm;
    l4var17z := 0;
    endOfStmt := 0;
    allClauses := NULL;
    formJump(l4var17z);
    checkSymAndRead(OFSY);
    firstType := NULL;
    goodMode := true;
    /* 15640 */
    repeat
        if not (SY IN [SEMICOLON, ENDSY]) then {
            padToLeft;
            arithMode := 1;
            if (SY = OTHERSY) then {
                if (otherSeen) then
                    error(73); /* errCaseLabelsIdentical */
                inSymbol;
                otherSeen := true;
                otherOffset := moduleOffset;
            } else /* 15657 */ repeat
                parseLiteral(itemtype, itemvalue, true);
                if (itemtype != NULL) then {
                    if (firstType = NULL) then {
                        firstType := itemtype;
                    } else {
                        if not typeCheck(itemtype, firstType) then
                            error(errConstOfOtherTypeNeeded);
                    }; /* 15700 */
                    new(clause);
                    clause->value := itemvalue;
                    clause->offset := moduleOffset;
                    curClause := allClauses;
(loop)              while (curClause != NULL) do {
                        if (itemvalue = curClause->value) then {
                            error(73); /* errCaseLabelsIdentical */
                            exit loop
                        } else if (itemvalue.i < curClause->value.i) then {
                            exit loop
                        } else (q) {
                            unused := curClause;
                            curClause := curClause->next;
                            /*=z-*/exit q/*=z+*/
                        }
                    }; /* 15723 */
                    if (curClause = allClauses) then {
                        clause->next := allClauses;
                        allClauses := clause;
                    } else {
                        clause->next := curClause;
                        unused->next := clause;
                    };
                    inSymbol;
                }; /* 15735 */
                itemsEnded := (SY != COMMA);
                if not itemsEnded then
                    inSymbol;
            until itemsEnded; /* 15745 */
            checkSymAndRead(COLON);
            statement;
            goodMode := goodMode and (arithMode = 1);
            formJump(endOfStmt);
        }; /* 15762 */
        itemsEnded := (SY = ENDSY);
        if not itemsEnded then
            inSymbol;
        /* 15771 */
    until itemsEnded;
    if (SY != ENDSY) then {
        requiredSymErr(ENDSY);
        stmtName := 'CASE  ';
        reportStmtType(startLine);
    } else
        inSymbol;
    if not typeCheck(firstType, exprtype) then {
        error(88); /* errDifferentTypesOfLabelsAndExpr */;
        exit
    };
    padToLeft;
    isIntCase := typeCheck(exprtype, IntegerType);
    if (allClauses != NULL) then {
        expected := allClauses->value;
        minValue := expected;
        curClause := allClauses;
        while (curClause != NULL) do {
            if (expected = curClause->value) and
               (exprtype->k = kindScalar) then {
                maxValue := expected;
                if (isIntCase) then {
                    expected.i := expected.i + 1;
                } else {
                    curVal := expected;
                    curVal.c := succ(curVal.c);
                    expected := curVal;
                };
                curClause := curClause->next;
            } else {
                itemSpan := 34000;
                P0715(0, l4var17z);
                if (firstType->k = kindRange) then {
                    itemSpan := max(abs(firstType->left),
                                    abs(firstType->right));
                } else {
                    if (firstType->k = kindScalar) then
                        itemSpan := firstType->numen;
                };
                itemsEnded := (itemSpan < 32000);
                if (itemsEnded) then {
                    form1Insn(KATI+14);
                } else {
                    form1Insn(KATX+SP+1);
                };
                minValue.i := (minValue.i - minValue.i); /* WTF? */
                while (allClauses != NULL) do {
                    if (itemsEnded) then {
                        curVal.i := (minValue.i - allClauses->value.i);
                        curVal.m := (curVal.m + intZero);
                        form1Insn(getValueOrAllocSymtab(curVal.i) +
                                  (KUTM+I14));
                        form1Insn(KVZM+I14 + allClauses->offset);
                        minValue := allClauses->value;
                    } else {
                        form1Insn(KXTA+SP+1);
                        curVal := allClauses->value;
                        form2Insn(KAEX + I8 + getFCSToffset,
                                  InsnTemp[UZA] + allClauses->offset);
                    };
                    allClauses := allClauses->next;
                };
                if (otherSeen) then
                    form1Insn(InsnTemp[UJ] + otherOffset);
                goto 16211;
            }; /* if 16141 */
        }; /* while 16142 */
        if (not otherSeen) then {
            otherOffset := moduleOffset;
            formJump(endOfStmt);
        };
        P0715(0, l4var17z);
        curVal := minValue;
        P0715(-(InsnTemp[U1A]+otherOffset), maxValue.i);
        curVal := minValue;
        curVal.m := (curVal.m + intZero);
        form1Insn(KATI+14);
        curVal.i := ((moduleOffset + (1)) - curVal.i);
        if (curVal.i < 40000B) then {
            curVal.i := (curVal.i - 40000B);
            curVal.i := allocSymtab([24, 29] +
                        (curVal.m * O77777));
        };
        form1Insn(KUJ+I14 + curVal.i);
        while (allClauses != NULL) do {
            padToLeft;
            form1Insn(InsnTemp[UJ] + allClauses->offset);
            allClauses := allClauses->next;
        };
        16211:
        P0715(0, endOfStmt);
        if (not goodMode) then
           disableNorm;
        /* 16217 */
    }
}; /* caseStatement */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure assignStatement(doLHS: boolean);
label
    16332;
var
    lhsExpr, assnExpr: eptr;
    indCnt: Integer;
    srcType, targType: tptr;
{
    if (doLHS) then
        parseLval
    else {
        new(curExpr);
        with curExpr@ do {
            typ := hashTravPtr->typ;
            op := GETVAR;
            id1 := hashTravPtr;
        };
        inSymbol;
    };
    checkSymAndRead(BECOMES);
    bool102z := false;
    targType := curExpr->typ;
    if (targType->k = kindRecord) and
       (SY = LBRACK) then {
        formOperator(gen5);
        indCnt := 0;
        inSymbol;
        l3bool5z := false;
(indices)
        {
            if (SY = COMMA) then {
                indCnt := indCnt + 1;
                inSymbol;
            } else if (SY = RBRACK) then {
                inSymbol;
                exit indices;
            } else /* 16262 */ (q) {
                bool102z := false;
                expression;
                curVal.i := indCnt;
                formOperator(gen6);
                /*=z-*/exit q/*=z+*/
            }; /* 16270 */
            goto indices;
        };
        curExpr := NULL;
    } else /* 16273 */
    if (SY = SEMICOLON) and allowCompat then {
        formOperator(STORE);
        bool102z := true;
        curExpr := NULL;
    } else /* 16303 */ {
        lhsExpr := curExpr;
        expression;
        srcType := curExpr->typ;
        if (typeCheck(targType, srcType)) then {
            if (srcType->k = kindFile) then
                error(75) /*errCannotAssignFiles*/
            else {
                if rangeMismatch and (targType->k = kindRange) then {
                    new(assnExpr);
                    with assnExpr@ do {
                        typ := srcType;
                        op := BOUNDS;
                        expr1 := curExpr;
                        typ2 := targType;
                    };
                    curExpr := assnExpr;
                };
16332:          new(assnExpr);
                with assnExpr@ do {
                    typ := targType;
                    op := ASSIGNOP;
                    expr1 := lhsExpr;
                    expr2 := curExpr;
                };
                curExpr := assnExpr;
            }
        } else if (targType = realType) and
            typeCheck(IntegerType, srcType) then {
            castToReal(curExpr);
            goto 16332;
        } else {
            error(33); /*errIllegalTypesForAssignment*/
        }
    }
    /* 16356 */
}; /* assignStatement */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure compoundStatement;
{
(loop) {
        statement;
        if (SY = SEMICOLON) then {
            inSymbol;
            goto loop;
        }
    }
}; /* compoundStatement */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure ifWhileStatement(delim: Symbol);
{
    disableNorm;
    expression;
    if (curExpr->typ != booleanType) then
        error(errBooleanNeeded)
    else {
        jumpTarget := 0;
        formOperator(gen15);
        l3var10z.i := jumpTarget;
    };
    checkSymAndRead(delim);
    statement;
}; /* ifWhileStatement */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseData;
label
    16545;
type
    DATAREC = record case boolean of
            false: (a: packed array [0..3] of 0..4095);
            true:  (b: Bitset)
        end;
var
    dsize, setcount: Integer;
    l4var3z, l4var4z, l4var5z: Word;
    boundary: eptr;
    l4var7z, l4var8z, l4var9z: Word;
    F: file of DATAREC;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P16432(l5arg1z: Integer);
var
    l5var1z: DATAREC;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function allocDataRef(l6arg1z: Integer): Integer;
{
    if (l6arg1z >= 2048) then {
        curVal.i := l6arg1z;
        allocDataRef := allocSymtab((curVal.m + [24]) * halfWord);
    } else {
        allocDataRef := l6arg1z;
    }
}; /* allocDataRef */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* P16432 */
    l5var1z.a[0] := allocDataRef(l4var4z.i);
    if (FcstCnt = l4var3z.i) then {
        curVal := l4var8z;
        curVal.i := addCurValToFCST;
    } else {
        curVal := l4var3z;
    };
    l5var1z.a[1] := allocSymtab([12,23] + curVal.m * halfWord);
    l5var1z.a[2] := allocDataRef(l5arg1z);
    if (l4var9z.i = 0) then {
        curVal := l4var7z;
        besm(ASN64+24);
        curVal := ;
    } else {
        curVal.i := allocSymtab(l4var7z.m + l4var9z.m * halfWord);
    };
    l5var1z.a[3] := curVal.i;
    l4var9z.i := l5arg1z * l4var4z.i + l4var9z.i;
    F@ := l5var1z;
    put(F);
    setcount := setcount + 1;
    l4var4z.i := 0;
    l4var3z.i := FcstCnt;
}; /* P16432 */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* parseData */
    dsize := FcstCnt;
    inSymbol;
    setcount := 0;
(loop)
    repeat /* 16530 */
        inSymbol;
        setup(boundary);
        if SY != IDENT then {
            if SY = ENDSY then
                exit loop;
            error(errNoIdent);
            curExpr := uVarPtr;
        } else /* 16543 */ {
            if (hashTravPtr = NULL) then {
16545:          error(errNotDefined);
                curExpr := uVarPtr;
                inSymbol;
            } else {
                if (hashTravPtr->cl = VARID) then {
                    parseLval;
                } else goto 16545;
            }
        }; /* 16557 */
        putLeft := true;
        objBufIdx := 1;
        formOperator(gen5);
        if (objBufIdx != 1) then
            error(errVarTooComplex);
        l4var7z.m := (leftInsn * [12,13,14,15,16,17,18,19,20,21,22,23]);
        l4var3z.i := FcstCnt;
        l4var4z.i := 0;
        l4var9z.i := 0;
        repeat /* 16574 */
            expression;
            formOperator(LITINSN);
            l4var8z := curVal;
            if (SY = COLON) then {
                inSymbol;
                l4var5z := curToken;
                if (SY != INTCONST) then {
                    error(62); /* errIntegerNeeded */
                    l4var5z.i := 0;
                } else
                    inSymbol;
            } else
                l4var5z.i := 1;
            if (l4var5z.i != 1) then {
                if (l4var4z.i != 0) then
                    P16432(1);
                l4var4z.i := 1;
                P16432(l4var5z.i);
            } else {
                l4var4z.i := l4var4z.i + 1;
                if (SY = COMMA) then {
                    curVal := l4var8z;
                    toFCST;
                } else {
                    if (l4var4z.i != 1) then {
                        curVal := l4var8z;
                        toFCST;
                    };
                    P16432(1);
                }
            }; /* 16641 */
        until SY != COMMA;
        rollup(boundary);
    until SY != SEMICOLON; /* 16645 */
    if (SY != ENDSY) then
        error(errBadSymbol);
    reset(F);
    while not eof(F) do {
        write(FCST, F->b);
        get(F);
    };
    int92z := FcstCnt - dsize;
    FcstCnt := dsize;
    int93z := setcount;
    /* 16666 */
}; /* parseData */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure standProc;
label
    17753, 20041;
var
    l4typ1z, l4typ2z, l4typ3z: tptr;
    l4var4z, l4var5z: eptr;
    l4exp6z: eptr;
    l4exp7z, l4exp8z, l4exp9z: eptr;
    l4bool10z,
    l4bool11z, l4bool12z: boolean;
    l4var13z, l4var14z, l4var15z: Word;
    procNo: Integer;
    helperNo: Integer;
    l4var18z: opgen;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure verifyType(l5arg1z: tptr);
{
    if (hashTravPtr != NULL) and
       (hashTravPtr->cl >= VARID) then {
        parseLval;
        if (l5arg1z != NULL) and
           not typeCheck(l5arg1z, curExpr->typ) then
            error(errNeedOtherTypesOfOperands);
    } else {
        error(errNotDefined);
        curExpr := uVarPtr;
    }
}; /* verifyType */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure startReadOrWrite(l5arg1z: boolean);
{
    expression;
    l4typ3z := curExpr->typ;
    l4exp7z := curExpr;
    if not (l5arg1z) then {
        if not (curExpr->op IN lvalOpSet) then
            error(27); /* errExpressionWhereVariableExpected */
    };
    if (l4exp9z = NULL) then {
        if (l4typ3z->k = kindFile) then {
            l4exp9z := curExpr;
        } else {
            new(l4exp9z);
            l4exp9z->typ := textType;
            l4exp9z->op := GETVAR;
            if (l5arg1z) then {
                l4exp9z->id1 := outputFile;
            } else {
                if (inputFile != NULL) then
                    l4exp9z->id1 := inputFile
                else (q) {
                    error(37); /* errInputMissingInProgramHeader */
                    /*=z-*/exit q/*=z+*/
                }
            }
        };
        arg2Type := l4exp9z->typ;
        l4var13z.b := typeCheck(arg2Type->base, charType);
        l4bool12z := true;
        new(l4exp8z);
        l4exp8z->typ := arg2Type->base;
        l4exp8z->op := FILEPTR;
        l4exp8z->expr1 := l4exp9z;
        new(l4exp6z);
        l4exp6z->typ := l4exp8z->typ;
        l4exp6z->op := ASSIGNOP;
        if (l5arg1z) then
            l4exp6z->expr1 := l4exp8z
        else
            l4exp6z->expr2 := l4exp8z;
    } /* 17002 */
}; /* startReadOrWrite */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseWidthSpecifier(var l5arg1z: eptr);
{
    expression;
    if not typeCheck(IntegerType, curExpr->typ) then {
        error(14); /* errExprIsNotInteger */
        curExpr := uVarPtr;
    };
    l5arg1z := curExpr;
}; /* parseWidthSpecifier */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure callHelperWithArg;
{
    if ([12] <= set145z) or l4bool12z then {
        curExpr := l4exp9z;
        formOperator(gen8);
    };
    l4bool12z := false;
    formAndAlign(getHelperProc(helperNo));
    disableNorm;
}; /* callHelperWithArg */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P17037;
{
    set145z := set145z - [12];
    if (helperNo != 49) and             /* P/RDC */
       not typeCheck(l4exp8z->typ, l4exp7z->typ) then
        error(34) /* errTypeIsNotAFileElementType */
    else {
        if (helperNo = 29) then {       /* P/PF */
            l4exp6z->expr2 := l4exp7z;
        } else {
            if (helperNo = 49) then
                helperNo := 30;         /* P/GF */
            l4exp6z->expr1 := l4exp7z;
        };
        curExpr := l4exp6z;
        formOperator(gen7);
        callHelperWithArg;
    }
}; /* P17037 */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure checkElementForReadWrite;
var
    l5typ1z: tptr;
{
    set145z := set145z - [12];
    if (l4typ3z->k = kindRange) then
        l4typ3z := l4typ3z->base;
    curVarKind := l4typ3z->k;
    helperNo := 36;                   /* P/WI */
    if (l4typ3z = IntegerType) then
        l4var15z.i := 10
    else if (l4typ3z = realType) then {
        helperNo := 37;               /* P/WR */
        l4var15z.i := 14;
    } else if (l4typ3z = charType) then {
        helperNo := 38;               /* P/WC */
        l4var15z.i := 1;
    } else if (curVarKind = kindScalar) then {
        helperNo := 41;               /* P/WX */
        dumpEnumNames(l4typ3z);
        l4var15z.i := 8;
    } else if (isCharArray(l4typ3z)) then {
        l5typ1z := ref(l4typ3z->range@);
        l4var15z.i := l5typ1z->right - l5typ1z->left + 1;
        if not (l4typ3z->pck) then
            helperNo := 81            /* P/WA */
        else if (6 >= l4var15z.i) then
            helperNo := 39            /* P/A6 */
        else
            helperNo := 40;           /* P/A7 */
    } else if (l4typ3z->size = 1) then {
        helperNo := 42;               /* P/WO */
        l4var15z.i := 17;
    } else (q) {
        error(34); /* errTypeIsNotAFileElementType */
        /*=z-*/exit q/*=z+*/
    }
}; /* checkElementForReadWrite */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure writeProc;
{
    l4exp9z := NULL;
    l4var13z.b := true;
    repeat {
        startReadOrWrite(true);
        if (l4exp7z != l4exp9z) then {
            if not (l4var13z.b) then {
                helperNo := 29;         /* P/PF */
                P17037;
            } else {
                checkElementForReadWrite;
                l4var5z := NULL;
                l4var4z := NULL;
                if (SY = COLON) then
                    parseWidthSpecifier(l4var4z);
                if (SY = COLON) then {
                    parseWidthSpecifier(l4var5z);
                    if (helperNo != 37) then    /* P/WR */
                        error(35); /* errSecondSpecifierForWriteOnlyForReal */
                } else {
                    if (curToken = litOct) then {
                        helperNo := 42; /* P/WO */
                        l4var15z.i := 17;
                        if (l4typ3z->size != 1) then
                            error(34); /* errTypeIsNotAFileElementType */
                        inSymbol;
                    }
                };
                l4bool11z := false;
                if (l4var4z = NULL) and
                   (helperNo IN [38,39,40]) then {  /* WC,A6,A7 */
                    helperNo := helperNo + 5;       /* CW,6A,7A */
                    l4bool11z := true;
                } else {
                    if (l4var4z = NULL) then {
                        curVal := l4var15z;
                        formOperator(gen9);
                    } else {
                        curExpr := l4var4z;
                        formOperator(LOAD);
                    }
                };
                if (helperNo = 37) then {       /* P/WR */
                    if (l4var5z = NULL) then {
                        curVal.i := 4;
                        form1Insn(KXTS+I8 + getFCSToffset);
                    } else {
                        curExpr := l4var5z;
                        formOperator(gen10);
                    }
                };
                curExpr := l4exp7z;
                if (l4bool11z) then {
                    if (helperNo = 45) then     /* P/7A */
                        l4var18z := gen11
                    else
                        l4var18z := LOAD;
                } else {
                    if (helperNo = 40) or       /* P/A7 */
                       (helperNo = 81) then     /* P/WA */
                        l4var18z := gen12
                    else
                        l4var18z := gen10;
                };
                formOperator(l4var18z);
                if (helperNo IN [39,40,44,45]) or /* A6,A7,6A,7A */
                   (helperNo = 81) then
                    form1Insn(KVTM+I10 + l4var15z.i)
                else {
                    if (helperNo = 41) then /* P/WX */
                        form1Insn(KVTM+I11 + l4typ3z->start);
                };
                callHelperWithArg;
            }
        }
    } until (SY != COMMA);
    if (procNo = 11) then {
        helperNo := 46;                 /* P/WL */
        callHelperWithArg;
    };
    set145z := set145z + [12];
    if (l4var14z.i = moduleOffset) then
        error(36); /*errTooFewArguments */
}; /* writeProc */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure readProc;
label
    17346, 17362;
{
    l4exp9z := NULL;
    l4var13z.b := true;
    l4var14z.i := moduleOffset;
    repeat {
        startReadOrWrite(false);
        if (l4exp7z != l4exp9z) then {
            if not (l4var13z.b) then {
                helperNo := 30;         /* P/GF */
17346:
                P17037;
           } else {
                checkElementForReadWrite;
                if (helperNo = 38) then {       /* P/WC */
                    helperNo := 49;             /* P/RDC */
                    goto 17346;
                };
                if (helperNo = 39) or           /* A6,A7 */
                   (helperNo = 40) then {
                    helperNo := 51;             /* P/RA7 */
17362:
                    curExpr := l4exp7z;
                    formOperator(gen5);
                    form1Insn(KVTM+I10 + l4var15z.i);
                    callHelperWithArg;
                } else (q) {
                    if (helperNo = 81) then {   /* P/WA */
                        helperNo := 90;         /* P/RA */
                        goto 17362;
                    };
                    helperNo := helperNo + 11;
                    callHelperWithArg;
                    curExpr := l4exp7z;
                    formOperator(STORE);
                    /*=z-*/exit q/*=z+*/
                }
            }
        }
    } until (SY != COMMA);
    set145z := set145z + [12];
    if (procNo = 13) then {
        helperNo := 53;                 /* P/RL */
        callHelperWithArg;
    };
    if (l4var14z.i = moduleOffset) then
        error(36); /* errTooFewArguments */
}; /* readProc */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure checkArrayArg;
{
    verifyType(NULL);
    l4exp9z := curExpr;
    l4typ1z := curExpr->typ;
    if (l4typ1z->pck) or
       (l4typ1z->k != kindArray) then
        error(errNeedOtherTypesOfOperands);
    checkSymAndRead(COMMA);
    bool102z := false;
    expression;
    l4exp8z := curExpr;
    if not typeCheck(l4typ1z->range, l4exp8z->typ) then
        error(errNeedOtherTypesOfOperands);
}; /* checkArrayArg */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure doPackUnpack;
var
    t: tptr;
{
    new(l4exp7z);
    l4exp7z->typ := l4typ1z->base;
    l4exp7z->op := GETELT;
    l4exp7z->expr1 := l4exp9z;
    l4exp7z->expr2 := l4exp8z;
    t := ref(l4exp6z->typ@);
    if (t->k != kindArray) or
       not t->pck or
       not typeCheck(t->base, l4typ1z->base) or
       not typeCheck(l4typ1z->range, t->range) then
        error(errNeedOtherTypesOfOperands);
    new(curExpr);
    curExpr->val.c := chr(procNo + 50);
    curExpr->expr1 := l4exp7z;
    curExpr->expr2 := l4exp6z;
    formOperator(gen16);
}; /* doPackUnpack */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* standProc */
    curVal.i := l3idr12z->low;
    procNo := curVal.i;
    l4bool10z := (SY = LPAREN);
    l4var14z.i := moduleOffset;
    if not l4bool10z and
       (procNo IN [0:5,8:10,12,16:28]) then
        error(45); /* errNoOpenParenForStandProc */
    if (procNo IN [0,1,2,3,4,5,8,9]) then {
        inSymbol;
        if (hashTravPtr->cl < VARID) then
            error(46); /* errNoVarForStandProc */
        parseLval;
        arg1Type := curExpr->typ;
        curVarKind := arg1Type->k;
    };
    if (procNo IN [0..6]) then
        jumpTarget := getHelperProc(29 + procNo); /* P/PF */
    case procNo of
    0, 1, 2, 3: { /* put, get, rewrite, reset */
        if (curVarKind != kindFile) then
            error(47); /* errNoVarOfFileType */
        if (procNo = 3) and
           (SY = COMMA) then {
            formOperator(gen8);
            expression;
            if (not typeCheck(IntegerType, curExpr->typ)) then
                error(14); /* errExprIsNotInteger */
            formOperator(LOAD);
            formAndAlign(getHelperProc(97)); /*"P/RE"*/
        } else {
            formOperator(FILEACCESS);
        }
    };
    4, 5: { /* new, dispose */
        if (curVarKind != kindPtr) then
            error(13); /* errVarIsNotPointer */
        heapCallsCnt := heapCallsCnt + 1;
        l4exp9z := curExpr;
        if (procNo = 5) then
            formOperator(gen5);
        l2typ13z := arg1Type->base;
        ii := l2typ13z->size;
        if (charClass = EQOP) then {
            expression;
            if not typeCheck(IntegerType, curExpr->typ) then
                error(14); /* errExprIsNotInteger */
            formOperator(LOAD);
            form1Insn(KATI+14);
        } else {
            if (arg1Type->base->k = kindRecord) then /*=z-*/(x)/*=z+*/ {
                l4typ1z := l2typ13z->base;
(loop)          while (SY = COMMA) and (l4typ1z != NULL) do {
                    with l4typ1z@ do
                        ; /* useless */
                    inSymbol;
                    parseLiteral(l4typ2z, curVal, true);
                    if (l4typ2z = NULL) then
                        exit loop
                    else (q) {
                        inSymbol;
(loop2)                 while (l4typ1z != NULL) do {
                            l4typ2z := l4typ1z;
                            while (l4typ2z != NULL) do {
                                if (l4typ2z->sel = curVal) then {
                                    ii := l4typ1z->size;
                                    exit loop2;
                                };
                                l4typ2z := l4typ2z->r6;
                                /*=z-*/(x);/*=z+*/
                            };
                            l4typ1z := l4typ1z->next;
                        };
                        /*=z-*/exit q/*=z+*/
                    };
                }
            };
            form1Insn(KVTM+I14+getValueOrAllocSymtab(ii));
        };
        formAndAlign(jumpTarget);
        if (procNo = (4)) then {
            curExpr := l4exp9z;
            formOperator(STORE);
        }
    };
    6: { /* halt */
        formAndAlign(jumpTarget);
        exit
    };
    7: { /* stop */
        form1Insn(KE74);
        exit
    };
    8, 9: { /* setup, rollup */
        if (curVarKind != kindPtr) then
            error(13); /* errVarIsNotPointer */
        if (procNo = 8) then {
            form1Insn(KXTA+HEAPPTR);
            formOperator(STORE);
        } else {
            formOperator(LOAD);
            form1Insn(KATX+HEAPPTR);
        }
    };
    10: { /* write */
        writeProc;
    };
    11:
17753: { /* writeln */
        if (SY = LPAREN) then {
            writeProc;
        } else {
            formAndAlign(getHelperProc(54)); /*"P/WOLN"*/
            exit
        }
    };
    12: { /* read */
        readProc;
    };
    13: { /* readln */
        if (SY = LPAREN) then {
            readProc;
        } else {
            formAndAlign(getHelperProc(55)); /*"P/RILN"*/
            exit
        }
    };
    14: { /* exit */
        l4bool10z := (SY = LPAREN);
        if (l4bool10z) then
            inSymbol;
        if (SY = IDENT) then {
            if not structBranch(false) then
                error(1); /* errCommaOrSemicolonNeeded */
            inSymbol;
        } else {
            formJump(int53z);
        };
        if not (l4bool10z) then
            exit
    };
    15: { /* debug */
        if (debugPrint IN optSflags.m) then {
            procNo := 11;
            goto 17753;
        };
        while (SY != RPAREN) do
            inSymbol;
    };
    16: { /* besm */
        expression;
        formOperator(LITINSN);
        formAndAlign(curVal.i);
    };
    17: { /* mapia */
        l4typ1z := IntegerType;
        l4typ2z := alfaType;
20041:
        expression;
        if not typeCheck(curExpr->typ, l4typ1z) then
            error(errNeedOtherTypesOfOperands);
        checkSymAndRead(COMMA);
        formOperator(LOAD);
        if (procNo = 17) then {
            form3Insn(ASN64-33, KAUX+BITS15, KAEX+ASCII0);
        } else {
            form3Insn(KAPX+BITS15, ASN64+33, KAEX+ZERO);
        };
        verifyType(l4typ2z);
        formOperator(STORE);
    };
    18: { /* mapai */
        l4typ1z := alfaType;
        l4typ2z := IntegerType;
        goto 20041;
    };
    19, 20: { /* pck, unpck */
        inSymbol;
        verifyType(charType);
        checkSymAndRead(COMMA);
        formOperator(gen8);
        verifyType(alfaType);
        if (procNo = 20) then {
            formOperator(LOAD);
        };
        formAndAlign(getHelperProc(procNo - 6));
        if (procNo = 19) then
            formOperator(STORE);
    };
    21: { /* pack */
        inSymbol;
        checkArrayArg;
        checkSymAndRead(COMMA);
        verifyType(NULL);
        l4exp6z := curExpr;
        doPackUnpack;
    };
    22: { /* unpack */
        inSymbol;
        verifyType(NULL);
        l4exp6z := curExpr;
        checkSymAndRead(COMMA);
        checkArrayArg;
        doPackUnpack;
    };
    23, 24, 25, 26, 27, 28: { /* MARS procedures */
        l3bool5z := 24 < procNo;
        repeat {
            expression;
            if (curExpr->typ->size != 1) then
                error(5); /*errSimpleTypeReq*/
            formOperator(LOAD);
            if (l3bool5z) then {
                checkSymAndRead(COMMA);
                verifyType(NULL);
                l4exp9z := curExpr;
                if (SY = COLON) then {
                    expression;
                    formOperator(gen10);
                } else {
                    form2Insn(KVTM + I14 + l4exp9z->typ->size,
                              KITS + 14);
                };
                curExpr := l4exp9z;
                formOperator(gen12);
            } else {
                form2Insn(InsnTemp[XTS], InsnTemp[XTS]);
            };
            form1Insn(KWTC + I14 + 77751B + procNo);
            formAndAlign(getHelperProc(80)); /*"PAIB"*/
        } until (SY != COMMA);
    };
    end; /* 20257 */
    if procNo in [0,1,2,3,5,10,11,12,13,21,22] then
        arithMode := 1;
    checkSymAndRead(RPAREN);
    /* 20265 */
}; /* standProc */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* statement */
    setup(boundary);
    bool110z := false;
    startLine := lineCnt;
    if set147z = halfWord then
        parseData
    else {
        if SY = INTCONST then {
            set146z := [];
            l3var2z := numLabList;
            disableNorm;
            l3bool5z := true;
            padToLeft;
            while l3var2z != l2var16z do { with l3var2z@ do
                if id != curToken then {
                    l3var2z := next;
                } else {
                    l3bool5z := false;
                    if (defined) then {
                        curVal.i := line;
                        error(17); /* errLblAlreadyDefinedInLine */;
                    } else {
                        l3var2z->line := lineCnt;
                        l3var2z->defined := true;
                        padToLeft;
                        if l3var2z->offset = 0 then {
                            /* empty */
                        } else if (l3var2z->offset >= 74000B) then {
                            curVal.i := (moduleOffset - 40000B);
                            symTab[l3var2z->offset] := [24,29] +
                                                         curVal.m * O77777;
                        } else (q) {
                            P0715(0, l3var2z->offset);
                            /*=z-*/exit q/*=z+*/
                        }; /* 20342 */
                        l3var2z->offset := moduleOffset;
                    };
                    l3var2z := l2var16z;
                };
            }; /* while 20346 */
            if (l3bool5z) then
                error(16); /* errLblNotDefinedInBlock */;
            inSymbol;
            checkSymAndRead(COLON);
        }; /* 20355*/
        if (DebugInteractive IN optSflags.m) and
           (debugLine != lineCnt) then {
            P0715(-1, 96 /* "P/DD" */);
            debugLine := lineCnt;
            arithMode := 1;
        };
        l3var4z.b := (SY IN [BEGINSY,CASESY,REPEATSY,SELECTSY]);
        if (l3var4z.b) then
            lineNesting := lineNesting + 1;
(ident)
        if SY = IDENT then {
            if hashTravPtr != NULL then {
                l3var6z := hashTravPtr->cl;
                if l3var6z >= VARID then {
                    assignStatement(true);
                } else {
                    if l3var6z = ROUTINEID then {
                        if hashTravPtr->typ = NULL then {
                            l3idr12z := hashTravPtr;
                            inSymbol;
                            if l3idr12z->offset = 0 then {
                                standProc;
                                exit ident;
                            };
                            parseCallArgs(l3idr12z);
                        } else {
                            assignStatement(false);
                        };
                    } else {
                        error(32); /* errWrongStartOfOperator */
                        goto 8888;
                    }
                };
                formOperator(gen7);
            } else {
                error(errNotDefined);
8888:           skip(skipToSet + statEndSys);
            };
        } else /* 20431 */ if (SY = LPAREN) then {
            set146z := [];
            inSymbol;
            if (SY != IDENT) then {
                error(errNoIdent);
                goto 8888;
            };
            new(l3var3z);
            padToLeft;
            disableNorm;
            with l3var3z@ do {
                next := strLabList;
                ident := curIdent;
                offset := moduleOffset;
                exitTarget := 0;
            };
            strLabList := l3var3z;
            inSymbol;
            checkSymAndRead(RPAREN);
            statement;
            P0715(0, l3var3z->exitTarget);
            strLabList := strLabList->next;
        } else /* 20463 */ if (SY = BEGINSY) then
(rep)   {
            inSymbol;
(skip)      {
                compoundStatement;
                if (SY != ENDSY) then {
                    stmtName := ' BEGIN';
                    requiredSymErr(SEMICOLON);
                    reportStmtType(startLine);
                    skip(bigSkipSet);
                    if (SY IN statBegSys) then
                        goto skip;
                    if (SY != SEMICOLON) then
                        exit rep;
                    goto rep;
                    /*=z-*/(q) exit rep;/*=z+*/
                };
            };
            inSymbol;
        } else /* 20511 */ if (SY = GOTOSY) then {
            inSymbol;
            if (SY != INTCONST) then {
                if structBranch(true) then {
                    inSymbol;
                    exit;
                } else
                    goto 8888;
            };
            disableNorm;
            l3var2z := numLabList;
(loop)      if (l3var2z != NULL) then with l3var2z@ do {
                if (id != curToken) then {
                    l3var2z := next;
                } else {
                    if (curFrameRegTemplate = frame) then {
                        if (offset >= 40000B) then {
                            form1Insn(InsnTemp[UJ] + offset);
                        } else {
                            formJump(offset);
                        }
                    } else {
                        if offset = 0 then {
                            offset := symTabPos;
                            putToSymTab([]);
                        };
                        form3Insn(frame + (KMTJ + 13), KVTM+I14 + offset,
                                  getHelperProc(18/*"P/RC"*/) + (-64100000B));
                    };
                    exit loop;
                };
                goto loop;
            } else
                error(18); /* errLblNotDefined */
            inSymbol;
        } else /* 20571 */ if (SY = IFSY) then {
            ifWhileStatement(THENSY);
            if (SY = ELSESY) then {
                l3var11z.i := 0;
                formJump(l3var11z.i);
                P0715(0, l3var10z.i);
                l3var8z.i := arithMode;
                arithMode := 1;
                inSymbol;
                statement;
                P0715(0, l3var11z.i);
                if (l3var8z.i != arithMode) then {
                    arithMode := 2;
                    disableNorm;
                }
            } else {
                P0715(0, l3var10z.i);
            }
        } else /* 20625 */ if (SY = WHILESY) then {
            set146z := [];
            disableNorm;
            padToLeft;
            l3var8z.i := moduleOffset;
            ifWhileStatement(DOSY);
            disableNorm;
            form1Insn(InsnTemp[UJ] + l3var8z.i);
            P0715(0, l3var10z.i);
            arithMode := 1;
        } else /* 20644 */ if (SY = REPEATSY) then {
            set146z := [];
            disableNorm;
            padToLeft;
            l3var7z.i := moduleOffset;
            repeat
                inSymbol;
                statement;
            until (SY != SEMICOLON);
            if (SY != UNTILSY) then {
                requiredSymErr(UNTILSY);
                stmtName := 'REPEAT';
                reportStmtType(startLine);
                goto 8888;
            };
            disableNorm;
            expression;
            if (curExpr->typ != booleanType) then {
                error(errBooleanNeeded)
            } else {
                jumpTarget := l3var7z.i;
                formOperator(gen15);
            };
        } else /* 20676 */
        if (SY = FORSY) then {
            set146z := [];
            forStatement;
        } else /* 20702 */ if (SY = SELECTSY) then {
            disableNorm;
            l3bool5z := true;
            l3var11z.i := 0;
            /* 20707 */
            repeat
                arithMode := 1;
                expression;
                if (curExpr->typ != booleanType) then {
                    error(errBooleanNeeded);
                } else {
                    jumpTarget := 0;
                    formOperator(gen15);
                    l3var10z.i := jumpTarget;
                };
                checkSymAndRead(COLON);
                statement;
                formJump(l3var11z.i);
                l3bool5z := l3bool5z and (arithMode = 1);
                P0715(0, l3var10z.i);
            until (SY != SEMICOLON);
            checkSymAndRead(ENDSY);
            P0715(0, l3var11z.i);
            if not l3bool5z then {
                arithMode := 2;
                disableNorm;
            }
        } else /* 20751 */ if (SY = CASESY) then {
            caseStatement
        } else if (SY = WITHSY) then {
            withStatement;
            /*=z-*/(q) exit q;/*=z+*/
        }; /* 20757 */
        if (l3var4z.b) then
            lineNesting := lineNesting - 1;
        rollup(boundary);
        if (bool110z) then {
            bool110z := false;
            goto 8888;
        }
    }
    /* 20766 */
}; /* statement */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure outputObjFile;
var
    idx: Integer;
{
    padToLeft;
    objBufIdx := objBufIdx - 1;
    for idx to objBufIdx do
        write(CHILD, objBuffer[idx]);
    lineStartOffset := moduleOffset;
    prevOpcode := 0;
}; /* outputObjFile */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure defineRoutine;
var
    l3var1z, l3var2z, l3var3z: Word;
    l3int4z: Integer;
    l3idr5z: irptr;
    l3var6z, l3var7z: Word;
{
    objBufIdx := 1;
    objBuffer[objBufIdx] := [];
    curInsnTemplate := InsnTemp[XTA];
    bool48z := 22 IN l2idr2z->flags;
    lineStartOffset := moduleOffset;
    l3var1z := ;
    int92z := 2;
    expr63z := NULL;
    arithMode := 1;
    set146z := [];
    set147z := [curProcNesting+1..6];
    set148z := set147z - [minel(set147z)];
    l3var7z.m := set147z;
    int53z := 0;
    set145z := [1:15] - set147z;
    if (curProcNesting != 1) then
        parseDecls(2);
    l2int21z := localSize;
    if (SY != BEGINSY) then
        requiredSymErr(BEGINSY);
    if 23 IN l2idr2z->flags then {
        l3idr5z := l2idr2z->argList;
        l3int4z := 3;
        if (l2idr2z->typ != NULL) then
        l3int4z := 4;
        while (l3idr5z != l2idr2z) do {
            if (l3idr5z->cl = VARID) then {
                l3var2z.i := l3idr5z->typ->size;
                if (l3var2z.i != 1) then {
                    form3Insn(KVTM+I14 + l3int4z,
                              KVTM+I12 + l3var2z.i,
                              KVTM+I11 + l3idr5z->value);
                    formAndAlign(getHelperProc(73)); /* "P/LNGPAR" */
                }
            };
            l3int4z := l3int4z + 1;
            l3idr5z := l3idr5z->list;
        }
    }; /* 21105 */
    if checkBounds or not (NoStackCheck IN optSflags.m) then
        P0715(-1, 95); /* P/SC */
    l3var2z.i := lineNesting;
    repeat
        statement;
        if (SY = SEMICOLON) then {
            if (curProcNesting = 1) then
                requiredSymErr(PERIOD);
            inSymbol;
            l2bool8z := (SY IN blockBegSys);
            if not l2bool8z and not errors then
                error(84); /* errErrorInDeclarations */
        } else {
            if (SY = PERIOD) and (curProcNesting = 1) then
                l2bool8z := true
            else (q) {
                errAndSkip(errBadSymbol, skipToSet);
                l2bool8z := (SY IN blockBegSys);
                /*=z-*/exit q/*=z+*/
            }
        };
    until l2bool8z;
    l2idr2z->flags := (set145z * [0:15]) + (l2idr2z->flags - l3var7z.m);
    lineNesting := l3var2z.i - 1;
    if (int53z != 0) then
        P0715(0, int53z);
    if not bool48z and not doPMD and (l2int21z = 3) and
       (curProcNesting != 1) and (set145z * [1:15] != [1:15]) then {
        objBuffer[1] := [7:11,21:23,28,31];
        with l2idr2z@ do
            flags := flags + [25];
        if (objBufIdx = 2) then {
            objBuffer[1] := [0,1,3:5];
            putLeft := true;
        } else {
            l2idr2z->pos := l3var1z.i;
            if 13 IN set145z then {
                curVal.i := minel([1:15] - set145z);
                besm(ASN64-24);
                l3var7z := ;
                objBuffer[2] := objBuffer[2] + [0,1,3,6,9] + l3var7z.m;
            } else {
                curVal.i := (13);
            };
            form1Insn(InsnTemp[UJ] + indexreg[curVal.i]);
        }
    } else /* 21220 */ {
        if (l2int11z = 0) then
            jj := 27    /* P/E */
        else
            jj := 28;   /* P/EF */
        form1Insn(getHelperProc(jj) + (-I13-100000B));
        if (curProcNesting = 1) then {
            parseDecls(2);
            if S3 IN optSflags.m then
                formAndAlign(getHelperProc(78)); /* "P/PMDSET" */
            form1Insn(InsnTemp[UJ] + l3var1z.i);
            curVal.i := l2idr2z->pos - 40000B;
            symTab[74002B] := [24,29] + (curVal.m * halfWord);
        };
        curVal.i := l2int21z;
        if (curProcNesting != 1) then {
            curVal.i := curVal.i - 2;
            l3var7z := curVal;
            besm(ASN64-24);
            l3var7z := ;
            objBuffer[savedObjIdx] := objBuffer[savedObjIdx] +
                                       l3var7z.m + [0,1,2,3,4,6,8];
        }
    }; /* 21261 */
    outputObjFile;
}; /* defineRoutine */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure initScalars;
var
    l3var1z, noProgram, l3var3z, l3var4z: Word;
    l3var5z, l3var6z: Integer;
    l3var7z: irptr;
    l3var8z, l3var9z: Integer;
    temptype: tptr;
    l3var11z: Word;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure regSysType(l4arg1z:Integer; l4arg2z: tptr);
{
    new(curIdRec = 5);
    curIdRec@ := [l4arg1z, 0, , l4arg2z, TYPEID];
    addToHashTab(curIdRec);
}; /* regSysType */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure regSysEnum(l4arg1z: Integer; l4arg2z: Integer);
{
    new(curIdRec = 7);
    curIdRec@ := [l4arg1z, 48, , temptype, ENUMID, NULL, l4arg2z];
    addToHashTab(curIdRec);
}; /* regSysEnum */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure regSysProc(l4arg1z: Integer);
{
    new(curIdRec = 6);
    curIdRec@ := [l4arg1z, 0, , temptype, ROUTINEID, l3var9z];
    l3var9z := l3var9z + 1;
    addToHashTab(curIdRec);
}; /* registerSysProc */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* initScalars */
    new(booleanType, kindScalar);
    with booleanType@ do {
        size := 1;
        bits := 1;
        k := kindScalar;
        numen := 2;
        start := 0;
    };
    new(IntegerType, kindScalar);
    with IntegerType@ do {
        size := 1;
        bits := 48;
        k := kindScalar;
        numen := 100000;
        start := -1;
        enums := NULL;
    };
    new(charType, kindScalar);
    with charType@ do {
        size := 1;
        bits := (8);
        k := kindScalar;
        numen := 256;
        start := -1;
        enums := NULL;
    };
    new(realType, kindArray);   /* could use kindReal to save 5 Words */
    with realType@ do {
        size := 1;
        bits := 48;
        k := kindReal;
    };
    new(setType, kindSet);
    with setType@ do {
        size := 1;
        bits := 48;
        k := kindSet;
        base := IntegerType;
    };
    new(pointerType, kindPtr);
    with pointerType@ do {
        size := 1;
        bits := 48;
        k := kindPtr;
        base := pointerType;
    };
    new(textType, kindFile);
    with textType@ do {
        size := 30;
        bits := 48;
        k := kindFile;
        base := charType;
        elsize := 8;
    };
    new(alfaType,kindArray);
    with alfaType@ do {
        size := 1;
        bits := 48;
        k := kindArray;
        base := charType;
        range := temptype;
        pck := true;
        perWord := 6;
        pcksize := 8;
    };
    smallStringType[6] := alfaType;
    regSysType(51566445474562C/*" INTEGER"*/, IntegerType);
    regSysType(42575754454156C/*" BOOLEAN"*/, booleanType);
    regSysType(43504162C/*"    CHAR"*/, charType);
    regSysType(62454154C/*"    REAL"*/, realType);
    regSysType(41544641C/*"    ALFA"*/, alfaType);
    regSysType(64457064C/*"    TEXT"*/, textType);
    temptype := booleanType;
    regSysEnum(64626545C/*"    TRUE"*/, (1C));
    hashTravPtr := curIdRec;
    regSysEnum(4641546345C/*"   FALSE"*/, (0C));
    curIdRec->list := hashTravPtr;
    booleanType->enums := curIdRec;
    maxSmallString := 0;
    for strLen := 2 to 5 do
        makeStringType(smallStringType[strLen]);
    maxSmallString := 6;
    new(curIdRec = 7);
    with curIdRec@ do {
        offset := 0;
        typ := IntegerType;
        cl := VARID;
        list := NULL;
        value := 7;
    };
    new(uVarPtr);
    with uVarPtr@ do {
        typ := IntegerType;
        op := GETVAR;
        id1 := curIdRec;
    };
    new(uProcPtr, 12);
    with uProcPtr@ do {
        typ := NULL;
        list := NULL;
        argList := NULL;
        preDefLink := NULL;
        pos := 0;
    };
    temptype := NULL;
    l3var9z := 0;
    for l3var5z := 0 to 28 do
        regSysProc(systemProcNames[l3var5z]);
    l3var9z := 0;
    temptype := realType;
    regSysProc(63616264C/*"    SQRT"*/);
    regSysProc(635156C/*"     SIN"*/);
    regSysProc(435763C/*"     COS"*/);
    regSysProc(416243644156C/*"  ARCTAN"*/);
    regSysProc(416243635156C/*"  ARCSIN"*/);
    regSysProc(5456C/*"      LN"*/);
    regSysProc(457060C/*"     EXP"*/);
    regSysProc(414263C/*"     ABS"*/);
    temptype := IntegerType;
    regSysProc(6462655643C/*"   TRUNC"*/);
    temptype := booleanType;
    regSysProc(574444C/*"     ODD"*/);
    temptype := IntegerType;
    regSysProc(576244C/*"     ORD"*/);
    temptype := charType;
    regSysProc(435062C/*"     CHR"*/);
    regSysProc(63654343C/*"    SUCC"*/);
    regSysProc(60624544C/*"    PRED"*/);
    temptype := booleanType;
    regSysProc(455746C/*"     EOF"*/);
    temptype := pointerType;
    regSysProc(624546C/*"     REF"*/);
    temptype := booleanType;
    regSysProc(45575456C/*"    EOLN"*/);
    temptype := IntegerType;
    regSysProc(636162C/*"     SQR"*/);
    regSysProc(6257655644C/*"   ROUND"*/);
    regSysProc(43416244C/*"    CARD"*/);
    regSysProc(5551564554C/*"   MINEL"*/);
    temptype := pointerType;
    regSysProc(606462C/*"     PTR"*/);
    l3var11z.i := 30;
    l3var11z.m := l3var11z.m * halfWord + [24,27,28,29];
    new(programObj, 12);
    curVal.i := 576564606564C/*"  OUTPUT"*/;
    l3var3z := curVal;
    curVal.i := 5156606564C/*"   INPUT"*/;
    l3var4z := curVal;
    curVal.i := 5657606257476241C/*"NOPROGRA"*/;
    noProgram := curVal;
    test1(PROGRAMSY, (skipToSet + [IDENT,LPAREN]));
    symTabPos := 74004B;
    with programObj@ do {
        if (SY = IDENT) then {
            curVal := curIdent;
            id := ;
            pos := 0;
            symTab[74000B] := makeNameWithStars(true);
        } else {
            id.m := [3];
            error(errNoIdent);
            skip(skipToSet + [LPAREN]);
        };
    };
    if (curIdent != noProgram) then {
        entryPtTable[1] := symTab[74000B];
        entryPtTable[3] :=
            [0,1,6,7,10,12,14:18,21:25,28,30,35,36,38,39,41];/*"PROGRAM "*/
        entryPtTable[2] := [1];
        entryPtTable[4] := [1];
        entryPtCnt := 5;
        write(CHILD, [0,4,6,9:12,23,28,29,33:36,46]);/*10 24 74001 00 30 74002*/
        moduleOffset := 40001B;
    } else {
        entryPtCnt := 1;
        moduleOffset := 40000B;
    };
    programObj->argList := NULL;
    programObj->flags := [];
    objBufIdx := 1;
    temptype := IntegerType;
    defineRange(temptype, 1, 6);
    alfaType->range := temptype;
    int93z := 0;
    inSymbol;
    test1(LPAREN, skipToSet + [IDENT]);
    outputObjFile;
    outputFile := NULL;
    inputFile := NULL;
    externFileList := NULL;
    new(l3var7z, 12);
    lineStartOffset := moduleOffset;
    with l3var7z@ do {
        id := l3var3z;
        offset := 0;
        typ := textType;
        cl := VARID;
        list := NULL;
    };
    curVal.i := 1257656460656412C/*"*OUTPUT*"*/;
    l3var7z->value := allocExtSymbol(l3var11z.m);
    addToHashTab(l3var7z);
    l3var5z := 1;
    while SY = IDENT do {
        l3var8z := 0;
        curVal := curIdent;
        l3var1z.m := makeNameWithStars(false);
        if (curIdent = l3var4z) then {
            new(inputFile, 12);
            with inputFile@ do {
                id := curIdent;
                offset := 0;
                typ := textType;
                cl := VARID;
                list := NULL;
            };
            curVal := l3var1z;
            inputFile->value := allocExtSymbol(l3var11z.m);
            addToHashTab(inputFile);
            l3var8z := lineCnt;
        } else if (curIdent = l3var3z) then {
            outputFile := l3var7z;
            l3var8z := lineCnt;
        }; /* 21745 */
        curExternFile := externFileList;
        while (curExternFile != NULL) do {
            if (curExternFile->id = curIdent) then {
                curExternFile := NULL;
                error(errIdentAlreadyDefined);
            } else {
                curExternFile := curExternFile->next;
            };
        }; /* 21760 */
        new(curExternFile);
        with curExternFile@ do {
            id := curIdent;
            next := externFileList;
            line := l3var8z;
            offset := l3var1z.i;
        };
        if l3var8z != 0 then {
            if (curIdent = l3var3z) then {
                fileForOutput := curExternFile;
            } else {
                fileForInput := curExternFile;
            }
        };
        externFileList := curExternFile;
        l3var6z := l3var5z;
        l3var5z := l3var5z + 1;
        inSymbol;
        if (charClass = MUL) then {
            l3var6z := l3var6z + 64;
            inSymbol;
        };
        if (SY = INTCONST) then {
            l3var6z := 1000B * curToken.i + l3var6z;
            if (suffix = noSuffix) and
               (1 < curToken.i) and
               (curToken.i < 127) then {
                l3var6z := l3var6z + 128;
            } else if (suffix = suffixB) and
                      (1000000B < curToken.i) and
                      (curToken.i < 1743671743B) then {
                l3var6z := l3var6z + 256;
            } else (q) {
                error(76); /* errWrongNumberForExternalFile */
                /*=z-*/exit q/*=z+*/
            };
            inSymbol;
        } else {
            l3var6z := 512;
        };
        curExternFile->location := l3var6z;
        if (SY = COMMA) then
            inSymbol;
    }; /* 22042 */
    checkSymAndRead(RPAREN);
    checkSymAndRead(SEMICOLON);
    if (outputFile = NULL) then {
        error(77); /* errNoOutput */
        outputFile := l3var7z;
    };
    l3var6z := 40;
    repeat
        programme(l3var6z, programObj);
    until (SY = PERIOD);
    if (CH != 'D') then {
        int92z := 0;
        int93z := ;
    } else {
        set147z := halfWord;
        dataCheck := false;
        statement;
    };
    readToPos80;
    curVal.i := l3var6z;
    symTab[74003B] := (helperNames[25] + [24,27,28,29]) +
                        (curVal.m * halfWord);
}; /* initScalars */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure makeExtFile;
{
    new(l2var10z);
    with l2var10z@ do {
        typ := ptr(ord(curExternFile));
        id2 := workidr;
        expr1 := curExpr;
    };
    curExpr := l2var10z;
}; /* makeExtFile */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseParameters;
var
    l3var1z, l3var2z, l3var3z: irptr;
    parClass: idclass;
    l3var5z, l3var6z: Integer;
    l3sym7z: Symbol;
    noComma: boolean;
    expType: tptr;
{
    int92z := 0;
    l3var5z := 0;
    int93z := 0;
    inSymbol;
    l3var2z := NULL;
    if not (SY IN [IDENT,VARSY,FUNCSY,PROCSY]) then
        errAndSkip(errBadSymbol, (skipToSet + [IDENT,RPAREN]));
    int92z := 1;
    while (SY IN [IDENT,VARSY,FUNCSY,PROCSY]) do {
        l3sym7z := SY;
        if (SY = IDENT) then
            parClass := VARID
        else if (SY = VARSY) then
            parClass := FORMALID
        else {
            parClass := ROUTINEID;
            /*=z-*/(q) exit q;/*=z+*/
        };
        l3var3z := NULL;
        if (SY = PROCSY) then
            expType := NULL
        else
            expType := IntegerType;
        l3var6z := 0;
        if (SY != IDENT) then {
            int93z := 0;
            inSymbol;
        };
        repeat if (SY = IDENT) then {
            if (isDefined) then
                error(errIdentAlreadyDefined);
            l3var6z := l3var6z + 1;
            new(l3var1z, FORMALID);
            with l3var1z@ do {
                id := curIdent;
                offset := curFrameRegTemplate;
                cl := parClass;
                next := symHashTabBase[bucket];
                typ := NULL;
                list := curIdRec;
                value := l2int18z;
            };
            symHashTabBase[bucket] := l3var1z;
            l2int18z := l2int18z + 1;
            if (l3var2z = NULL) then
                curIdRec->argList := l3var1z
            else
                l3var2z->list := l3var1z;
            l3var2z := l3var1z;
            if (l3var3z = NULL) then
                l3var3z := l3var1z;
            inSymbol;
        } else
            errAndSkip(errNoIdent, skipToSet + [RPAREN,COMMA,COLON]);
        noComma := (SY != COMMA);
        if not noComma then {
            int93z := 0;
            inSymbol;
        };
        until noComma;
        if (l3sym7z != PROCSY) then {
            checkSymAndRead(COLON);
            parseTypeRef(expType, (skipToSet + [IDENT,RPAREN]));
            if (l3sym7z != VARSY) then {
                if (isFileType(expType)) then
                error(5) /*errSimpleTypeReq */
                else if (expType->size != 1) then
                     l3var5z := l3var6z * expType->size + l3var5z;
            };
            if (l3var3z != NULL) then
                while (l3var3z != curIdRec) do with l3var3z@ do {
                    typ := expType;
                    l3var3z := list;
                };
        };

        if (SY = SEMICOLON) then {
            int93z := 0;
            inSymbol;
            if not (SY IN (skipToSet + [IDENT,VARSY,FUNCSY,PROCSY])) then
                errAndSkip(errBadSymbol, (skipToSet + [IDENT,RPAREN]));
        };
    };
    /* 22276 */
    if (l3var5z != 0) then {
        curIdRec->flags := (curIdRec->flags + [23]);
        l3var6z := l2int18z;
        l2int18z := l2int18z + l3var5z;
        l3var2z := curIdRec->argList;
        /* 22306 */
        while (l3var2z != curIdRec) do {
            if (l3var2z->cl = VARID) then {
                l3var5z := l3var2z->typ->size;
                if (l3var5z != 1) then {
                    l3var2z->value := l3var6z;
                    l3var6z := l3var6z + l3var5z;
                }
            };
            l3var2z := l3var2z->list;
        };
    };
    /* 22322 */
    checkSymAndRead (RPAREN);
}; /* parseParameters */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure exitScope(var arg: array [0..127] of irptr);
{
    for ii := 0 to 127 do {
        workidr := arg[ii];
        while (workidr != NULL) and
              (workidr >= scopeBound) do
            workidr := workidr->next;
        arg[ii] := workidr;
    };
}; /* exitScope */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
{ /* programme */
    localSize := l2arg1z;
    if (localSize = 0) then {
        inSymbol;
        initScalars;
        exit;
    };
    preDefHead := ptr(0);
    inTypeDef := false;
    l2int11z := 0;
    strLabList := NULL;
    lineNesting := lineNesting + 1;
    l2var16z := numLabList;
    repeat
    if (SY = LABELSY) then {
        /*22367*/
        repeat
            inSymbol;
            if (SY != INTCONST) then {
                requiredSymErr(INTCONST);
                goto 22421;
            };
            l2var15z := numLabList;
            while (l2var15z != l2var16z) do {
                if (l2var15z->id != curToken) then {
                    l2var15z := l2var15z->next;
                } else {
                    int97z := l2var15z->line;
                    error(17); /* errLblAlreadyDefinedInLine */
                    goto 22420;
                }
            };
            new(l2var15z);
            with l2var15z@ do {
                id := curToken;
                frame := curFrameRegTemplate;
                offset := 0;
                line := lineCnt;
                defined := false;
                next := numLabList;
            };
            numLabList := l2var15z;
22420:      inSymbol;
22421:      if not (SY IN [COMMA,SEMICOLON]) then
                errAndSkip(1, skipToSet + [COMMA,SEMICOLON]);
        until SY != COMMA;
        if SY = SEMICOLON then
            inSymbol;
    }; /* 22432 */
    if (SY = CONSTSY) then {
        parseDecls(0);
        while  (SY = IDENT) do {
            if (isDefined) then
                error(errIdentAlreadyDefined);
            new(workidr=7);
            workidr@ := [curIdent, curFrameRegTemplate,
                           symHashTabBase[bucket], , ENUMID, NULL];
            symHashTabBase[bucket] := workidr;
            inSymbol;
            if (charClass != EQOP) then
                error(errBadSymbol)
            else
                inSymbol;
            with workidr@ do
                parseLiteral(typ, high, true);
            if (workidr->typ = NULL) then {
                error(errNoConstant);
                workidr->typ := IntegerType;
                workidr->value := 1;
            } else
                inSymbol;
            if (SY = SEMICOLON) then {
                int93z := 0;
                inSymbol;
                if not (SY IN (skipToSet + [IDENT])) then {
                    errAndSkip(errBadSymbol, skipToSet + [IDENT]);
                }
            } else {
                requiredSymErr(SEMICOLON);
            }
        }
    }; /* 22511 */
    objBufIdx := 1;
    if (SY = TYPESY) then {
        inTypeDef := true;
        typelist := NULL;
        parseDecls(0);
        while SY = IDENT do {
            if isDefined then
                error(errIdentAlreadyDefined);
            ii := bucket;
            l2var12z := curIdent;
            inSymbol;
            if (charClass != EQOP) then
                error(errBadSymbol)
            else
                inSymbol;
            parseTypeRef(l2typ13z, skipToSet + [SEMICOLON]);
            curIdent := l2var12z;
            if (knownInType(curIdRec)) then {
                l2typ14z := curIdRec->typ;
                if (l2typ14z->base = booleanType) then {
                    if (l2typ13z->k != kindPtr) then {
                        parseDecls(1);
                        error(78); /* errPredefinedAsPointer */
                    };
                    l2typ14z->base := l2typ13z->base;
                } else {
                    l2typ14z->base := l2typ13z;
                    curIdRec->typ := l2typ13z;
                };
                P2672(typelist, curIdRec);
            } else {
                new(curIdRec=5);
                with curIdRec@ do {
                    id := l2var12z;
                    offset := curFrameRegTemplate;
                    typ := l2typ13z;
                    cl := TYPEID;
                }
            }; /* 22574 */
            curIdRec->next := symHashTabBase[ii];
            symHashTabBase[ii] := curIdRec;
            int93z := 0;
            checkSymAndRead(SEMICOLON);
        }; /* 22602 */
        while (typelist != NULL) do {
            l2var12z := typelist->id;
            curIdRec := typelist;
            parseDecls(1);
            error(79); /* errNotFullyDefined */
            typelist := typelist->next;
        }
    }; /* TYPESY -> 22612 */
    inTypeDef := false;
    curExpr := NULL;
    if (SY = VARSY) then {
        parseDecls(0);
        /*22617*/
        repeat
            workidr := NULL;
            /*22620*/
            repeat
            if (SY = IDENT) then {
                new(curIdRec=7);
                if (isDefined) then
                    error(errIdentAlreadyDefined);
                with curIdRec@ do {
                    id := curIdent;
                    offset := curFrameRegTemplate;
                    next := symHashTabBase[bucket];
                    cl := VARID;
                    list := NULL;
                };
                symHashTabBase[bucket] := curIdRec;
                inSymbol;
                if (workidr = NULL) then
                    workidr := curIdRec
                else
                    l2var4z->list := curIdRec;
                l2var4z := curIdRec;
            } else
                error(errNoIdent);
            if not (SY IN [COMMA,COLON]) then
                errAndSkip(1, skipToSet + [IDENT,COMMA]);
            l2bool8z := SY != COMMA;
            if not l2bool8z then {
                int93z := 0;
                inSymbol;
            };
            /* 22663 -> 22620 */ until l2bool8z;
            checkSymAndRead(COLON);
            parseTypeRef(l2typ13z, skipToSet + [IDENT,SEMICOLON]);
            jj := l2typ13z->size;
            while workidr != NULL do with workidr@ do {
                curIdRec := list;
                typ := l2typ13z;
                list := NULL;
                l2bool8z := true;
                if (curProcNesting = 1) then {
                    curExternFile := externFileList;
                    l2var12z := id;
                    curVal.i := jj;
                    toAlloc := curVal.m * halfWord + [24,27,28,29];
                    while l2bool8z and (curExternFile != NULL) do {
                        if (curExternFile->id = l2var12z) then {
                            l2bool8z := false;
                            if (curExternFile->line = 0) then {
                                curVal.i := curExternFile->offset;
                                workidr->value := allocExtSymbol(toAlloc);
                                curExternFile->line := lineCnt;
                            }
                        } else {
                            curExternFile := curExternFile->next;
                        }
                    }
                }; /* 22731 */
                if (l2bool8z) then {
                    workidr->value := localSize;
                    if (PASINFOR.listMode = 3) then {
                        write('VARIABLE ':25);
                        printTextWord(workidr->id);
                        writeln(' OFFSET (', curProcNesting:0, ') ',
                                localSize:5 oct, 'B. WORDS=',
                                jj:5 oct, 'B');
                    };
                    localSize := localSize + jj;
                    curExternFile := NULL;
                }; /*22764*/
                if isFileType(l2typ13z) then
                    makeExtFile;
                workidr := curIdRec;
            }; /* 22771 */
            int93z := 0;
            checkSymAndRead(SEMICOLON);
            if (SY != IDENT) and not (SY IN skipToSet) then
                errAndSkip(errBadSymbol, skipToSet + [IDENT]);
        /* 23001 -> 22617 */ until SY != IDENT;
    }; /* VARSY -> 23003 */
    if (curProcNesting = 1) then {
        workidr := outputFile;
        curExternFile := fileForOutput;
        makeExtFile;
        if (inputFile != NULL) then {
            workidr := inputFile;
            curExternFile := fileForInput;
            makeExtFile;
        }
    };
    if (curExpr != NULL) then {
        l2int11z := moduleOffset;
        formOperator(gen14);
    } else
        l2int11z := 0;
    if (curProcNesting = 1) then {
        curExternFile := externFileList;
        while (curExternFile != NULL) do {
            if (curExternFile->line = 0) then {
                error(80); /* errUndefinedExternFile */
                printTextWord(curExternFile->id);
                writeLN;
            };
            curExternFile := curExternFile->next;
        }
    }; /*23035*/
    outputObjFile;
    while (SY = PROCSY) or (SY = FUNCSY) do {
        l2bool8z := SY = PROCSY;
        if (curFrameRegTemplate = 7) then {
            error(81); /* errProcNestingTooDeep */
        };
        int93z := 0;
        inSymbol;
        if (SY != IDENT) then {
            error(errNoIdent);
            curIdRec := uProcPtr;
            isPredefined := false;
        } else {
            if (isDefined) then with hashTravPtr@ do {
                if (cl = ROUTINEID) and
                   (list = NULL) and
                   (preDefLink != NULL) and
                   ((typ = NULL) = l2bool8z) then {
                    isPredefined := true;
                } else {
                    isPredefined := false;
                    error(errIdentAlreadyDefined);
                    printErrMsg(82); /* errPrevDeclWasNotForward */
                };
            } else
                isPredefined := false;
        }; /* 23103 */
        if not isPredefined then {
            new(curIdRec, 12);
            with curIdRec@ do {
                id := curIdent;
                offset := curFrameRegTemplate;
                next := symHashTabBase[bucket];
                typ := NULL;
                symHashTabBase[bucket] := curIdRec;
                cl := ROUTINEID;
                list := NULL;
                value := 0;
                argList := NULL;
                preDefLink := NULL;
                if (declExternal) then
                    flags := [0:15,22]
                else
                    flags := [0:15];
                pos := 0;
                curFrameRegTemplate := curFrameRegTemplate + frameRegTemplate;
                if l2bool8z then
                    l2int18z := 3
                else
                    l2int18z := 4;
            };
            curProcNesting := curProcNesting + 1;
            inSymbol;
            if (6 < curProcNesting) then
                error(81); /* errProcNestingTooDeep */
            if not (SY IN [LPAREN,SEMICOLON,COLON]) then
                errAndSkip(errBadSymbol, skipToSet + [LPAREN,SEMICOLON,COLON]);
            if (SY = LPAREN) then
                parseParameters;
            if not l2bool8z then {
                if (SY != COLON) then
                    errAndSkip(106 /*:*/, skipToSet + [SEMICOLON])
                else {
                    inSymbol;
                    parseTypeRef(curIdRec->typ, skipToSet + [SEMICOLON]);
                    if (curIdRec->typ->size != 1) then
                        error(errTypeMustNotBeFile);
                }
            };
        } else /*23167*/ {
            with hashTravPtr@ do {
                l2int18z := level;
                curFrameRegTemplate := curFrameRegTemplate + indexreg[1];
                curProcNesting := curProcNesting + 1;
                if (preDefHead = hashTravPtr) then {
                    preDefHead := preDefLink;
                } else {
                    curIdRec := preDefHead;
                    while (hashTravPtr != curIdRec) do {
                        workidr := curIdRec;
                        curIdRec := curIdRec->preDefLink;
                    };
                    workidr->preDefLink := hashTravPtr->preDefLink;
                }
            };
            hashTravPtr->preDefLink := NULL;
            curIdRec := hashTravPtr->argList;
            if (curIdRec != NULL) then {
                while (curIdRec != hashTravPtr) do {
                    addToHashTab(curIdRec);
                    curIdRec := curIdRec->list;
                }
            };
            curIdRec := hashTravPtr;
            setup(scopeBound);
            inSymbol;
        }; /* 23224 */
        checkSymAndRead(SEMICOLON);
        with curIdRec@ do if (curIdent = litForward) then {
            if (isPredefined) then
                error(83); /* errRepeatedPredefinition */
            level := l2int18z;
            preDefLink := preDefHead;
            preDefHead := curIdRec;
        } else /* 23237 */ if (curIdent = litExternal) or
            (curIdent = litFortran) then {
            if (curIdent = litExternal) then {
                curVal.m := [20];
            } else if (checkFortran) then {
                curVal.m := [21,24];
                checkFortran := false;
            } else {
                curVal.m := [21];
                /*=z-*/(q) exit q;/*=z+*/
            };
            curIdRec->flags := curIdRec->flags + curVal.m;
        } else /* 23257 */ {
            repeat
                setup(scopeBound);
                programme(l2int18z, curIdRec);
                if not (SY IN [FUNCSY,PROCSY,BEGINSY]) then
                    errAndSkip(errBadSymbol, skipToSet);
            until SY IN [FUNCSY,PROCSY,BEGINSY];
            rollup(scopeBound);
            exitScope(symHashTabBase);
            exitScope(typeHashTabBase);
            goto 23301;
        }; /* 23277 */
        inSymbol;
        checkSymAndRead(SEMICOLON);
23301:  workidr := curIdRec->argList;
        if (workidr != NULL) then {
            while (workidr != curIdRec) do {
                scopeBound := NULL;
                P2672(scopeBound, workidr);
                workidr := workidr->list;
            }
        }; /* 23314 */
        curFrameRegTemplate := curFrameRegTemplate - indexreg[1];
        curProcNesting := curProcNesting - 1;
    }; /* 23320 */
    if (SY != BEGINSY) and
       (not allowCompat or not (SY IN blockBegSys)) then
        errAndSkip(84 /* errErrorInDeclarations */, skipToSet);
    until SY in statBegSys;
    if (preDefHead != ptr(0)) then {
        error(85); /* errNotFullyDefinedProcedures */
        while (preDefHead != ptr(0)) do {
            printTextWord(preDefHead->id);
            preDefHead := preDefHead->preDefLink;
        };
        writeLN;
    };
    defineRoutine;
    while (numLabList != l2var16z) do {
        if not (numLabList->defined) then {
            write(' ', numLabList->id.i:0, ':');
            l2bool8z := false;
        };
        numLabList := numLabList->next;
    };
    if not l2bool8z then {
        printTextWord(l2idr2z->id);
        error(90); /* errLblDefinitionInBlock */
    };
    l2arg1z := l2int21z;
    /* 23364 */
}; /* programme */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure initTables;
var
    idx, jdx: Integer;
    l2unu3z, l2unu4z, l2unu5z: Word;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure initInsnTemplates;
var
    l3var1z: insn;
    l3var2z: operator;
{
    for l3var1z := ATX to JADDM do
        InsnTemp[l3var1z] := ord(l3var1z) * 10000B;
    InsnTemp[ELFUN] := 500000B;
    jdx := KUTC;
    for l3var1z := UTC to VJM do {
        InsnTemp[l3var1z] := jdx;
        jdx := (jdx + 100000B);
    };
    for idx to 15 do
        indexreg[idx] := idx * frameRegTemplate;
    jumpType := InsnTemp[UJ];
    for l3var2z := MUL to ASSIGNOP do {
        opFlags[l3var2z] := opfCOMM;
        opToInsn[l3var2z] := 0;
        if (l3var2z IN [MUL, RDIVOP, PLUSOP, MINUSOP]) then {
            opToMode[l3var2z] := 3;
        } else if (l3var2z IN [IDIVOP, IMODOP]) then {
            opToMode[l3var2z] := 2;
        } else if (l3var2z IN [IMULOP, INTPLUS, INTMINUS, badop27]) then {
            opToMode[l3var2z] := 1;
        } else if (l3var2z IN [IDIVROP,badop30,badop31]) then {
            opToMode[l3var2z] := 4;
        } else (q) {
            opToMode[l3var2z] := 0;
            /*=z-*/exit q/*=z+*/
        }
    };
    opToInsn[MUL] := InsnTemp[AMULX];
    opToInsn[RDIVOP] := InsnTemp[ADIVX];
    opToInsn[IDIVOP] := 17; /* P/DI */
    opToInsn[IMODOP] := 11; /* P/MD */
    opToInsn[PLUSOP] := InsnTemp[ADD];
    opToInsn[MINUSOP] := InsnTemp[SUB];
    opToInsn[IMULOP] := InsnTemp[AMULX];
    opToInsn[SETAND] := InsnTemp[AAX];
    opToInsn[SETXOR] := InsnTemp[AEX];
    opToInsn[SETOR] := InsnTemp[AOX];
    opToInsn[INTPLUS] := InsnTemp[ADD];
    opToInsn[INTMINUS] := InsnTemp[SUB];
    opToInsn[IDIVROP] := 67; /* P/IS */
    opToInsn[badop27] := 22; /* P/II unused, undefined */
    opToInsn[badop30] := 23; /* P/RR */
    opToInsn[badop31] := 24; /* P/RI */
    opToInsn[MKRANGE] := 61; /* P/PI */
    opToInsn[SETSUB] := InsnTemp[AAX];
    opFlags[AMPERS] := opfAND;
    opFlags[IDIVOP] := opfDIV;
    opFlags[OROP] := opfOR;
    opFlags[IMULOP] := opfMULMSK;
    opFlags[IMODOP] := opfMOD;
    opFlags[badop27] := opfHELP;
    opFlags[badop30] := opfHELP;
    opFlags[badop31] := opfHELP;
    opFlags[MKRANGE] := opfHELP;
    opFlags[IDIVROP] := opfHELP;
    opFlags[ASSIGNOP] := opfASSN;
    opFlags[SETSUB] := opfINV;
    for jdx := 0 to 6 do {
        funcInsn[jdx] := (500000B + jdx);
    }
/* 23516 */}; /* initInsnTemplates */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure regKeyWords;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure regResWord(l4arg1z: Integer);
var
    kw: @kWord;
    l4var2z: Word;
{
    curVal.i := l4arg1z;
    curVal.m := curVal.m * hashMask.m;
    mapai(curVal.a, curVal.i);
    l4var2z.i := l4arg1z;
    new(kw);
    with kw@ do {
        w := l4var2z;
        sym := SY;
        op := charClass;
        next := KeyWordHashTabBase[curVal.i];
    };
    KeyWordHashTabBase[curVal.i] := kw;
    if (charClass = NOOP) then {
        SY := succ(SY);
    } else {
        charClass := succ(charClass);
    }
}; /* regResWord */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* regKeyWords */
    SY := MULOP;
    charClass := AMPERS;
    regResWord(415644C/*"     AND"*/);
    regResWord(445166C/*"     DIV"*/);
    regResWord(555744C/*"     MOD"*/);
    SY := GTSY; /* reused as NULLSY */
    charClass := NOOP;
    regResWord(565154C/*"     NULL"*/);
    SY := ADDOP;
    charClass := OROP;
    regResWord(5762C/*"      OR"*/);
    SY := RELOP;
    charClass := INOP;
    regResWord(5156C/*"      IN"*/);
    SY := NOTSY;
    charClass := NOOP;
    regResWord(565764C/*"     NOT"*/);
    SY := LABELSY;
    charClass := NOOP;
    for idx := 0 to 29 do
        regResWord(resWordNameBase[idx]);
}; /* regKeyWords */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure initArrays;
var
    l3var1z, l3var2z: Word;
{
    FcstCnt := 0;
    FcstCount := 0;
    for idx := 3 to 6 do {
        l3var2z.i := (idx - (2));
        for jdx to l3var2z.i do
            frameRestore[idx][jdx] := 0;
    };
    for idx to 99 do
        helperMap[idx] := 0;
}; /* initArrays */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure initSets;
{
    skipToSet := blockBegSys + statBegSys - [CASESY];
    bigSkipSet := skipToSet + statEndSys;
}; /* initSets */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{ /* initTables */
    initArrays;
    initInsnTemplates;
    initSets;
    unpack(PASINFOR.a3@, iso2text, '_052'); /* '*' */
    rewrite(CHILD);
    for jdx to 10 do
        put(CHILD);
    for idx := 0 to 127 do {
        symHashTabBase[idx] := NULL;
        typeHashTabBase[idx] := ;
        KeyWordHashTabBase[idx] := ;
    };
    regKeyWords;
    numLabList := NULL;
    totalErrors := 0;
    heapCallsCnt := 0;
    putLeft := true;
    bool102z := true;
    curFrameRegTemplate := frameRegTemplate;
    curProcNesting := 1;
}; /* initTables */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure finalize;
var
    idx, cnt, unused: Integer;
    sizes: array [1..10] of @Integer;
{
    sizes[1] := ptr(1);
    sizes[2] := ptr(symTabPos - 74000B - 1);
    sizes[5] := ptr(longSymCnt);
    sizes[6] := ptr(moduleOffset - 40000B);
    sizes[8] := ptr(FcstCnt);
    sizes[3] := ptr(0);
    sizes[4] := ;
    sizes[7] := ;
    sizes[9] := ptr(int92z);
    sizes[10] := ptr(int93z);
    curVal.i := moduleOffset - 40000B;
    symTab[74001B] := [24,29] + curVal.m - intZero;
    reset(FCST);
    while not eof(FCST) do {
        write(CHILD, FCST@);
        get(FCST);
    };
    curVal.i := (symTabPos - 70000B) * 100000000B;
    for cnt to longSymCnt do {
        idx := longSymTabBase[cnt];
        symTab[idx] := (symTab[idx] + (curVal.m * [9:23]));
        curVal.i := (curVal.i + 100000000B);
    };
    symTabPos := (symTabPos - (1));
    for cnt := 74000B to symTabPos do
        write(CHILD, symTab[cnt]);
    for cnt to longSymCnt do
        write(CHILD, longSyms[cnt]);
    if (allowCompat) then {
        write((lineCnt - 1):6, ' LINES STRUCTURE ');
        for idx to 10 do
            write(ord(sizes[idx]):0, ' ');
        writeln;
    };
    entryPtTable[entryPtCnt] := [];
    PASINFOR.entryptr@ := entryPtTable;
    PASINFOR.sizes := sizes;
}; /* finalize */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure initOptions;
{
    PASINFOR.startOffset := PASINFOR.startOffset - 16384;
    commentModeCH := ' ';
    lineNesting := 0;
    maxLineLen := 72;
    CH := ' ';
    linePos := 0;
    prevErrPos := 0;
    errsInLine := 0;
    lineCnt := 1;
    checkFortran := false;
    bool110z := false;
    int93z := 1;
    int92z := 1;
    moduleOffset := 16384;
    lineStartOffset := ;
    int94z := 1;
    bool47z := false;
    dataCheck := ;
    heapSize := 100;
    bool49z := true;
    atEOL := false;
    curVal.m := PASINFOR.flags;
    besm(ASN64 - 39);
    besm(ASN64 + 45);
    optSflags := ;
    doPMD := not (42 in curVal.m);
    checkTypes := true;
    fixMult := true;
    fuzzReals := true;
    pseudoZ := ;
    checkBounds := not (44 in curVal.m);
    declExternal := false;
    errors := false;
    allowCompat := false;
    litExternal.i := 4570644562564154C;
    litForward.i := 46576267416244C;
    litFortran.i := 46576264624156C;
    fileBufSize := 1;
    charEncoding := 2;
    chain := NULL;
    litOct.i := 574364C;
    longSymCnt := 0;
    PASINFOR.errors@ := true;
    extSymAdornment := 0;
    symTabCnt := 0;
}; /* initOptions */
%
{ /* main */
    if PASINFOR.listMode != 0 then
        writeln(boilerplate);
    initOptions;
    curInsnTemplate := 0;
    initTables;
    try {
        programme(curInsnTemplate, hashTravPtr);
    } catch (int foo) {
        if (foo == 9999) goto L9999;
    }
    if errors then {
L9999:   writeln(' IN ', (lineCnt-1):0, ' LINES ',
            totalErrors:0, ' ERRORS');
    } else {
        finalize;
        PASINFOR.errors@ := false;
    }
}
.data
    frameRegTemplate := 04000000B;
    constRegTemplate := I8;
    disNormTemplate :=  KNTR+7;
    blockBegSys := [LABELSY, CONSTSY, TYPESY, VARSY, FUNCSY, PROCSY, BEGINSY];
    statBegSys :=  [BEGINSY, IFSY, CASESY, REPEATSY, WHILESY, FORSY, WITHSY,
                    GOTOSY, SELECTSY];
    O77777 := [33:47];
    intZero := 0;
    unused138z := (63000000C);
    extSymMask := (43000000C);
    halfWord := [24:47];
    hashMask := 203407C;
    statEndSys := [SEMICOLON, ENDSY, ELSESY, UNTILSY];
    lvalOpSet := [GETELT, GETVAR, op36, op37, GETFIELD, DEREF, FILEPTR];
    resWordNameBase :=
        5441424554C             /*"   LABEL"*/,
        4357566364C             /*"   CONST"*/,
        64716045C               /*"    TYPE"*/,
        664162C                 /*"     VAR"*/,
        4665564364515756C       /*"FUNCTION"*/,
        6062574345446562C       /*"PROCEDUR"*/,
        634564C                 /*"     SET"*/,
        604143534544C           /*"  PACKED"*/,
        4162624171C             /*"   ARRAY"*/,
        624543576244C           /*"  RECORD"*/,
        46515445C               /*"    FILE"*/,
        4245475156C             /*"   BEGIN"*/,
        5146C                   /*"      IF"*/,
        43416345C               /*"    CASE"*/,
        624560454164C           /*"  REPEAT"*/,
        6750515445C             /*"   WHILE"*/,
        465762C                 /*"     FOR"*/,
        67516450C               /*"    WITH"*/,
        47576457C               /*"    GOTO"*/,
        455644C                 /*"     END"*/,
        45546345C               /*"    ELSE"*/,
        6556645154C             /*"   UNTIL"*/,
        5746C                   /*"      OF"*/,
        4457C                   /*"      DO"*/,
        6457C                   /*"      TO"*/,
        445767566457C           /*"  DOWNTO"*/,
        64504556C               /*"    THEN"*/,
        634554454364C           /*"  SELECT"*/,
        60625747624155C         /*" PROGRAM"*/,
        576450456263C           /*"  OTHERS"*/;
%
    charSymTabBase := NOSY:128;
    chrClassTabBase := NOOP:128;
    charSymTabBase['0'] := INTCONST:10;
    chrClassTabBase['0'] := ALNUM:10;
    charSymTabBase['A'] := IDENT:26;
    chrClassTabBase['A'] := ALNUM:26;
    charSymTabBase['Ю'] := IDENT:31;
    chrClassTabBase['Ю'] := ALNUM:31;
    funcInsn[fnABS] := KAMX;
    funcInsn[fnTRUNC] := KADD+ZERO;
    funcInsn[fnODD] := KAAX+E1;
    funcInsn[fnORD] := KAOX+ZERO;
    funcInsn[fnCHR] := KAAX+MANTISSA;
    funcInsn[fnSUCC] := KARX+E1;
    funcInsn[fnPRED] := KSUB+E1;
    funcInsn[fnSQR] := macro + mcSQRR;
    funcInsn[fnROUND] := macro + mcROUND;
    funcInsn[fnCARD] := macro + mcCARD;
    funcInsn[fnMINEL] := macro + mcMINEL;
    funcInsn[fnPTR] := KAAX+MANTISSA;
    funcInsn[fnABSI] := KAMX;
    funcInsn[fnSQRI] := macro + mcSQRI;
    iAddOpMap[PLUSOP] := INTPLUS, INTMINUS;
    setOpMap[PLUSOP] := SETOR, SETSUB;
    imulOpMap := IMULOP, IDIVROP;
    setOpMap[MUL] := SETAND, SETXOR;
    charSymTabBase[chr(27)] := CHARCONST;
    charSymTabBase[''''] := CHARCONST;
    charSymTabBase['_'] := REALCONST;
    charSymTabBase['<'] := LTSY;
    charSymTabBase['>'] := GTSY;
    chrClassTabBase['+'] := PLUSOP;
    chrClassTabBase['-'] := MINUSOP;
    chrClassTabBase['*'] := MUL;
    chrClassTabBase['/'] := RDIVOP;
    chrClassTabBase['='] := EQOP;
    chrClassTabBase['&'] := AMPERS;
    chrClassTabBase['÷'] := IDIVOP;
    chrClassTabBase['∨'] := OROP;
    chrClassTabBase['>'] := GTOP;
    chrClassTabBase['<'] := LTOP;
    chrClassTabBase['#'] := NEOP;
    chrClassTabBase['='] := EQOP;
    chrClassTabBase['×'] := MUL;
    chrClassTabBase['≤'] := LEOP;
    chrClassTabBase['≥'] := GEOP;
    charSymTabBase['≤'] := RELOP;
    charSymTabBase['≥'] := RELOP;
    charSymTabBase['+'] := ADDOP;
    charSymTabBase['-'] := ADDOP;
    charSymTabBase['∨'] := ADDOP;
    charSymTabBase['*'] := MULOP;
    charSymTabBase['/'] := MULOP;
    charSymTabBase['&'] := MULOP;
    charSymTabBase['×'] := MULOP;
    charSymTabBase[','] := COMMA;
    charSymTabBase['.'] := PERIOD;
    charSymTabBase[chr(22)] := ARROW;
    charSymTabBase['@'] := ARROW;
    charSymTabBase['^'] := ARROW;
    charSymTabBase['('] := LPAREN;
    charSymTabBase[')'] := RPAREN;
    charSymTabBase[';'] := SEMICOLON;
    charSymTabBase['['] := LBRACK;
    charSymTabBase[']'] := RBRACK;
    charSymTabBase['#'] := RELOP;
    charSymTabBase['='] := RELOP;
    charSymTabBase[':'] := COLON;
    charSymTabBase['÷'] := MULOP;
    charSymTabBase['~'] := NOTSY;
    helperNames :=
        6017210000000000C      /*"P/1     "*/,
        6017220000000000C      /*"P/2     "*/,
        6017230000000000C      /*"P/3     "*/,
        6017240000000000C      /*"P/4     "*/,
        6017250000000000C      /*"P/5     "*/,
        6017260000000000C      /*"P/6     "*/,
        6017434100000000C      /*"P/CA    "*/,
        6017455700000000C      /*"P/EO    "*/,
        6017636300000000C      /*"P/SS    "*/,
/*10*/  6017455400000000C      /*"P/EL    "*/,
        6017554400000000C      /*"P/MD    "*/,
        6017555100000000C      /*"P/MI    "*/,
        6017604100000000C      /*"P/PA    "*/,
        6017655600000000C      /*"P/UN    "*/,
        6017436000000000C      /*"P/CP    "*/,
        6017414200000000C      /*"P/AB    "*/,
        6017445100000000C      /*"P/DI    "*/,
        6017624300000000C      /*"P/RC    "*/,
        6017454100000000C      /*"P/EA    "*/,
/*20*/  6017564100000000C      /*"P/NA    "*/,
        6017424100000000C      /*"P/BA    "*/,
        6017515100000000C      /*"P/II   u"*/,
        6017626200000000C      /*"P/RR    "*/,
        6017625100000000C      /*"P/RI    "*/,
        6017214400000000C      /*"P/1D    "*/,
        6017474400000000C      /*"P/GD    "*/,
        6017450000000000C      /*"P/E     "*/,
        6017454600000000C      /*"P/EF    "*/,
        6017604600000000C      /*"P/PF    "*/,
/*30*/  6017474600000000C      /*"P/GF    "*/,
        6017644600000000C      /*"P/TF    "*/,
        6017624600000000C      /*"P/RF    "*/,
        6017566700000000C      /*"P/NW    "*/,
        6017446300000000C      /*"P/DS    "*/,
        6017506400000000C      /*"P/HT    "*/,
        6017675100000000C      /*"P/WI    "*/,
        6017676200000000C      /*"P/WR    "*/,
        6017674300000000C      /*"P/WC    "*/,
        6017412600000000C      /*"P/A6    "*/,
/*40*/  6017412700000000C      /*"P/A7    "*/,
        6017677000000000C      /*"P/WX    "*/,
        6017675700000000C      /*"P/WO    "*/,
        6017436700000000C      /*"P/CW    "*/,
        6017264100000000C      /*"P/6A    "*/,
        6017274100000000C      /*"P/7A    "*/,
        6017675400000000C      /*"P/WL    "*/,
        6017624451000000C      /*"P/RDI   "*/,
        6017624462000000C      /*"P/RDR   "*/,
        6017624443000000C      /*"P/RDC   "*/,
/*50*/  6017624126000000C      /*"P/RA6   "*/,
        6017624127000000C      /*"P/RA7   "*/,
        6017627000000000C      /*"P/RX   u"*/,
        6017625400000000C      /*"P/RL    "*/,
        6017675754560000C      /*"P/WOLN  "*/,
        6017625154560000C      /*"P/RILN  "*/,
        6017626200000000C      /*"P/RR    "*/,
        6017434500000000C      /*"P/CE    "*/,
        6017646200000000C      /*"P/TR    "*/,
        6017546600000000C      /*"P/LV    "*/,
/*60*/  6017724155000000C      /*"P/ZAM  u"*/,
        6017605100000000C      /*"P/PI    "*/,
        6017426000000000C      /*"P/BP    "*/,
        6017422600000000C      /*"P/B6    "*/,
        6017604200000000C      /*"P/PB    "*/,
        6017422700000000C      /*"P/B7    "*/,
        6017515600000000C      /*"P/IN    "*/,
        6017516300000000C      /*"P/IS    "*/,
        6017444100000000C      /*"P/DA    "*/,
        6017435700000000C      /*"P/CO    "*/,
/*70*/  6017516400000000C      /*"P/IT    "*/,
        6017435300000000C      /*"P/CK    "*/,
        6017534300000000C      /*"P/KC    "*/,
        6017545647604162C      /*"P/LNGPAR"*/,
        6017544441620000C      /*"P/LDAR  "*/,
        6017544441625156C      /*"P/LDARIN"*/,
        6017202043000000C      /*"P/00C   "*/,
        6017636441620000C      /*"P/STAR  "*/,
        6017605544634564C      /*"P/PMDSET"*/,
        6017435100000000C      /*"P/CI    "*/,
/*80*/  6041514200000000C      /*"PAIB    "*/,
        6017674100000000C      /*"P/WA    "*/,
        6361626412000000C      /*"SQRT*   "*/,
        6351561200000000C      /*"SIN*    "*/,
        4357631200000000C      /*"COS*    "*/,
        4162436441561200C      /*"ARCTAN* "*/,
        4162436351561200C      /*"ARCSIN* "*/,
        5456120000000000C      /*"LN*     "*/,
        4570601200000000C      /*"EXP*    "*/,
        6017456100000000C      /*"P/EQ    "*/,
/*90*/  6017624100000000C      /*"P/RA    "*/,
        6017474500000000C      /*"P/GE    "*/,
        6017554600000000C      /*"P/MF    "*/,
        6017465500000000C      /*"P/FM    "*/,
        6017565600000000C      /*"P/NN    "*/,
        6017634300000000C      /*"P/SC    "*/,
        6017444400000000C      /*"P/DD    "*/,
        6017624500000000C      /*"P/RE    "*/;
    systemProcNames :=
/*0*/   606564C                /*"     PUT"*/,
        474564C                /*"     GET"*/,
        62456762516445C        /*" REWRITE"*/,
        6245634564C            /*"   RESET"*/,
        564567C                /*"     NEW"*/,
        44516360576345C        /*" DISPOSE"*/,
        50415464C              /*"    HALT"*/,
        63645760C              /*"    STOP"*/,
        6345646560C            /*"   SETUP"*/,
        625754546560C          /*"  ROLLUP"*/,
/*10*/  6762516445C            /*"   WRITE"*/,
        67625164455456C        /*" WRITELN"*/,
        62454144C              /*"    READ"*/,
        624541445456C          /*"  READLN"*/,
        45705164C              /*"    EXIT"*/,
        4445426547C            /*"   DEBUG"*/,
        42456355C              /*"    BESM"*/,
        5541605141C            /*"   MAPIA"*/,
        5541604151C            /*"   MAPAI"*/,
        604353C                /*"     PCK"*/,
/*20*/  6556604353C            /*"   UNPCK"*/,
        60414353C              /*"    PACK"*/,
        655660414353C          /*"  UNPACK"*/,
        5760455644C            /*"   OPEND"*/,
        44455444C              /*"    DELD"*/,
        56456744C              /*"    NEWD"*/,
        60656444C              /*"    PUTD"*/,
        47456444C              /*"    GETD"*/,
        55574444C              /*"    MODD"*/,
        46515644C              /*"    FIND"*/;
end

#endif
