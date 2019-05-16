(*=p-,t-,s8,u-,y+,k9,l0,d4*)
program pascompl(output, child, pasinput, pasinfor);
%
label 9999;
%
const
    boilerplate = ' PASCAL COMPILER 15.0 (15.02.82)';
%   boilerplate = ' PASCAL BACK TO LIFE (15.08.2017)';
%
    fnSQRT  = 0;  fnSIN  = 1;  fnCOS  = 2;  fnATAN  = 3;  fnASIN = 4;
    fnLN    = 5;  fnEXP  = 6;  fnABS =  7;  fnTRUNC = 8;  fnODD  = 9;
    fnORD   = 10; fnCHR  = 11; fnSUCC = 12; fnPRED  = 13; fnEOF  = 14;
    fnREF   = 15; fnEOLN = 16; fnSQR =  17; fnROUND = 18; fnCARD = 19;
    fnMINEL = 20; fnPTR  = 21; fnABSI = 22; fnSQRI  = 23; fn24 = 24;
    fn29    = 29;
%
    S3 = 0;
    S4 = 1;
    S5 = 2;
    S6 = 3;
    NoPtrCheck = 4;
    NoStackCheck = 5;
%
    DebugInteractive = 44;
    DebugCode  = 45;
    DebugPrint = 46;
    DebugEntry = 47;
%
    ASN64 = 360100B;
%
    errBooleanNeeded = 0;
    errIdentAlreadyDefined = 2;
    errNoIdent = 3;
    errNotAType = 4;
    errNoConstant = 6;
    errConstOfOtherTypeNeeded = 7;
    errTypeMustNotBeFile = 9;
    errNotDefined = 11;
    errBadSymbol = 12;
    errNeedOtherTypesOfOperands = 21;
    errWrongVarTypeBefore = 22;
    errUsingVarAfterIndexingPackedArray = 28;
    errNoSimpleVarForLoop = 30;
    errTooManyArguments = 38;
    errNoCommaOrParenOrTooFewArgs = 41;
    errNumberTooLarge = 43;
    errVarTooComplex = 48;
    errEOFEncountered = 52;
    errFirstDigitInCharLiteralGreaterThan3 = 60;
%
    macro = 100000000B;
    mcACC2ADDR = 6;
    mcPOP = 4;
    mcPUSH = 5;
    mcMULTI = 7;
    mcADDSTK2REG = 8;
    mcADDACC2REG = 9;
    mcODD = 10;
    mcSQRR = 12;
    mcROUND = 11;
    mcMINEL = 15;
    mcSQRI = 13;
    mcPOP2ADDR = 19;
    mcCARD = 23;
%
    ASCII0 =    4000007B;
    E1 =        4000010B;
    ZERO =      4000011B;
    MULTMASK =  4000012B;
    MANTISSA =  4000014B;
    MINUS1 =    4000017B;
    PLUS1 =     4000021B;
    BITS15 =    4000022B;
    REAL05 =    4000023B;
    ALLONES =   4000024B;
    HEAPPTR =   4000027B;

    KATX =      0000000B;
%   KSTX =      0010000B;
    KXTS =      0030000B;
    KADD =      0040000B;
    KSUB =      0050000B;
    KRSUB =     0060000B;
    KAMX =      0070000B;
    KXTA =      0100000B;
    KAAX =      0110000B;
    KAEX =      0120000B;
    KARX =      0130000B;
    KAVX =      0140000B;
    KAOX =      0150000B;
%   KDIV =      0160000B;
    KMUL =      0170000B;
    KAPX =      0200000B;
    KAUX =      0210000B;
    KACX =      0220000B;
    KANX =      0230000B;
    KYTA =      0310000B;
%   KASN =      0360000B;
    KNTR =      0370000B;
    KATI =      0400000B;
%   KSTI =      0410000B;
    KITA =      0420000B;
    KITS =      0430000B;
    KMTJ =      0440000B;
    KJADDM =    0450000B;
    KE74 =      0740000B;
    KUTC =      2200000B;
    CUTC =      2200000C;
    KWTC =      2300000B;
    CWTC =      2300000C;
    KVTM =      2400000B;
    KUTM =      2500000B;
%   KUZA =      2600000B;
%   KU1A =      2700000B;
    KUJ =       3000000B;
    KVJM =      3100000B;
    KVZM =      3400000B;
%   KV1M =      3500000B;
    KVLM =      3700000B;
%
    I7 =        34000000B;      (* frame pointer *)
    I8 =        40000000B;      (* const pointer *)
    I9 =        44000000B;      (* temp register *)
    I10 =       50000000B;      (* temp register *)
    I11 =       54000000B;      (* temp register *)
    I12 =       60000000B;      (* temp register *)
    I13 =       64000000B;      (* link register *)
    I14 =       70000000B;      (* temp register *)
    SP =        74000000B;      (* stack pointer, reg 15 *)
%
type
    symbol = (
(*0B*)  IDENT,      INTCONST,   REALCONST,  CHARCONST,
        LTSY,       GTSY,       NOTSY,      LPAREN,
(*10B*) LBRACK,     MULOP,      ADDOP,      RELOP,
        RPAREN,     RBRACK,     COMMA,      SEMICOLON,
(*20B*) PERIOD,     ARROW,      COLON,      BECOMES,
        LABELSY,    CONSTSY,    TYPESY,     VARSY,
(*30B*) FUNCSY,     PROCSY,     SETSY,      PACKEDSY,
        ARRAYSY,    RECORDSY,   FILESY,     BEGINSY,
(*40B*) IFSY,       CASESY,     REPEATSY,   WHILESY,
        FORSY,      WITHSY,     GOTOSY,     ENDSY,
(*50B*) ELSESY,     UNTILSY,    OFSY,       DOSY,
        TOSY,       DOWNTOSY,   THENSY,     SELECTSY,
(*60B*) PROGRAMSY,  OTHERSY,    NOSY
);
%
idclass = (
        TYPEID,     ENUMID,     ROUTINEID,  VARID,
        FORMALID,   FIELDID
);
%
insn = (
(*000*) ATX,   STX,   OP2,   XTS,   ADD,   SUB,   RSUB,  AMX,
(*010*) XTA,   AAX,   AEX,   ARX,   AVX,   AOX,   ADIVX, AMULX,
(*020*) APX,   AUX,   ACX,   ANX,   EADD,  ESUB,  ASX,   XTR,
(*030*) RTE,   YTA,   OP32,  OP33,  EADDI, ESUBI, ASN,   NTR,
(*040*) ATI,   STI,   ITA,   ITS,   MTJ,   JADDM, ELFUN,
(*047*) UTC,   WTC,   VTM,   UTM,   UZA,   U1A,   UJ,    VJM
);
%
setofsys = set of ident .. selectsy;
%
operator = (
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
);
%
opgen = (
    gen0,  STORE, LOAD,  gen3,  SETREG,
    gen5,  gen6,  gen7,  gen8,  gen9,
    gen10, gen11, gen12, FILEACCESS, gen14,
    gen15, gen16, LITINSN
);
%
% Flags for ops that can potentially be optimized if one operand is a constant
opflg = (
    opfCOMM, opfHELP, opfAND, opfOR, opfDIV, opfMOD, opfMULMSK, opfASSN, opfINV
);
%
kind = (
    kindReal, kindScalar, kindRange, kindPtr,
    kindSet, kindArray, kindRecord, kindFile,
    kindCases
);
%
bitset = set of 0..47;
%
eptr = @expr;
tptr = @types;
irptr = @identrec;
%
word = record case integer of
    0: (i: integer);
    1: (r: real);
    2: (b: boolean);
    3: (a: alfa);
    7: (c: char);
    8: (cl: idclass);
    13: (m: bitset)
    end;
%
oiptr = @oneinsn;
%
oneinsn  = record
    next: oiptr;
    mode, code, offset: integer;
end;
%
ilmode = (ilCONST, il1, il2, il3);
state = (st0, st1, st2);
%
insnltyp  = record
    next, next2: oiptr;
    typ: tptr;
    regsused: bitset;
    ilm: ilmode;
    ilf5: word;
    ilf6: integer;
    ilf7: integer;
    st: state;
    width, shift: integer
end;
%
types = record
    size,
    bits:   integer;
    k:      kind;
    case kind of
    kindReal:   ();
    kindRange:  (base:      tptr;
                 checker,
                 left,
                 right:     integer);
    kindArray:  (abase,
                 range:     tptr;
                 pck:       boolean;
                 perword,
                 pcksize:   integer);
    kindScalar: (enums:     irptr;
                 numen,
                 start:     integer);
    kindSet,
    kindPtr:    (sbase:      tptr);
    kindFile:   (fbase:      tptr;
                 elsize:    integer);
    kindRecord: (ptr1,
                 ptr2:      irptr;
                 flag,
                 pckrec:    boolean);
    kindCases:  (sel:       word;
                 first,
                 next:      tptr;
                 r6:        tptr)
    end;
%
typechain = record
    next:         @typechain;
    type1, type2: tptr;
end;

charmap   = packed array ['_000'..'_176'] of char;
textmap   = packed array ['_052'..'_177'] of '_000'..'_077';
%
four = array [1..4] of integer;
entries   = array [1..42] of bitset;
%
expr = record
    case operator of (* arbitrary so far *)
    NOOP:       (val:    word;
                 op:     operator;
                 d1, d2: word);
    MUL:        (typ:    tptr;
                 d3:     word;
                 expr1, expr2: eptr);
    BOUNDS:     (d4, d5: word;
                 typ1, typ2: tptr);
    NOTOP:      (d6, d7: word;
                 id1, id2: irptr);
    STANDPROC:  (d8, d9: word;
                 num1, num2: integer);
end;
%
kword = record
    w:      word;
    sym:    symbol;
    op:     operator;
    next:   @kword;
end;
%
strLabel = record
    next:       @strLabel;
    ident:      word;
    offset:     integer;
    exitTarget: integer;
end;
%
numLabel = record
    id:         word;
    line:       integer;
    frame:      integer;
    offset:     integer;
    next:       @numLabel;
    defined:    boolean;
end;
%
identrec = record
    id:     word;
    offset: integer;
    next:   irptr;
    typ: tptr;
    cl: idclass;
    case idclass of
    TYPEID,
    VARID:  ();
    ENUMID,
    FORMALID:
            (list: irptr; value: integer);
    FIELDID:
            (maybeUnused: integer;
             uptype: tptr;
             pckfield:  boolean;
             shift:     integer;
             width:     integer);
    ROUTINEID:
            (low: integer;
             high: word;
             argList, preDefLink: irptr;
             level, pos: integer;
             flags: bitset
            );
end;
extfilerec = record
    id:     word;
    offset: integer;
    next:   @extfilerec;
    location,
    line: integer
end;
numberSuffix = (noSuffix, suffixB, suffixT, suffixC);
%
var (* total size 4791 words *)
%
(*40*)      suffix: numberSuffix;
(*41-46*)   bigSkipSet, statEndSys, blockBegSys, statBegSys,
            skipToSet, lvalOpSet: setofsys;
(*47-49*)   bool47z, bool48z, bool49z: boolean;
(*50*)      dataCheck: boolean;
(*51*)      jumpType: integer;
(*52*)      jumpTarget: integer;
(*53*)      int53z: integer;
(*54*)      charClass: operator;
(*55-56*)   SY, prevSY: symbol;
(*57*)      savedObjIdx: integer;
(*58*)      FcstCnt: integer;
(*59*)      symTabPos: integer;
(*60*)      entryPtCnt: integer;
(*61*)      fileBufSize: integer;
(*62-63*)   expr62z, expr63z: eptr;
(*64*)      curInsnTemplate: integer;
(*65*)      maxLineLen: integer;
(*66*)      linePos: integer;
(*67*)      prevErrPos: integer;
(*68*)      errsInLine: integer;
(*69*)      moduleOffset: integer;
(*70*)      lineStartOffset: integer;
(*71*)      curFrameRegTemplate: integer;
(*72*)      curProcNesting: integer;
(*73*)      totalErrors: integer;
(*74*)      lineCnt: integer;
(*75*)      bucket: integer;
(*76*)      strLen: integer;
(*77*)      heapCallsCnt: integer;
(*78*)      heapSize: integer;
(*79*)      arithMode: integer;
(*80*)      stmtName: alfa;
(*81*)      keywordHashPtr: @kword;
(*82*)      curVarKind: kind;
(*83*)      curExternFile: @extfilerec;
(*84*)      commentModeCH: char;
(*85*)      unused85z: word;
(*86*)      CH: char;
(*87*)      sunused87z: word;
(*88*)      debugLine: integer;
(*89*)      lineNesting: integer;
(*90*)      FcstCountTo500: integer;
(*91*)      objBufIdx: integer;
(*92-94*)   int92z, int93z, int94z: integer;
(*95*)      prevOpcode: integer;
(*96*)      charEncoding: integer;
(*97*)      int97z: integer;
(*98*)      atEOL: boolean;
(*99*)      checkTypes: boolean;
(*100-102*) isDefined, putLeft, bool102z: boolean;
(*103*)     errors: boolean;
(*104*)     declExternal: boolean;
(*105*)     rangeMismatch: boolean;
(*106*)     doPMD: boolean;
(*107*)     checkBounds: boolean;
(*108*)     fuzzReals: boolean;
(*109*)     fixMult: boolean;
(*110*)     bool110z: boolean;
(*111*)     pseudoZ: boolean;
(*112*)     allowCompat: boolean;
(*113*)     checkFortran: boolean;
(*114*)     outputFile: irptr;
(*115*)     inputFile: irptr;
(*116*)     programObj: irptr;
(*117*)     hashTravPtr: irptr;
(*118*)     uProcPtr: irptr;
(*119*)     externFileList: @extfilerec;
(*120-121*) typ120z, typ121z: tptr;
(*122*)     pointerType: tptr;
(*123*)     setType: tptr;
(*124*)     booleanType: tptr;
(*125*)     textType: tptr;
(*126*)     integerType: tptr;
(*127*)     realType: tptr;
(*128*)     charType: tptr;
(*129*)     alfaType: tptr;
(*130*)     arg1Type: tptr;
(*131*)     arg2Type: tptr;
(*132*)     numLabList: @numLabel;
(*133*)     chain: @typechain;
(*134*)     curToken: word;
(*135*)     curVal: word;
(*136*)     O77777: bitset;
(*137*)     intZero: bitset;
(*138-139*) unused138z, extSymMask: bitset;
(*140*)     halfWord: bitset;
(*141*)     leftInsn: bitset;
(*142*)     hashMask: word;
(*143*)     curIdent: word;
(*144-148*) toAlloc, set145z, set146z, set147z, set148z: bitset;
(*149*)     optSflags: word;
(*150*)     litOct: word;
(*151*)     litExternal: word;
(*152*)     litForward: word;
(*153*)     litFortran: word;
(*154*)     uVarPtr: eptr;
(*155*)     curExpr: eptr;
(*156*)     insnList: @insnltyp;
(*157-158*) fileForOutput, fileForInput: @extfilerec;
(*159*)     maxSmallString: integer;
(*160*)     extSymAdornment: integer;
(*161-165*) smallStringType: array [2..6] of tptr;
(*166*)     symTabCnt: integer;
(*167-246*) symtabarray: array [1..80] of word;
(*247-326*) symtbidx: array [1..80] of integer;
(*327-331*) iMulOpMap: array [MUL..IMODOP] of operator;
(*332-338*) setOpMap: array [MUL..MINUSOP] of operator;
(*339-340*) iAddOpMap: array [PLUSOP..MINUSOP] of operator;
(*341-369*) entryPtTable: entries;
(*370-397*) frameRestore: array [3..6] of four;
(*398-413*) indexreg: array [1..15] of integer;
(*414-450*) opToInsn: array [MUL..op44] of integer;
(*451-487*) opToMode: array [MUL..op44] of integer;
(*488-524*) opFlags: array [MUL..op44] of opflg;
(*525-548*) funcInsn: array [0..23] of integer;
(*549-595*) insnTemp: array [insn] of integer;
(*596*)     frameRegTemplate: integer;
(*597*)     constRegTemplate: integer;
(*598*)     disNormTemplate: integer;
(*599-728*) lineBufBase: array [1..130] of char;
(*729*)     errMapBase: array [0..9] of integer;
(*739*)     chrClassTabBase: array ['_000'..'_177'] of operator;
(*867*)     kwordHashTabBase: array [0..127] of @kword;
(*995*)     charSymTabBase: array ['_000'..'_177'] of symbol;
(*1123*)    symHashTabBase: array [0..127] of irptr;
(*1251*)    typeHashTabBase: array [0..127] of irptr;
(*1378*)    helperMap: array [1..99] of integer;
(*1477*)    helperNames: array [1..99] of bitset;
(*1577-
  2409*)    symTab: array [74000B..75500B] of bitset;
(*2410*)    systemProcNames: array [0..29] of integer;
(*2440*)    resWordNameBase: array [0..29] of integer;
(*2470*)    longSymCnt: integer;
(*2471*)    longSymTabBase: array [1..90] of integer;
(*2560*)    longSyms: array [1..90] of bitset;
(*2651*)    constVals: array [1..500] of alfa;
(*3151*)    constNums: array [1..500] of integer;
(*3651*)    objBuffer: array [1..1024] of bitset;
(*4675*)    iso2text: array ['_052'..'_177'] of '_000'..'_077';
(*4761*)    fcst: file of bitset; (* last *)
%
    pasinput: text;
%
    child: file of bitset;
%
    pasinfor: record
        (*0*) listMode:     integer;
        (*1*) errors:       @boolean;
        (*2*) entryptr:     @entries;
        (*3*) startOffset:  integer;
      (*4-6*) a0, a1, a4:   @charmap;
        (*7*) a3:           @textmap;
     (*8-17*) sizes:        array [1..10] of @integer;
       (*18*) flags:        bitset;
        end;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              PROGRAMME                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure programme(var l2arg1z: integer; l2idr2z: irptr);
label 22420, 22421, 23301;
var
    preDefHead, typelist, scopeBound, l2var4z, curIdRec, workidr: irptr;
    isPredefined, l2bool8z, inTypeDef: boolean;
    l2var10z: eptr;
    l2int11z: integer;
    l2var12z: word;
    l2typ13z, l2typ14z: tptr;
    l2var15z, l2var16z: @numLabel;
    strLabList: @strLabel;
%
    l2int18z, ii, localSize, l2int21z, jj: integer;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              PrintErrMsg               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure printErrMsg(errno: integer);
type
    errtxt = packed array [0..100] of '_000'..'_077';
var
    errptr: @errtxt;
    errtext: array [0..100] of '_000'..'_077';
    i: integer;
    c: char;
%
    function pasmitxt(errno: integer): @errtxt;
        fortran;
%
    function pasisoxt(txtchar: '_000'..'_077'): char;
        fortran;
%
begin (* PrintErrMsg *)
    write(' ');
    if errno >= 200 then
        write('system=', errno:0)
    else begin
        if (errno > 88) then
            printErrMsg(86)
        else if errno in [16..18, 20] then begin
            if errno = 20 then
                errno := ord(sy = ident)*2 + 1
            else
                write(curToken.i:0,' ');
        end (*=z-*)else(*=z+*) ;
        errptr := pasmitxt(errno);
        unpack(errptr@, errtext, 0);
(loop)  for i:=0 to 100 do begin
            c := pasisoxt(errtext[i]);
            if c = '*' then
                exit loop;
            write(c);
        end;
        write(' ');
        if errno in [17, 22] then
            if errno = 17 then
                write(int97z:0)
            else
                write(stmtName);
    end;
    if errno <> 86 then
        writeln;
end; (* PrintErrMsg *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              printTextWord             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure printTextWord(val: word);
%
    procedure PASTPR(val: word);
        external;
%
begin (* printTextWord *)
    write(' ');
    PASTPR(val)
end; (* printTextWord *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              makeStringType                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure makeStringType(var res: tptr);
var span: tptr;
begin
    if maxSmallString >= strLen then
        res := smallStringType[strLen]
    else begin
        new(span = 7);
        new(res, kindArray);
        with span@ do begin
            size := 1;
            checker := 0;
            bits := 12;
            k := kindRange;
            base := integerType;
            left := 1;
            right := strLen;
        end;
        with res@ do begin
            size := (strLen + 5) div 6;
            if size = 1 then
                bits := strLen * 8
            else
                bits := 0;
            k := kindArray;
            base := charType;
            range := span;
            pck := true;
            perword := 6;
            pcksize := 8;
        end
    end
end; (* makeStringType *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              addToHashTab              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure addToHashTab(arg: irptr);
begin
    curVal.m := arg@.id.m * hashMask.m;
    mapai(curval.a, curval.i);
    arg@.next := symHashTabBase[curval.i];
    symHashTabBase[curval.i] := arg;
end; (* addToHashTab *)
%
procedure error(errno: integer);
    forward;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              storeObjWord              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure storeObjWord(insn: bitset);
begin
    objBuffer[objBufIdx] := insn;
    moduleOffset := moduleOffset + 1;
    if objBufIdx = 1024 then begin
        error(49); (* errTooManyInsnsInBlock *)
        objBufIdx := 1
    end else
        objBufIdx := objBufIdx + 1;
end; (* storeObjWord *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              form1Insn                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure form1Insn(arg: integer);
var
    insn, opcode: word;
    half1, half2: bitset;
    pos: integer;
begin
    insn.i := arg;
    opcode.m := insn.m * [0, 1, 3, 24..32];
    if opcode.i = insnTemp[UJ] then begin
        if prevOpcode = opcode.i then
            exit;
        if putLeft and (prevOpcode = 1) then begin
            pos := objBufIdx - 1;
            if objBuffer[pos] * [0..8] = [0, 1, 3..5, 8] then begin
                prevOpcode := opcode.i;
                half1 := insn.m * [33..47];
                besm(ASN64-24);
                half1 :=;
                half2 := objBuffer[pos] * [9..23];
                besm(ASN64+24);
                half2 :=;
                objBuffer[pos] := [0, 1, 3, 4, 6, 28, 29] +
                    half1 + half2;
                exit;
            end
       end
    end;
    prevOpcode := opcode.i;
    if (putLeft) then begin
        leftInsn := insn.m * halfWord;
        besm(ASN64-24);
        leftInsn :=;
        putLeft := false
    end else begin
        putLeft := true;
        storeObjWord(leftInsn + (insn.m * halfWord))
    end
end; (* form1Insn *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure form2Insn(i1, i2: integer);
begin
    form1Insn(i1);
    form1Insn(i2);
end; (* form2Insn *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure form3Insn(i1, i2, i3: integer);
begin
    form2Insn(i1, i2);
    form1Insn(i3);
end; (* form3Insn *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure disableNorm;
begin
    if arithMode <> 1 then begin
        form1Insn(disNormTemplate);
        arithMode := 1;
    end
end; (* disableNorm *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function getObjBufIdxPlus: integer;
begin
   if putLeft then
       getObjBufIdxPlus := objBufIdx + 4096
   else
       getObjBufIdxPlus := objBufIdx
end; (* getObjBufIdxPlus *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure formJump(var arg: integer);
var
    pos: integer;
    isLeft: boolean;
begin
    if prevOpcode <> insnTemp[UJ] then begin
        pos := getObjBufIdxPlus;
        isLeft := putLeft;
        form1Insn(jumpType + arg);
        if putLeft = isLeft then
            pos := pos - 1;
        arg := pos;
    end
end; (* formJump *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure padToLeft;
begin
    if not putLeft then
        form1Insn(insnTemp[UTC]);
    prevOpcode := 0;
end; (* padToLeft *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure formAndAlign(arg: integer);
begin
    form1Insn(arg);
    padToLeft;
    prevOpcode := 1;
end; (* formAndAlign *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure putToSymTab(arg: bitset);
begin
    symTab[symTabPos] := arg;
    if symTabPos = 75500B then begin
        error(50); (* errSymbolTableOverflow *)
        symTabPos := 74000B;
    end else
        symTabPos := symTabPos + 1;
end; (* putToSymTab *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function allocExtSymbol(l3arg1z: bitset): integer;
var
    l3var1z: word;
    l3var2z: integer;
begin
    allocExtSymbol := symTabPos;
    if (curVal.m * halfWord <> []) then begin
        for l3var2z to longSymCnt do
            if (curVal.m = longSyms[l3var2z]) then begin
                allocExtSymbol := longSymTabBase[l3var2z];
                exit
            end;
        longSymCnt := longSymCnt + 1;
        if (longSymCnt >= 90) then begin
            error(51); (* errLongSymbolOverflow *)
            longSymCnt := 1;
        end;
        longSymTabBase[longSymCnt] := symTabPos;
        longSyms[longSymCnt] := curVal.m;
        l3arg1z := l3arg1z + [25];
    end else
        l3arg1z := l3arg1z + curVal.m;
    putToSymTab(l3arg1z);
end; (* allocExtSymbol *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function getHelperProc(l3arg1z: integer): integer;
begin
    if (helperMap[l3arg1z] = 0) then begin
        curVal.m := helperNames[l3arg1z];
        helperMap[l3arg1z] := allocExtSymbol(extSymMask);
    end;
    getHelperProc := helperMap[l3arg1z] + (KVJM+I13);
end; (*getHelperProc *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure toFCST;
begin
    write(FCST, curVal.m);
    FcstCnt := FcstCnt + 1;
end; (* toFCST *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function addCurValToFCST: integer;
var
    low, high, mid: integer;
begin
    low := 1;
    if (FcstCountTo500 = 0) then begin
        addCurValToFCST := FcstCnt;
        FcstCountTo500 := 1;
        constVals[1] := curVal.a;
        constNums[1] := FcstCnt;
        toFCST;
    end else begin
        high := FcstCountTo500;
        repeat
            mid := (low + high) div 2;
            if (curVal.a = constVals[mid]) then begin
                addCurValToFCST := constNums[mid];
                exit
            end;
            if curval.a < constVals[mid] then
                high := mid - 1
            else
                low := mid + 1
        until high < low;
        addCurValToFCST := FcstCnt;
        if FcstCountTo500 <> 500 then begin
            if curval.a < constVals[mid] then
                high := mid
            else
                high := mid + 1;
            for mid := FcstCountTo500 downto high do begin
                low := mid + 1;
                constVals[low] := constVals[mid];
                constNums[low] := constNums[mid];
            end;
            FcstCountTo500 := FcstCountTo500 + 1;
            constVals[high] := curVal.a;
            constNums[high] := FcstCnt;
        end;
        toFCST;
    end
end; (* addCurValToFCST *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function allocSymtab(l3arg1z: bitset): integer;
var
    low, high, mid: integer;
    value: word;
begin
    low := 1;
    value.m := l3arg1z;
    if symTabCnt = 0 then begin
        allocSymtab := symTabPos;
        symTabCnt := 1;
        symTabArray[1].m := l3arg1z;
        symtbidx[1] := symTabPos;
    end else begin
        high := symTabCnt;
        repeat
            mid := (low + high) div 2;
            if (value = symTabArray[mid]) then begin
                allocSymtab := symtbidx[mid];
                exit
            end;
            if  value.a < symTabArray[mid].a then
                 high := mid - 1
            else
                 low := mid + 1;
        until high < low;
        allocSymtab := symTabPos;
        if symTabCnt <> 80 then begin
            if value.a < symTabArray[mid].a then
                high := mid
            else
                high := mid + 1;
            for mid := symTabCnt downto high do begin
                low := mid + 1;
                symTabArray[low] := symTabArray[mid];
                symtbidx[low] := symtbidx[mid];
            end;
            symTabCnt := symTabCnt + 1;
            symTabArray[high] := value;
            symtbidx[high] := symTabPos;
        end
    end;
    putToSymTab(value.m);
end; (* allocSymtab *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function getFCSToffset: integer;
var
    offset: word;
begin
    getFCSToffset := addCurValToFCST;
    offset :=;
    if (offset.i < 2048) then begin
        (* empty *)
    end else if (offset.i >= 4096) then
        error(204)
    else begin
        getFCSToffset := allocSymtab(offset.m + [24]) - 70000B;
        exit
    end
end; (* getFCSToffset *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function nrOfBits(value: integer): integer;
begin
    curVal.i := value;
    curVal.m := curVal.m * [7..47];
    nrOfBits := 48-minel(curval.m);
end; (* nrOfBits *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure defineRange(var res: tptr; l, r: integer);
var
    temp: tptr;
    dummyloc: word;
begin
    new(temp=7);
    with temp@ do begin
        size := 1;
        bits := 48;
        base := res;
        checker := 0;
        k := kindRange;
        curVal.i := l;
        curVal.m := curVal.m + intZero;
        left := curVal.i;
        curVal.i := r;
        curVal.m := curVal.m + intZero;
        right := curVal.i;
        if (left >= 0) then
            bits := nrOfBits(curVal.i);
        res := temp
    end
end; (* defineRange *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function getValueOrAllocSymtab(value: integer): integer;
begin
    curVal.i := value;
    curVal.i := curVal.i MOD 32768;
    if (40000B >= curVal.i) then
        getValueOrAllocSymtab := curVal.i
    else
        getValueOrAllocSymtab :=
            allocSymtab((curVal.m + [24]) * halfWord);
end; (* getValueOrAllocSymtab *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P0715(mode, arg: integer);
label 1;
var
    addr, insn, leftHalf: bitset;
    isLarge: boolean;
    work, offset: integer;
begin
    if mode = 0 then begin
        padToLeft;
        curVal.i := moduleOffset;
1:      addr := curval.m * [33..47];
        curVal := curVal;
        besm(ASN64-24);
        leftHalf:=;
        while arg <> 0 do begin
            if 4096 < arg then begin
                isLarge := true;
                arg := arg - 4096;
            end else isLarge := false;
            insn := objBuffer[arg];
            if isLarge then begin
                curVal.m := insn * [9..23];
                besm(ASN64+24);
                curVal :=;
                curVal.m := curVal.m + intZero;
                insn := insn * [0..8, 24..47] + leftHalf;
            end else begin
                curVal.m := intZero + insn * [33..47];
                insn := insn * [0..32] + addr;
            end;
            objBuffer[arg] := insn;
            arg := curVal.i;
        end;
        exit;
    end else if mode = 2 then begin
        form1Insn(KVTM+I14 + curVal.i);
        if curVal.i = 74001B then
            form1Insn(KUTM+I14 + FcstCnt);
        form3Insn(KITA+14, insnTemp[ASN] + arg, KAOX+I7+1);
        form1Insn(KATX+I7+1);
        exit;
    end else if (mode = 1) or (mode < -2) then begin
        arg := arg - curVal.i;
        offset := getFCSToffset;
        if mode = 1 then
            work := getHelperProc(68) + (-64200000B) (* P/DA *)
        else
            work := -mode;
        curVal.i := arg;
        arg := getFCSToffset;
        form3Insn(KATX+SP+1, KSUB+I8 + offset, work);
        form3Insn(KRSUB+I8 + arg, work, KXTA+SP+1);
        exit;
    end else if mode = -1 then begin
        form1Insn(KVTM+I14 + lineCnt);
        formAndAlign(getHelperProc(arg));
        exit;
    end;
    curVal.i := mode;
    goto 1;
end; (* P0715 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure endOfLine;
var
    unused: array [1..14] of integer;
    err, errPos, prevPos, listMode,
    startPos, lastErr: integer;
%
    procedure OBPROG(var start, fin: bitset);
        external;
begin
    listMode := pasinfor.listMode;
    if (listMode <> 0) or (errsInLine <> 0) then
    begin
        write(' ', (lineStartOffset + PASINFOR.startOffset):5 oct,
              lineCnt:5, lineNesting:3, commentModeCH);
        startPos := 13;
        if (S4 in optSflags.m)
            and (maxLineLen = 72)
            and (linePos >= 80)
        then begin
            for err := 73 to 80 do
                write(lineBufBase[err]);
            write(' ');
            linePos := 73;
            startPos := 22;
        end; (* 1106 *)
        repeat
            linePos := linePos-1
        until (lineBufBase[linePos]  <> ' ') or (linePos = 0);
        for err to linePos do begin
            output@ := lineBufBase[err];
            put(output);
        end;
        writeln;
        if errsInLine <> 0 then begin
            write('*****':startPos, ' ':errMapBase[0], '0');
            lastErr := errsInLine - 1;
            for err to lastErr do begin
                errPos := errMapBase[err];
                prevPos := errMapBase[err-1];
                if errPos <> prevPos then begin
                    if prevPos + 1 <> errPos then
                        write(' ':(errPos-prevPos-1));
                    write(chr(err + 48));
                end
            end;
            writeln;
            errsInLine := 0;
            prevErrPos := 0;
        end
    end; (* 1160 *)
    if (listMode = 2) and (moduleOffset <> lineStartOffset) then begin
        OBPROG(objBuffer[objBufIdx - moduleOffset + lineStartOffset],
               objBuffer[objBufIdx-1]);
    end; (* 1174 *)
    lineStartOffset := moduleOffset;
    linePos := 0;
    lineCnt := lineCnt + 1;
    if eof(pasinput) then begin
        error(errEOFEncountered);
        goto 9999;
    end
end; (* endOfLine *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure requiredSymErr(sym: symbol);
begin
    if linePos <> prevErrPos then
        error(ord(sym) + 88);
end; (* requiredSymErr *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure readToPos80;
begin
    while linePos < 81 do begin
        linePos := linePos + 1;
        lineBufBase[linePos] := PASINPUT@;
        if linePos <> 81 then get(PASINPUT);
    end;
    endOfLine
end; (* readToPos80 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure inSymbol;
label
    1473, 1, 2, 2175, 2233, 2320;
var
    localBuf: array [0..130] of char;
    tokenLen, tokenIdx: integer;
    expSign: boolean;
    l3var135z: irptr;
    expMultiple, expValue: real;
    curChar: char;
    numstr: array [1..16] of word;
    l3vars2: array [155..159] of word;
    expLiteral, expMagnitude: integer;
    l3int162z: integer;
    chord: integer;
    l3var164z: integer;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure nextCH;
begin
    repeat
        atEOL := eoln(PASINPUT);
        CH := PASINPUT@;
        get(PASINPUT);
        linePos := linePos + 1;
        lineBufBase[linePos] := CH;
    until (maxLineLen >= linePos) or atEOL;
end; (* nextCH *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseComment;
var
    badOpt, flag: boolean;
    c: char;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure readOptVal(var res: integer; limit: integer);
begin
    nextCH;
    res := 0;
    while ('9' >= CH) and (CH >= '0') do begin
        res := 10 * res + ord(CH) - ord('0');
        nextCH;
        badOpt := false;
    end;
    if limit < res then badOpt := true;
end; (* readOptVal *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure readOptFlag(var res: boolean);
begin
    nextCH;
    if (CH = '-') or (CH = '+') then begin
        res := CH = '+';
        badOpt := false;
    end;
    nextCH
end; (* readOptFlag *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* parseComment *)
    nextCH;
    if CH = '=' then begin
        repeat nextCH;
        badOpt := true;
        case CH of
        'D': begin
            readOptVal(curVal.i, 15);
            optSflags.m := optSflags.m * [0..40] + curVal.m * [41..47];
        end;
        'Y': readOptFlag(allowCompat);
        'E': readOptFlag(declExternal);
        'U': begin
            readOptFlag(flag);
            if flag then maxLineLen := 72 else maxLineLen := 120;
        end;
        'S': begin
            readOptVal(curVal.i, 9);
            if curVal.i = 3 then lineCnt := 1
            else if curVal.i in [4..9] then
                optSflags.m := optSflags.m + [curVal.i - 3]
            else begin
                extSymAdornment := curVal.i;
                (*=z-*)(q) exit q(*=z+*)
            end;
        end;
        'F': readOptFlag(checkFortran);
        'L': readOptVal(PASINFOR.listMode, 3);
        'P': readOptFlag(doPMD);
        'T': readOptFlag(checkBounds);
        'A': readOptVal(charEncoding, 3);
        'C': readOptFlag(checkTypes);
        'R': readOptFlag(fuzzReals);
        'M': readOptFlag(fixMult);
        'B': readOptVal(fileBufSize, 4);
        'K': readOptVal(heapSize, 23);
        'Z': readOptFlag(pseudoZ);
        end;
        if badOpt then
            error(54); (* errErrorInPseudoComment *)
        until CH <> ',';
    end; (* 1446 *)
    repeat
        while CH <> '*' do begin
            c := commentModeCH;
            commentModeCH := '*';
            if atEOL then
                endOfLine;
            nextCH;
            commentModeCH := c;
        end;
        nextCH
    until CH = ')';
    nextCH;
end; (* parseComment *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin
(again) begin (* inSymbol *)
        if dataCheck then begin
            error(errEOFEncountered);
            readToPos80;
            goto 9999;
        end;
1473:
        while (CH = ' ') and not atEOL do
            nextCH;
        if '_200' < CH then begin
            lineBufBase[linePos] := ' ';
            chord := ord(CH);
            for jj := 130 to chord do begin
                linePos := linePos + 1;
                lineBufBase[linePos] := ' ';
            end;
            nextCH;
            goto 1473;
        end;
        if atEOL then begin
            endOfLine;
            nextCH;
            if CH = '%' then while not atEOL do
                nextCH;
            goto 1473;
        end;
        hashTravPtr := NIL;
        SY := charSymTabBase[CH];
        charClass := chrClassTabBase[CH];
(lexer)
        if SY <> NOSY then begin
            case SY of
            IDENT: begin
1:              curToken.m := [];
                tokenLen := 1;
                repeat
                    curVal.c := iso2text[CH];
                    nextCH;
                    if 8 >= tokenLen then begin
                        tokenLen := tokenLen + 1;
                        curToken := curToken;
                        besm(ASN64-6);
                        curToken:=;
                        curToken.m := curToken.m + curVal.m;
                    end;
                until chrClassTabBase[CH] <> ALNUM;
                curVal.m := curToken.m * hashMask.m;
                mapAI(curVal.a, bucket);
                curIdent := curToken;
                keywordHashPtr := kwordHashTabBase[bucket];
                while keywordHashPtr <> NIL do begin
                    if keywordHashPtr@.w = curToken then begin
                        SY := keywordHashPtr@.sym;
                        charClass := keywordHashPtr@.op;
                        exit lexer;
                    end;
                    keywordHashPtr := keywordHashPtr@.next;
                    (*=z-*)besm(2200000B);(*=z+*)
                end;
                isDefined := false;
                SY := IDENT;
                case int93z of
                0: begin
                    hashTravPtr := symHashTabBase[bucket];
                    while hashTravPtr <> NIL do begin
                        if hashTravPtr@.offset = curFrameRegTemplate then
                        begin
                            if hashTravPtr@.id <> curIdent then
                                hashTravPtr := hashTravPtr@.next
                            else begin
                                isDefined := true;
                                exit lexer;
                            end
                        end else
                            exit lexer;
                    end;
                end;
                1: begin
2:                  hashTravPtr := symHashTabBase[bucket];
                    while hashTravPtr <> NIL do begin
                        if hashTravPtr@.id <> curIdent then
                            hashTravPtr := hashTravPtr@.next
                        else
                            exit lexer;
                    end;
                end;
                2: (q) begin
                    if expr63z = NIL then
                        goto 2;
                    expr62z := expr63z;
                    l3var135z := typeHashTabBase[bucket];
                    if l3var135z <> NIL then begin
                        while expr62z <> NIL do begin
                            l3int162z := expr62z@.typ2@.size;
                            hashTravPtr := l3var135z;
                            while hashTravPtr <> NIL do begin
                                if (hashTravPtr@.id = curIdent)
                                and (hashTravPtr@.value = l3int162z) then
                                    exit lexer;
                                hashTravPtr := hashTravPtr@.next;
                                (*=z-*)besm(2200000B);(*=z+*)
                            end;
                            expr62z := expr62z@.expr1;
                        end;
                    end;
                    goto 2;
                    (*=z-*)exit q(*=z+*)
                end;
                3: begin
                    hashTravPtr := typeHashTabBase[bucket];
                    while hashTravPtr <> NIL do begin
                        with hashTravPtr@ do begin
                            if (id = curIdent) and
                               (typ121z = uptype)
                            then
                                exit lexer;
                            hashTravPtr := next;
                       end
                   end
                end;
                end;
            end; (* IDENT *)
            REALCONST: begin
                nextCH;
                if charSymTabBase[CH] = IDENT then
                    goto 1;
                if CH = '(' then
                    SY := BEGINSY
                else if CH = ')' then
                    SY := ENDSY
                else begin
                    SY := NOSY;
                    exit
                end;
                nextCH;
            end; (* REALCONST *)
            INTCONST: begin (*=m-*)
                SY := INTCONST;
                tokenLen := 0;
                repeat
                    tokenLen := tokenLen + 1;
                    if (16 >= tokenLen) then
                        numstr[tokenLen].i := ord(CH)-ord('0')
                    else begin
                        error(55); (* errMoreThan16DigitsInNumber *)
                        tokenLen := 1;
                    end;
                    nextCH;
                until charSymTabBase[CH] <> INTCONST;
(octdec)        begin
                    if CH = 'B' then
                        suffix := suffixB
                    else if CH = 'C' then
                        suffix := suffixC
                    else if CH = 'T' then
                        suffix := suffixT
                    else begin
                        suffix := noSuffix;
                        exit octdec;
                    end;
                    nextCH;
                    curToken.c := chr(0);
                    for tokenIdx to tokenLen do begin
                        if 7 < numstr[tokenIdx].i then
                            error(20); (* errDigitGreaterThan7 *)
                        curToken := curToken;
                        besm(ASN64-3);
                        curToken:=;
                        curToken.m := numstr[tokenIdx].m * [45..47] +
                        curToken.m;
                    end;
                    if suffix = suffixB then begin
                        if curToken.m * [0..6] <> [] then begin
                            error(errNumberTooLarge);
                            curToken.i := 1;
                        end else
                            curToken.m := curToken.m + intZero;
                    end else if suffix = suffixT then begin
                        l3var164z := 16 - tokenLen;
                        for expMagnitude to l3var164z do begin
                            curToken := curToken;
                            besm(ASN64-3);
                            curToken :=;
                        end;
                    end (*=z-*)else(*=z+*) ;
                    exit lexer;
                end; (* octdec *)
                curToken.i := 0;
                for tokenIdx to tokenLen do begin
                    if 109951162777 >= curToken.i then
                        curToken.i := 10 * curToken.i +
                            numstr[tokenIdx].i
                    else begin
                        error(errNumberTooLarge);
                        curToken.i := 1;
                    end;
                end;
                expMagnitude := 0;
                if CH = '.' then begin
                    nextCH;
                    if CH = '.' then begin
                        CH := ':';
                        exit lexer
                    end;
                    curToken.r := curToken.i;
                    SY := REALCONST;
                    if charSymTabBase[CH] <> INTCONST then
                        error(56) (* errNeedMantissaAfterDecimal *)
                    else
                        repeat
                            curToken.r := 10.0*curToken.r + ord(CH)-48;
                            expMagnitude := expMagnitude-1;
                            nextCH;
                        until charSymTabBase[CH] <> INTCONST;
                end; (*2062*)
                if CH = 'E' then begin
                    if expMagnitude = 0 then begin
                        curToken.r := curToken.i;
                        SY := REALCONST;
                    end;
                    expSign := false;
                    nextCH;
                    if CH = '+' then
                        nextCH
                    else if CH = '-' then begin
                        expSign := true;
                        nextCH
                    end (*=z-*)else(*=z+*) ;
                    expLiteral := 0;
                    if charSymTabBase[CH] <> INTCONST then
                        error(57) (* errNeedExponentAfterE *)
                    else
                        repeat
                            expLiteral := 10 * expLiteral + ord(CH) - 48;
                            nextCH
                        until charSymTabBase[CH] <> INTCONST;
                    if expSign then
                        expMagnitude := expMagnitude - expLiteral
                    else
                        expMagnitude := expMagnitude + expLiteral;
                end; (* 2122 *)
                if expMagnitude <> 0 then begin
                    expValue := 1.0;
                    expSign := expMagnitude < 0;
                    expMagnitude := abs(expMagnitude);
                    expMultiple := 10.0;
                    if 18 < expMagnitude then begin
                        expMagnitude := 1;
                        error(58); (* errExponentGreaterThan18 *)
                    end;
                    repeat
                        if odd(expMagnitude) then
                            expValue := expValue * expMultiple;
                        expMagnitude := expMagnitude div 2;
                        if expMagnitude <> 0 then
                            expMultiple := expMultiple*expMultiple;
                    until expMagnitude = 0;
                    if expSign then
                        curToken.r := curToken.r / expValue
                    else
                        curToken.r := curToken.r * expValue;
                end;
                exit lexer
            end; (* INTCONST *) (*=m+*)
            CHARCONST: begin
(loop)          begin
                    for tokenIdx := 6 to 130 do begin
                        nextCH;
                        if charSymTabBase[CH] = CHARCONST then begin
                            nextCH;
                            if charSymTabBase[CH] <> CHARCONST then
                                exit loop
                            else
                                goto 2233;
                        end;
                        if atEOL then begin
2175:                       error(59); (* errEOLNInStringLiteral *)
                            exit loop
                        end else if ((CH = chr(35B)) or
                                   (charSymTabBase[CH] = REALCONST))
                               and (charSymTabBase[PASINPUT@] = INTCONST)
                        then begin
                            expLiteral := 0;
                            for tokenLen to 3 do begin
                                nextCH;
                                if '7' < CH then
                                    error(
                                        errFirstDigitInCharLiteralGreaterThan3
                                    );
                                expLiteral := 8*expLiteral + ord(CH) - 48;
                            end;
                            if 255 < expLiteral then
                                error(errFirstDigitInCharLiteralGreaterThan3);
                            localBuf[tokenIdx] := chr(expLiteral);
                        end else
2233:                       with PASINFOR do begin
                                if charEncoding = 3 then begin
                                    if (ch < '*') or ('_176' < CH) then
                                        curChar := chr(0)
                                    else begin
                                        curChar := iso2text[CH];
                                        (*=z-*)besm(3042246B);(*=z+*)
                                    end
                                end else if '_176' < CH then begin
                                    curChar := CH;
                                end else if charEncoding = 0 then begin
                                    curChar := a0@[CH];
                                end else if charEncoding = 1 then begin
                                    curChar := a1@[CH];
                                end else if charEncoding = 4 then begin
                                    curChar := a4@[CH];
                                end else begin
                                    curChar := CH;
                                    (*=z-*)(q) exit q(*=z+*)
                                end;
                                localBuf[tokenIdx] := curChar;
                            end;
                    end;
                    goto 2175
                end;
                strLen := tokenIdx - 6;
                if strLen = 0 then begin
                   error(61); (* errEmptyString *)
                   strLen := 1;
                   goto 2320
                end else if strLen = 1 then begin
                    SY := CHARCONST;
                    tokenLen := 1;
                    curToken.c := chr(0);
                    unpck(localBuf[0], curToken.a);
                    pck(localBuf[tokenLen], curToken.a);
                    exit lexer;
                end else 2320: begin
                    curVal.a := '      ';
                    SY := LTSY;
                    unpck(localBuf[tokenIdx], curVal.a);
                    pck(localBuf[6], curToken.a);
                    curVal :=;
                    if 6 >= strLen then
                        exit lexer
                    else begin
                        curToken.i := FcstCnt;
                        tokenLen := 6;
                        (loop) begin
                            toFCST;
                            tokenLen := tokenLen + 6;
                            if tokenIdx < tokenLen then
                                exit lexer;
                            pck(localBuf[tokenLen], curVal.a);
                            goto loop
                        end
                    end
                end;
            end; (* CHARCONST *)
            LTSY: begin
                SY := RELOP;
                nextCH;
                if CH = '>' then begin
                    charClass := NEOP;
                    nextCH
                end else if CH = '=' then begin
                    charClass := LEOP;
                    nextCH;
                end
            end; (* LTOP *)
            GTSY: begin
                SY := RELOP;
                nextCH;
                if CH = '=' then begin
                    charClass := GEOP;
                    nextCH
                end
            end; (* GTOP *)
            LPAREN: begin
                nextCH;
                if CH = '*' then begin
                    parseComment;
                    goto 1473
                end
            end;
            COLON: begin
                nextCH;
                if CH = '=' then begin
                    nextCH;
                    SY := BECOMES;
                    charClass := NOOP
                end
            end;
            NOTSY, LBRACK, MULOP, ADDOP, RELOP, RPAREN, RBRACK,
            COMMA, SEMICOLON, ARROW: begin
                nextCH;
            end;
            PERIOD: begin
                nextCH;
                if CH = '.' then begin
                    nextCH;
                    SY := COLON;
                    charClass := NOOP
                end else begin
                    if prevSY = ENDSY then
                        dataCheck := true;
                end
            end;
            end (* case *)
        end else begin (* 2444 *)
            nextCH;
        end;
        prevSY := SY;
        if not pseudoZ and not (DebugCode in optSflags.m) then begin
            commentModeCH := '=';
            goto again;
        end;
        commentModeCH := ' ';
        int93z := int92z;
    end
end; (* inSymbol *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure error;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure skipToEnd;
var
    sym: symbol;
begin
    sym := SY;
    while (sym <> ENDSY) or (SY <> PERIOD) do begin
        sym := SY;
        inSymbol
    end;
    if CH = 'D' then
        while SY <> ENDSY do
            inSymbol;
    goto 9999;
end;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* error *)
    errors := true;
    bool110z :=;
    if ((linePos <> prevErrPos) and (9 >= errsInLine))
        or (errno = 52)
    then begin
        write(' ');
        totalErrors := totalErrors + 1;
        errMapBase[errsInLine] := linePos;
        errsInLine := errsInLine + 1;
        prevErrPos := linePos;
        write('******', errno:0);
        printErrMsg(errno);
        if 60 < totalErrors then begin
            writeln;
            endOfLine;
            printErrMsg(53);
            skipToEnd
        end
    end
end;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure skip(toset: setofsys);
begin
    while not (SY IN toset) do
        inSymbol;
end; (* skip *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure test1(sym: symbol; toset: setofsys);
begin
    if (SY <> sym) then begin
        requiredSymErr(sym);
        skip(toset)
    end else
        inSymbol;
end; (* test1 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure errAndSkip(errno: integer; toset: setofsys);
begin
    error(errno);
    skip(toset)
end; (* errAndSkip *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseLiteral(var litType: tptr; var litValue: word;
    allowSign: boolean);
label
    99;
var
    l3var1z: operator;
begin
    litValue := curToken;
    if (GTSY < SY) then begin
        if allowSign and (charClass IN [PLUSOP, MINUSOP]) then begin
            l3var1z := charClass;
            inSymbol;
            parseLiteral(litType, litValue, false);
            if (litType <> integerType) then begin
                error(62); (* errIntegerNeeded *)
                litType := integerType;
                litValue.i := 1;
            end else begin
                if (l3var1z = MINUSOP) then
                    litValue.i := -litValue.i;
            end;
        end else
99:     begin
            litType := NIL;
            error(errNoConstant);
        end
    end else
        case SY of
        IDENT: begin
            if (hashTravPtr = NIL) or
               (hashTravPtr@.cl <> ENUMID) then
                goto 99;
            litType := hashTravPtr@.typ;
            litValue.i := hashTravPtr@.value;
        end;
        INTCONST:
            litType := integerType;
        REALCONST:
            litType := realType;
        CHARCONST:
            litType := charType;
        LTSY:
            makeStringType(litType);
        GTSY: begin
            litType := pointerType;
            litValue.i := 74000C;
        end;
        end (* case *)
end; (* parseLiteral *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P2672(var l3arg1z: irptr; l3arg2z: irptr);
var
    l3var1z: boolean;
    l3var2z: integer;
    l3var3z, l3var4z: irptr;
begin
    if l3arg1z = NIL then begin
        curVal.m := l3arg2z@.id.m * hashMask.m;
        mapAI(curVal.a, l3var2z);
        l3var1z := true;
        l3arg1z := symHashTabBase[l3var2z];
    end else begin
        l3var1z := false;
    end;
    if (l3arg1z = l3arg2z) then begin
        if (l3var1z) then begin
            symHashTabBase[l3var2z] :=
                symHashTabBase[l3var2z]@.next;
        end else begin
            l3arg1z := l3arg2z@.next;
        end;
    end else begin
        l3var3z := l3arg1z;
        while (l3var3z <> l3arg2z) do begin
            l3var4z := l3var3z;
            if (l3var3z <> NIL) then begin
                l3var3z := l3var3z@.next;
            end else begin
                exit
            end
        end;
        l3var4z@.next := l3arg2z@.next;
    end
end; (* P2672 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function isFileType(typtr: tptr): boolean;
begin
    isFileType := (typtr@.k = kindFile) or
        (typtr@.k = kindRecord) and typtr@.flag;
end; (* isFileType *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function knownInType(var rec: irptr): boolean;
begin
    if (typelist <> NIL) then begin
        rec := typelist;
        while (rec <> NIL) do begin
            if (rec@.id = curIdent) then begin
                knownInType := true;
                exit
            end;
            rec := rec@.next;
            (*=z-*)besm(2200000B);(*=z+*)
        end
    end;
    knownInType := false;
end; (* knownInType *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure checkSymAndRead(sym: symbol);
begin
    if (SY <> sym) then
        requiredSymErr(sym)
    else
        inSymbol
end; (* checkSymAndRead *)
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
    span1, span2: integer;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure allocWithTypeCheck;
begin
    new(link);
    link@ := [chain, basetyp1, basetyp2];
    chain := link;
    typeCheck := typeCheck(basetyp1, basetyp2);
end; (* allocWithTypeCheck *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function checkRecord(l4arg1z, l4arg2z: tptr): boolean;
var
    l4var1z: boolean;
begin
    l4var1z := (l4arg1z = NIL) or (l4arg2z = NIL);
    if (l4var1z) then begin
        checkRecord := l4arg1z = l4arg2z;
    end else begin
        checkRecord := typeCheck(l4arg1z@.base, l4arg2z@.base) and
                 checkRecord(l4arg1z@.next, l4arg2z@.next);
    end;
end; (* checkRecord *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* typeCheck *)
    rangeMismatch := false;
    if (type1@.k = kindRange) then begin
        typ120z := type1@.base;
    end else begin
        typ120z := type1;
    end;
    if not checkTypes or (type1 = type2) then
1:      typeCheck := true
    else
        with type1@ do begin
            kind1 := k;
            kind2 := type2@.k;
            if (kind1 = kind2) then begin
                case kind1 of
                kindReal:
                    (* empty *);
                kindScalar: begin
(chain)             if (type1@.numen = type2@.numen) then begin
                        enums1 := type1@.enums;
                        enums2 := type2@.enums;
                        while (enums1 <> NIL) and (enums2 <> NIL) do begin
                            if (enums1@.id <> enums2@.id) then
                                exit chain;
                            enums1 := enums1@.list;
                            enums2 := enums2@.list;
                        end;
                        if (enums1 = NIL) and (enums2 = NIL) then
                            goto 1;
                    end
                end;
                kindRange: begin
                    baseMatch := (type1@.base = type2@.base);
                    typ120z := type1@.base;
                    rangeMismatch := (type1@.left <> type2@.left) or
                                (type1@.right <> type2@.right);
                    typeCheck := baseMatch;
                    exit
                end;
                kindPtr: begin
                    if (type1 = pointerType) or (type2 = pointerType) then
                        goto 1;
                    basetyp1 := type1@.base;
                    basetyp2 := type2@.base;
                    if (chain <> NIL) then begin
                        link := chain;
                        while (link <> NIL) do with link@ do begin
                            if (type1 = basetyp1) and
                               (type2 = basetyp2) or
                               (type2 = basetyp1) and
                               (type1 = basetyp2) then
                                goto 1;
                            link := next;
                        end;
                        allocWithTypeCheck;
                    end else begin
                        setup(type1);
                        allocWithTypeCheck;
                        chain := NIL;
                        rollup(type1);
                        exit
                    end
                end;
                kindSet:
                    goto 1;
                kindArray: begin
                    with type1@.range@ do
                        span1 := right - left;
                    with type2@.range@ do
                        span2 := right - left;
                    if typeCheck(type1@.base, type2@.base) and
                       (span1 = span2) and
                       (type1@.pck = type2@.pck) and
                       not rangeMismatch then begin
                        if type1@.pck then begin
                            if (type1@.pcksize = type2@.pcksize) then
                                goto 1
                        end else
                            goto 1
                    end
                end;
                kindFile: begin
                    if typeCheck(type1@.base, type2@.base) then
                        goto 1;
                end;
                kindRecord: begin
                    if checkRecord(type1@.first, type2@.first) then
                        goto 1;
                end
                end (* case *)
            end else begin
                if (kind1 = kindRange) then begin
                    rangeMismatch := true;
                    typ120z := type2;
                    if (type1@.base = type2) then
                        goto 1;
                end else if (kind2 = kindRange) and
                          (type1 = type2@.base) then
                    goto 1;
            end;
            typeCheck := false;
        end
end; (* typeCheck *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function F3307(l3arg1z: irptr): integer;
var
    l3var1z: integer;
    l3var2z: irptr;
begin
    l3var2z := l3arg1z@.argList;
    l3var1z := 0;
    if (l3var2z <> NIL) then
        while (l3var2z <> l3arg1z) do begin
            l3var1z := l3var1z + 1;
            l3var2z := l3var2z@.list;
        end;
    F3307 := l3var1z;
end; (* F3307 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function makeNameWithStars(isProc: boolean): bitset;
var
    wantBoth: boolean;
begin
    wantBoth := not isProc and (extSymAdornment = 0);
    if curVal.m * [0..5] = [] then begin
        curVal := curVal;
        besm(ASN64-6);
        curVal := ;
        if wantBoth or (extSymAdornment = 1) then
            curVal.m := curVal.m + [44, 46];
        while curVal.m * [0..11] = [] do begin
            curVal := curVal;
            besm(ASN64-6);
            curVal := ;
        end;
        if curVal.m * [0..5] = [] then begin
            if wantBoth then
                curVal.m := [2, 4] + curVal.m
            else begin
                curVal := curVal;
                besm(ASN64-6);
                curVal := ;
            end
        end
    end;
    makeNameWithStars := curVal.m;
end; (* makeNameWithStars *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure formOperator(l3arg1z: opgen);
var
    l3int1z, l3int2z, l3int3z: integer;
    nextInsn: integer;
    l3var5z: eptr;
    flags: opflg;
    l3var7z,
    l3var8z: word;
    l3bool9z: boolean;
    l3var10z, l3var11z: word;
    saved: @insnltyp;
    l3bool13z: boolean;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genOneOp;
label
    3556;
var
    insnBufIdx: integer;
    l4var2z, l4var3z, l4var4z: integer;
    l4var5z: word;
    l4inl6z, l4inl7z, l4inl8z: oiptr;
    l4var9z: integer;
    insnBuf: array [1..200] of word;
    curInsn: word;
    tempInsn: word;
    l4oi212z: oiptr;
    l4var213z: boolean;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P3363;
begin
    if l4var213z then
        form1Insn(insnTemp[XTA])
    else
        form1Insn(KXTA+E1)
end; (* P3363 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure  addInsnToBuf(insn: integer);
begin
    insnBuf[insnBufIdx].i := insn;
    insnBufIdx := insnBufIdx + 1;
end; (* addInsnToBuf *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure add2InsnsToBuf(insn1, insn2: integer);
begin
    insnBuf[insnBufIdx].i := insn1;
    insnBuf[insnBufIdx+1].i := insn2;
    insnBufIdx := insnBufIdx + 2;
end; (* add2InsnsToBuf *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function F3413: boolean;
begin
    l4inl7z := l4inl6z;
    while l4inl7z <> NIL do begin
        if (l4inl7z@.mode = curInsn.i) then begin
            F3413 := true;
            while (l4inl7z@.code = macro) do begin
                l4inl7z := ptr(l4inl7z@.offset);
            end;
            exit
        end else begin
            l4inl7z := l4inl7z@.next;
        end
    end;
    F3413 := false;
end; (* F3413 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure addJumpInsn(opcode: integer);
begin
    if not F3413 then begin
        new(l4inl7z);
        l4inl7z@.next := l4inl6z;
        l4inl7z@.mode := curInsn.i;
        l4inl7z@.code := 0;
        l4inl7z@.offset := 0;
        l4inl6z := l4inl7z;
    end;
    addInsnToBuf(macro + opcode + ord(l4inl7z))
end; (* addJumpInsn *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* genOneOp *)
    if insnList = NIL
        then exit;
    set145z := set145z + insnList@.regsused;
    l4oi212z := insnList@.next2;
    l4var9z := 370007B;
    insnBufIdx := 1;
    if l4oi212z = NIL then
        exit;
    l4inl6z := NIL;
    while l4oi212z <> NIL do begin
        tempInsn.i := l4oi212z@.code;
        l4var4z := tempInsn.i -  macro;
        curInsn.i := l4oi212z@.offset;
        case l4oi212z@.mode of
         0: ;
         1: if arithMode <> 1 then begin
                addInsnToBuf(370007B);
                arithMode := 1
            end;
         2: arithMode := 1;
         3: if arithMode <> 2 then begin
                addInsnToBuf(insnTemp[NTR]);
                arithMode := 2;
            end;
         4: arithMode := 2;
        end; (* case *)
        l4oi212z := l4oi212z@.next;
        if l4var4z >= 0 then begin
            case l4var4z of
            mcCARD: begin
                add2InsnsToBuf(KACX, KAEX+ZERO);
            end;
            21: goto 3556;
            0:  addJumpInsn(insnTemp[UZA]);
            1:  addJumpInsn(insnTemp[U1A]);
            2: begin
                tempInsn.i := curInsn.i mod 4096;
                curInsn.i := curInsn.i div 4096;
                addJumpInsn(insnTemp[UJ]);
                curInsn.i := tempInsn.i;
3556:           if F3413 then
                    addInsnToBuf(2*macro+ord(l4inl7z))
                else
                    error(206);
            end;
            3: begin
                 tempInsn.i := curInsn.i mod 4096;
                 curInsn.i := curInsn.i div 4096;
                 l4var213z :=  F3413;
                 l4inl8z := l4inl7z;
                 curInsn.i := tempInsn.i;
                 l4var213z := l4var213z & F3413;
                 if l4var213z then
                    with l4inl7z@ do begin
                        code := macro;
                        offset := ord(l4inl8z);
                    end
                else
                    error(207);
            end;
            20: addInsnToBuf(3*macro + curInsn.i);
            4: begin
                if insnBuf[insnBufIdx-1].m * [21:23, 28:35] = [] then
                    insnBuf[insnBufIdx-1].m := insnBuf[insnBufIdx-1].m + [35]
                else
                    addInsnToBuf(KXTA+SP)
            end;
            5:
(blk)       begin
                if l4oi212z <> NIL then begin
                    tempInsn.i := l4oi212z@.code;
                    if tempInsn.m * [21:23, 28:35] = [32] then begin
                        l4oi212z@.code :=
                            tempInsn.i - insnTemp[XTA] + insnTemp[XTS];
                        exit blk
                    end
                end;
                addInsnToBuf(KATX+SP);
            end;
            mcACC2ADDR:  add2InsnsToBuf(KATI+14, KUTC+I14);
            mcMULTI: begin
                addInsnToBuf(getHelperProc(12));        (* P/MI *)
            end;
            mcADDSTK2REG:  add2InsnsToBuf(KWTC+SP, KUTM+
                               indexreg[curInsn.i]);
            mcADDACC2REG:  add2InsnsToBuf(KATI+14, KJADDM+I14 + curInsn.i);
            mcODD: begin
                add2InsnsToBuf(KAAX+E1, KAEX+ZERO);
            end;
            mcROUND: begin
                addInsnToBuf(KADD+REAL05);                (* round *)
                add2InsnsToBuf(KNTR+7, KADD+ZERO)
            end;
            mcSQRR: begin
                add2InsnsToBuf(KATX+SP, KMUL+SP);   (* sqr *)
            end;
            mcSQRI: begin
                add2InsnsToBuf(KATX+SP, KAEX+MULTMASK);   (* sqrint *)
                add2InsnsToBuf(KMUL+SP, KYTA+64)
            end;
            14: add2InsnsToBuf(indexreg[curInsn.i] + KVTM,
                               KITA + curInsn.i);
            mcMINEL: begin
                add2InsnsToBuf(KANX+ZERO, KSUB+PLUS1);   (* minel *)
            end;
            16: add2InsnsToBuf(insnTemp[XTA], KATX+SP + curInsn.i);
            17: begin
                addInsnToBuf(KXTS);
                add2InsnsToBuf(KATX+SP+1, KUTM+SP + curInsn.i)
            end;
            18: add2InsnsToBuf(KVTM+I10, getHelperProc(65)); (* P/B7 *)
            mcPOP2ADDR: begin
                addInsnToBuf(KVTM+I14);
                add2InsnsToBuf(KXTA+SP, KATX+I14)
            end;
            22: begin
                add2InsnsToBuf(KVTM+I14, KXTA+I14);
                curVal.i := 40077777C;
                add2InsnsToBuf(allocSymtab(curVal.m) + (KXTS+SP),
                               KAAX+I8 + curInsn.i);
                add2InsnsToBuf(KAEX+SP, KATX+I14)
            end;
            end; (* case *)
        end else begin (* 4003 *)
            if 28 in tempInsn.m then begin
                addInsnToBuf(getValueOrAllocSymtab(curInsn.i)+tempInsn.i);
            end else begin
                curval.i := curInsn.i mod 32768;
                if curVal.i < 2048 then
                    addInsnToBuf(tempInsn.i + curInsn.i)
                else
(stmt)          if (curVal.i >= 28672) or (curVal.i < 4096) then begin
                    addInsnToBuf(
                        allocSymtab((curVal.m + [24])*halfWord)
                        + tempInsn.i - 28672);
                end else begin
                    add2InsnsToBuf(getValueOrAllocSymtab(curVal.i)
                                   + insnTemp[UTC], tempInsn.i);
                    (*=z-*)exit stmt;(*=z+*)
                end
            end
        end
    end; (* 4037 *)
    insnBufIdx := insnBufIdx-1;
    for l4var4z := insnBufIdx downto 1 do begin
        curInsn := insnBuf[l4var4z];
        if (curInsn.i = insnTemp[NTR]) or
           (curInsn.i = 370007B)
        then begin
            l4var3z := l4var4z - 1;
            l4var213z := false;
(loop)      if l4var3z < 1 then exit loop else begin
                tempInsn.m := insnBuf[l4var3z].m * [28:32];
                if (tempInsn.i = CUTC) or (tempInsn.i = CWTC)
                then begin
                    l4var3z := l4var3z-1;
                    goto loop;
                end
            end;
(* one word shorter
(loop)      while l4var3z >= 1 do begin
                tempInsn.m := insnBuf[l4var3z].m * [28:32];
                if (tempInsn.i # CUTC) and (tempInsn.i # CWTC)
                then
                    exit loop;
                l4var3z := l4var3z-1;
            end;
*)
            l4var3z := l4var3z + 1;
            if (l4var3z <> l4var4z) then begin
                for l4var2z := l4var4z-1 downto l4var3z do begin
                    insnBuf[l4var2z+1] := insnBuf[l4var2z]
                end;
            end;
            insnBuf[l4var3z] := curInsn;
        end; (* 4103 *)
    end;
    for l4var4z to insnBufIdx do
(iter)  begin
        curInsn := insnBuf[l4var4z];
        tempInsn.m := curInsn.m * [0, 1, 3, 23:32];
        if tempInsn.i = KATX+SP then begin
            l4var2z := l4var4z + 1;
            while insnBufIdx + 1 <> l4var2z do begin
                curVal.m := insnBuf[l4var2z].m * [0, 1, 3, 23, 28:35];
                tempInsn.m := curVal.m * [0, 1, 3, 23, 28:32];
                if curVal.i = insnTemp[XTA] then begin
                    insnBuf[l4var2z].m :=
                        insnBuf[l4var2z].m mod [32, 34, 35];
                    exit iter;
                end else if curVal.i = insnTemp[ITA] then begin
                    insnBuf[l4var2z].m := insnBuf[l4var2z].m + [35];
                    exit iter;
                end else if (curVal.i = insnTemp[NTR]) or
                    (tempInsn.i = insnTemp[UTC]) or
                    (tempInsn.i = insnTemp[WTC]) or
                    (tempInsn.i = insnTemp[VTM])
                then
                    l4var2z := l4var2z + 1
                else (q) begin
                    l4var2z := insnBufIdx + 1;
                    (*=z-*)exit q(*=z+*)
                end
            end;
        end; (* 4150 *)
        if curInsn.i = insnTemp[UTC] then
            exit iter;
        if curInsn.i < macro then begin
            form1Insn(curInsn.i);
            tempInsn.m := curInsn.m * [28:32];
            if (tempInsn.i = 3100000C) or (* VJM *)
               (tempInsn.i = 0500000C)    (* ELFUN *)
            then begin
                padToLeft;
                prevOpcode := 1;
            end;
            exit iter;
        end;
        if (curInsn.i >= 3*macro) then begin
            curInsn.i := curInsn.i - (3*macro);
            if curInsn.i >= 4096 then begin
                l4var213z := true;
                curInsn.i := curInsn.i - 4096;
            end else begin
                l4var213z := false;
            end;
            if (curInsn.i = 0) then
                form1Insn(insnTemp[UZA] + moduleOffset + 2);
            P3363;
            form1Insn(insnTemp[UJ] + 2 + moduleOffset);
            padToLeft;
            if (curInsn.i <> 0) then begin
                if (not F3413) then
                    error(211);
                P0715(0, l4inl7z@.code);
            end;
            l4var213z := not l4var213z;
            P3363;
            padToLeft;
            exit iter
        end; (* 4230 *)
        if (curInsn.i >= 2*macro) then begin
            l4inl7z := ptr(curInsn.i - (2*macro));
            P0715(0, l4inl7z@.code);
            l4inl7z@.offset := moduleOffset;
        end else begin
            curInsn.i := curInsn.i - macro;
            curVal.m := curInsn.m * [0, 1, 3, 28:32];
            jumpType := curVal.i;
            curVal.m := [0, 1, 3, 33:47] * curInsn.m;
            l4inl7z := ptr(curVal.i);
            formJump(l4inl7z@.code);
            jumpType := insnTemp[UJ];
            exit iter
        end
    end; (* loop *)
    insnList := NIL;
    while (l4inl6z <> NIL) do begin
        with l4inl6z@ do
            if (offset = 0) then begin
                jumpTarget := code;
                exit;
            end else
                l4inl6z := next;
    end;
    set146z := set146z - set145z;
end; (* genOneOp *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure addToInsnList(insn: integer);
var elt: oiptr;
begin
    new(elt);
    with elt@ do begin
        next := NIL;
        mode := 0;
        code := insn;
        offset := 0;
    end;
    with insnList@ do begin
        if next = NIL then
            next2 := elt
        else
            next@.next := elt;
        next := elt
    end
end; (* addToInsnList *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure addInsnAndOffset(insn, l4arg2z: integer);
begin
    addToInsnList(insn);
    insnlist@.next@.offset := l4arg2z
end; (* addInsnAndOffset *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure addxToInsnList(insn: integer);
var
    elt: oiptr;
begin
    new(elt);
    with elt@ do begin
        next := insnList@.next2;
        mode := 0;
        code := insn;
        offset := 0;
    end;
    if (insnList@.next2 = NIL) then begin
        insnList@.next := elt;
    end;
    insnList@.next2 := elt;
end; (* addxToInsnList *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure prepLoad;
label
    4545, 4602;
var
    helper, l4int2z, l4int3z: integer;
    l4typ4z: tptr;
    l4var5z: kind;
    l4st6z: state;
    l4bool7z, l4bool8z, l4bool9z: boolean;
begin
    l4typ4z := insnList@.typ;
    with insnList@ do begin
        case ilm of
        ilCONST: begin
            curVal := ilf5;
            if (l4typ4z@.size = 1) then
                curVal.i := getFCSToffset;
            addToInsnList(constRegTemplate + curInsnTemplate + curVal.i);
        end;
        il1: begin
            helper := insnList@.ilf7;
            l4int2z := insnList@.ilf5.i;
            l4int3z := insnList@.ilf6;
            if (15 < helper) then begin
                (* empty *)
            end else begin
                if (helper = 15) then begin (* P/CP *)
                    addToInsnList(macro + mcACC2ADDR);
                end else begin
                    helper := indexreg[insnList@.ilf7];
                    if (l4int2z = 0) and (insnList@.st = st0) then begin
                        addInsnAndOffset(helper + curInsnTemplate,
                                         l4int3z);
                        goto 4602;
                    end else begin
                        addToInsnList(helper + insnTemp[UTC]);
                        (*=z-*)(q) exit q(*=z+*)
                    end
                end
            end;
            l4st6z := insnList@.st;
            if l4st6z = st0 then begin
                addInsnAndOffset(l4int2z + curInsnTemplate, l4int3z);
            end else begin
                l4var5z := l4typ4z@.k;
                if (l4var5z < kindSet) or
                   (l4var5z = kindRecord) and (s6 in optSflags.m) then begin
                    l4bool7z := true;
                    l4bool8z := typeCheck(l4typ4z, integerType);
                end else begin
                    l4bool7z := false;
                    l4bool8z := false;
                end;
                if l4st6z = st1 then begin
                    if (l4int3z <> l4int2z) or
                       (helper <> 18) or (* P/RC *)
                       (l4int2z <> 0) then
                        addInsnAndOffset(l4int2z + insnTemp[XTA],
                                         l4int3z);
                    l4int3z := insnList@.shift;
                    l4int2z := insnList@.width;
                    l4bool9z := true;
                    helper := l4int3z + l4int2z;
                    if l4bool7z then begin
                        if (30 < l4int3z) then begin
                            addToInsnList(ASN64-48 + l4int3z);
                            addToInsnList(insnTemp[YTA]);
                            if (helper = 48) then (* P/RDR *)
                                l4bool9z := false;
                        end else begin
                            if (l4int3z <> 0) then
                                addToInsnList(ASN64 + l4int3z);
                        end; (* 4477 *)
                        if l4bool9z then begin
                            curVal.m := [(48 - l4int2z)..47];
                            addToInsnList(KAAX+I8 + getFCSToffset);
                        end
                    end else begin (* 4511 *)
                        if (helper <> 48) then
                            addToInsnList(ASN64-48 + helper);
                        curVal.m := [0..(l4int2z-1)];
                        addToInsnList(KAAX+I8 + getFCSToffset);
                    end; (* 4525 *)
                    if l4bool8z then
                        addToInsnList(KAEX+ZERO);
                end else begin (* 4531 *)
                    if l4bool7z then
                        helper := ord(l4bool8z)+74 (* P/LDAR[IN] *)
                    else
                        helper := 56; (* P/RR *)
                    addToInsnList(getHelperProc(helper));
                    insnList@.next@.mode := 1;
                end
            end;
            goto 4545;
        end;
        il2: begin
4545:       if bool49z and (l4typ4z = booleanType) and
               (16 in insnList@.regsused) then
                addToInsnList(KAEX+E1);
        end;
        il3: begin (* 4555 *)
            if bool49z then
                addInsnAndOffset(macro+20,
                    ord(16 in insnList@.regsused)*10000B + insnList@.ilf5.i);
        end;
        end; (* case *)
4602:
    end; (* with *)
    with insnList@ do begin
        ilm := il2;
        regsused := regsused + [0];
    end;
end; (* prepLoad *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P4606;
begin
    prepLoad;
    addToInsnList(macro + mcPUSH)
end; (* P4606 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure setAddrTo(reg: integer);
label
    4650, 4654;
var
    l4var1z: word;
    l4int2z, opCode, l4var4z, l4var5z,
    l4var6z, regField: integer;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P4613;
begin
    l4var1z.i := insnList@.ilf6;
    l4var1z.i := l4var1z.i mod 32768;
    l4var6z := l4var1z.i
end; (* P4613 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* setAddrTo *)
    with insnList@ do begin
        l4int2z := ilf7;
        opCode := insnTemp[VTM];
        regField := indexreg[reg];
        l4var4z := ilf5.i;
        regsused := regsused + [reg];
        if (ilm = ilCONST) then begin
            curVal := ilf5;
            if (typ@.size = 1) then
                curVal.i := addCurValToFCST;
            l4var6z := curVal.i;
            l4var5z := 74001B;
            goto 4654;
        end else if (l4int2z = 18) then begin
4650:       P4613;
            if (l4var4z = indexreg[1]) then begin
                l4var5z := 74003B;
4654:           l4var1z.i := macro * l4var5z + l4var6z;
                l4var6z := allocSymtab(l4var1z.m * [12:47]);
                addToInsnList(regField + opCode + l4var6z);
            end else if (l4var4z <> 0) then begin
                addInsnAndOffset(l4var4z + insnTemp[UTC], l4var6z);
                addToInsnList(regField + opCode);
            end else (q) begin
                addInsnAndOffset(regField + opCode, l4var6z);
                (*=z-*)exit q(*=z+*)
            end
        end else if (l4int2z = 17) then begin
            P4613;
            l4var4z := insnList@.ilf6;
            l4var5z := insnList@.next@.code - insnTemp[UTC];
            if (l4var4z <> 0) then begin
                l4var1z.i := macro * l4var5z + l4var4z;
                l4var5z := allocSymtab(l4var1z.m * [12:47]);
            end;
            insnList@.next@.code := regField + l4var5z + opCode;
        end else if (l4int2z = 16) then begin
            P4613;
            if (l4var4z <> 0) then
                addToInsnList(l4var4z + insnTemp[UTC]);
            addInsnAndOffset(regField + opCode, l4var6z);
        end else if (l4int2z = 15) then begin
            addToInsnList(insnTemp[ATI] + reg);
            opCode := insnTemp[UTM];
            goto 4650;
        end else begin
            addToInsnList(indexreg[l4int2z] + insnTemp[UTC]);
            goto 4650;
        end
    end; (* with *)
    insnList@.ilm := il1;
    insnList@.ilf7 := reg;
    insnList@.ilf6 := 0;
    insnList@.ilf5.i := 0;
end; (* setAddrTo *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure prepStore;
var
    l4int1z: integer;
    l4int2z, l4int3z: integer;
    l4bool4z, l4bool5z: boolean;
    l4st6z: state;
    l4var7z: kind;
begin
    with insnList@ do
        l4int1z := ilf7;
    if (15 < l4int1z) then begin
        (* nothing? *)
    end else if (l4int1z = 15) then begin
        addToInsnList(macro + mcACC2ADDR)
    end else (q) begin
        addToInsnList(indexreg[l4int1z] + insnTemp[UTC]);
        (*=z-*)exit q(*=z+*)
    end;
    l4bool4z := 0 in insnList@.regsused;
    l4st6z := insnList@.st;
    if (l4st6z <> st0) or l4bool4z then
        addxToInsnList(macro + mcPUSH);
    if (l4st6z = st0) then begin
        if (l4bool4z) then begin
            addInsnAndOffset(insnList@.ilf5.i + insnTemp[UTC],
                             insnList@.ilf6);
            addToInsnList(macro+mcPOP2ADDR);
        end else begin
            addInsnAndOffset(insnList@.ilf5.i, insnList@.ilf6);
        end
    end else begin
        l4var7z := insnList@.typ@.k;
        l4int1z := insnList@.typ@.bits;
        l4bool5z := (l4var7z < kindSet) or
                     (l4var7z = kindRecord) and (S6 in optSflags.m);
        if (l4st6z = st1) then begin
            l4int2z := insnList@.shift;
            l4int3z := l4int2z + insnList@.width;
            if l4bool5z then begin
                if (l4int2z <> 0) then
                    addxToInsnList(ASN64 - l4int2z);
            end else begin
                if (l4int3z <> 48) then
                    addxToInsnList(ASN64 + 48 - l4int3z);
            end;
            addInsnAndOffset(insnTemp[UTC] + insnList@.ilf5.i,
                             insnList@.ilf6);
            curVal.m := [0..47] - [(48-l4int3z)..(47 -l4int2z)];
            addInsnAndOffset(macro+22, getFCSToffset);
        end else begin
            if not l4bool5z then begin
                l4int2z := (insnList@.width - l4int1z);
                if (l4int2z <> 0) then
                    addxToInsnList(ASN64 - l4int2z);
                addxToInsnList(insnTemp[YTA]);
                addxToInsnList(ASN64 - l4int1z);
            end;
            addToInsnList(getHelperProc(77)); (* "P/STAR" *)
            insnList@.next@.mode := 1;
        end
    end
end; (* prepStore *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P5117(op: operator);
begin
    addInsnAndOffset(curFrameRegTemplate, localSize);
    new(curExpr);
    with curExpr@ do
        typ := insnList@.typ;
    genOneOp;
    curExpr@.op := op;
    curExpr@.num1 := localSize;
    localSize := localSize + 1;
    if (l2int21z < localSize) then
        l2int21z := localSize;
end; (* P5117 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function insnCount: integer;
var
    cnt: integer;
    cur: oiptr;
begin
    cnt := 0;
    cur := insnList@.next2;
    while (cur <> NIL) do begin
        cur := cur@.next;
        cnt := cnt + 1;
    end;
    insnCount := cnt;
end; (* insnCount *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genFullExpr(exprToGen: eptr);
label
    7567, 7760, 10075, 10122;
var
    arg1Const, arg2Const: boolean;
    otherIns: @insnltyp;
    arg1Val, arg2Val: word;
    curOP: operator;
    work: integer;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P5155;
begin
    prepLoad;
    insnList@.ilm := il1;
    insnList@.st := st0;
    insnList@.ilf6 := 0;
    insnList@.ilf5.i := 0;
    insnList@.ilf7 := 18;
end; (* P5155 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genDeref;
label
    5220;
var
    l5var1z, l5var2z: word;
    doPtrCheck: boolean;
begin
    doPtrCheck := checkBounds and not (NoPtrCheck in optSflags.m)
               and (curOP = DEREF);
    if not doPtrCheck and (
        (insnList@.st = st0) or
        (insnList@.st = st1) and
        (insnList@.shift = 0))
    then begin
        l5var1z.i := insnList@.ilf7;
        l5var2z.i := insnList@.ilf6;
        if (l5var1z.i = 18) or (l5var1z.i = 16) then begin
5220:       addInsnAndOffset((insnList@.ilf5.i + insnTemp[WTC]), l5var2z.i);
        end else begin
            if (l5var1z.i = 17) then begin
                if (l5var2z.i = 0) then begin
                    insnList@.next@.code := insnList@.next@.code +
                                                insnTemp[XTA];
                end else
                    goto 5220;
            end else if (l5var1z.i = 15) then begin
                addToInsnList(macro + mcACC2ADDR);
                goto 5220;
            end else (q) begin
                addInsnAndOffset((indexreg[l5var1z.i] + insnTemp[WTC]),
                                 l5var2z.i);
                (*=z-*)exit q(*=z+*)
            end
        end
    end else begin
        P5155;
        if (doPtrCheck) then begin
            addToInsnList(KVTM+I14 + lineCnt);
            addToInsnList(getHelperProc(7)); (* "P/CA"*)
            insnList@.next@.mode := 1;
        end;
        addToInsnList(macro + mcACC2ADDR);
    end;
    insnList@.ilf6 := 0;
    insnList@.ilf5.i := 0;
    insnList@.ilf7 := 16;
end; (* genDeref *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genHelper;
begin
    P4606;
    saved := insnList;
    insnList := otherIns;
    prepLoad;
    addToInsnList(getHelperProc(nextInsn));
    insnList@.regsused := insnList@.regsused + saved@.regsused + [11:14];
    saved@.next@.next := insnList@.next2;
    insnList@.next2 := saved@.next2;
end; (* genHelper *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure prepMultiWord;
var
    l5var1z: boolean;
    l5var2z: @insnltyp;
begin
    l5var1z := 12 in otherIns@.regsused;
    setAddrTo(12);
    if (l5var1z) then begin
        addToInsnList(KITA+12);
        addToInsnList(macro + mcPUSH);
    end;
    l5var2z := insnList;
    insnList := otherIns;
    setAddrTo(14);
    if (l5var1z) then begin
        addToInsnList(macro + mcPOP);
        addToInsnList(KATI+12);
    end;
    l5var2z@.regsused := insnList@.regsused + l5var2z@.regsused;
    l5var2z@.next@.next := insnList@.next2;
    l5var2z@.next := insnList@.next;
    insnList := l5var2z;
end; (* prepMultiWord *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genCheckBounds(l5arg1z: tptr);
var
    l5var1z: integer;
    l5var2z, l5var3z, l5var4z: word;
begin
    l5var1z := l5arg1z@.checker;
    if (l5var1z = 0) then begin
        curVal.i := l5arg1z@.left;
        l5var4z.i := l5arg1z@.right;
        if (l5arg1z@.base <> integerType) then begin
            curVal.m := curVal.m * [7:47];
            l5var4z.m := l5var4z.m * [7:47];
        end;
        prevOpcode := 0;
        formAndAlign(KUJ+5 + moduleOffset);
        l5arg1z@.checker := moduleOffset;
        l5var1z := moduleOffset;
        P0715(1, l5var4z.i);
        formAndAlign(KUJ+I13);
    end;
    prepLoad;
    addToInsnList(KVTM+I14 + lineCnt);
    addToInsnList(KVJM+I13 + l5var1z);
    insnList@.next@.mode := 1;
end; (* genCheckBounds *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure negateCond;
begin
    if (insnList@.ilm = ilCONST) then begin
        insnList@.ilf5.b := not insnList@.ilf5.b;
    end else begin
        insnList@.regsused := insnList@.regsused mod [16];
    end
end; (* negateCond *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure tryFlip(commutes: boolean);
label
    100, 22, 33;
var
    l5var1z: integer;
    l5var2z: @insnltyp;
begin
    if not (0 in otherIns@.regsused) then begin
        l5var1z := 0;
    end else if not (0 in insnList@.regsused) then begin
        l5var1z := ord(commutes) + 1;
    end else begin
        l5var1z := 3;
        (*=z-*)(q) exit q;(*=z+*)
    end;
    case l5var1z of
    0:
100: begin
        prepLoad;
        saved := insnList;
        insnList := otherIns;
        curInsnTemplate := nextInsn;
        prepLoad;
        curInsnTemplate := insnTemp[XTA];
    end;
    1:
        if (nextInsn = insnTemp[SUB]) then begin
            nextInsn := insnTemp[RSUB];
            goto 22;
        end else
            goto 33;
   2:
22: begin
        saved := insnList;
        insnList := otherIns;
        otherIns := saved;
        goto 100;
    end;
    3:
33: begin
        prepLoad;
        addToInsnList(indexreg[15] + nextInsn);
        l5var2z := insnList;
        insnList := otherIns;
        P4606;
        saved := insnList;
        insnList := l5var2z;
    end;
    end; (* case *)
    insnList@.next@.mode := 0;
    saved@.next@.next := insnList@.next2;
    insnList@.next2 := saved@.next2;
    insnList@.regsused := insnList@.regsused + [0];
end; (* tryFlip *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genBoolAnd;
var
    l5var1z, l5var2z: boolean;
    l5var3z, l5var4z, l5var5z, l5var6z, l5var7z: integer;
    l5ins8z: @insnltyp;
    l5var9z: word;
begin
    if (arg1Const) then begin
        if (arg1Val.b) then
            insnList := otherIns;
    end else if (arg2Const) then begin
        if (not arg2Val.b) then
            insnList := otherIns;
    end else begin
        l5var1z := 16 in insnList@.regsused;
        l5var2z := 16 in otherIns@.regsused;
        l5var5z := int94z;
        int94z := int94z + 1;
        bool49z := false;
        l5var6z := ord(l5var1z) + macro;
        l5var7z := ord(l5var2z) + macro;
        if (insnList@.ilm = il3) then begin
            l5var3z := insnList@.ilf5.i;
        end else begin
            l5var3z := 0;
            prepLoad;
        end;
        if (otherIns@.ilm = il3) then begin
            l5var4z := otherIns@.ilf5.i;
        end else begin
            l5var4z := 0;
        end;
        l5var9z.m := (insnList@.regsused + otherIns@.regsused);
        if (l5var3z = (0)) then begin
            if (l5var4z = (0)) then begin
                addInsnAndOffset(l5var6z, l5var5z);
                l5ins8z := insnList;
                insnList := otherIns;
                prepLoad;
                addInsnAndOffset(l5var7z, l5var5z);
            end else begin
                if (l5var2z) then begin
                    addInsnAndOffset(l5var6z, l5var5z);
                    l5ins8z := insnList;
                    insnList := otherIns;
                    addInsnAndOffset(macro + 2,
                                     10000B * l5var5z + l5var4z);
                end else begin
                    addInsnAndOffset(l5var6z, l5var4z);
                    l5var5z := l5var4z;
                    l5ins8z := insnList;
                    insnList := otherIns;
                end
            end;
        end else begin
            if (l5var4z = (0)) then begin
                if (l5var1z) then begin
                    addInsnAndOffset(macro + 2,
                                     10000B * l5var5z + l5var3z);
                    l5ins8z := insnList;
                    insnList := otherIns;
                    prepLoad;
                    addInsnAndOffset(l5var7z, l5var5z);
                end else begin
                    l5ins8z := insnList;
                    insnList := otherIns;
                    prepLoad;
                    addInsnAndOffset(l5var7z, l5var3z);
                    l5var5z := l5var3z;
                end;
            end else begin
                if (l5var1z) then begin
                    if (l5var2z) then begin
                        addInsnAndOffset(macro + 2,
                                         10000B * l5var5z + l5var3z);
                        l5ins8z := insnList;
                        insnList := otherIns;
                        addInsnAndOffset(macro + 2,
                                         10000B * l5var5z + l5var4z);
                    end else begin
                        addInsnAndOffset(macro + 2,
                                         10000B * l5var4z + l5var3z);
                        l5ins8z := insnList;
                        insnList := otherIns;
                        l5var5z := l5var4z;
                    end
                end else begin
                    l5ins8z := insnList;
                    insnList := otherIns;
                    l5var5z := l5var3z;
                    if (l5var2z) then
                        addInsnAndOffset(macro + 2,
                                         10000B * l5var3z + l5var4z)
                    else
                        addInsnAndOffset(macro + 3,
                                         10000B * l5var3z + l5var4z);
                end
            end
        end;
        insnList@.regsused := l5var9z.m - [16];
        l5ins8z@.next@.next := insnList@.next2;
        insnList@.next2 := l5ins8z@.next2;
        insnList@.ilm := il3;
        insnList@.ilf5.i := l5var5z;
        bool49z := true;
        (*=z-*)exit;(*=z+*)
    end
end; (* genBoolAnd *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genGetElt;
var
    l5var1z, dimCnt, curDim, l5var4z, l5var5z, l5var6z,
        l5var7z, l5var8z: integer;
    insnCopy: insnltyp;
    copyPtr, l5ins21z: @insnltyp;
    l5var22z, l5var23z: word;
    l5var24z: boolean;
    l5var25z: boolean;
    l5var26z, l5var27z: tptr;
    l5ilm28z: ilmode;
    l5var29z: eptr;
    getEltInsns: array [1..10] of @insnltyp;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function myminel(l6arg1z: bitset): integer;
begin
    myminel := minel(l6arg1z);
end; (* myminel *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* genGetElt *)
    dimCnt := 0;
    l5var29z := exprToGen;
    while (l5var29z@.op = GETELT) do begin
        genFullExpr(l5var29z@.expr2);
        dimCnt := dimCnt + 1;
        getEltInsns[dimCnt] := insnList;
        l5var29z := l5var29z@.expr1;
    end;
    genFullExpr(l5var29z);
    l5ins21z := insnList;
    insnCopy := insnList@;
    copyPtr := ref(insnCopy);
    l5var22z.m := set147z;
    for curDim to dimCnt do
       l5var22z.m := l5var22z.m - getEltInsns[curDim]@.regsused;
    for curDim := dimCnt downto 1 do begin
        l5var26z := insnCopy.typ@.base;
        l5var27z := insnCopy.typ@.range;
        l5var25z := insnCopy.typ@.pck;
        l5var7z := l5var27z@.left;
        l5var8z := l5var26z@.size;
        if not l5var25z then
            insnCopy.ilf6 := insnCopy.ilf6 - l5var8z * l5var7z;
        insnList := getEltInsns[curDim];
        l5ilm28z := insnList@.ilm;
        if (l5ilm28z = ilCONST) then begin
            curVal := insnList@.ilf5;
            curVal.m := curVal.m +  intZero;
            if (curVal.i < l5var7z) or
               (l5var27z@.right < curVal.i) then
                error(29); (* errIndexOutOfBounds *)
            if (l5var25z) then begin
                l5var4z := curVal.i - l5var7z;
                l5var5z := insnCopy.typ@.perword;
                insnCopy.regsused := insnCopy.regsused + [0];
                insnCopy.ilf6 := l5var4z DIV l5var5z + insnCopy.ilf6;
                l5var6z := (l5var5z-1-l5var4z MOD l5var5z) *
                           insnCopy.typ@.pcksize;
                case insnCopy.st of
                st0: insnCopy.shift := l5var6z;
                st1: insnCopy.shift := insnCopy.shift + l5var6z +
                                           insnCopy.typ@.bits - 48;
                st2: error(errUsingVarAfterIndexingPackedArray);
                end; (* case *)
                insnCopy.width := insnCopy.typ@.pcksize;
                insnCopy.st := st1;
            end (* 6116 *) else begin
                insnCopy.ilf6 := curVal.i  * l5var26z@.size +
                                  insnCopy.ilf6;
            end
        end else begin (* 6123*)
            if (checkBounds) then begin
                l5var24z := typeCheck(l5var27z, insnList@.typ);
                if (rangeMismatch) then
                    genCheckBounds(l5var27z);
            end;
            if (l5var8z <> 1) then begin
                prepLoad;
                if (l5var27z@.base = integerType) then begin
                    l5var4z := KYTA+64;
                end else begin
                    l5var4z := KYTA+64-40;
                end;
                addToInsnList(insnCopy.typ@.perword);
                insnList@.next@.mode := 1;
                if (l5var7z >= 0) then
                    addToInsnList(l5var4z)
                else
                    addToInsnList(macro + mcMULTI);
           end;
           if (l5ilm28z = il3) or
              (l5ilm28z = il1) and
              (insnList@.st <> st0) then
               prepLoad;
           l5var23z.m := insnCopy.regsused + insnList@.regsused;
           if (not l5var25z) then begin
               if (insnCopy.ilf7 = 18) then begin
                    if (insnList@.ilm = il2) then begin
                        insnCopy.ilf7 := 15;
                    end else begin (* 6200 *)
                        insnCopy.ilf7 := 16;
                        curInsnTemplate := insnTemp[WTC];
                        prepLoad;
                        curInsnTemplate := insnTemp[XTA];
                    end; (* 6205 *)
                    insnCopy.next := insnList@.next;
                    insnCopy.next2 := insnList@.next2;
                end else begin (* 6211 *)
                    if (insnCopy.ilf7 >= 15) then begin
                        l5var1z :=  myminel(l5var22z.m);
                        if (0 >= l5var1z) then begin
                            l5var1z := myminel(set147z - insnCopy.regsused);
                            if (0 >= l5var1z) then
                                l5var1z := 9;
                        end;
                        saved := insnList;
                        insnList := copyPtr;
                        l5var23z.m := l5var23z.m + [l5var1z];
                        if (insnCopy.ilf7 = 15) then begin
                            addToInsnList(insnTemp[ATI] + l5var1z);
                        end else begin
                            addToInsnList(indexreg[l5var1z] + insnTemp[VTM]);
                        end;
                        insnCopy.ilf7 := l5var1z;
                        insnCopy.regsused := insnCopy.regsused + [l5var1z];
                        insnList := saved;
                    end else begin
                            l5var1z := insnCopy.ilf7;
                    end; (* 6251 *)
                    if (l5var1z IN insnList@.regsused) then begin
                         P4606;
                         insnList@.next@.next := insnCopy.next2;
                         insnCopy.next2 := insnList@.next2;
                         insnList := copyPtr;
                         addInsnAndOffset(macro+mcADDSTK2REG, l5var1z);
                    end else begin
                         if (insnList@.ilm = il2) then begin
                             addInsnAndOffset(macro+mcADDACC2REG, l5var1z);
                         end else begin
                             curInsnTemplate := insnTemp[WTC];
                             prepLoad;
                             curInsnTemplate := insnTemp[XTA];
                             addToInsnList(indexreg[l5var1z] + insnTemp[UTM]);
                         end;
                         insnCopy.next@.next := insnList@.next2;
                         insnCopy.next := insnList@.next;
                     end
                end; (* 6305 *)
           end else begin (* 6306 *)
                if (insnCopy.st = st0) then begin
                    prepLoad;
                    if (l5var7z <> 0) then begin
                        curVal.i := 0 - l5var7z;
                        if (not typeCheck(insnList@.typ, integerType)) then
                            curVal.m := curVal.m - intZero;
                        addToInsnList(KADD+I8 + getFCSToffset);
                        insnList@.next@.mode := 1;
                    end;
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
                    insnCopy.width := insnCopy.typ@.pcksize;
                    curVal.i := insnCopy.width;
                    if (curVal.i = 24) then
                        curVal.i := 7;
                    curVal := curVal;besm(ASN64-24);curVal:=;
                    addToInsnList(allocSymtab(  (* P/00C *)
                        helperNames[76] + curVal.m)+(KVTM+I11));
                    insnCopy.ilf7 := 16;
                    insnCopy.shift := 0;
                    saved@.next@.next := insnCopy.next2;
                    insnCopy.next2 := saved@.next2;
                end else begin
                    error(errUsingVarAfterIndexingPackedArray);
                end
            end; (* 6403 *)
            insnCopy.regsused := l5var23z.m;
        end;
        insnCopy.typ := l5var26z;
    end; (* 6406 *)
    insnList := l5ins21z;
    insnList@ := insnCopy;
end; (* genGetElt *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genEntry;
var
    l5exp1z, l5exp2z: eptr;
    l5idr3z, l5idr4z, l5idr5z, l5idr6z: irptr;
    l5bool7z, l5bool8z, l5bool9z, l5bool10z, l5bool11z: boolean;
    l5var12z, l5var13z, l5var14z: word;
    l5var15z: integer;
    l5var16z, l5var17z, l5var18z, l5var19z: word;
    l5inl20z: @insnltyp;
    l5op21z: operator; l5idc22z: idclass;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function allocGlobalObject(l6arg1z: irptr): integer;
begin
    if (l6arg1z@.pos = 0) then begin
        if (l6arg1z@.flags * [20, 21] <> []) then begin
            curVal := l6arg1z@.id;
            curVal.m := makeNameWithStars(true);
            l6arg1z@.pos := allocExtSymbol(extSymMask);
        end else begin
            l6arg1z@.pos := symTabPos;
            putToSymTab([]);
        end
    end;
    allocGlobalObject := l6arg1z@.pos;
end; (* allocGlobalObject *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure traceEntry(isEntry: boolean);
begin
    if not (debugEntry in optSflags.m) then
        exit;
    curVal := l5idr5z@.id;
    addToInsnList(KVTM+I10 + addCurValToFCST);
    if (isEntry) then
        addToInsnList(KVTM+I11 + lineCnt);
    addToInsnList(getHelperProc(ord(isEntry) * 22 + 57)); (* P/C(E|I) *)
end; (* traceEntry *)
%
begin (* genEntry *)
    l5exp1z := exprToGen@.expr1;
    l5idr5z := exprToGen@.id2;
    l5bool7z := (l5idr5z@.typ = NIL);
    l5bool9z := (l5idr5z@.list = NIL);
    if (l5bool7z) then
        l5var13z.i := 3 else l5var13z.i := 4;
    l5var12z.m := l5idr5z@.flags;
    l5bool10z := (21 in l5var12z.m);
    l5bool11z := (24 in l5var12z.m);
    if (l5bool9z) then begin
        l5var14z.i := F3307(l5idr5z);
        l5idr6z := l5idr5z@.argList;
    end else begin
        l5var13z.i := l5var13z.i + 2;
    end;
    new(insnList);
    insnList@.next2 := NIL;
    insnList@.next := NIL;
    insnList@.typ := l5idr5z@.typ;
    insnList@.regsused := (l5idr5z@.flags + [7:15]) * [0:8, 10:15];
    insnList@.ilm := il2;
    if (l5bool10z) then begin
        l5bool8z := not l5bool7z;
        if (checkFortran) then begin
            addToInsnList(getHelperProc(92)); (* "P/MF" *)
        end
    end else begin
        l5bool8z := true;
        if (not l5bool9z) and (l5exp1z <> NIL)
            or (l5bool9z) and (l5var14z.i >= 2) then begin
            addToInsnList(KUTM+SP + l5var13z.i);
        end;
    end;
    l5var14z.i := 0;
(loop)
    while l5exp1z <> NIL do begin (* 6574 *)
        l5exp2z := l5exp1z@.expr2;
        l5exp1z := l5exp1z@.expr1;
        l5op21z := l5exp2z@.op;
        l5var14z.i := l5var14z.i + 1;
        l5inl20z := insnList;
        if (l5op21z = PCALL) or (l5op21z = FCALL) then begin
            l5idr4z := l5exp2z@.id2;
            new(insnList);
            insnList@.next2 := NIL;
            insnList@.next := NIL;
            insnList@.regsused := [];
            set145z := set145z + l5idr4z@.flags;
            if (l5idr4z@.list <> NIL) then begin
                addToInsnList(l5idr4z@.offset + insnTemp[XTA] +
                              l5idr4z@.value);
                if (l5bool10z) then
                    addToInsnList(getHelperProc(19)); (* "P/EA" *)
            end else
(a)         begin (* 6636 *)
                if (l5idr4z@.value = 0) then begin
                    if (l5bool10z) and (21 in l5idr4z@.flags) then begin
                        addToInsnList(allocGlobalObject(l5idr4z) +
                                      (KVTM+I14));
                        addToInsnList(KITA+14);
                        exit a;
                    end else begin (* 6651 *)
                        l5var16z.i := 0;
                        formJump(l5var16z.i);
                        padToLeft;
                        l5idr4z@.value := moduleOffset;
                        l5idr3z := l5idr4z@.argList;
                        l5var15z := ord(l5idr4z@.typ <> NIL);
                        l5var17z.i := F3307(l5idr4z);
                        form3Insn(KVTM+I10+ 4+moduleOffset,
                                  KVTM+I9 + l5var15z,
                                  KVTM+I8 + 74001B);
                        formAndAlign(getHelperProc(62)); (* "P/BP" *)
                        l5var15z := l5var17z.i + 2 + l5var15z;
                        form1Insn(KXTA+SP + l5var15z);
                        if ((1) < l5var17z.i) then
                            form1Insn(KUTM+SP + l5var15z)
                        else
                            form1Insn(0);
                        form2Insn(
                            getHelperProc(63(*P/B6*)) + 6437777777300000C,
                            allocGlobalObject(l5idr4z) + KUJ);
                        if (l5idr3z <> NIL) then begin
                            repeat
                                l5idc22z := l5idr3z@.cl;
                                if (l5idc22z = ROUTINEID) and
                                   (l5idr3z@.typ <> NIL) then
                                    l5idc22z := ENUMID;
                                form2Insn(0, ord(l5idc22z));
                                l5idr3z := l5idr3z@.list;
                            until (l5idr4z = l5idr3z);
                        end; (* 6745 *)
                        storeObjWord([]);
                        P0715(0, l5var16z.i);
                    end
                end; (* 6752 *)
                addToInsnList(KVTM+I14 + l5idr4z@.value);
                if 21 in l5idr4z@.flags then
                    addToInsnList(KITA+14)
                else
                    addToInsnList(getHelperProc(64)); (* "P/PB" *)
            end; (* 6765 *)
            if (l5op21z = PCALL) then
                l5idc22z := ROUTINEID
            else
                l5idc22z := ENUMID;
        end else begin (* 6772 *)
            genFullExpr(l5exp2z);
            if (insnList@.ilm = il1) then
                l5idc22z := FORMALID
            else
                l5idc22z := VARID;
        end; (* 7001 *)
        if not (not l5bool9z or (l5idc22z <> FORMALID) or
               (l5idr6z@.cl <> VARID)) then
            l5idc22z := VARID;
(loop)      if (l5idc22z = FORMALID) or (l5bool11z) then begin
            setAddrTo(14);
            addToInsnList(KITA+14);
        end else if (l5idc22z = VARID) then begin
            if (insnList@.typ@.size <> 1) then begin
                l5idc22z := FORMALID;
                goto loop;
            end else begin
                prepLoad;
                (*=z-*)(q) exit q(*=z+*)
            end
        end; (* 7027 *)
        if not l5bool8z then
            addxToInsnList(macro + mcPUSH);
        l5bool8z := false;
        if (l5inl20z@.next <> NIL) then begin
            l5inl20z@.next@.next := insnList@.next2;
            insnList@.next2 := l5inl20z@.next2;
        end;
        insnList@.regsused := insnList@.regsused + l5inl20z@.regsused;
        if not l5bool9z then begin
            curVal.cl := l5idc22z;
            addToInsnList(KXTS+I8 + getFCSToffset);
        end;
        if l5bool9z and not l5bool11z then
            l5idr6z := l5idr6z@.list;
    end; (* while -> 7061 *)
    traceEntry(true);
    if l5bool10z then begin
        addToInsnList(KNTR+2);
        insnList@.next@.mode := 4;
    end;
    if l5bool9z then begin
        addToInsnList(allocGlobalObject(l5idr5z) + (KVJM+I13));
        if (20 in l5idr5z@.flags) then begin
            l5var17z.i := 1;
        end else begin
            l5var17z.i := l5idr5z@.offset div 4000000B;
        end (* 7102 *)
    end else begin (* 7103 *)
        l5var15z := 0;
        if (l5var14z.i = 0) then begin
            l5var17z.i := l5var13z.i + 1;
        end else begin
            l5var17z.i := -(2 * l5var14z.i + l5var13z.i);
            l5var15z := 1;
        end; (* 7115 *)
        addInsnAndOffset(macro+16 + l5var15z,
                         getValueOrAllocSymtab(l5var17z.i));
        addToInsnList(l5idr5z@.offset + insnTemp[UTC] + l5idr5z@.value);
        addToInsnList(macro+18);
        l5var17z.i := 1;
    end; (* 7132 *)
    insnList@.next@.mode := 2;
    if (curProcNesting <> l5var17z.i) then begin
        if not l5bool10z then begin
            if (l5var17z.i + 1 = curProcNesting) then begin
                addToInsnList(KMTJ+I7 + curProcNesting);
            end else begin
                l5var15z := frameRestore[curProcNesting][l5var17z.i];
                if (l5var15z = (0)) then begin
                    curVal.i := 6017T; (* P/ *)
                    l5var19z.i := curProcNesting + 16;
                    besm(ASN64-30);
                    l5var19z := ;
                    l5var18z.i := l5var17z.i + 16;
                    besm(ASN64-24);
                    l5var18z := ;
                    curVal.m := curVal.m + l5var19z.m + l5var18z.m;
                    l5var15z := allocExtSymbol(extSymMask);
                    frameRestore[curProcNesting][l5var17z.i] := l5var15z;
                end;
                addToInsnList(KVJM+I13 + l5var15z);
            end
        end
    end; (* 7176 *)
    if not l5bool9z or ([20, 21] * l5var12z.m <> []) then begin
        addToInsnList(KVTM+40074001B);
    end;
    set145z := (set145z + l5var12z.m) * [1:15];
    traceEntry(false);
    if l5bool10z then begin
        if (not checkFortran) then
            addToInsnList(KNTR+7)
        else
            addToInsnList(getHelperProc(93));    (* "P/FM" *)
        insnList@.next@.mode := 2;
    end else begin
        if not l5bool7z then
            addToInsnList(KXTA+SP + l5var13z.i - 1);
    end; (* 7226 *)
    if not l5bool7z then begin
        insnList@.typ := l5idr5z@.typ;
        insnList@.regsused := insnList@.regsused + [0];
        insnList@.ilm := il2;
        set146z := set146z - l5var12z.m;
    end
    (* 7237 *)
end; (* genEntry *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure startInsnList(l5arg1z: ilmode);
begin
    new(insnList);
    insnList@.next := NIL;
    insnList@.next2 := NIL;
    insnList@.typ := exprToGen@.typ;
    insnList@.regsused := [];
    insnList@.ilm := l5arg1z;
    if (l5arg1z = ilCONST) then begin
        insnList@.ilf5.i := exprToGen@.num1;
        insnList@.ilf7 := exprToGen@.num2;
    end else begin
        insnList@.st := st0;
        insnList@.ilf7 := 18;
        insnList@.ilf5.i := curFrameRegTemplate;
        insnList@.ilf6 := exprToGen@.num1;
    end
end; (* startInsnList *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genCopy;
var
    size: integer;
begin
    size := insnList@.typ@.size;
    if (size = 1) then begin
        saved := insnList;
        insnList := otherIns;
        prepLoad;
        genOneOp;
        insnList := saved;
        prepStore;
        genOneOp;
    end else begin
        prepMultiWord;
        genOneOp;
        size := size - 1;
        formAndAlign(KVTM+I13 + getValueOrAllocSymtab(-size));
        work := moduleOffset;
        form2Insn(KUTC+I14 + size, KXTA+I13);
        form3Insn(KUTC+I12 + size, KATX+I13,
                  KVLM+I13 + work);
        set145z := set145z + [12:14];
    end
end; (* genCopy *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genConstDiv;
    function PASDIV(r: real): word;
        external;
begin
    curVal := PASDIV(1/arg2Val.i);
    addToInsnList(KMUL+I8 + getFCSToffset);
end; (* genConstDiv *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure genComparison;
label
    7475, 7504, 7514, 7530;
var
    hasEq: boolean;
    l5set2z: bitset;
    mode, size: integer;
begin
    l3int3z := ord(curOP) - ord(NEOP);
    hasEq := odd(l3int3z);
    if (l3int3z = 6) then begin     (* IN *)
        if (arg1Const) then begin
            if (arg2Const) then begin
                insnList@.ilf5.b := (arg1Val.i IN arg2Val.m);
            end else begin
                l5set2z := [arg1Val.i];
                if (l5set2z = []) then begin
                    insnList@.ilf5.b := false;
                end else begin
                    insnList := otherIns;
                    prepLoad;
                    curVal.m := l5set2z;
                    addToInsnList(KAAX+I8 + getFCSToffset);
                    insnList@.ilf5.i := 0;
                    insnList@.ilm := il3;
                end
            end; (* 7412 *)
        end else begin (* 7413 *)
            saved := insnList;
            insnList := otherIns;
            otherIns := saved;
            nextInsn := 66;      (* P/IN *)
            genHelper;
            insnList@.ilm := il2;
        end
    end else begin (* 7423 *)
        if hasEq then
            l3int3z := l3int3z - 1;
        l2typ13z := insnList@.typ;
        curVarKind := l2typ13z@.k;
        size := l2typ13z@.size;
        if (l2typ13z = realType) then begin
            if (fuzzReals) then
                work := 0
            else
                work := 1;
        end else if (curVarKind = kindSet) then
            work := 2
        else if (curVarKind IN [kindScalar, kindRange]) then
            work := 3
        else begin
            work := 4;
            (*=z-*)(a) exit a(*=z+*)
        end;
        if (size <> 1) then begin
            prepMultiWord;
            addInsnAndOffset(KVTM+I11, 1 - size);
            addToInsnList(getHelperProc(89 + l3int3z)); (* P/EQ *)
            insnList@.ilm := il2;
            hasEq := not hasEq;
        end else (* 7471 *) if l3int3z = 0 then begin
            if work = 0 then begin
                nextInsn := 15;         (* P/CP *)
7475:           genHelper;
                insnList@.ilm := il2;
            end else begin (* 7501 *)
                nextInsn := insnTemp[AEX];
                tryFlip(true);
7504:           insnList@.ilm := il3;
                insnList@.ilf5.i := 0;
            end;
        end else begin (* 7510 *)
            case work of
            0: begin (*7511*)
                nextInsn := 16;         (* P/AB *)
                goto 7475;
            end;
            1: begin (*7513*)
                mode := 3;
7514:           nextInsn := insnTemp[SUB];
                tryFlip(false);
                insnList@.next@.mode := mode;
                if mode = 3 then begin
                    addToInsnList(KNTR+23B);
                    insnList@.next@.mode := 2;
                end;
                goto 7504;
            end;
            2: begin (*7527*)
                nextInsn := insnTemp[AAX];
7530:           prepLoad;
                addToInsnList(KAEX+ALLONES);
                tryFlip(true);
                goto 7504;
            end;
            3: begin (*7536*)
                mode := 1;
                goto 7514;
            end;
            4: begin (*7540*)
                nextInsn := insnTemp[ARX];
                goto 7530;
            end;
            end; (* case *)
        end; (* 7554 *)
        insnList@.regsused := insnList@.regsused - [16];
        if (hasEq)
            then negateCond;
    end; (* 7562 *)
    (* 7562 *)
end; (* genComparison *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* genFullExpr *);
    if exprToGen = NIL then
        exit;
7567:
    curOP := exprToGen@.op;
    if (curOP < GETELT) then begin
        genFullExpr(exprToGen@.expr2);
        otherIns := insnList;
        if (curOP = ASSIGNOP) then
            l3bool13z := false;
        genFullExpr(exprToGen@.expr1);
        if (curOP = ASSIGNOP) then
            l3bool13z := true;
        if (insnList@.ilm = ilCONST) then begin
            arg1Const := true;
            arg1Val := insnList@.ilf5;
        end else
            arg1Const := false;
        if (otherIns@.ilm = ilCONST) then begin
            arg2Const := true;
            arg2Val := otherIns@.ilf5;
        end else
            arg2Const := false;
        if (curOP IN [NEOP, EQOP, LTOP, GEOP, GTOP, LEOP, INOP]) then begin
            genComparison;
        end else begin (* 7625 *)
            if arg1Const and arg2Const then begin
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
                end; (* case 7750 *)
                insnList@.ilf5 := arg1Val;
            end else begin (*7752*)
                l3int3z := opToMode[curOP];
                flags := opFlags[curOP];
                nextInsn := opToInsn[curOP];
                case flags of
                opfCOMM:
7760:               tryFlip(curOP in [MUL, PLUSOP, SETAND, INTPLUS]);
                opfHELP:
                    genHelper;
                opfASSN: begin
                    genCopy;
                    exit
                end;
                opfAND: begin
                    genBoolAnd;
                    exit
                end;
                opfOR: begin
                    negateCond;
                    saved := insnList;
                    insnList := otherIns;
                    negateCond;
                    otherIns := insnList;
                    insnList := saved;
                    genBoolAnd;
                    negateCond;
                    exit
                end;
                opfMOD:
                    if (arg2Const) then begin
                        prepLoad;
                        if card(arg2Val.m) = 4 then begin
                            curVal.m := [0,1,3,minel(arg2Val.m-intZero)+1..47];
                            addToInsnList(KAAX+I8 +getFCSToffset);
                            l3int3z := 0;
                        end else begin (* 10016 *)
                            addToInsnList(macro + mcPUSH);
                            genConstDiv;
                            insnList@.next@.mode := 1;
                            curVal.m := arg2Val.m - [1, 3];
                            addToInsnList(KMUL+I8 + getFCSToffset);
                            addToInsnList(KYTA+64);
                            addToInsnList(KRSUB+SP);
                            l3int3z := 1;
                        end (* 10036 *)
                    end else begin (* 10037 *)
                        genHelper;
                    end;
                opfDIV: begin
                    if arg2Const then begin
                        prepLoad;
                        genConstDiv;
                        l3int3z := 1;
                    end else
                        genHelper;
                end;
                opfMULMSK: begin
                    if (arg1Const) then begin
                        insnList@.ilf5.m := arg1Val.m MOD [1, 3];
                    end else begin
                        if (arg2Const) then begin
                            otherIns@.ilf5.m := arg2Val.m MOD [1, 3];
                        end else (q) begin
                            prepLoad;
                            addToInsnList(KAEX+MULTMASK);
                            (*=z-*)exit q(*=z+*)
                        end
                    end;
                    tryFlip(true);
                    insnList@.next@.mode := 1;
                    if (fixMult) then
                        addToInsnList(macro + mcMULTI)
                    else
                        addToInsnList(KYTA+64);
                end;
                opfINV: begin
10075:              saved := insnList;
                    insnList := otherIns;
                    otherIns := saved;
                    prepLoad;
                    addToInsnList(KAEX+ALLONES);
                    goto 7760
                end
                end; (* case 10122 *)
10122:          insnList@.next@.mode := l3int3z;
            end
        end
    end else begin (* 10125 *)
        if (FILEPTR >= curOP) then begin
            if (curOP = GETVAR) then begin
                new(insnList);
                curIdRec := exprToGen@.id1;
                with insnList@ do begin
                    next := NIL;
                    next2 := NIL;
                    regsused := [];
                    ilm := il1;
                    ilf5.i := curIdRec@.offset;
                    ilf6 := curIdRec@.high.i;
                    st := st0;
                    ilf7 := 18;
                end;
                if (curIdRec@.cl = FORMALID) then begin
                    genDeref;
                end else if (curIdRec@.cl = ROUTINEID) then begin
                    insnList@.ilf6 := 3;
                    insnList@.ilf5.i := (insnList@.ilf5.i + frameRegTemplate);
                end else if (insnList@.ilf6 >= 74000B) then begin
                    addToInsnList(insnTemp[UTC] + insnList@.ilf6);
                    insnList@.ilf6 := 0;
                    insnList@.ilf7 := 17;
                    insnList@.ilf5.i := 0;
                end
            end else (* 10171 *)
            if (curOP = GETFIELD) then begin
                genFullExpr(exprToGen@.expr1);
                curIdRec := exprToGen@.id2;
                with insnList@ do begin
                    ilf6 := ilf6 + curIdRec@.offset;
                    if (curIdRec@.pckfield) then begin
                        case st of
                        st0:
                            shift := curIdRec@.shift;
                        st1: begin
                            shift := shift + curIdRec@.shift;
                            if not (S6 IN optSflags.m) then
                                shift := shift +
                                           curIdRec@.uptype@.bits - 48;
                        end;
                        st2:
                            if (not l3bool13z) then
                                error(errUsingVarAfterIndexingPackedArray)
                            else begin
                                P5155;
                                insnList@.shift := curIdRec@.shift;
                            end
                        end; (* 10235*)
                        insnList@.width := curIdRec@.width;
                        insnList@.st := st1;
                        insnList@.regsused := insnList@.regsused + [0];
                    end
                end;
            end else (* 10244 *)
            if (curOP = GETELT) then
                genGetElt
            else
            if (curOP = DEREF) or (curOP = FILEPTR) then begin
                genFullExpr(exprToGen@.expr1);
                genDeref;
            end else
            if (curOP = op36) then begin
                startInsnList(il1);
            end else
            if (curOP = op37) then begin
                startInsnList(il1);
                genDeref;
            end else
            if (curOP = GETENUM) then
                startInsnList(ilCONST)
            (*=z-*)else ;(*=z+*)
        end else (* 10272 *)
        if (curOP = ALNUM) then
            genEntry
        else if (curOP IN [BOUNDS..RNEGOP]) then begin
            genFullExpr(exprToGen@.expr1);
            if (insnList@.ilm = ilCONST) then begin
                arg1Val := insnList@.ilf5;
                case curOP of
                BOUNDS: begin
                    arg2Val.m := [0,1,3] + arg1Val.m;
                    with exprToGen@.typ2@ do begin
                        if (arg2Val.i < left) or
                           (right < arg2Val.i) then
                            error(errNeedOtherTypesOfOperands)
                    end
                end;
                TOREAL: arg1Val.r := arg1Val.i;
                NOTOP:  arg1Val.b := not arg1Val.b;
                RNEGOP: arg1Val.r := -arg1Val.r;
                INEGOP: arg1Val.i := -arg1Val.i;
                end; (* case 10345 *)
                insnList@.ilf5 := arg1Val;
            end else (* 10347 *)
            if (curOP = NOTOP) then begin
                negateCond;
            end else begin
                prepLoad;
                if (curOP = BOUNDS) then begin
                    if (checkBounds) then
                        genCheckBounds(exprToGen@.typ2);
                end else if (curOP = TOREAL) then begin
                    addToInsnList(insnTemp[AVX]);
                    l3int3z := 3;
                    goto 10122;
                end else begin
                    addToInsnList(KAVX+MINUS1);
                    if (curOP = RNEGOP) then
                        l3int3z := 3
                    else
                        l3int3z := 1;
                    goto 10122;
                end
            end
        end else (* 10376 *)
        if (curOP = STANDPROC) then begin
            genFullExpr(exprToGen@.expr1);
            work := exprToGen@.num2;
            if (100 < work) then begin
                prepLoad;
                addToInsnList(getHelperProc(work - 100));
            end else begin
                if (insnList@.ilm = ilCONST) then begin
                    arg1Const := true;
                    arg1Val := insnList@.ilf5;
                end else
                    arg1Const := false;
                arg2Const := (insnList@.typ = realType);
                if (arg1Const) then begin
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
                    end; (* 10546 *)
                    insnList@.ilf5 := arg1Val;
                end else (* 10550 *)
                if (work >= fnEOF) and (fnEOLN >= work) then begin
                    if (work = fnREF) then begin
                        setAddrTo(14);
                        addToInsnList(KITA+14);
                    end else begin
                        setAddrTo(12);
                        addToInsnList(getHelperProc(work - 6));
                    end;
                    with insnList@ do begin
                        ilm := il2;
                        regsused := regsused + [0];
                    end
                end else begin
                    prepLoad;
                    if (work = fnTRUNC) then begin
                        l3int3z := 2;
                        addToInsnList(getHelperProc(58)); (*"P/TR"*)
                        goto 10122;
                    end;
                    if (work IN [fnSQRT:fnEXP,
                                 fnODD:fnSUCC, fnCARD, fnPTR]) then begin
                        l3int3z := 0;
                    end else if (work IN [fnABS, fnSQR]) then
                        l3int3z := 3
                    else begin
                        l3int3z := 1;
                        (*=z-*)(q) exit q(*=z+*)
                    end;
                    addToInsnList(funcInsn[work]);
                    goto 10122;
                end
            end
        end else begin (* 10621 *)
            if (curOP = NOOP) then begin
                curVal := exprToGen@.val;
                if (curVal.i IN set146z) then begin
                    new(insnList);
                    with insnList@ do begin
                        typ := exprToGen@.expr2@.typ;
                        next := NIL;
                        next2 := ;
                        regsused := [];
                        ilm := il1;
                        ilf7 := 18;
                        ilf5.i := indexreg[curVal.i];
                        ilf6 := 0;
                        st := st0;
                    end
                end else begin
                    curVal.i := 14;
                    exprToGen@.val := curVal;
                    exprToGen := exprToGen@.expr2;
                    goto 7567;
                end;
                exit
            end else (q) begin
                error(220);
                (*=z-*)exit q(*=z+*)
            end
        end;
    end; (* 10654 *)
    insnList@.typ := exprToGen@.typ;
    (* 10656 *)
end; (* genFullExpr *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure formFileInit;
var l4exf1z: @extfilerec;
    l4var2z: tptr;
    l4var3z: irptr;
    l4int4z, l4int5z: integer;
begin
    if (S5 IN optSflags.m) then begin
        formAndAlign(KUJ+I13);
        exit
    end;
    form2Insn(KITS+13, KATX+SP);
    while (curExpr <> NIL) do begin
        l4exf1z := ptr(ord(curExpr@.typ));
        l4var3z := curExpr@.id2;
        l4int4z := l4var3z@.value;
        l4var2z := l4var3z@.typ@.base;
        l4int5z := l4var3z@.typ@.elsize;
        if (l4int4z < 74000B) then begin
            form1Insn(getValueOrAllocSymtab(l4int4z) +
                      insnTemp[UTC] + I7);
            l4int4z := 0;
        end;
        form3Insn(KVTM+I12 + l4int4z, KVTM+I10 + fileBufSize,
                  KVTM+I9 + l4int5z);
        form1Insn(KVTM+I11 + l4var2z@.size);
        if (l4exf1z = NIL) then begin
            form1Insn(insnTemp[XTA]);
        end else begin
            curVal.i := l4exf1z@.location;
            if (curVal.i = 512) then
                curVal.i := l4exf1z@.offset;
            form1Insn(KXTA+I8 + getFCSToffset);
        end;
        formAndAlign(getHelperProc(69)); (*"P/CO"*)
        curVal := l4var3z@.id;
        form2Insn(KXTA+I8+getFCSToffset, KATX+I12+26);
        if (l4int5z <> 0) and
           typeCheck(l4var2z, integerType) then
            form2Insn(KXTA+ZERO, KATX+I12+25);
        curExpr := curExpr@.expr1;
    end;
    form1Insn(getHelperProc(70)(*"P/IT"*) + (-I13-100000B));
    padToLeft;
end; (* formFileInit *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* formOperator *)
    l3bool13z := true;
    if (errors and (l3arg1z <> SETREG)) or (curExpr = NIL) then
        exit;
    if not (l3arg1z IN [gen3, gen6, gen9, gen14, gen16]) then
        genFullExpr(curExpr);
    case l3arg1z of
    gen7: genOneOp;
    SETREG: begin
        with insnList@ do begin
            l3int3z := insnCount;
            new(l3var5z);
            l3var5z@.expr1 := expr63z;
            expr63z := l3var5z;
            l3var5z@.op := NOOP;
            case st of
            st0: begin
                if (l3int3z = 0) then begin
                    l3int2z := 14;
                end else begin
                    l3var10z.m := set148z * set147z;
                    if (l3var10z.m <> []) then begin
                        l3int2z := minel(l3var10z.m);
                    end else begin
                        l3int2z := 14;
                    end;
                    if (l3int3z <> 1) then begin
                        setAddrTo(l3int2z);
                        addToInsnList(KITA + l3int2z);
                        P5117(op37);
                    end else if (l3int2z <> 14) then begin
                        setAddrTo(l3int2z);
                        genOneOp;
                    end;
                    l3var11z.m := [l3int2z] - [14];
                    set145z := set145z - l3var11z.m;
                    set147z := set147z - l3var11z.m;
                    set146z := set146z + l3var11z.m;
                end;
                curVal.i := l3int2z;
                l3var5z@.val := curVal;
            end;
            st1: begin
                curVal.i := 14;
                l3var5z@.val := curVal;
            end;
            st2:
                error(errVarTooComplex);
            end; (* case *)
        end; (* with *)
        l3var5z@.expr2 := curExpr;
    end; (* SETREG *)
    gen0: begin
        prepLoad;
        if (insnCount > 1) then
            P5117(op36)
    end;
    STORE: begin
        prepStore;
        genOneOp
    end;
    gen3: begin
        curInsnTemplate := curVal.i;
        formOperator(LOAD);
        curInsnTemplate := insnTemp[XTA];
    end;
    gen5: begin
        if (insnList@.st <> st0) then
            error(errVarTooComplex);
        setAddrTo(9);
        genOneOp;
    end;
    gen6: begin
        l3int1z := curVal.i;
        genFullExpr(curExpr);
        prepLoad;
        if (9 IN insnList@.regsused) then
            error(errVarTooComplex);
        genOneOp;
        form1Insn(KATX+I9 + l3int1z);
    end;
    gen8: begin
        setAddrTo(12);
        genOneOp
    end;
    gen9: begin
        curVal.m := curVal.m + intZero;
        form1Insn(KXTA+I8 + getFCSToffset);
    end;
    gen10: begin
        prepLoad;
        addxToInsnList(macro + mcPUSH);
        genOneOp;
    end;
    gen11, gen12: begin
        setAddrTo(11);
        if (l3arg1z = gen12) then
            addxToInsnList(macro + mcPUSH);
        genOneOp;
        set145z := set145z + [12];
    end;
    FILEACCESS: begin
        setAddrTo(12);
        genOneOp;
        formAndAlign(jumpTarget);
    end;
    gen14:
        formFileInit;
    LOAD: begin
        prepLoad;
        genOneOp
    end;
    gen15:
        with insnList@ do begin
            l3bool9z := jumpTarget = 0;
            l3int3z := jumpTarget;
            if (ilm = ilCONST) then begin
                if (ilf5.b) then begin
                    jumpTarget := 0;
                end else begin
                    if (l3bool9z) then begin
                        formJump(jumpTarget);
                    end else begin
                        form1Insn(insnTemp[UJ] + jumpTarget);
                    end
                end
            end else begin
                l3var8z.b := (16 in insnList@.regsused);
                if (insnList@.ilm = il3) and
                   (insnList@.ilf5.i <> 0) then begin
                    genOneOp;
                    if (l3var8z.b) then begin
                        if (l3bool9z) then
                            formJump(l3int3z)
                        else
                            form1Insn(insnTemp[UJ] + l3int3z);
                        P0715(0, jumpTarget);
                        jumpTarget := l3int3z;
                    end else begin
                        if (not l3bool9z) then begin
                            if (not putLeft) then
                                padToLeft;
                            P0715(l3int3z, jumpTarget);
                        end
                    end;
                end else begin
                    if (insnList@.ilm = il1) then begin
                        bool49z := false;
                        prepLoad;
                        bool49z := true;
                    end;
                    genOneOp;
                    if (l3var8z.b) then
                        nextInsn := insnTemp[U1A]
                    else
                        nextInsn := insnTemp[UZA];
                    if (l3bool9z) then begin
                        jumpType := nextInsn;
                        formJump(l3int3z);
                        jumpType := insnTemp[UJ];
                        jumpTarget := l3int3z;
                    end else begin
                        form1Insn(nextInsn + l3int3z);
                    end
                end
            end
        end; (* gen15 *)
    gen16: begin
        l3var5z := curExpr;
        curExpr := curExpr@.expr1;
        formOperator(gen11);
        genFullExpr(l3var5z@.expr2);
        if (11 IN insnList@.regsused) then
            error(44); (* errIncorrectUsageOfStandProcOrFunc *)
        setAddrTo(12);
        genOneOp;
        arg1Type := l3var5z@.expr2@.typ;
        with arg1Type@.range@ do
            l3int3z := right - left + 1;
        form2Insn((KVTM+I14) + l3int3z,
                  (KVTM+I10+64) - arg1Type@.pcksize);
        l3int3z := ord(l3var5z@.typ);
        l3int1z := arg1Type@.perword;
        if (l3int3z = 72) then          (* P/KC *)
            l3int1z := 1 - l3int1z;
        form1Insn(getValueOrAllocSymtab(l3int1z) + (KVTM+I9));
        if typeCheck(curExpr@.typ, integerType) then begin
            l3int1z := KXTA+ZERO;
        end else begin
            l3int1z := insnTemp[XTA];
        end;
        form1Insn(l3int1z);
        formAndAlign(getHelperProc(l3int3z));
   end;
   LITINSN: begin
        with insnList@ do begin
            if (ilm <> ilCONST) then
                error(errNoConstant);
            if (insnList@.typ@.size <> 1) then
                error(errConstOfOtherTypeNeeded);
            curVal := insnList@.ilf5;
        end
    end;
    end; (* case *)
end; (* formOperator *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseTypeRef(var newtype: tptr; skipTarget: setofsys);
label
    12247, 12366, 12476, 12760, 13020;
type
    pair = record
            first, second: integer
        end;
    pair7 = array [1..7] of pair;
    caserec = record
            size, count: integer;
            pairs: pair7;
        end;
var
    isPacked: boolean;
    cond: boolean;
    cases: caserec;
    leftBound, rightBound: word;
    numBits, l3int22z, span: integer;
    curEnum, curField: irptr;
    l3typ26z, nestedType, tempType, curType: tptr;
    l3unu30z: word;
    l3idr31z: irptr;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure definePtrType(toType: tptr);
begin
    new(curType = 4);
    curType@ := [1, 15, kindPtr, toType];
    new(curEnum = 5);
    curEnum@ := [curIdent, lineCnt, typelist, curType, TYPEID];
    typelist := curEnum;
end; (* definePtrType *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseRecordDecl(rectype: tptr; isOuterDecl: boolean);
var
    l4typ1z, selType, l4var3z, l4var4z, l4var5z: tptr;
    l4var6z: irptr;
    l4var7z, l4var8z: word;
    l4var9z: integer;
    cases1, cases2: caserec;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure addFieldToHash;
begin
    curEnum@ := [curIdent, , typeHashTabBase[bucket], ,
                    FIELDID, NIL, curType, isPacked];
    typeHashTabBase[bucket] := curEnum;
end; (* addFieldToHash *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure packFields;
label
    11523, 11622;
var
    l5var1z, pairIdx, l5var3z, l5var4z, l5var5z: integer;
    l5var6z: @pair;
begin
    parseTypeRef(selType, skipTarget + [CASESY]);
    if (curType@.ptr2 = NIL) then begin
        curType@.ptr2 := curField;
    end else begin
        l3idr31z@.list := curField;
    end;
    cond := isFileType(selType);
    if (not isOuterDecl) and cond then
        error(errTypeMustNotBeFile);
    curType@.flag := cond or curType@.flag;
    l3idr31z := curEnum;
    repeat
        curField@.typ := selType;
(q)     if (isPacked) then begin
            l5var1z := selType@.bits;
            curField@.width := l5var1z;
            if (l5var1z <> 48) then begin
                for pairIdx to cases.count do
11523:          begin
                    l5var6z := ref(cases.pairs[pairIdx]);
                    if (l5var6z@.first >= l5var1z) then begin
                        curField@.shift := 48 - l5var6z@.first;
                        curField@.offset := l5var6z@.second;
                        if not (S6 IN optSflags.m) then
                            curField@.shift := 48 - curField@.width -
                                                  curField@.shift;
                        l5var6z@.first := l5var6z@.first - l5var1z;
                        if l5var6z@.first = 0 then begin
                            cases.pairs[pairIdx] :=
                                cases.pairs[cases.count];
                            cases.count := cases.count - 1;
                        end; (* 11562 *)
                        goto 11622;
                    end
                end; (* 11564 *)
                if (cases.count <> 7) then begin
                    cases.count := cases.count + 1;
                    pairIdx := cases.count;
                end else begin
                    l5var3z := 48;
                    for l5var4z to 7 do begin
                        l5var5z := cases.pairs[l5var4z].first;
                        if (l5var5z < l5var3z) then begin
                            l5var3z := l5var5z;
                            pairIdx := l5var4z;
                        end
                    end; (* for *)
                end; (* 11606 *)
                cases.pairs[pairIdx] := [48, cases.size];
                cases.size := cases.size + 1;
                goto 11523;
            end
        end; (* 11615 *)
        curField@.pckfield := false;
        curField@.offset := cases.size;
        cases.size := cases.size + selType@.size;
11622:
        if (PASINFOR.listMode = 3) then begin
            write(' ':16);
            if (curField@.pckfield) then
                write('PACKED');
            write(' FIELD ');
            printTextWord(curField@.id);
            write('.OFFSET=', curField@.offset:5 oct, 'B');
            if (curField@.pckfield) then begin
                write('.<<=SHIFT=', curField@.shift:2,
                      '. WIDTH=', curField@.width:2, ' BITS');
            end else begin
                write('.WORDS=', selType@.size:0);
            end;
            writeLN;
        end;
        cond := (curField = curEnum);
        curField := curField@.list;
    until cond;
    (* 11674 *)
end; (* packFields *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* parseRecordDecl *)
    int93z := 3;
    inSymbol;
    (*11702*)
    while (SY = IDENT) do begin
        l4var6z := NIL;
        repeat
            if (SY <> IDENT) then begin
                error(errNoIdent);
            end else begin
                if (hashTravPtr <> NIL) then
                    error(errIdentAlreadyDefined);
                new(curEnum = 10);
                addFieldToHash;
                if (l4var6z = NIL) then begin
                    curField := curEnum;
                end else begin
                    l4var6z@.list := curEnum;
                end;
                l4var6z := curEnum;
                int93z := 3;
                inSymbol;
            end;
            cond := (SY <> COMMA);
            if (not cond) then begin
                int93z := 3;
                inSymbol;
            end
        until cond;
        checkSymAndRead(COLON);
        packFields;
        if (SY = SEMICOLON) then begin
            int93z := 3;
            inSymbol;
        end
    end; (*11752*)
    if (SY = CASESY) then begin
        int93z := 3;
        inSymbol;
        selType := integerType;
(identif)
        if (SY <> IDENT) then begin
            error(3);
            skip(skipTarget + [OFSY]);
        end else begin (* 11766 *)
            l4var8z := curIdent;
            l4var9z := bucket;
            curEnum := hashTravPtr;
            inSymbol;
            if (SY = COLON) then begin
                if (curEnum <> NIL) then
                    error(errIdentAlreadyDefined);
                new(curEnum = 10);
                curIdent := l4var8z;
                bucket := l4var9z;
                addFieldToHash;
                inSymbol;
                curField := curEnum;
                packFields;
            end else begin
                curEnum := symHashTabBase[l4var9z];
                while (curEnum <> NIL) do begin
                    if (curEnum@.id <> l4var8z) then begin
                        curEnum := curEnum@.next;
                    end else begin
                        if (curEnum@.cl <> TYPEID) then begin
                            error(errNotAType);
                            selType := integerType;
                        end else begin
                            selType := curEnum@.typ;
                        end;
                        exit identif;
                    end;
                end;
                error(errNotDefined)
            end;
        end; (* 12035 *)
        if (selType@.k = kindRange) then
            selType := selType@.base;
        checkSymAndRead(OFSY);
        cases1 := cases;
        cases2 := cases;
        l4typ1z := NIL;
        repeat
            l4var3z := NIL;
            repeat
                parseLiteral(l4var4z, l4var7z, false);
                if (l4var4z = NIL) then
                    error(errNoConstant)
                else if (not typeCheck(l4var4z, selType)) then
                    error(errConstOfOtherTypeNeeded);
                new(l4var5z = 7);
                l4var5z@ := [cases.size, 48, kindCases,
                                    l4var7z, NIL, NIL, NIL];
                if (l4var3z = NIL) then begin
                    tempType := l4var5z;
                end else begin
                    l4var3z@.r6 := l4var5z;
                end;
                l4var3z := l4var5z;
                inSymbol;
                cond := (SY <> COMMA);
                if (not cond) then
                    inSymbol;
            until cond;
            if (l4typ1z = NIL) then begin
                if (curType@.base = NIL) then begin
                    curType@.base := tempType;
                end else begin
                    rectype@.first := tempType;
                end
            end else begin
                l4typ1z@.next := tempType;
            end;
            l4typ1z := tempType;
            checkSymAndRead(COLON);
            if (SY <> LPAREN) then
                requiredSymErr(LPAREN);
            parseRecordDecl(tempType, false);
            if (cases2.size < cases.size) or
               isPacked and (cases.size = 1) and (cases2.size = 1) and
                (cases.count = 1) and (cases2.count = 1) and
                (cases.pairs[1].first < cases2.pairs[1].first) then begin
                cases2 := cases;
            end; (* 12201 *)
            cases := cases1;
            checkSymAndRead(RPAREN);
            cond := SY <> SEMICOLON;
            if (not cond) then
                inSymbol;
            if (SY = ENDSY) then
                cond := true;
        until cond;
        cases := cases2;
    end; (* 12232 *)
    rectype@.size := cases.size;
    if isPacked and (cases.size = 1) and (cases.count = 1) then begin
        rectype@.bits := 48 - cases.pairs[1].first;
    end
    (* 12242 *)
end; (* parseRecordDecl*)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* parseTypeRef *)
    isPacked := false;
12247:
    if (SY = LPAREN) then begin
        span := 0;
        int93z := 0;
        inSymbol;
        curField := NIL;
        new(curType = 6);
        while (SY = IDENT) do begin
            if (isDefined) then
                error(errIdentAlreadyDefined);
            new(curEnum = 7);
            curEnum@ := [curIdent, curFrameRegTemplate,
                            symHashTabBase[bucket], curType,
                            ENUMID, NIL, ptr(span)];
            symHashTabBase[bucket] := curEnum;
            span := span + 1;
            if (curField = NIL) then begin
                curType@.enums := curEnum;
            end else begin
                curField@.list := curEnum;
            end;
            curField := curEnum;
            inSymbol;
            if (SY = COMMA) then begin
                int93z := 0;
                inSymbol;
            end else begin
                if (SY <> RPAREN) then
                    requiredSymErr(RPAREN);
            end;
        end; (* 12324 *)
        checkSymAndRead(RPAREN);
        if (curField = NIL) then begin
            curType := booleanType;
            error(errNoIdent);
        end else begin
            curType@ := [1, nrOfBits(span - 1), kindScalar, ,
                          span, 0];
        end;
    end else (* 12344 *)
    if (SY = ARROW) then begin
        inSymbol;
        if (SY <> IDENT) then begin
            error(errNoIdent);
            curType := pointerType;
        end else begin
            if (hashTravPtr = NIL) then begin
                if (inTypeDef) then begin
                    if (knownInType(curEnum)) then begin
                        curType := curEnum@.typ;
                    end else begin
                        definePtrType(integerType);
                    end;
                end else begin
12366:              error(errNotAType);
                    curType := pointerType;
                end;
            end else begin
                if (hashTravPtr@.cl <> TYPEID) then begin
                    goto 12366
                end;
                new(curType = 4);
                with curType@ do begin
                    size := 1;
                    bits := 15;
                    k := kindPtr;
                    base := hashTravPtr@.typ;
                end
            end; (* 12405 *)
            inSymbol;
        end
    end else (* 12410 *)
    if (SY = IDENT) then begin
        if (hashTravPtr <> NIL) then begin
            if (hashTravPtr@.cl = TYPEID) then begin
                curType := hashTravPtr@.typ;
            end else begin
                goto 12760;
            end
        end else begin
            if (inTypeDef) then begin
                if (knownInType(curEnum)) then begin
                    curType := curEnum@.typ;
                    curType@.base := booleanType;
                end else begin
                    definePtrType(booleanType);
                end;
            end else begin
                error(errNotAType);
                curType := integerType;
            end;
        end;
        inSymbol;
    end else begin (* 12440 *)
        if (SY = PACKEDSY) then begin
            isPacked := true;
            inSymbol;
            goto 12247;
        end;
        if (SY = RECORDSY) then begin (* 12446 *)
            new(curType = 7);
            typ121z := curType;
            with curType@ do begin
                size := 0;
                bits := 48;
                k := kindRecord;
                ptr1 := NIL;
                first := NIL;
                flag := false;
                pckrec := isPacked;
            end;
            cases.size := 0;
            cases.count := 0;
            parseRecordDecl(curType, true);
            checkSymAndRead(ENDSY);
        end else (* 12467 *)
        if (SY = ARRAYSY) then begin
            inSymbol;
            if (SY = LBRACK) then
                inSymbol;
            tempType := NIL;
12476:      parseTypeRef(nestedType, skipTarget + [OFSY]);
            curVarKind := nestedType@.k;
            if (curVarKind <> kindRange) then begin
                if (curVarKind = kindScalar) and
                   (nestedType <> integerType) then begin
                    span := nestedType@.numen;
                end else begin
                    error(8); (* errNotAnIndexType *)
                    nestedType := integerType;
                    span := 10;
                end;
                defineRange(nestedType, 0, span - 1);
            end; (* 12524 *)
            new(l3typ26z, kindArray);
            with l3typ26z@ do begin
                size := ord(tempType);
                bits := 48;
                k := kindArray;
                range := nestedType;
            end;
            if (tempType = NIL) then
                curType := l3typ26z
            else
                tempType@.base := l3typ26z;
            tempType := l3typ26z;
            if (SY = COMMA) then begin
                inSymbol;
                goto 12476;
            end;
            if (SY = RBRACK) then
                inSymbol;
            checkSymAndRead(OFSY);
            parseTypeRef(nestedType, skipTarget);
            l3typ26z@.base := nestedType;
            if isFileType(nestedType) then
                error(errTypeMustNotBeFile);
            repeat with l3typ26z@, ptr2@ do begin
                span := high.i - low + 1;
                tempType := ptr(size);
                l3int22z := base@.bits;
                if (24 < l3int22z) then
                    isPacked := false;
                bits := 48;
                if (isPacked) then begin
                    l3int22z := 48 DIV l3int22z;
                    if (l3int22z = 9) then begin
                        l3int22z := 8;
                    end else if (l3int22z = 5) then begin
                        l3int22z := 4
                    end (*=z-*)else(*=z+*) ;
                    perword := l3int22z;
                    pcksize := 48 DIV l3int22z;
                    l3int22z := span * pcksize;
                    if l3int22z mod 48 = 0 then
                        numBits := 0
                    else
                        numBits := 1;
                    size := l3int22z div 48 + numBits;
                    if (size = 1) then
                        bits := l3int22z;
                end else begin (* 12633 *)
                    size := span * base@.size;
                    curVal.i := base@.size;
                    curVal.m := curVal.m * [7:47] + [0];
                    if (range@.base <> integerType) then
                        curVal.m := curVal.m + [1, 3];
                    l3typ26z@.perword := KMUL+ I8 + getFCSToffset;
                end; (* 12652 *)
                l3typ26z@.pck := isPacked;
                isPacked := false;
                cond := (curType = l3typ26z);
                l3typ26z := tempType;
            end until cond;
        end else (* 12663 *)
        if (SY = FILESY) then begin
            inSymbol;
            checkSymAndRead(OFSY);
            parseTypeRef(nestedType, skipTarget);
            if (isFileType(nestedType)) then
                error(errTypeMustNotBeFile);
            if (isPacked) then begin
                l3int22z := nestedType@.bits;
                if (24 < l3int22z) then
                    isPacked := false;
            end;
            new(curType, kindFile);
            if (not isPacked) then
                l3int22z := 0;
            with curType@ do begin
                size := 30;
                bits := 48;
                k := kindFile;
                base := nestedType;
                elsize := l3int22z;
            end
        end else (* 12721 *)
        if (SY = SETSY) then begin
            inSymbol;
            checkSymAndRead(OFSY);
            parseTypeRef(nestedType, skipTarget);
            with nestedType@ do begin
                if (k = kindRange) and
                   (left >= 0) and
                   (47 >= right) then
                    numBits := right + 1
                else if (k = kindScalar) and
                        (48 >= numen) then
                    numBits := numen
                else (q) begin
                    numBits := 48;
                    error(63); (* errBadBaseTypeForSet *)
                    (*=z-*)exit q(*=z+*)
                end
            end;
            new(curType, kindSet);
            with curType@ do begin
                size := 1;
                bits := numBits;
                k := kindSet;
                base := nestedType;
            end
        end else (q) begin
12760:      ;
            parseLiteral(tempType, leftBound, true);
            if (tempType <> NIL) then begin
                inSymbol;
                if (SY <> COLON) then begin
                    requiredSymErr(COLON);
                end else begin
                    inSymbol;
                end;
                parseLiteral(curType, rightBound, true);
                if (curType = tempType) and
                   (curType@.k = kindScalar) then begin
                    defineRange(curType, leftBound.i, rightBound.i);
                    inSymbol;
                    goto 13020;
                end
            end;
            error(64); (* errIncorrectRangeDefinition *)
            curType := booleanType;
            (*=z-*)exit q(*=z+*)
        end;
    end;
13020:
    if (errors) then
        skip(skipToSet + [RPAREN, RBRACK, SEMICOLON, OFSY]);
    newtype := curType;
end; (* parseTypeRef *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure dumpEnumNames(l3arg1z: tptr);
var
    l3var1z: irptr;
begin
    if (l3arg1z@.start = 0) then begin
        l3arg1z@.start := FcstCnt;
        l3var1z := l3arg1z@.enums;
        while (l3var1z <> NIL) do begin
            curVal := l3var1z@.id;
            l3var1z := l3var1z@.list;
            toFCST;
        end
    end
end; (* dumpEnumNames *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure formPMD;
var
    l3typ1z: tptr;
    l3var2z: word;
    l3var3z: bitset;
    l3var4z: boolean;
    l3var5z: kind;
begin
    for l3var4z := false to true do begin
        if l3var4z then begin
            optSflags.m := (optSflags.m + [S3]);
            curVal.i := 74001B;
            P0715(2, 34); (*"P/DS"*)
            curVal := l2idr2z@.id;
            toFCST;
            curVal.i := lineCnt;
            toFCST;
        end; (* 13063 *)
        for jj := 0 to 127 do begin
            curIdRec := symHashTabBase[jj];
            (*13066*)
            while (curIdRec <> NIL) and
                  (l2idr2z < curIdRec) do with curIdRec@ do begin
                l3var2z.i := typ@.size;
                if (cl IN [VARID, FORMALID]) and
                  (value < 74000B) then begin
                    curVal := id;
                    if (l3var4z) then
                        toFCST;
                    l3typ1z := typ;
                    l3var5z := l3typ1z@.k;
                    l3var3z := [];
                    if (l3var5z = kindPtr) then begin
                        l3typ1z := l3typ1z@.base;
                        l3var5z := l3typ1z@.k;
                        l3var3z := [0];
                    end;
                    if (l3typ1z = realType) then
                        curVal.i := 0
                    else if typeCheck(l3typ1z, integerType) then
                        curVal.i := 100000B
                    else if typeCheck(l3typ1z, charType) then
                        curVal.i := 200000B
                    else if (l3var5z = kindArray) then
                        curVal.i := 400000B
                    else if (l3var5z = kindScalar) then begin
                        dumpEnumNames(l3typ1z);
                        curVal.i := 1000000B * l3typ1z@.start + 300000B;
                    end else if (l3var5z = kindFile) then
                        curVal.i := 600000B
                    else begin
                        curVal.i := 500000B;
                        (*=z-*)(q) exit q(*=z+*)
                    end;
                    curVal.i := curVal.i + curIdRec@.value;
                    l3var2z := l3var2z;
                    besm(ASN64-33);
                    l3var2z := ;
                    curVal.m := curVal.m * [15:47] + l3var2z.m + l3var3z;
                    if (l3var4z) then
                        toFCST;
                end; (* 13164 *)
                curIdRec := curIdRec@.next;
            end; (* 13166 *)
        end; (*13167+*)
        curVal.m := [];
        if l3var4z then
            toFCST;
    end
end; (* formPMD *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseDecls(l3arg1z: integer);
var
    l3int1z: integer;
    frame:   word;
    l3var3z: boolean;
begin
    case l3arg1z of
    0: begin
        int93z := 0;
        inSymbol;
        if (SY <> IDENT) then
            errAndSkip(3, skipToSet + [IDENT]);
    end;
    1: begin
        prevErrPos := 0;
        write('IDENT ');
        printTextWord(l2var12z);
        write(' IN LINE ', curIdRec@.offset:0);
    end;
    2: begin
        with l2idr2z@ do
            ; (* useless *)
        padToLeft;
        l3var3z := 22 IN l2idr2z@.flags;
        l3arg1z := l2idr2z@.pos;
        frame.i := moduleOffset - 40000B;
        if (l3arg1z <> 0) then
            symTab[l3arg1z] := [24, 29] + frame.m * halfWord;
        l2idr2z@.pos := moduleOffset;
        l3arg1z := F3307(l2idr2z);
        if l3var3z then begin
            if (41 >= entryPtCnt) then begin
                curVal := l2idr2z@.id;
                entryPtTable[entryPtCnt] := makeNameWithStars(true);
                entryPtTable[entryPtCnt+1] := [1] + frame.m - [0, 3];
                entryPtCnt := entryPtCnt + 2;
            end else
                error(87); (* errTooManyEntryProcs *)
        end;
        if (l2idr2z@.typ = NIL) then begin
            frame.i := 3;
        end else begin
            frame.i := 4;
        end;
        if l3var3z then
            form2Insn((KVTM+I14) + l3arg1z + (frame.i - 3) * 1000B,
                      getHelperProc(94 (*"P/NN"*)) - 10000000B);
        if 1 < l3arg1z then begin
            frame.i := getValueOrAllocSymtab(-(frame.i+l3arg1z));
        end;
        if (S5 IN optSflags.m) and
           (curProcNesting = 1) then
            l3int1z := 59  (* P/LV *)
        else
            l3int1z := curProcNesting;
        l3int1z := getHelperProc(l3int1z) - (-4000000B);
        if l3arg1z = 1 then begin
            form1Insn((KATX+SP) + frame.i);
        end else if (l3arg1z <> 0) then begin
            form2Insn(KATX+SP, (KUTM+SP) + frame.i);
        end (*=z-*)else(*=z+*) ;
        formAndAlign(l3int1z);
        savedObjIdx := objBufIdx;
        if (curProcNesting <> 1) then
            form1Insn(0);
        if l3var3z then
            form1Insn(KVTM+I8+74001B);
        if (l2int11z <> 0) then begin
            form1Insn(insnTemp[XTA]);
            formAndAlign(KVJM+I13 + l2int11z);
            curVal.i := l2int11z;
            P0715(2, 49 (* "P/RDC" *));
        end;
        if (curProcNesting = 1) then begin
            if (heapCallsCnt <> 0) and
               (heapSize = 0) then
                error(65 (*errCannotHaveK0AndNew*));
            l3var3z := (heapSize = 0) or
                (heapCallsCnt = 0) and (heapSize = 100);
            if (heapSize = 100) then
                heapSize := 4;
            if (not l3var3z) then begin
                form2Insn(KVTM+I14+getValueOrAllocSymtab(heapSize*2000B),
                          getHelperProc(26 (*"P/GD"*)));
                padToLeft;
            end
        end;
        if (doPMD) then
            formPMD;
    end
    end; (* case *)
end; (* parseDecls *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure statement;
label
    8888;
var
    boundary: eptr;
    l3var2z: @numLabel;
    l3var3z: @strLabel;
    l3var4z: word;
    l3bool5z: boolean;
    l3var6z: idclass;
    l3var7z, l3var8z: word;
    startLine: integer;
    l3var10z, l3var11z: word;
    l3idr12z: irptr;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function isCharArray(arg: tptr): boolean;
begin
    with arg@ do
        isCharArray := (k = kindArray) and (base = charType);
end; (* isCharArray *)
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
begin
    if (hashTravPtr@.cl = FIELDID) then begin
        curExpr := expr62z;
        goto 13530;
    end else begin
        new(curExpr);
        with curExpr@ do begin
            typ := hashTravPtr@.typ;
            op := GETVAR;
            id1 := hashTravPtr;
        end;
13462:  inSymbol;
        l4typ3z := curExpr@.typ;
        l4var4z := l4typ3z@.k;
        if (SY = ARROW) then begin
            new(l4exp1z);
            with l4exp1z@ do begin
                expr1 := curExpr;
                if (l4var4z = kindPtr) then begin
                    typ := l4typ3z@.base;
                    op := DEREF;
                end else if (l4var4z = kindFile) then begin
                    typ := l4typ3z@.base;
                    op := FILEPTR;
                end else (q) begin
                    stmtName := '  ^   ';
                    error(errWrongVarTypeBefore);
                    l4exp1z@.typ := l4typ3z;
                    (*=z-*)exit q(*=z+*)
                end
            end;
            curExpr := l4exp1z;
        end else if (SY = PERIOD) then begin
            if (l4var4z = kindRecord) then begin
                int93z := 3;
                typ121z := l4typ3z;
                inSymbol;
                if (hashTravPtr = NIL) then begin
                    error(20); (* errDigitGreaterThan7 ??? *)
                end else 13530: begin
                    new(l4exp1z);
                    with l4exp1z@ do begin
                        typ := hashTravPtr@.typ;
                        op := GETFIELD;
                        expr1 := curExpr;
                        id2 := hashTravPtr;
                    end;
                    curExpr := l4exp1z;
                end
            end else begin
                stmtName := '  .   ';
                error(errWrongVarTypeBefore);
            end;
        end else if (SY = LBRACK) then begin
            stmtName := '  [   ';
            repeat
                l4exp1z := curExpr;
                expression;
                l4typ3z := l4exp1z@.typ;
                if (l4typ3z@.k <> kindArray) then begin
                    error(errWrongVarTypeBefore);
                end else begin
                    if (not typeCheck(l4typ3z@.range, curExpr@.typ)) then
                        error(66 (*errOtherIndexTypeNeeded *));
                    new(l4exp2z);
                    with l4exp2z@ do begin
                        typ := l4typ3z@.base;
                        expr1 := l4exp1z;
                        expr2 := curExpr;
                        op := GETELT;
                    end;
                    l4exp1z := l4exp2z;
                end;
                curExpr := l4exp1z;
                stmtName := '  ,   ';
            until (SY <> COMMA);
            if (SY <> RBRACK) then
                error(67 (*errNeedBracketAfterIndices*));
        end else exit;
    end;
    goto 13462;
end; (* parseLval *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure castToReal(var value: eptr);
var
    cast: eptr;
begin
    new(cast);
    with cast@ do begin
        typ := realType;
        op := TOREAL;
        expr1 := value;
        value := cast;
    end
end; (* castToReal *)
%
function areTypesCompatible(var l4arg1z: eptr): boolean;
begin
    if (arg1Type = realType) then begin
        if typeCheck(integerType, arg2Type) then begin
            castToReal(l4arg1z);
            areTypesCompatible := true;
            exit
        end;
    end else if (arg2Type = realType) and
               typeCheck(integerType, arg1Type) then begin
        castToReal(curExpr);
        areTypesCompatible := true;
        exit
    end;
    areTypesCompatible := false;
end; (* areTypesCompatible *)
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
begin
    with l4arg1z@ do begin
        if typ <> NIL then
            set146z := set146z - flags;
        l4var1z := (list = NIL) and not (24 in flags);
    end;
    new(l4exp3z);
    l4exp4z := l4exp3z;
    bool48z := true;
    with l4exp3z@ do begin
        typ := l4arg1z@.typ;
        op := ALNUM;
        id2 := l4arg1z;
        id1 := NIL;
    end;
    if (SY = LPAREN) then begin
        if (l4var1z) then begin
            l4idr5z := l4arg1z@.argList;
            if (l4idr5z = NIL) then begin
                error(errTooManyArguments);
                goto 8888;
            end
        end;
        repeat
            if (l4var1z) and (l4arg1z = l4idr5z) then begin
                error(errTooManyArguments);
                goto 8888;
            end;
            bool47z := true;
            expression;
            l4op6z := curExpr@.op;
(a)         if l4var1z then begin
                l4idc7z := l4idr5z@.cl;
                if (l4op6z = PCALL) then begin
                    if (l4idc7z <> ROUTINEID) or
                       (l4idr5z@.typ <> NIL) then begin
13736:                  error(39); (*errIncompatibleArgumentKinds*)
                        exit a
                    end
                end else begin (* 13741 *)
                    if (l4op6z = FCALL) then begin
                        if (l4idc7z = ROUTINEID) then begin
                            if (l4idr5z@.typ = NIL) then
                                goto 13736
                        end else (* 13750 *)
                        if (curExpr@.id2@.argList = NIL) and
                           (l4idc7z = VARID) then begin
                            curExpr@.op := ALNUM;
                            curExpr@.expr1 := NIL;
                        end else
                            goto 13736;
                    end else (* 13762 *)
                    if (l4op6z IN lvalOpSet) then begin
                        if (l4idc7z <> VARID) and
                           (l4idc7z <> FORMALID) then
                            goto 13736;
                    end else begin
                        if (l4idc7z <> VARID) then
                            goto 13736;
                        (*=z-*)(q) exit q(*=z+*)
                    end
                end;
                arg1Type := curExpr@.typ;
                if (arg1Type <> NIL) then begin
                    if not typeCheck(arg1Type, l4idr5z@.typ) then
                        error(40); (*errIncompatibleArgumentTypes*)
                end
            end; (* 14006 *)
            new(l4exp2z);
            with l4exp2z@ do begin
                typ := NIL;
                expr1 := NIL;
                expr2 := curExpr;
            end;
            l4exp4z@.expr1 := l4exp2z;
            l4exp4z := l4exp2z;
            if (l4var1z) then
                l4idr5z := l4idr5z@.list;
        until (SY <> COMMA);
        if (SY <> RPAREN) or
           l4var1z and (l4idr5z <> l4arg1z) then
            error(errNoCommaOrParenOrTooFewArgs)
        else
            inSymbol;
    end else begin (* 14035 *)
        if (l4var1z) and (l4arg1z@.argList <> NIL) then
            error(42); (*errNoArgList*)
    end;
    curExpr := l4exp3z;
    (* 14042 *)
end; (* parseCallArgs *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure factor;
label
    14567;
var
    l4var1z: word;
    l4var2z: boolean;
    l4var3z, l4var4z: word;
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
    asBitset: bitset;
    stProcNo, checkMode: integer;
begin
    curVal.i := routine@.low;
    stProcNo := curVal.i;
    if (SY <> LPAREN) then begin
        requiredSymErr(LPAREN);
        goto 8888;
    end;
    expression;
    if (stProcNo >= fnEOF) and
       (fnEOLN >= stProcNo) and
       not (curExpr@.op IN [GETELT..FILEPTR]) then begin
        error(27); (* errExpressionWhereVariableExpected *)
        exit;
    end;
    arg1Type := curExpr@.typ;
    if (arg1Type@.k = kindRange) then
        arg1Type := arg1Type@.base;
    argKind := arg1Type@.k;
    if (arg1Type = realType) then
        checkMode := chkREAL
    else if (arg1Type = integerType) then
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
    else begin
        checkMode := chkOTHER;
        (*=z-*)(q) exit q(*=z+*)
    end;
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
    if not (asBitset <= [fnABS, fnSUCC, fnPRED, fnSQR]) then begin
        arg1Type := routine@.typ;
    end else if (checkMode = chkINT) and (asBitset <= [fnABS, fnSQR]) then begin
        if stProcNo = fnABS then
            stProcNo := fnABSI
        else
            stProcNo := fnSQRI;
    end;
    new(l4exp6z);
    l4exp6z@.op := STANDPROC;
    l4exp6z@.expr1 := curExpr;
    l4exp6z@.num2 := stProcNo;
    if stProcNo = fn24 then begin
        if SY <> COMMA then begin
            requiredSymErr(COMMA);
            goto 8888;
        end;
        expression;
        l5var2z := curExpr@.typ;
        l5op1z := badop27;
        if (l5var2z <> realType) and
            not typeCheck(l5var2z, integerType) then
            error(errNeedOtherTypesOfOperands);
        if (l5var2z = realType) then
            l5op1z := badop30
        else if (checkMode = chkREAL) then
            l5op1z := badop31;
        l4exp6z@.expr2 := curExpr;
        l4exp6z@.op := l5op1z;
    end;
    curExpr := l4exp6z;
    curExpr@.typ := arg1Type;
    checkSymAndRead(RPAREN);
    (* 14247 *)
end; (* stdCall *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* factor *)
    l4var2z := bool47z;
    bool47z := false;
    if (SY < MULOP) then begin
        case SY of
        IDENT: begin
            if (hashTravPtr = NIL) then begin
                error(errNotDefined);
                curExpr := uVarPtr;
            end else
                case hashTravPtr@.cl of
                TYPEID: begin
                    error(23); (* errTypeIdInsteadOfVar *)
                    curExpr := uVarPtr;
                end;
                ENUMID: begin
                    new(curExpr);
                    with curExpr@ do begin
                        typ := hashTravPtr@.typ;
                        op := GETENUM;
                        num1 := hashTravPtr@.value;
                        num2 := 0;
                    end;
                    inSymbol;
                end;
                ROUTINEID: begin
                    routine := hashTravPtr;
                    inSymbol;
                    if (routine@.offset = 0) then begin
                        if (routine@.typ <> NIL) and
                           (SY = LPAREN) then begin
                            stdCall;
                            exit;
                            (*=z-*)end else begin begin(*=z+*)
                        end;
                        error(44) (* errIncorrectUsageOfStandProcOrFunc *)
                        (*=z-*)end(*=z+*)
                    end else if (routine@.typ = NIL) then begin
                        if (l4var2z) then begin
                            l4op10z := PCALL;
                        end else begin
                            error(68); (* errUsingProcedureInExpression *)
                        end
                   end else (* 14330 *) begin
                        if (SY = LPAREN) then begin
                            parseCallArgs(routine);
                            exit
                        end;
                        if (l4var2z) then begin
                            l4op10z := FCALL;
                        end else begin
                            parseCallArgs(routine);
                            exit
                        end;
                        (q) exit q
                    end; (* 14342 *)
                    new(curExpr);
                    if not (SY IN [RPAREN, COMMA]) then begin
                        error(errNoCommaOrParenOrTooFewArgs);
                        goto 8888;
                    end;
                    with curExpr@ do begin
                        typ := routine@.typ;
                        op := l4op10z;
                        expr1 := NIL;
                        id2 := routine;
                    end
                end;
                VARID, FORMALID, FIELDID:
                    parseLval;
                end (* case *)
        end;
        LPAREN: begin
            expression;
            checkSymAndRead(RPAREN);
        end;
        INTCONST, REALCONST, CHARCONST, LTSY, GTSY: begin
            new(curExpr);
            parseLiteral(curExpr@.typ, curExpr@.d1, false);
            curExpr@.num2 := ord(suffix);
            curExpr@.op := GETENUM;
            inSymbol;
        end;
        NOTSY: begin
            inSymbol;
            factor;
            if (curExpr@.typ <> booleanType) then
                error(1); (* errNoCommaNorSemicolon *)
            l4exp6z := curExpr;
            new(curExpr);
            with curExpr@ do begin
                typ := booleanType;
                op := NOTOP;
                expr1 := l4exp6z;
            end
        end;
        LBRACK: begin
            new(curExpr);
            inSymbol;
            l4var8z := curExpr;
            l4var1z.m := [];
            if (SY <> RBRACK) then begin
                l4var12z := true;
                bool102z := false;
                repeat
                    l4exp6z := curExpr;
                    expression;
                    if (l4var12z) then begin
                        l4typ11z := curExpr@.typ;
                        if not (l4typ11z@.k IN [kindScalar, kindRange]) then
                            error(23); (* errTypeIdInsteadOfVar *)
                    end else begin
                        if not typeCheck(l4typ11z, curExpr@.typ) then
                            error(24); (*errIncompatibleExprsInSetCtor*)
                    end;
                    l4var12z := false;
                    l4exp5z := curExpr;
                    if (SY = COLON) then begin
                        expression;
                        if not typeCheck(l4typ11z, curExpr@.typ) then
                            error(24); (*errIncompatibleExprsInSetCtor*)
                        if (l4exp5z@.op = GETENUM) and
                           (curExpr@.op = GETENUM) then begin
                            l4var4z.i := l4exp5z@.num1;
                            l4var3z.i := curExpr@.num1;
                            l4var4z.m := l4var4z.m - intZero;
                            l4var3z.m := l4var3z.m - intZero;
                            l4var1z.m := l4var1z.m + [l4var4z.i..l4var3z.i];
                            curExpr := l4exp6z;
                            goto 14567;
                        end;
                        new(l4var7z);
                        with l4var7z@ do begin
                            typ := setType;
                            op := MKRANGE;
                            expr1 := l4exp5z;
                            expr2 := curExpr;
                        end;
                        l4exp5z := l4var7z;
                        (*=z-*)(q);(*=z+*)
                   end else begin(* 14535 *)
                        if (l4exp5z@.op = GETENUM) then begin
                            l4var4z.i := l4exp5z@.num1;
                            l4var4z.m := l4var4z.m - intZero;
                            l4var1z.m := l4var1z.m + [l4var4z.i];
                            curExpr := l4exp6z;
                            goto 14567;
                        end;
                        new(l4var7z);
                        with l4var7z@ do begin
                            typ := setType;
                            op := STANDPROC;
                            expr1 := l4exp5z;
                            num2 := 109;
                            l4exp5z := l4var7z;
                        end
                    end; (* 14560 *)
                    new(curExpr);
                    with curExpr@ do begin
                        typ := setType;
                        op := SETOR;
                        expr1 := l4exp6z;
                        expr2 := l4exp5z;
                    end;
14567:              ;
                until SY <> COMMA;
            end; (* 14571 *)
            checkSymAndRead(RBRACK);
            with l4var8z@ do begin
                op := GETENUM;
                typ := setType;
                d1 := l4var1z;
            end
        end;
        end; (* case *)
    end else begin
        error(errBadSymbol);
        goto 8888;
    end
    (* 14623 *)
end; (* factor *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure term;
label
    14650;
var
    l4var1z: operator;
    l4var2z, l4var3z: eptr;
    l4var4z: boolean;
begin
    factor;
    while (SY = MULOP) do begin
        l4var1z := charClass;
        inSymbol;
        l4var2z := curExpr;
        factor;
        arg1Type := curExpr@.typ;
        arg2Type := l4var2z@.typ;
        l4var4z := typeCheck(arg1Type, arg2Type);
        if (not l4var4z) and
           (RDIVOP < l4var1z) then
14650:      error(errNeedOtherTypesOfOperands)
        else begin
            case l4var1z of
            MUL, RDIVOP: begin
                if (l4var4z) then begin
                    if (arg1Type = realType) then begin
                        (* empty *)
                    end else begin
                        if (typ120z = integerType) then begin
                            if (l4var1z = MUL) then begin
                                arg1Type := integerType;
                            end else begin
                                arg1Type := realType;
                            end;
                            l4var1z := imulOpMap[l4var1z];
                        end else begin
                            if (arg1Type@.k = kindSet) then begin
                                l4var1z := setOpMap[l4var1z];
                            end else
                                goto 14650;
                        end
                    end
                end else begin
                    if areTypesCompatible(l4var2z) then begin
                        arg1Type := realType;
                    end else
                        goto 14650;
                end
            end;
            AMPERS: begin
                if (arg1Type <> booleanType) then
                    goto 14650;
            end;
            IDIVOP: begin
                if (typ120z <> integerType) then
                    goto 14650;
                arg1Type := integerType;
            end;
            IMODOP: begin
                if (typ120z = integerType) then begin
                    arg1Type := integerType;
                end else begin
                    if (arg1Type@.k = kindSet) then
                        l4var1z := SETXOR
                    else
                        goto 14650;
                end
            end;
            end;
            new(l4var3z);
            with l4var3z@ do begin
                op := l4var1z;
                expr1 := l4var2z;
                expr2 := curExpr;
                curExpr := l4var3z;
                typ := arg1Type;
            end
        end
    end
    (* 14746 *)
end; (* term *)
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
begin
    l4bool5z := false;
    if (charClass IN [PLUSOP, MINUSOP]) then begin
        if (charClass = MINUSOP) then
            l4bool5z := true;
        inSymbol;
    end;
    term;
(minus)
    if (l4bool5z) then begin
        arg1Type := curExpr@.typ;
        new(l4var2z);
        with l4var2z@ do begin
            typ := arg1Type;
            expr1 := curExpr;
            if (arg1Type = realType) then begin
                op := RNEGOP;
            end else if typeCheck(arg1Type, integerType) then begin
                l4var2z@.op := INEGOP;
                l4var2z@.typ := integerType;
            end else begin
                error(69); (* errUnaryMinusNeedRealOrInteger *)
                exit minus
            end;
            curExpr := l4var2z;
        end
    end; (* 15010 *)
    while (SY = ADDOP) do begin
        l4var3z := charClass;
        inSymbol;
        l4var2z := curExpr;
        term;
        arg1Type := curExpr@.typ;
        arg2Type := l4var2z@.typ;
        l4bool5z := typeCheck(arg1Type, arg2Type);
        argKind := arg2Type@.k;
        if (kindSet < argKind) then begin
15031:      error(errNeedOtherTypesOfOperands);
        end else begin
            new(l4var1z);
            with l4var1z@ do begin
                if (l4var3z = OROP) then begin
                    if (not l4bool5z) or
                       (arg1Type <> booleanType) then
                        goto 15031;
                    typ := booleanType;
                    op := l4var3z;
                end else (* 15046 *) begin
                    if (l4bool5z) then begin
                        if (arg1Type = realType) then begin
                            op := l4var3z;
                            typ := realType;
                        end else if (typ120z = integerType) then begin
                            op := iAddOpMap[l4var3z];
                            typ := integerType;
                        end else if (argKind = kindSet) then begin
                            op := setOpMap[l4var3z];
                            typ := arg1Type;
                        end else begin
                            goto 15031
                        end
                    end else if areTypesCompatible(l4var2z) then begin
                        l4var1z@.typ := realType;
                        l4var1z@.op := l4var3z;
                    end else
                        goto 15031
                end; (* 15077 *)
                l4var1z@.expr1 := l4var2z;
                l4var1z@.expr2 := curExpr;
                curExpr := l4var1z;
            end
        end;
    end
    (* 15104 *)
end; (* simpleExpression *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure expression;
var
    oper: operator;
    l4var2z, l4var3z: eptr;
begin
    if (bool102z) then
        inSymbol
    else
        bool102z := true;
    simpleExpression;
    if (SY = RELOP) then begin
        oper := charClass;
        inSymbol;
        l4var3z := curExpr;
        simpleExpression;
        arg1Type := curExpr@.typ;
        arg2Type := l4var3z@.typ;
        if typeCheck(arg1Type, arg2Type) then begin
            if (oper = INOP) or
               (arg1Type@.k = kindFile) or
               (arg1Type@.size <> 1) and
               (oper >= LTOP) and
               not isCharArray(arg1Type) then
                error(errNeedOtherTypesOfOperands);
        end else (* 15150 *) begin
            if not areTypesCompatible(l4var3z) and
               ((arg1Type@.k <> kindSet) or
               not (arg2Type@.k IN [kindScalar, kindRange]) or
               (oper <> INOP)) then begin
                (*=z-*)besm(2200000B); besm(2200000B);(*=z+*)
                error(errNeedOtherTypesOfOperands);
            end (*=z-*)else;(*=z+*)
        end; (* 15167 *)
        new(l4var2z);
        if (arg2Type@.k = kindSet) and
           (oper IN [LTOP, GTOP]) then
            error(errNeedOtherTypesOfOperands);
        with l4var2z@ do begin
            typ := booleanType;
            if (oper IN [GTOP, LEOP]) then begin
                expr1 := curExpr;
                expr2 := l4var3z;
                if (oper = GTOP) then
                    op := LTOP
                else
                    op := GEOP;
            end else begin
                expr1 := l4var3z;
                expr2 := curExpr;
                op := oper;
            end;
            curExpr := l4var2z;
        end
    end
    (* 15217 *)
end; (* expression *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure forStatement;
var
    l4typ1z: tptr;
    l4exp2z, l4var3z, l4var4z: eptr;
    l4int5z, l4int6z, l4int7z, l4int8z: integer;
    l4var9z: boolean;
begin
    inSymbol;
    disableNorm;
    curExpr := NIL;
    if (SY = IDENT) then begin
        if (hashTravPtr <> NIL) and (hashTravPtr@.cl = VARID) then begin
            parseLval;
            if (curExpr@.op <> GETVAR) then
                error(errNoSimpleVarForLoop);
        end else
            error(errNoSimpleVarForLoop);
    end else begin
        errAndSkip(errNoIdent, skipToSet + [BECOMES, DOSY, TOSY, DOWNTOSY]);
    end; (* 15251 *)
    if (curExpr = NIL) then
        curExpr := uVarPtr;
    l4exp2z := curExpr;
    l4typ1z := l4exp2z@.typ;
    if not (l4typ1z@.k IN [kindScalar, kindRange]) then
        error(25); (* errExprNotOfADiscreteType *)
    if typeCheck(integerType, l4typ1z) then
        l4int5z := KATX+PLUS1
    else
        l4int5z := KATX+E1;
    if (SY = BECOMES) then begin
        expression;
        l4var9z := true;
    end else begin
        l4var9z := false;
    end;
    l4var3z := curExpr;
    l4int6z := insnTemp[ADD];
    if not typeCheck(l4typ1z, l4var3z@.typ) then
        error(31); (* errIncompatibleTypesOfLoopIndexAndExpr *)
(todownto)
    if (SY = TOSY) then
        exit todownto
    else if (SY = DOWNTOSY) then
        l4int6z := insnTemp[SUB]
    else (q) begin
        error(70); (* errNeitherToNorDownto *)
        (*=z-*)exit q(*=z+*)
    end;
    expression;
    if not typeCheck(l4typ1z, curExpr@.typ) then
        error(31); (* errIncompatibleTypesOfLoopIndexAndExpr *)
    formOperator(gen0);
    l4var4z := curExpr;
    if (l4var9z) then begin
        curExpr := l4var3z;
        formOperator(LOAD);
    end else begin
        form1Insn(insnTemp[XTA] + l4int5z);
    end;
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
    if (l4int6z = insnTemp[SUB]) then
        curVal.i := l4int6z
    else
        curVal.i := insnTemp[RSUB];
    (*15401*)
    formOperator(gen3);
    form1Insn(insnTemp[UZA] + l4int8z);
end; (* forStatement *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure withStatement;
var
    l4exp1z: eptr;
    l4var2z, l4var3z: bitset;
    l4var4z: integer;
begin
    l4exp1z := expr63z;
    l4var4z := localSize;
    l4var2z := set147z;
    l4var3z := [];
    repeat
        inSymbol;
        if (hashTravPtr <> NIL) and
           (hashTravPtr@.cl >= VARID) then begin
            parseLval;
            if (curExpr@.typ@.k = kindRecord) then begin
                formOperator(SETREG);
                l4var3z := (l4var3z + [curVal.i]) * set148z;
            end else begin
                error(71); (* errWithOperatorNotOfARecord *)
            end;
        end else begin
            error(72); (* errWithOperatorNotOfAVariable *)
        end
    until (SY <> COMMA);
    checkSymAndRead(DOSY);
    statement;
    expr63z := l4exp1z;
    localSize := l4var4z;
    set147z := l4var2z;
    set145z := set145z + l4var3z;
end; (* withStatement *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure reportStmtType(l4arg1z: integer);
begin
    writeln(' STATEMENT ', stmtname:0, ' IN ', startLine:0, ' LINE');
end; (* reportStmtType *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function structBranch(isGoto: boolean): boolean;
var
    curLab: @strLabel;
begin
    structBranch := true;
    if (SY = IDENT) or not isGoto then begin
        curLab := strLabList;
        ii := 1;
        while (curLab <> NIL) do begin
            with curLab@ do begin
                if (ident.m = []) then begin
                    ii := ii - 1;
                end else begin
                    if (ident = curIdent) then begin
                        if (ii = 1) then begin
                            if (isGoto) then begin
                                form1Insn(insnTemp[UJ] + offset);
                            end else begin
                                formJump(curLab@.exitTarget);
                            end;
                        end else begin
                            form1Insn(getValueOrAllocSymtab(ii) +
                                      (KVTM+I13));
                            if (isGoto) then begin
                                form1Insn(KVTM+I10 + curLab@.offset);
                            end else begin
                                jumpType := KVTM+I10;
                                formJump(curLab@.exitTarget);
                                jumpType := insnTemp[UJ];
                            end;
                            form1Insn(getHelperProc(60) +
                                      6437777713700000C); (* P/ZAM *)
                        end;
                        exit
                    end
                end;
                curLab := curLab@.next;
            end
        end;
        if not isGoto and (SY <> IDENT) then begin
            if (ii <> 1) then begin
                form1Insn(getValueOrAllocSymtab(ii) + (KVTM+I13));
                form1Insn(getHelperProc(60)); (* P/ZAM *)
            end;
            formJump(int53z);
        end else begin
            error(errNotDefined);
        end
    end else
        structBranch := false;
end; (* structBranch *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure caseStatement;
label
    16211;
type
    casechain = record
        next:   @casechain;
        value:  word;
        offset: integer;
    end;
var
    allClauses, curClause, clause, unused: @casechain;
    isIntCase: boolean;
    otherSeen: boolean;
    otherOffset: integer;
    itemsEnded, goodMode: boolean;
    firstType, itemtype, exprtype: tptr;
    itemvalue: word;
    itemSpan: integer;
    expected: word;
    startLine, l4var17z, endOfStmt: integer;
    minValue, unused2, maxValue: word;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function max(a, b: integer): integer;
begin
    if (b < a) then
        max := a
    else
        max := b;
end; (* max *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* caseStatement *)
    startLine := lineCnt;
    expression;
    exprtype := curExpr@.typ;
    otherSeen := false;
    if (exprtype = alfaType) or
       (exprtype@.k IN [kindScalar, kindRange]) then
        formOperator(LOAD)
    else
        error(25); (* errExprNotOfADiscreteType *)
    disableNorm;
    l4var17z := 0;
    endOfStmt := 0;
    allClauses := NIL;
    formJump(l4var17z);
    checkSymAndRead(OFSY);
    firstType := NIL;
    goodMode := true;
    (* 15640 *)
    repeat
        if not (SY IN [SEMICOLON, ENDSY]) then begin
            padToLeft;
            arithMode := 1;
            if (SY = OTHERSY) then begin
                if (otherSeen) then
                    error(73); (* errCaseLabelsIdentical *)
                inSymbol;
                otherSeen := true;
                otherOffset := moduleOffset;
            end else (* 15657 *) repeat
                parseLiteral(itemtype, itemvalue, true);
                if (itemtype <> NIL) then begin
                    if (firstType = NIL) then begin
                        firstType := itemtype;
                    end else begin
                        if not typeCheck(itemtype, firstType) then
                            error(errConstOfOtherTypeNeeded);
                    end; (* 15700 *)
                    new(clause);
                    clause@.value := itemvalue;
                    clause@.offset := moduleOffset;
                    curClause := allClauses;
(loop)              while (curClause <> NIL) do begin
                        if (itemvalue = curClause@.value) then begin
                            error(73); (* errCaseLabelsIdentical *)
                            exit loop;
                        end else if (itemvalue.i < curClause@.value.i) then
                        begin
                            exit loop;
                        end else (q) begin
                            unused := curClause;
                            curClause := curClause@.next;
                            (*=z-*)exit q(*=z+*)
                        end
                    end; (* 15723 *)
                    if (curClause = allClauses) then begin
                        clause@.next := allClauses;
                        allClauses := clause;
                    end else begin
                        clause@.next := curClause;
                        unused@.next := clause;
                    end;
                    inSymbol;
                end; (* 15735 *)
                itemsEnded := (SY <> COMMA);
                if not itemsEnded then
                    inSymbol;
            until itemsEnded; (* 15745 *)
            checkSymAndRead(COLON);
            statement;
            goodMode := goodMode and (arithMode = 1);
            formJump(endOfStmt);
        end; (* 15762 *)
        itemsEnded := (SY = ENDSY);
        if not itemsEnded then
            inSymbol;
        (* 15771 *)
    until itemsEnded;
    if (SY <> ENDSY) then begin
        requiredSymErr(ENDSY);
        stmtName := 'CASE  ';
        reportStmtType(startLine);
    end else
        inSymbol;
    if not typeCheck(firstType, exprtype) then begin
        error(88); (* errDifferentTypesOfLabelsAndExpr *);
        exit
    end;
    padToLeft;
    isIntCase := typeCheck(exprtype, integerType);
    if (allClauses <> NIL) then begin
        expected := allClauses@.value;
        minValue := expected;
        curClause := allClauses;
        while (curClause <> NIL) do begin
            if (expected = curClause@.value) and
               (exprtype@.k = kindScalar) then begin
                maxValue := expected;
                if (isIntCase) then begin
                    expected.i := expected.i + 1;
                end else begin
                    curVal := expected;
                    curVal.c := succ(curVal.c);
                    expected := curVal;
                end;
                curClause := curClause@.next;
            end else begin
                itemSpan := 34000;
                P0715(0, l4var17z);
                if (firstType@.k = kindRange) then begin
                    itemSpan := max(abs(firstType@.left),
                                    abs(firstType@.right));
                end else begin
                    if (firstType@.k = kindScalar) then
                        itemSpan := firstType@.numen;
                end;
                itemsEnded := (itemSpan < 32000);
                if (itemsEnded) then begin
                    form1Insn(KATI+14);
                end else begin
                    form1Insn(KATX+SP+1);
                end;
                minValue.i := (minValue.i - minValue.i); (* WTF? *)
                while (allClauses <> NIL) do begin
                    if (itemsEnded) then begin
                        curVal.i := (minValue.i - allClauses@.value.i);
                        curVal.m := (curVal.m + intZero);
                        form1Insn(getValueOrAllocSymtab(curVal.i) +
                                  (KUTM+I14));
                        form1Insn(KVZM+I14 + allClauses@.offset);
                        minValue := allClauses@.value;
                    end else begin
                        form1Insn(KXTA+SP+1);
                        curVal := allClauses@.value;
                        form2Insn(KAEX + I8 + getFCSToffset,
                                  insnTemp[UZA] + allClauses@.offset);
                    end;
                    allClauses := allClauses@.next;
                end;
                if (otherSeen) then
                    form1Insn(insnTemp[UJ] + otherOffset);
                goto 16211;
            end; (* if 16141 *)
        end; (* while 16142 *)
        if (not otherSeen) then begin
            otherOffset := moduleOffset;
            formJump(endOfStmt);
        end;
        P0715(0, l4var17z);
        curVal := minValue;
        P0715(-(insnTemp[U1A]+otherOffset), maxValue.i);
        curVal := minValue;
        curVal.m := (curVal.m + intZero);
        form1Insn(KATI+14);
        curVal.i := ((moduleOffset + (1)) - curVal.i);
        if (curVal.i < 40000B) then begin
            curVal.i := (curVal.i - 40000B);
            curVal.i := allocSymtab([24, 29] +
                        (curVal.m * O77777));
        end;
        form1Insn(KUJ+I14 + curVal.i);
        while (allClauses <> NIL) do begin
            padToLeft;
            form1Insn(insnTemp[UJ] + allClauses@.offset);
            allClauses := allClauses@.next;
        end;
        16211:
        P0715(0, endOfStmt);
        if (not goodMode) then
           disableNorm;
        (* 16217 *)
    end
end; (* caseStatement *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure assignStatement(doLHS: boolean);
label
    16332;
var
    lhsExpr, assnExpr: eptr;
    indCnt: integer;
    srcType, targType: tptr;
begin
    if (doLHS) then
        parseLval
    else begin
        new(curExpr);
        with curExpr@ do begin
            typ := hashTravPtr@.typ;
            op := GETVAR;
            id1 := hashTravPtr;
        end;
        inSymbol;
    end;
    checkSymAndRead(BECOMES);
    bool102z := false;
    targType := curExpr@.typ;
    if (targType@.k = kindRecord) and
       (SY = LBRACK) then begin
        formOperator(gen5);
        indCnt := 0;
        inSymbol;
        l3bool5z := false;
(indices)
        begin
            if (SY = COMMA) then begin
                indCnt := indCnt + 1;
                inSymbol;
            end else if (SY = RBRACK) then begin
                inSymbol;
                exit indices;
            end else (* 16262 *) (q) begin
                bool102z := false;
                expression;
                curVal.i := indCnt;
                formOperator(gen6);
                (*=z-*)exit q(*=z+*)
            end; (* 16270 *)
            goto indices;
        end;
        curExpr := NIL;
    end else (* 16273 *)
    if (SY = SEMICOLON) and allowCompat then begin
        formOperator(STORE);
        bool102z := true;
        curExpr := NIL;
    end else (* 16303 *) begin
        lhsExpr := curExpr;
        expression;
        srcType := curExpr@.typ;
        if (typeCheck(targType, srcType)) then begin
            if (srcType@.k = kindFile) then
                error(75) (*errCannotAssignFiles*)
            else begin
                if rangeMismatch and (targType@.k = kindRange) then begin
                    new(assnExpr);
                    with assnExpr@ do begin
                        typ := srcType;
                        op := BOUNDS;
                        expr1 := curExpr;
                        typ2 := targType;
                    end;
                    curExpr := assnExpr;
                end;
16332:          new(assnExpr);
                with assnExpr@ do begin
                    typ := targType;
                    op := ASSIGNOP;
                    expr1 := lhsExpr;
                    expr2 := curExpr;
                end;
                curExpr := assnExpr;
            end
        end else if (targType = realType) and
            typeCheck(integerType, srcType) then begin
            castToReal(curExpr);
            goto 16332;
        end else begin
            error(33); (*errIllegalTypesForAssignment*)
        end
    end
    (* 16356 *)
end; (* assignStatement *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure compoundStatement;
begin
(loop) begin
        statement;
        if (SY = SEMICOLON) then begin
            inSymbol;
            goto loop;
        end
    end
end; (* compoundStatement *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure ifWhileStatement(delim: symbol);
begin
    disableNorm;
    expression;
    if (curExpr@.typ <> booleanType) then
        error(errBooleanNeeded)
    else begin
        jumpTarget := 0;
        formOperator(gen15);
        l3var10z.i := jumpTarget;
    end;
    checkSymAndRead(delim);
    statement;
end; (* ifWhileStatement *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseData;
label
    16545;
type
    DATAREC = record case boolean of
            false: (a: packed array [0..3] of 0..4095);
            true:  (b: bitset)
        end;
var
    dsize, setcount: integer;
    l4var3z, l4var4z, l4var5z: word;
    boundary: eptr;
    l4var7z, l4var8z, l4var9z: word;
    F: file of DATAREC;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P16432(l5arg1z: integer);
var
    l5var1z: DATAREC;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function allocDataRef(l6arg1z: integer): integer;
begin
    if (l6arg1z >= 2048) then begin
        curVal.i := l6arg1z;
        allocDataRef := allocSymtab((curVal.m + [24]) * halfWord);
    end else begin
        allocDataRef := l6arg1z;
    end
end; (* allocDataRef *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* P16432 *)
    l5var1z.a[0] := allocDataRef(l4var4z.i);
    if (FcstCnt = l4var3z.i) then begin
        curVal := l4var8z;
        curVal.i := addCurValToFCST;
    end else begin
        curVal := l4var3z;
    end;
    l5var1z.a[1] := allocSymtab([12,23] + curVal.m * halfWord);
    l5var1z.a[2] := allocDataRef(l5arg1z);
    if (l4var9z.i = 0) then begin
        curVal := l4var7z;
        besm(ASN64+24);
        curVal := ;
    end else begin
        curVal.i := allocSymtab(l4var7z.m + l4var9z.m * halfWord);
    end;
    l5var1z.a[3] := curVal.i;
    l4var9z.i := l5arg1z * l4var4z.i + l4var9z.i;
    F@ := l5var1z;
    put(F);
    setcount := setcount + 1;
    l4var4z.i := 0;
    l4var3z.i := FcstCnt;
end; (* P16432 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* parseData *)
    dsize := FcstCnt;
    inSymbol;
    setcount := 0;
(loop)
    repeat (* 16530 *)
        inSymbol;
        setup(boundary);
        if SY <> IDENT then begin
            if SY = ENDSY then
                exit loop;
            error(errNoIdent);
            curExpr := uVarPtr;
        end else (* 16543 *) begin
            if (hashTravPtr = NIL) then begin
16545:          error(errNotDefined);
                curExpr := uVarPtr;
                inSymbol;
            end else begin
                if (hashTravPtr@.cl = VARID) then begin
                    parseLval;
                end else goto 16545;
            end
        end; (* 16557 *)
        putLeft := true;
        objBufIdx := 1;
        formOperator(gen5);
        if (objBufIdx <> 1) then
            error(errVarTooComplex);
        l4var7z.m := (leftInsn * [12,13,14,15,16,17,18,19,20,21,22,23]);
        l4var3z.i := FcstCnt;
        l4var4z.i := 0;
        l4var9z.i := 0;
        repeat (* 16574 *)
            expression;
            formOperator(LITINSN);
            l4var8z := curVal;
            if (SY = COLON) then begin
                inSymbol;
                l4var5z := curToken;
                if (SY <> INTCONST) then begin
                    error(62); (* errIntegerNeeded *)
                    l4var5z.i := 0;
                end else
                    inSymbol;
            end else
                l4var5z.i := 1;
            if (l4var5z.i <> 1) then begin
                if (l4var4z.i <> 0) then
                    P16432(1);
                l4var4z.i := 1;
                P16432(l4var5z.i);
            end else begin
                l4var4z.i := l4var4z.i + 1;
                if (SY = COMMA) then begin
                    curVal := l4var8z;
                    toFCST;
                end else begin
                    if (l4var4z.i <> 1) then begin
                        curVal := l4var8z;
                        toFCST;
                    end;
                    P16432(1);
                end
            end; (* 16641 *)
        until SY <> COMMA;
        rollup(boundary);
    until SY <> SEMICOLON; (* 16645 *)
    if (SY <> ENDSY) then
        error(errBadSymbol);
    reset(F);
    while not eof(F) do begin
        write(FCST, F@.b);
        get(F);
    end;
    int92z := FcstCnt - dsize;
    FcstCnt := dsize;
    int93z := setcount;
    (* 16666 *)
end; (* parseData *)
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
    l4var13z, l4var14z, l4var15z: word;
    procNo: integer;
    helperNo: integer;
    l4var18z: opgen;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure verifyType(l5arg1z: tptr);
begin
    if (hashTravPtr <> NIL) and
       (hashTravPtr@.cl >= VARID) then begin
        parseLval;
        if (l5arg1z <> NIL) and
           not typeCheck(l5arg1z, curExpr@.typ) then
            error(errNeedOtherTypesOfOperands);
    end else begin
        error(errNotDefined);
        curExpr := uVarPtr;
    end
end; (* verifyType *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure startReadOrWrite(l5arg1z: boolean);
begin
    expression;
    l4typ3z := curExpr@.typ;
    l4exp7z := curExpr;
    if not (l5arg1z) then begin
        if not (curExpr@.op IN lvalOpSet) then
            error(27); (* errExpressionWhereVariableExpected *)
    end;
    if (l4exp9z = NIL) then begin
        if (l4typ3z@.k = kindFile) then begin
            l4exp9z := curExpr;
        end else begin
            new(l4exp9z);
            l4exp9z@.typ := textType;
            l4exp9z@.op := GETVAR;
            if (l5arg1z) then begin
                l4exp9z@.id1 := outputFile;
            end else begin
                if (inputFile <> NIL) then
                    l4exp9z@.id1 := inputFile
                else (q) begin
                    error(37); (* errInputMissingInProgramHeader *)
                    (*=z-*)exit q(*=z+*)
                end
            end
        end;
        arg2Type := l4exp9z@.typ;
        l4var13z.b := typeCheck(arg2Type@.base, charType);
        l4bool12z := true;
        new(l4exp8z);
        l4exp8z@.typ := arg2Type@.base;
        l4exp8z@.op := FILEPTR;
        l4exp8z@.expr1 := l4exp9z;
        new(l4exp6z);
        l4exp6z@.typ := l4exp8z@.typ;
        l4exp6z@.op := ASSIGNOP;
        if (l5arg1z) then
            l4exp6z@.expr1 := l4exp8z
        else
            l4exp6z@.expr2 := l4exp8z;
    end (* 17002 *)
end; (* startReadOrWrite *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseWidthSpecifier(var l5arg1z: eptr);
begin
    expression;
    if not typeCheck(integerType, curExpr@.typ) then begin
        error(14); (* errExprIsNotInteger *)
        curExpr := uVarPtr;
    end;
    l5arg1z := curExpr;
end; (* parseWidthSpecifier *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure callHelperWithArg;
begin
    if ([12] <= set145z) or l4bool12z then begin
        curExpr := l4exp9z;
        formOperator(gen8);
    end;
    l4bool12z := false;
    formAndAlign(getHelperProc(helperNo));
    disableNorm;
end; (* callHelperWithArg *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure P17037;
begin
    set145z := set145z - [12];
    if (helperNo <> 49) and             (* P/RDC *)
       not typeCheck(l4exp8z@.typ, l4exp7z@.typ) then
        error(34) (* errTypeIsNotAFileElementType *)
    else begin
        if (helperNo = 29) then begin       (* P/PF *)
            l4exp6z@.expr2 := l4exp7z;
        end else begin
            if (helperNo = 49) then
                helperNo := 30;         (* P/GF *)
            l4exp6z@.expr1 := l4exp7z;
        end;
        curExpr := l4exp6z;
        formOperator(gen7);
        callHelperWithArg;
    end
end; (* P17037 *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure checkElementForReadWrite;
var
    l5typ1z: tptr;
begin
    set145z := set145z - [12];
    if (l4typ3z@.k = kindRange) then
        l4typ3z := l4typ3z@.base;
    curVarKind := l4typ3z@.k;
    helperNo := 36;                   (* P/WI *)
    if (l4typ3z = integerType) then
        l4var15z.i := 10
    else if (l4typ3z = realType) then begin
        helperNo := 37;               (* P/WR *)
        l4var15z.i := 14;
    end else if (l4typ3z = charType) then begin
        helperNo := 38;               (* P/WC *)
        l4var15z.i := 1;
    end else if (curVarKind = kindScalar) then begin
        helperNo := 41;               (* P/WX *)
        dumpEnumNames(l4typ3z);
        l4var15z.i := 8;
    end else if (isCharArray(l4typ3z)) then begin
        l5typ1z := ref(l4typ3z@.range@);
        l4var15z.i := l5typ1z@.right - l5typ1z@.left + 1;
        if not (l4typ3z@.pck) then
            helperNo := 81            (* P/WA *)
        else if (6 >= l4var15z.i) then
            helperNo := 39            (* P/A6 *)
        else
            helperNo := 40;           (* P/A7 *)
    end else if (l4typ3z@.size = 1) then begin
        helperNo := 42;               (* P/WO *)
        l4var15z.i := 17;
    end else (q) begin
        error(34); (* errTypeIsNotAFileElementType *)
        (*=z-*)exit q(*=z+*)
    end
end; (* checkElementForReadWrite *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure writeProc;
begin
    l4exp9z := NIL;
    l4var13z.b := true;
    repeat begin
        startReadOrWrite(true);
        if (l4exp7z <> l4exp9z) then begin
            if not (l4var13z.b) then begin
                helperNo := 29;         (* P/PF *)
                P17037;
            end else begin
                checkElementForReadWrite;
                l4var5z := NIL;
                l4var4z := NIL;
                if (SY = COLON) then
                    parseWidthSpecifier(l4var4z);
                if (SY = COLON) then begin
                    parseWidthSpecifier(l4var5z);
                    if (helperNo <> 37) then    (* P/WR *)
                        error(35); (* errSecondSpecifierForWriteOnlyForReal *)
                end else begin
                    if (curToken = litOct) then begin
                        helperNo := 42; (* P/WO *)
                        l4var15z.i := 17;
                        if (l4typ3z@.size <> 1) then
                            error(34); (* errTypeIsNotAFileElementType *)
                        inSymbol;
                    end
                end;
                l4bool11z := false;
                if (l4var4z = NIL) and
                   (helperNo IN [38,39,40]) then begin  (* WC,A6,A7 *)
                    helperNo := helperNo + 5;       (* CW,6A,7A *)
                    l4bool11z := true;
                end else begin
                    if (l4var4z = NIL) then begin
                        curVal := l4var15z;
                        formOperator(gen9);
                    end else begin
                        curExpr := l4var4z;
                        formOperator(LOAD);
                    end
                end;
                if (helperNo = 37) then begin       (* P/WR *)
                    if (l4var5z = NIL) then begin
                        curVal.i := 4;
                        form1Insn(KXTS+I8 + getFCSToffset);
                    end else begin
                        curExpr := l4var5z;
                        formOperator(gen10);
                    end
                end;
                curExpr := l4exp7z;
                if (l4bool11z) then begin
                    if (helperNo = 45) then     (* P/7A *)
                        l4var18z := gen11
                    else
                        l4var18z := LOAD;
                end else begin
                    if (helperNo = 40) or       (* P/A7 *)
                       (helperNo = 81) then     (* P/WA *)
                        l4var18z := gen12
                    else
                        l4var18z := gen10;
                end;
                formOperator(l4var18z);
                if (helperNo IN [39,40,44,45]) or (* A6,A7,6A,7A *)
                   (helperNo = 81) then
                    form1Insn(KVTM+I10 + l4var15z.i)
                else begin
                    if (helperNo = 41) then (* P/WX *)
                        form1Insn(KVTM+I11 + l4typ3z@.start);
                end;
                callHelperWithArg;
            end
        end
    end until (SY <> COMMA);
    if (procNo = 11) then begin
        helperNo := 46;                 (* P/WL *)
        callHelperWithArg;
    end;
    set145z := set145z + [12];
    if (l4var14z.i = moduleOffset) then
        error(36); (*errTooFewArguments *)
end; (* writeProc *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure readProc;
label
    17346, 17362;
begin
    l4exp9z := NIL;
    l4var13z.b := true;
    l4var14z.i := moduleOffset;
    repeat begin
        startReadOrWrite(false);
        if (l4exp7z <> l4exp9z) then begin
            if not (l4var13z.b) then begin
                helperNo := 30;         (* P/GF *)
17346:
                P17037;
           end else begin
                checkElementForReadWrite;
                if (helperNo = 38) then begin       (* P/WC *)
                    helperNo := 49;             (* P/RDC *)
                    goto 17346;
                end;
                if (helperNo = 39) or           (* A6,A7 *)
                   (helperNo = 40) then begin
                    helperNo := 51;             (* P/RA7 *)
17362:
                    curExpr := l4exp7z;
                    formOperator(gen5);
                    form1Insn(KVTM+I10 + l4var15z.i);
                    callHelperWithArg;
                end else (q) begin
                    if (helperNo = 81) then begin   (* P/WA *)
                        helperNo := 90;         (* P/RA *)
                        goto 17362;
                    end;
                    helperNo := helperNo + 11;
                    callHelperWithArg;
                    curExpr := l4exp7z;
                    formOperator(STORE);
                    (*=z-*)exit q(*=z+*)
                end
            end
        end
    end until (SY <> COMMA);
    set145z := set145z + [12];
    if (procNo = 13) then begin
        helperNo := 53;                 (* P/RL *)
        callHelperWithArg;
    end;
    if (l4var14z.i = moduleOffset) then
        error(36); (* errTooFewArguments *)
end; (* readProc *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure checkArrayArg;
begin
    verifyType(NIL);
    l4exp9z := curExpr;
    l4typ1z := curExpr@.typ;
    if (l4typ1z@.pck) or
       (l4typ1z@.k <> kindArray) then
        error(errNeedOtherTypesOfOperands);
    checkSymAndRead(COMMA);
    bool102z := false;
    expression;
    l4exp8z := curExpr;
    if not typeCheck(l4typ1z@.range, l4exp8z@.typ) then
        error(errNeedOtherTypesOfOperands);
end; (* checkArrayArg *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure doPackUnpack;
var
    t: tptr;
begin
    new(l4exp7z);
    l4exp7z@.typ := l4typ1z@.base;
    l4exp7z@.op := GETELT;
    l4exp7z@.expr1 := l4exp9z;
    l4exp7z@.expr2 := l4exp8z;
    t := ref(l4exp6z@.typ@);
    if (t@.k <> kindArray) or
       not t@.pck or
       not typeCheck(t@.base, l4typ1z@.base) or
       not typeCheck(l4typ1z@.range, t@.range) then
        error(errNeedOtherTypesOfOperands);
    new(curExpr);
    curExpr@.val.c := chr(procNo + 50);
    curExpr@.expr1 := l4exp7z;
    curExpr@.expr2 := l4exp6z;
    formOperator(gen16);
end; (* doPackUnpack *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* standProc *)
    curVal.i := l3idr12z@.low;
    procNo := curVal.i;
    l4bool10z := (SY = LPAREN);
    l4var14z.i := moduleOffset;
    if not l4bool10z and
       (procNo IN [0:5,8:10,12,16:28]) then
        error(45); (* errNoOpenParenForStandProc *)
    if (procNo IN [0,1,2,3,4,5,8,9]) then begin
        inSymbol;
        if (hashTravPtr@.cl < VARID) then
            error(46); (* errNoVarForStandProc *)
        parseLval;
        arg1Type := curExpr@.typ;
        curVarKind := arg1Type@.k;
    end;
    if (procNo IN [0..6]) then
        jumpTarget := getHelperProc(29 + procNo); (* P/PF *)
    case procNo of
    0, 1, 2, 3: begin (* put, get, rewrite, reset *)
        if (curVarKind <> kindFile) then
            error(47); (* errNoVarOfFileType *)
        if (procNo = 3) and
           (SY = COMMA) then begin
            formOperator(gen8);
            expression;
            if (not typeCheck(integerType, curExpr@.typ)) then
                error(14); (* errExprIsNotInteger *)
            formOperator(LOAD);
            formAndAlign(getHelperProc(97)); (*"P/RE"*)
        end else begin
            formOperator(FILEACCESS);
        end
    end;
    4, 5: begin (* new, dispose *)
        if (curVarKind <> kindPtr) then
            error(13); (* errVarIsNotPointer *)
        heapCallsCnt := heapCallsCnt + 1;
        l4exp9z := curExpr;
        if (procNo = 5) then
            formOperator(gen5);
        l2typ13z := arg1Type@.base;
        ii := l2typ13z@.size;
        if (charClass = EQOP) then begin
            expression;
            if not typeCheck(integerType, curExpr@.typ) then
                error(14); (* errExprIsNotInteger *)
            formOperator(LOAD);
            form1Insn(KATI+14);
        end else begin
            if (arg1Type@.base@.k = kindRecord) then (*=z-*)(x)(*=z+*) begin
                l4typ1z := l2typ13z@.base;
(loop)          while (SY = COMMA) and (l4typ1z <> NIL) do begin
                    with l4typ1z@ do
                        ; (* useless *)
                    inSymbol;
                    parseLiteral(l4typ2z, curVal, true);
                    if (l4typ2z = NIL) then
                        exit loop
                    else (q) begin
                        inSymbol;
(loop2)                 while (l4typ1z <> NIL) do begin
                            l4typ2z := l4typ1z;
                            while (l4typ2z <> NIL) do begin
                                if (l4typ2z@.sel = curVal) then begin
                                    ii := l4typ1z@.size;
                                    exit loop2;
                                end;
                                l4typ2z := l4typ2z@.r6;
                                (*=z-*)(x);(*=z+*)
                            end;
                            l4typ1z := l4typ1z@.next;
                        end;
                        (*=z-*)exit q(*=z+*)
                    end;
                end
            end;
            form1Insn(KVTM+I14+getValueOrAllocSymtab(ii));
        end;
        formAndAlign(jumpTarget);
        if (procNo = (4)) then begin
            curExpr := l4exp9z;
            formOperator(STORE);
        end
    end;
    6: begin (* halt *)
        formAndAlign(jumpTarget);
        exit
    end;
    7: begin (* stop *)
        form1Insn(KE74);
        exit
    end;
    8, 9: begin (* setup, rollup *)
        if (curVarKind <> kindPtr) then
            error(13); (* errVarIsNotPointer *)
        if (procNo = 8) then begin
            form1Insn(KXTA+HEAPPTR);
            formOperator(STORE);
        end else begin
            formOperator(LOAD);
            form1Insn(KATX+HEAPPTR);
        end
    end;
    10: begin (* write *)
        writeProc;
    end;
    11:
17753: begin (* writeln *)
        if (SY = LPAREN) then begin
            writeProc;
        end else begin
            formAndAlign(getHelperProc(54)); (*"P/WOLN"*)
            exit
        end
    end;
    12: begin (* read *)
        readProc;
    end;
    13: begin (* readln *)
        if (SY = LPAREN) then begin
            readProc;
        end else begin
            formAndAlign(getHelperProc(55)); (*"P/RILN"*)
            exit
        end
    end;
    14: begin (* exit *)
        l4bool10z := (SY = LPAREN);
        if (l4bool10z) then
            inSymbol;
        if (SY = IDENT) then begin
            if not structBranch(false) then
                error(1); (* errCommaOrSemicolonNeeded *)
            inSymbol;
        end else begin
            formJump(int53z);
        end;
        if not (l4bool10z) then
            exit
    end;
    15: begin (* debug *)
        if (debugPrint IN optSflags.m) then begin
            procNo := 11;
            goto 17753;
        end;
        while (SY <> RPAREN) do
            inSymbol;
    end;
    16: begin (* besm *)
        expression;
        formOperator(LITINSN);
        formAndAlign(curVal.i);
    end;
    17: begin (* mapia *)
        l4typ1z := integerType;
        l4typ2z := alfaType;
20041:
        expression;
        if not typeCheck(curExpr@.typ, l4typ1z) then
            error(errNeedOtherTypesOfOperands);
        checkSymAndRead(COMMA);
        formOperator(LOAD);
        if (procNo = 17) then begin
            form3Insn(ASN64-33, KAUX+BITS15, KAEX+ASCII0);
        end else begin
            form3Insn(KAPX+BITS15, ASN64+33, KAEX+ZERO);
        end;
        verifyType(l4typ2z);
        formOperator(STORE);
    end;
    18: begin (* mapai *)
        l4typ1z := alfaType;
        l4typ2z := integerType;
        goto 20041;
    end;
    19, 20: begin (* pck, unpck *)
        inSymbol;
        verifyType(charType);
        checkSymAndRead(COMMA);
        formOperator(gen8);
        verifyType(alfaType);
        if (procNo = 20) then begin
            formOperator(LOAD);
        end;
        formAndAlign(getHelperProc(procNo - 6));
        if (procNo = 19) then
            formOperator(STORE);
    end;
    21: begin (* pack *)
        inSymbol;
        checkArrayArg;
        checkSymAndRead(COMMA);
        verifyType(NIL);
        l4exp6z := curExpr;
        doPackUnpack;
    end;
    22: begin (* unpack *)
        inSymbol;
        verifyType(NIL);
        l4exp6z := curExpr;
        checkSymAndRead(COMMA);
        checkArrayArg;
        doPackUnpack;
    end;
    23, 24, 25, 26, 27, 28: begin (* MARS procedures *)
        l3bool5z := 24 < procNo;
        repeat begin
            expression;
            if (curExpr@.typ@.size <> 1) then
                error(5); (*errSimpleTypeReq*)
            formOperator(LOAD);
            if (l3bool5z) then begin
                checkSymAndRead(COMMA);
                verifyType(NIL);
                l4exp9z := curExpr;
                if (SY = COLON) then begin
                    expression;
                    formOperator(gen10);
                end else begin
                    form2Insn(KVTM + I14 + l4exp9z@.typ@.size,
                              KITS + 14);
                end;
                curExpr := l4exp9z;
                formOperator(gen12);
            end else begin
                form2Insn(insnTemp[XTS], insnTemp[XTS]);
            end;
            form1Insn(KWTC + I14 + 77751B + procNo);
            formAndAlign(getHelperProc(80)); (*"PAIB"*)
        end until (SY <> COMMA);
    end;
    end; (* 20257 *)
    if procNo in [0,1,2,3,5,10,11,12,13,21,22] then
        arithMode := 1;
    checkSymAndRead(RPAREN);
    (* 20265 *)
end; (* standProc *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* statement *)
    setup(boundary);
    bool110z := false;
    startLine := lineCnt;
    if set147z = halfWord then
        parseData
    else begin
        if SY = INTCONST then begin
            set146z := [];
            l3var2z := numLabList;
            disableNorm;
            l3bool5z := true;
            padToLeft;
            while l3var2z <> l2var16z do begin with l3var2z@ do
                if id <> curToken then begin
                    l3var2z := next;
                end else begin
                    l3bool5z := false;
                    if (defined) then begin
                        curVal.i := line;
                        error(17); (* errLblAlreadyDefinedInLine *);
                    end else begin
                        l3var2z@.line := lineCnt;
                        l3var2z@.defined := true;
                        padToLeft;
                        if l3var2z@.offset = 0 then begin
                            (* empty *)
                        end else if (l3var2z@.offset >= 74000B) then begin
                            curVal.i := (moduleOffset - 40000B);
                            symTab[l3var2z@.offset] := [24,29] +
                                                         curVal.m * O77777;
                        end else (q) begin
                            P0715(0, l3var2z@.offset);
                            (*=z-*)exit q(*=z+*)
                        end; (* 20342 *)
                        l3var2z@.offset := moduleOffset;
                    end;
                    l3var2z := l2var16z;
                end;
            end; (* while 20346 *)
            if (l3bool5z) then
                error(16); (* errLblNotDefinedInBlock *);
            inSymbol;
            checkSymAndRead(COLON);
        end; (* 20355*)
        if (DebugInteractive IN optSflags.m) and
           (debugLine <> lineCnt) then begin
            P0715(-1, 96 (* "P/DD" *));
            debugLine := lineCnt;
            arithMode := 1;
        end;
        l3var4z.b := (SY IN [BEGINSY,CASESY,REPEATSY,SELECTSY]);
        if (l3var4z.b) then
            lineNesting := lineNesting + 1;
(ident)
        if SY = IDENT then begin
            if hashTravPtr <> NIL then begin
                l3var6z := hashTravPtr@.cl;
                if l3var6z >= VARID then begin
                    assignStatement(true);
                end else begin
                    if l3var6z = ROUTINEID then begin
                        if hashTravPtr@.typ = NIL then begin
                            l3idr12z := hashTravPtr;
                            inSymbol;
                            if l3idr12z@.offset = 0 then begin
                                standProc;
                                exit ident;
                            end;
                            parseCallArgs(l3idr12z);
                        end else begin
                            assignStatement(false);
                        end;
                    end else begin
                        error(32); (* errWrongStartOfOperator *)
                        goto 8888;
                    end
                end;
                formOperator(gen7);
            end else begin
                error(errNotDefined);
8888:           skip(skipToSet + statEndSys);
            end;
        end else (* 20431 *) if (SY = LPAREN) then begin
            set146z := [];
            inSymbol;
            if (SY <> IDENT) then begin
                error(errNoIdent);
                goto 8888;
            end;
            new(l3var3z);
            padToLeft;
            disableNorm;
            with l3var3z@ do begin
                next := strLabList;
                ident := curIdent;
                offset := moduleOffset;
                exitTarget := 0;
            end;
            strLabList := l3var3z;
            inSymbol;
            checkSymAndRead(RPAREN);
            statement;
            P0715(0, l3var3z@.exitTarget);
            strLabList := strLabList@.next;
        end else (* 20463 *) if (SY = BEGINSY) then
(rep)   begin
            inSymbol;
(skip)      begin
                compoundStatement;
                if (SY <> ENDSY) then begin
                    stmtName := ' BEGIN';
                    requiredSymErr(SEMICOLON);
                    reportStmtType(startLine);
                    skip(bigSkipSet);
                    if (SY IN statBegSys) then
                        goto skip;
                    if (SY <> SEMICOLON) then
                        exit rep;
                    goto rep;
                    (*=z-*)(q) exit rep;(*=z+*)
                end;
            end;
            inSymbol;
        end else (* 20511 *) if (SY = GOTOSY) then begin
            inSymbol;
            if (SY <> INTCONST) then begin
                if structBranch(true) then begin
                    inSymbol;
                    exit;
                end else
                    goto 8888;
            end;
            disableNorm;
            l3var2z := numLabList;
(loop)      if (l3var2z <> NIL) then with l3var2z@ do begin
                if (id <> curToken) then begin
                    l3var2z := next;
                end else begin
                    if (curFrameRegTemplate = frame) then begin
                        if (offset >= 40000B) then begin
                            form1Insn(insnTemp[UJ] + offset);
                        end else begin
                            formJump(offset);
                        end
                    end else begin
                        if offset = 0 then begin
                            offset := symTabPos;
                            putToSymTab([]);
                        end;
                        form3Insn(frame + (KMTJ + 13), KVTM+I14 + offset,
                                  getHelperProc(18(*"P/RC"*)) + (-64100000B));
                    end;
                    exit loop;
                end;
                goto loop;
            end else
                error(18); (* errLblNotDefined *)
            inSymbol;
        end else (* 20571 *) if (SY = IFSY) then begin
            ifWhileStatement(THENSY);
            if (SY = ELSESY) then begin
                l3var11z.i := 0;
                formJump(l3var11z.i);
                P0715(0, l3var10z.i);
                l3var8z.i := arithMode;
                arithMode := 1;
                inSymbol;
                statement;
                P0715(0, l3var11z.i);
                if (l3var8z.i <> arithMode) then begin
                    arithMode := 2;
                    disableNorm;
                end
            end else begin
                P0715(0, l3var10z.i);
            end
        end else (* 20625 *) if (SY = WHILESY) then begin
            set146z := [];
            disableNorm;
            padToLeft;
            l3var8z.i := moduleOffset;
            ifWhileStatement(DOSY);
            disableNorm;
            form1Insn(insnTemp[UJ] + l3var8z.i);
            P0715(0, l3var10z.i);
            arithMode := 1;
        end else (* 20644 *) if (SY = REPEATSY) then begin
            set146z := [];
            disableNorm;
            padToLeft;
            l3var7z.i := moduleOffset;
            repeat
                inSymbol;
                statement;
            until (SY <> SEMICOLON);
            if (SY <> UNTILSY) then begin
                requiredSymErr(UNTILSY);
                stmtName := 'REPEAT';
                reportStmtType(startLine);
                goto 8888;
            end;
            disableNorm;
            expression;
            if (curExpr@.typ <> booleanType) then begin
                error(errBooleanNeeded)
            end else begin
                jumpTarget := l3var7z.i;
                formOperator(gen15);
            end;
        end else (* 20676 *)
        if (SY = FORSY) then begin
            set146z := [];
            forStatement;
        end else (* 20702 *) if (SY = SELECTSY) then begin
            disableNorm;
            l3bool5z := true;
            l3var11z.i := 0;
            (* 20707 *)
            repeat
                arithMode := 1;
                expression;
                if (curExpr@.typ <> booleanType) then begin
                    error(errBooleanNeeded);
                end else begin
                    jumpTarget := 0;
                    formOperator(gen15);
                    l3var10z.i := jumpTarget;
                end;
                checkSymAndRead(COLON);
                statement;
                formJump(l3var11z.i);
                l3bool5z := l3bool5z and (arithMode = 1);
                P0715(0, l3var10z.i);
            until (SY <> SEMICOLON);
            checkSymAndRead(ENDSY);
            P0715(0, l3var11z.i);
            if not l3bool5z then begin
                arithMode := 2;
                disableNorm;
            end
        end else (* 20751 *) if (SY = CASESY) then begin
            caseStatement
        end else if (SY = WITHSY) then begin
            withStatement;
            (*=z-*)(q) exit q;(*=z+*)
        end; (* 20757 *)
        if (l3var4z.b) then
            lineNesting := lineNesting - 1;
        rollup(boundary);
        if (bool110z) then begin
            bool110z := false;
            goto 8888;
        end
    end
    (* 20766 *)
end; (* statement *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure outputObjFile;
var
    idx: integer;
begin
    padToLeft;
    objBufIdx := objBufIdx - 1;
    for idx to objBufIdx do
        write(CHILD, objBuffer[idx]);
    lineStartOffset := moduleOffset;
    prevOpcode := 0;
end; (* outputObjFile *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure defineRoutine;
var
    l3var1z, l3var2z, l3var3z: word;
    l3int4z: integer;
    l3idr5z: irptr;
    l3var6z, l3var7z: word;
begin
    objBufIdx := 1;
    objBuffer[objBufIdx] := [];
    curInsnTemplate := insnTemp[XTA];
    bool48z := 22 IN l2idr2z@.flags;
    lineStartOffset := moduleOffset;
    l3var1z := ;
    int92z := 2;
    expr63z := NIL;
    arithMode := 1;
    set146z := [];
    set147z := [curProcNesting+1..6];
    set148z := set147z - [minel(set147z)];
    l3var7z.m := set147z;
    int53z := 0;
    set145z := [1:15] - set147z;
    if (curProcNesting <> 1) then
        parseDecls(2);
    l2int21z := localSize;
    if (SY <> BEGINSY) then
        requiredSymErr(BEGINSY);
    if 23 IN l2idr2z@.flags then begin
        l3idr5z := l2idr2z@.argList;
        l3int4z := 3;
        if (l2idr2z@.typ <> NIL) then
        l3int4z := 4;
        while (l3idr5z <> l2idr2z) do begin
            if (l3idr5z@.cl = VARID) then begin
                l3var2z.i := l3idr5z@.typ@.size;
                if (l3var2z.i <> 1) then begin
                    form3Insn(KVTM+I14 + l3int4z,
                              KVTM+I12 + l3var2z.i,
                              KVTM+I11 + l3idr5z@.value);
                    formAndAlign(getHelperProc(73)); (* "P/LNGPAR" *)
                end
            end;
            l3int4z := l3int4z + 1;
            l3idr5z := l3idr5z@.list;
        end
    end; (* 21105 *)
    if checkBounds or not (NoStackCheck IN optSflags.m) then
        P0715(-1, 95); (* P/SC *)
    l3var2z.i := lineNesting;
    repeat
        statement;
        if (SY = SEMICOLON) then begin
            if (curProcNesting = 1) then
                requiredSymErr(PERIOD);
            inSymbol;
            l2bool8z := (SY IN blockBegSys);
            if not l2bool8z and not errors then
                error(84); (* errErrorInDeclarations *)
        end else begin
            if (SY = PERIOD) and (curProcNesting = 1) then
                l2bool8z := true
            else (q) begin
                errAndSkip(errBadSymbol, skipToSet);
                l2bool8z := (SY IN blockBegSys);
                (*=z-*)exit q(*=z+*)
            end
        end;
    until l2bool8z;
    l2idr2z@.flags := (set145z * [0:15]) + (l2idr2z@.flags - l3var7z.m);
    lineNesting := l3var2z.i - 1;
    if (int53z <> 0) then
        P0715(0, int53z);
    if not bool48z and not doPMD and (l2int21z = 3) and
       (curProcNesting <> 1) and (set145z * [1:15] <> [1:15]) then begin
        objBuffer[1] := [7:11,21:23,28,31];
        with l2idr2z@ do
            flags := flags + [25];
        if (objBufIdx = 2) then begin
            objBuffer[1] := [0,1,3:5];
            putLeft := true;
        end else begin
            l2idr2z@.pos := l3var1z.i;
            if 13 IN set145z then begin
                curVal.i := minel([1:15] - set145z);
                besm(ASN64-24);
                l3var7z := ;
                objBuffer[2] := objBuffer[2] + [0,1,3,6,9] + l3var7z.m;
            end else begin
                curVal.i := (13);
            end;
            form1Insn(insnTemp[UJ] + indexreg[curVal.i]);
        end
    end else (* 21220 *) begin
        if (l2int11z = 0) then
            jj := 27    (* P/E *)
        else
            jj := 28;   (* P/EF *)
        form1Insn(getHelperProc(jj) + (-I13-100000B));
        if (curProcNesting = 1) then begin
            parseDecls(2);
            if S3 IN optSflags.m then
                formAndAlign(getHelperProc(78)); (* "P/PMDSET" *)
            form1Insn(insnTemp[UJ] + l3var1z.i);
            curVal.i := l2idr2z@.pos - 40000B;
            symTab[74002B] := [24,29] + (curVal.m * halfWord);
        end;
        curVal.i := l2int21z;
        if (curProcNesting <> 1) then begin
            curVal.i := curVal.i - 2;
            l3var7z := curVal;
            besm(ASN64-24);
            l3var7z := ;
            objBuffer[savedObjIdx] := objBuffer[savedObjIdx] +
                                       l3var7z.m + [0,1,2,3,4,6,8];
        end
    end; (* 21261 *)
    outputObjFile;
end; (* defineRoutine *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure initScalars;
var
    l3var1z, noProgram, l3var3z, l3var4z: word;
    l3var5z, l3var6z: integer;
    l3var7z: irptr;
    l3var8z, l3var9z: integer;
    temptype: tptr;
    l3var11z: word;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure regSysType(l4arg1z:integer; l4arg2z: tptr);
begin
    new(curIdRec = 5);
    curIdRec@ := [l4arg1z, 0, , l4arg2z, TYPEID];
    addToHashTab(curIdRec);
end; (* regSysType *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure regSysEnum(l4arg1z: integer; l4arg2z: integer);
begin
    new(curIdRec = 7);
    curIdRec@ := [l4arg1z, 48, , temptype, ENUMID, NIL, l4arg2z];
    addToHashTab(curIdRec);
end; (* regSysEnum *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure regSysProc(l4arg1z: integer);
begin
    new(curIdRec = 6);
    curIdRec@ := [l4arg1z, 0, , temptype, ROUTINEID, l3var9z];
    l3var9z := l3var9z + 1;
    addToHashTab(curIdRec);
end; (* registerSysProc *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* initScalars *)
    new(booleanType, kindScalar);
    with booleanType@ do begin
        size := 1;
        bits := 1;
        k := kindScalar;
        numen := 2;
        start := 0;
    end;
    new(integerType, kindScalar);
    with integerType@ do begin
        size := 1;
        bits := 48;
        k := kindScalar;
        numen := 100000;
        start := -1;
        enums := NIL;
    end;
    new(charType, kindScalar);
    with charType@ do begin
        size := 1;
        bits := (8);
        k := kindScalar;
        numen := 256;
        start := -1;
        enums := NIL;
    end;
    new(realType, kindArray);   (* could use kindReal to save 5 words *)
    with realType@ do begin
        size := 1;
        bits := 48;
        k := kindReal;
    end;
    new(setType, kindSet);
    with setType@ do begin
        size := 1;
        bits := 48;
        k := kindSet;
        base := integerType;
    end;
    new(pointerType, kindPtr);
    with pointerType@ do begin
        size := 1;
        bits := 48;
        k := kindPtr;
        base := pointerType;
    end;
    new(textType, kindFile);
    with textType@ do begin
        size := 30;
        bits := 48;
        k := kindFile;
        base := charType;
        elsize := 8;
    end;
    new(alfaType,kindArray);
    with alfaType@ do begin
        size := 1;
        bits := 48;
        k := kindArray;
        base := charType;
        range := temptype;
        pck := true;
        perword := 6;
        pcksize := 8;
    end;
    smallStringType[6] := alfaType;
    regSysType(51566445474562C(*" INTEGER"*), integerType);
    regSysType(42575754454156C(*" BOOLEAN"*), booleanType);
    regSysType(43504162C(*"    CHAR"*), charType);
    regSysType(62454154C(*"    REAL"*), realType);
    regSysType(41544641C(*"    ALFA"*), alfaType);
    regSysType(64457064C(*"    TEXT"*), textType);
    temptype := booleanType;
    regSysEnum(64626545C(*"    TRUE"*), (1C));
    hashTravPtr := curIdRec;
    regSysEnum(4641546345C(*"   FALSE"*), (0C));
    curIdRec@.list := hashTravPtr;
    booleanType@.enums := curIdRec;
    maxSmallString := 0;
    for strLen := 2 to 5 do
        makeStringType(smallStringType[strLen]);
    maxSmallString := 6;
    new(curIdRec = 7);
    with curIdRec@ do begin
        offset := 0;
        typ := integerType;
        cl := VARID;
        list := NIL;
        value := 7;
    end;
    new(uVarPtr);
    with uVarPtr@ do begin
        typ := integerType;
        op := GETVAR;
        id1 := curIdRec;
    end;
    new(uProcPtr, 12);
    with uProcPtr@ do begin
        typ := NIL;
        list := NIL;
        argList := NIL;
        preDefLink := NIL;
        pos := 0;
    end;
    temptype := NIL;
    l3var9z := 0;
    for l3var5z := 0 to 28 do
        regSysProc(systemProcNames[l3var5z]);
    l3var9z := 0;
    temptype := realType;
    regSysProc(63616264C(*"    SQRT"*));
    regSysProc(635156C(*"     SIN"*));
    regSysProc(435763C(*"     COS"*));
    regSysProc(416243644156C(*"  ARCTAN"*));
    regSysProc(416243635156C(*"  ARCSIN"*));
    regSysProc(5456C(*"      LN"*));
    regSysProc(457060C(*"     EXP"*));
    regSysProc(414263C(*"     ABS"*));
    temptype := integerType;
    regSysProc(6462655643C(*"   TRUNC"*));
    temptype := booleanType;
    regSysProc(574444C(*"     ODD"*));
    temptype := integerType;
    regSysProc(576244C(*"     ORD"*));
    temptype := charType;
    regSysProc(435062C(*"     CHR"*));
    regSysProc(63654343C(*"    SUCC"*));
    regSysProc(60624544C(*"    PRED"*));
    temptype := booleanType;
    regSysProc(455746C(*"     EOF"*));
    temptype := pointerType;
    regSysProc(624546C(*"     REF"*));
    temptype := booleanType;
    regSysProc(45575456C(*"    EOLN"*));
    temptype := integerType;
    regSysProc(636162C(*"     SQR"*));
    regSysProc(6257655644C(*"   ROUND"*));
    regSysProc(43416244C(*"    CARD"*));
    regSysProc(5551564554C(*"   MINEL"*));
    temptype := pointerType;
    regSysProc(606462C(*"     PTR"*));
    l3var11z.i := 30;
    l3var11z.m := l3var11z.m * halfWord + [24,27,28,29];
    new(programObj, 12);
    curVal.i := 576564606564C(*"  OUTPUT"*);
    l3var3z := curVal;
    curVal.i := 5156606564C(*"   INPUT"*);
    l3var4z := curVal;
    curVal.i := 5657606257476241C(*"NOPROGRA"*);
    noProgram := curVal;
    test1(PROGRAMSY, (skipToSet + [IDENT,LPAREN]));
    symTabPos := 74004B;
    with programObj@ do begin
        if (SY = IDENT) then begin
            curVal := curIdent;
            id := ;
            pos := 0;
            symTab[74000B] := makeNameWithStars(true);
        end else begin
            id.m := [3];
            error(errNoIdent);
            skip(skipToSet + [LPAREN]);
        end;
    end;
    if (curIdent <> noProgram) then begin
        entryPtTable[1] := symTab[74000B];
        entryPtTable[3] :=
            [0,1,6,7,10,12,14:18,21:25,28,30,35,36,38,39,41];(*"PROGRAM "*)
        entryPtTable[2] := [1];
        entryPtTable[4] := [1];
        entryPtCnt := 5;
        write(CHILD, [0,4,6,9:12,23,28,29,33:36,46]);(*10 24 74001 00 30 74002*)
        moduleOffset := 40001B;
    end else begin
        entryPtCnt := 1;
        moduleOffset := 40000B;
    end;
    programObj@.argList := NIL;
    programObj@.flags := [];
    objBufIdx := 1;
    temptype := integerType;
    defineRange(temptype, 1, 6);
    alfaType@.range := temptype;
    int93z := 0;
    inSymbol;
    test1(LPAREN, skipToSet + [IDENT]);
    outputObjFile;
    outputFile := NIL;
    inputFile := NIL;
    externFileList := NIL;
    new(l3var7z, 12);
    lineStartOffset := moduleOffset;
    with l3var7z@ do begin
        id := l3var3z;
        offset := 0;
        typ := textType;
        cl := VARID;
        list := NIL;
    end;
    curVal.i := 1257656460656412C(*"*OUTPUT*"*);
    l3var7z@.value := allocExtSymbol(l3var11z.m);
    addToHashTab(l3var7z);
    l3var5z := 1;
    while SY = IDENT do begin
        l3var8z := 0;
        curVal := curIdent;
        l3var1z.m := makeNameWithStars(false);
        if (curIdent = l3var4z) then begin
            new(inputFile, 12);
            with inputFile@ do begin
                id := curIdent;
                offset := 0;
                typ := textType;
                cl := VARID;
                list := NIL;
            end;
            curVal := l3var1z;
            inputFile@.value := allocExtSymbol(l3var11z.m);
            addToHashTab(inputFile);
            l3var8z := lineCnt;
        end else if (curIdent = l3var3z) then begin
            outputFile := l3var7z;
            l3var8z := lineCnt;
        end; (* 21745 *)
        curExternFile := externFileList;
        while (curExternFile <> NIL) do begin
            if (curExternFile@.id = curIdent) then begin
                curExternFile := NIL;
                error(errIdentAlreadyDefined);
            end else begin
                curExternFile := curExternFile@.next;
            end;
        end; (* 21760 *)
        new(curExternFile);
        with curExternFile@ do begin
            id := curIdent;
            next := externFileList;
            line := l3var8z;
            offset := l3var1z.i;
        end;
        if l3var8z <> 0 then begin
            if (curIdent = l3var3z) then begin
                fileForOutput := curExternFile;
            end else begin
                fileForInput := curExternFile;
            end
        end;
        externFileList := curExternFile;
        l3var6z := l3var5z;
        l3var5z := l3var5z + 1;
        inSymbol;
        if (charClass = MUL) then begin
            l3var6z := l3var6z + 64;
            inSymbol;
        end;
        if (SY = INTCONST) then begin
            l3var6z := 1000B * curToken.i + l3var6z;
            if (suffix = noSuffix) and
               (1 < curToken.i) and
               (curToken.i < 127) then begin
                l3var6z := l3var6z + 128;
            end else if (suffix = suffixB) and
                      (1000000B < curToken.i) and
                      (curToken.i < 1743671743B) then begin
                l3var6z := l3var6z + 256;
            end else (q) begin
                error(76); (* errWrongNumberForExternalFile *)
                (*=z-*)exit q(*=z+*)
            end;
            inSymbol;
        end else begin
            l3var6z := 512;
        end;
        curExternFile@.location := l3var6z;
        if (SY = COMMA) then
            inSymbol;
    end; (* 22042 *)
    checkSymAndRead(RPAREN);
    checkSymAndRead(SEMICOLON);
    if (outputFile = NIL) then begin
        error(77); (* errNoOutput *)
        outputFile := l3var7z;
    end;
    l3var6z := 40;
    repeat
        programme(l3var6z, programObj);
    until (SY = PERIOD);
    if (CH <> 'D') then begin
        int92z := 0;
        int93z := ;
    end else begin
        set147z := halfWord;
        dataCheck := false;
        statement;
    end;
    readToPos80;
    curVal.i := l3var6z;
    symTab[74003B] := (helperNames[25] + [24,27,28,29]) +
                        (curVal.m * halfWord);
end; (* initScalars *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure makeExtFile;
begin
    new(l2var10z);
    with l2var10z@ do begin
        typ := ptr(ord(curExternFile));
        id2 := workidr;
        expr1 := curExpr;
    end;
    curExpr := l2var10z;
end; (* makeExtFile *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure parseParameters;
var
    l3var1z, l3var2z, l3var3z: irptr;
    parClass: idclass;
    l3var5z, l3var6z: integer;
    l3sym7z: symbol;
    noComma: boolean;
    expType: tptr;
begin
    int92z := 0;
    l3var5z := 0;
    int93z := 0;
    inSymbol;
    l3var2z := NIL;
    if not (SY IN [IDENT,VARSY,FUNCSY,PROCSY]) then
        errAndSkip(errBadSymbol, (skipToSet + [IDENT,RPAREN]));
    int92z := 1;
    while (SY IN [IDENT,VARSY,FUNCSY,PROCSY]) do begin
        l3sym7z := SY;
        if (SY = IDENT) then
            parClass := VARID
        else if (SY = VARSY) then
            parClass := FORMALID
        else begin
            parClass := ROUTINEID;
            (*=z-*)(q) exit q;(*=z+*)
        end;
        l3var3z := NIL;
        if (SY = PROCSY) then
            expType := NIL
        else
            expType := integerType;
        l3var6z := 0;
        if (SY <> IDENT) then begin
            int93z := 0;
            inSymbol;
        end;
        repeat if (SY = IDENT) then begin
            if (isDefined) then
                error(errIdentAlreadyDefined);
            l3var6z := l3var6z + 1;
            new(l3var1z, FORMALID);
            with l3var1z@ do begin
                id := curIdent;
                offset := curFrameRegTemplate;
                cl := parClass;
                next := symHashTabBase[bucket];
                typ := NIL;
                list := curIdRec;
                value := l2int18z;
            end;
            symHashTabBase[bucket] := l3var1z;
            l2int18z := l2int18z + 1;
            if (l3var2z = NIL) then
                curIdRec@.argList := l3var1z
            else
                l3var2z@.list := l3var1z;
            l3var2z := l3var1z;
            if (l3var3z = NIL) then
                l3var3z := l3var1z;
            inSymbol;
        end else
            errAndSkip(errNoIdent, skipToSet + [RPAREN,COMMA,COLON]);
        noComma := (SY <> COMMA);
        if not noComma then begin
            int93z := 0;
            inSymbol;
        end;
        until noComma;
        if (l3sym7z <> PROCSY) then begin
            checkSymAndRead(COLON);
            parseTypeRef(expType, (skipToSet + [IDENT,RPAREN]));
            if (l3sym7z <> VARSY) then begin
                if (isFileType(expType)) then
                error(5) (*errSimpleTypeReq *)
                else if (expType@.size <> 1) then
                     l3var5z := l3var6z * expType@.size + l3var5z;
            end;
            if (l3var3z <> NIL) then
                while (l3var3z <> curIdRec) do with l3var3z@ do begin
                    typ := expType;
                    l3var3z := list;
                end;
        end;

        if (SY = SEMICOLON) then begin
            int93z := 0;
            inSymbol;
            if not (SY IN (skipToSet + [IDENT,VARSY,FUNCSY,PROCSY])) then
                errAndSkip(errBadSymbol, (skipToSet + [IDENT,RPAREN]));
        end;
    end;
    (* 22276 *)
    if (l3var5z <> 0) then begin
        curIdRec@.flags := (curIdRec@.flags + [23]);
        l3var6z := l2int18z;
        l2int18z := l2int18z + l3var5z;
        l3var2z := curIdRec@.argList;
        (* 22306 *)
        while (l3var2z <> curIdRec) do begin
            if (l3var2z@.cl = VARID) then begin
                l3var5z := l3var2z@.typ@.size;
                if (l3var5z <> 1) then begin
                    l3var2z@.value := l3var6z;
                    l3var6z := l3var6z + l3var5z;
                end
            end;
            l3var2z := l3var2z@.list;
        end;
    end;
    (* 22322 *)
    checkSymAndRead (RPAREN);
end; (* parseParameters *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure exitScope(var arg: array [0..127] of irptr);
begin
    for ii := 0 to 127 do begin
        workidr := arg[ii];
        while (workidr <> NIL) and
              (workidr >= scopeBound) do
            workidr := workidr@.next;
        arg[ii] := workidr;
    end;
end; (* exitScope *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
begin (* programme *)
    localSize := l2arg1z;
    if (localSize = 0) then begin
        inSymbol;
        initScalars;
        exit;
    end;
    preDefHead := ptr(0);
    inTypeDef := false;
    l2int11z := 0;
    strLabList := NIL;
    lineNesting := lineNesting + 1;
    l2var16z := numLabList;
    repeat
    if (SY = LABELSY) then begin
        (*22367*)
        repeat
            inSymbol;
            if (SY <> INTCONST) then begin
                requiredSymErr(INTCONST);
                goto 22421;
            end;
            l2var15z := numLabList;
            while (l2var15z <> l2var16z) do begin
                if (l2var15z@.id <> curToken) then begin
                    l2var15z := l2var15z@.next;
                end else begin
                    int97z := l2var15z@.line;
                    error(17); (* errLblAlreadyDefinedInLine *)
                    goto 22420;
                end
            end;
            new(l2var15z);
            with l2var15z@ do begin
                id := curToken;
                frame := curFrameRegTemplate;
                offset := 0;
                line := lineCnt;
                defined := false;
                next := numLabList;
            end;
            numLabList := l2var15z;
22420:      inSymbol;
22421:      if not (SY IN [COMMA,SEMICOLON]) then
                errAndSkip(1, skipToSet + [COMMA,SEMICOLON]);
        until SY <> COMMA;
        if SY = SEMICOLON then
            inSymbol;
    end; (* 22432 *)
    if (SY = CONSTSY) then begin
        parseDecls(0);
        while  (SY = IDENT) do begin
            if (isDefined) then
                error(errIdentAlreadyDefined);
            new(workidr=7);
            workidr@ := [curIdent, curFrameRegTemplate,
                           symHashTabBase[bucket], , ENUMID, NIL];
            symHashTabBase[bucket] := workidr;
            inSymbol;
            if (charClass <> EQOP) then
                error(errBadSymbol)
            else
                inSymbol;
            with workidr@ do
                parseLiteral(typ, high, true);
            if (workidr@.typ = NIL) then begin
                error(errNoConstant);
                workidr@.typ := integerType;
                workidr@.value := 1;
            end else
                inSymbol;
            if (SY = SEMICOLON) then begin
                int93z := 0;
                inSymbol;
                if not (SY IN (skipToSet + [IDENT])) then begin
                    errAndSkip(errBadSymbol, skipToSet + [IDENT]);
                end
            end else begin
                requiredSymErr(SEMICOLON);
            end
        end
    end; (* 22511 *)
    objBufIdx := 1;
    if (SY = TYPESY) then begin
        inTypeDef := true;
        typelist := NIL;
        parseDecls(0);
        while SY = IDENT do begin
            if isDefined then
                error(errIdentAlreadyDefined);
            ii := bucket;
            l2var12z := curIdent;
            inSymbol;
            if (charClass <> EQOP) then
                error(errBadSymbol)
            else
                inSymbol;
            parseTypeRef(l2typ13z, skipToSet + [SEMICOLON]);
            curIdent := l2var12z;
            if (knownInType(curIdRec)) then begin
                l2typ14z := curIdRec@.typ;
                if (l2typ14z@.base = booleanType) then begin
                    if (l2typ13z@.k <> kindPtr) then begin
                        parseDecls(1);
                        error(78); (* errPredefinedAsPointer *)
                    end;
                    l2typ14z@.base := l2typ13z@.base;
                end else begin
                    l2typ14z@.base := l2typ13z;
                    curIdRec@.typ := l2typ13z;
                end;
                P2672(typelist, curIdRec);
            end else begin
                new(curIdRec=5);
                with curIdRec@ do begin
                    id := l2var12z;
                    offset := curFrameRegTemplate;
                    typ := l2typ13z;
                    cl := TYPEID;
                end
            end; (* 22574 *)
            curIdRec@.next := symHashTabBase[ii];
            symHashTabBase[ii] := curIdRec;
            int93z := 0;
            checkSymAndRead(SEMICOLON);
        end; (* 22602 *)
        while (typelist <> NIL) do begin
            l2var12z := typelist@.id;
            curIdRec := typelist;
            parseDecls(1);
            error(79); (* errNotFullyDefined *)
            typelist := typelist@.next;
        end
    end; (* TYPESY -> 22612 *)
    inTypeDef := false;
    curExpr := NIL;
    if (SY = VARSY) then begin
        parseDecls(0);
        (*22617*)
        repeat
            workidr := NIL;
            (*22620*)
            repeat
            if (SY = IDENT) then begin
                new(curIdRec=7);
                if (isDefined) then
                    error(errIdentAlreadyDefined);
                with curIdRec@ do begin
                    id := curIdent;
                    offset := curFrameRegTemplate;
                    next := symHashTabBase[bucket];
                    cl := VARID;
                    list := NIL;
                end;
                symHashTabBase[bucket] := curIdRec;
                inSymbol;
                if (workidr = NIL) then
                    workidr := curIdRec
                else
                    l2var4z@.list := curIdRec;
                l2var4z := curIdRec;
            end else
                error(errNoIdent);
            if not (SY IN [COMMA,COLON]) then
                errAndSkip(1, skipToSet + [IDENT,COMMA]);
            l2bool8z := SY <> COMMA;
            if not l2bool8z then begin
                int93z := 0;
                inSymbol;
            end;
            (* 22663 -> 22620 *) until l2bool8z;
            checkSymAndRead(COLON);
            parseTypeRef(l2typ13z, skipToSet + [IDENT,SEMICOLON]);
            jj := l2typ13z@.size;
            while workidr <> NIL do with workidr@ do begin
                curIdRec := list;
                typ := l2typ13z;
                list := NIL;
                l2bool8z := true;
                if (curProcNesting = 1) then begin
                    curExternFile := externFileList;
                    l2var12z := id;
                    curVal.i := jj;
                    toAlloc := curVal.m * halfWord + [24,27,28,29];
                    while l2bool8z and (curExternFile <> NIL) do begin
                        if (curExternFile@.id = l2var12z) then begin
                            l2bool8z := false;
                            if (curExternFile@.line = 0) then begin
                                curVal.i := curExternFile@.offset;
                                workidr@.value := allocExtSymbol(toAlloc);
                                curExternFile@.line := lineCnt;
                            end
                        end else begin
                            curExternFile := curExternFile@.next;
                        end
                    end
                end; (* 22731 *)
                if (l2bool8z) then begin
                    workidr@.value := localSize;
                    if (PASINFOR.listMode = 3) then begin
                        write('VARIABLE ':25);
                        printTextWord(workidr@.id);
                        writeln(' OFFSET (', curProcNesting:0, ') ',
                                localSize:5 oct, 'B. WORDS=',
                                jj:5 oct, 'B');
                    end;
                    localSize := localSize + jj;
                    curExternFile := NIL;
                end; (*22764*)
                if isFileType(l2typ13z) then
                    makeExtFile;
                workidr := curIdRec;
            end; (* 22771 *)
            int93z := 0;
            checkSymAndRead(SEMICOLON);
            if (SY <> IDENT) and not (SY IN skipToSet) then
                errAndSkip(errBadSymbol, skipToSet + [IDENT]);
        (* 23001 -> 22617 *) until SY <> IDENT;
    end; (* VARSY -> 23003 *)
    if (curProcNesting = 1) then begin
        workidr := outputFile;
        curExternFile := fileForOutput;
        makeExtFile;
        if (inputFile <> NIL) then begin
            workidr := inputFile;
            curExternFile := fileForInput;
            makeExtFile;
        end
    end;
    if (curExpr <> NIL) then begin
        l2int11z := moduleOffset;
        formOperator(gen14);
    end else
        l2int11z := 0;
    if (curProcNesting = 1) then begin
        curExternFile := externFileList;
        while (curExternFile <> NIL) do begin
            if (curExternFile@.line = 0) then begin
                error(80); (* errUndefinedExternFile *)
                printTextWord(curExternFile@.id);
                writeLN;
            end;
            curExternFile := curExternFile@.next;
        end
    end; (*23035*)
    outputObjFile;
    while (SY = PROCSY) or (SY = FUNCSY) do begin
        l2bool8z := SY = PROCSY;
        if (curFrameRegTemplate = 7) then begin
            error(81); (* errProcNestingTooDeep *)
        end;
        int93z := 0;
        inSymbol;
        if (SY <> IDENT) then begin
            error(errNoIdent);
            curIdRec := uProcPtr;
            isPredefined := false;
        end else begin
            if (isDefined) then with hashTravPtr@ do begin
                if (cl = ROUTINEID) and
                   (list = NIL) and
                   (preDefLink <> NIL) and
                   ((typ = NIL) = l2bool8z) then begin
                    isPredefined := true;
                end else begin
                    isPredefined := false;
                    error(errIdentAlreadyDefined);
                    printErrMsg(82); (* errPrevDeclWasNotForward *)
                end;
            end else
                isPredefined := false;
        end; (* 23103 *)
        if not isPredefined then begin
            new(curIdRec, 12);
            with curIdRec@ do begin
                id := curIdent;
                offset := curFrameRegTemplate;
                next := symHashTabBase[bucket];
                typ := NIL;
                symHashTabBase[bucket] := curIdRec;
                cl := ROUTINEID;
                list := NIL;
                value := 0;
                argList := NIL;
                preDefLink := NIL;
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
            end;
            curProcNesting := curProcNesting + 1;
            inSymbol;
            if (6 < curProcNesting) then
                error(81); (* errProcNestingTooDeep *)
            if not (SY IN [LPAREN,SEMICOLON,COLON]) then
                errAndSkip(errBadSymbol, skipToSet + [LPAREN,SEMICOLON,COLON]);
            if (SY = LPAREN) then
                parseParameters;
            if not l2bool8z then begin
                if (SY <> COLON) then
                    errAndSkip(106 (*:*), skipToSet + [SEMICOLON])
                else begin
                    inSymbol;
                    parseTypeRef(curIdRec@.typ, skipToSet + [SEMICOLON]);
                    if (curIdRec@.typ@.size <> 1) then
                        error(errTypeMustNotBeFile);
                end
            end;
        end else (*23167*) begin
            with hashTravPtr@ do begin
                l2int18z := level;
                curFrameRegTemplate := curFrameRegTemplate + indexreg[1];
                curProcNesting := curProcNesting + 1;
                if (preDefHead = hashTravPtr) then begin
                    preDefHead := preDefLink;
                end else begin
                    curIdRec := preDefHead;
                    while (hashTravPtr <> curIdRec) do begin
                        workidr := curIdRec;
                        curIdRec := curIdRec@.preDefLink;
                    end;
                    workidr@.preDefLink := hashTravPtr@.preDefLink;
                end
            end;
            hashTravPtr@.preDefLink := NIL;
            curIdRec := hashTravPtr@.argList;
            if (curIdRec <> NIL) then begin
                while (curIdRec <> hashTravPtr) do begin
                    addToHashTab(curIdRec);
                    curIdRec := curIdRec@.list;
                end
            end;
            curIdRec := hashTravPtr;
            setup(scopeBound);
            inSymbol;
        end; (* 23224 *)
        checkSymAndRead(SEMICOLON);
        with curIdRec@ do if (curIdent = litForward) then begin
            if (isPredefined) then
                error(83); (* errRepeatedPredefinition *)
            level := l2int18z;
            preDefLink := preDefHead;
            preDefHead := curIdRec;
        end else (* 23237 *) if (curIdent = litExternal) or
            (curIdent = litFortran) then begin
            if (curIdent = litExternal) then begin
                curVal.m := [20];
            end else if (checkFortran) then begin
                curVal.m := [21,24];
                checkFortran := false;
            end else begin
                curVal.m := [21];
                (*=z-*)(q) exit q;(*=z+*)
            end;
            curIdRec@.flags := curIdRec@.flags + curVal.m;
        end else (* 23257 *) begin
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
        end; (* 23277 *)
        inSymbol;
        checkSymAndRead(SEMICOLON);
23301:  workidr := curIdRec@.argList;
        if (workidr <> NIL) then begin
            while (workidr <> curIdRec) do begin
                scopeBound := NIL;
                P2672(scopeBound, workidr);
                workidr := workidr@.list;
            end
        end; (* 23314 *)
        curFrameRegTemplate := curFrameRegTemplate - indexreg[1];
        curProcNesting := curProcNesting - 1;
    end; (* 23320 *)
    if (SY <> BEGINSY) and
       (not allowCompat or not (SY IN blockBegSys)) then
        errAndSkip(84 (* errErrorInDeclarations *), skipToSet);
    until SY in statBegSys;
    if (preDefHead <> ptr(0)) then begin
        error(85); (* errNotFullyDefinedProcedures *)
        while (preDefHead <> ptr(0)) do begin
            printTextWord(preDefHead@.id);
            preDefHead := preDefHead@.preDefLink;
        end;
        writeLN;
    end;
    defineRoutine;
    while (numLabList <> l2var16z) do begin
        if not (numLabList@.defined) then begin
            write(' ', numLabList@.id.i:0, ':');
            l2bool8z := false;
        end;
        numLabList := numLabList@.next;
    end;
    if not l2bool8z then begin
        printTextWord(l2idr2z@.id);
        error(90); (* errLblDefinitionInBlock *)
    end;
    l2arg1z := l2int21z;
    (* 23364 *)
end; (* programme *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure initTables;
var
    idx, jdx: integer;
    l2unu3z, l2unu4z, l2unu5z: word;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure initInsnTemplates;
var
    l3var1z: insn;
    l3var2z: operator;
begin
    for l3var1z := ATX to JADDM do
        insnTemp[l3var1z] := ord(l3var1z) * 10000B;
    insnTemp[ELFUN] := 500000B;
    jdx := KUTC;
    for l3var1z := UTC to VJM do begin
        insnTemp[l3var1z] := jdx;
        jdx := (jdx + 100000B);
    end;
    for idx to 15 do
        indexreg[idx] := idx * frameRegTemplate;
    jumpType := insnTemp[UJ];
    for l3var2z := MUL to ASSIGNOP do begin
        opFlags[l3var2z] := opfCOMM;
        opToInsn[l3var2z] := 0;
        if (l3var2z IN [MUL, RDIVOP, PLUSOP, MINUSOP]) then begin
            opToMode[l3var2z] := 3;
        end else if (l3var2z IN [IDIVOP, IMODOP]) then begin
            opToMode[l3var2z] := 2;
        end else if (l3var2z IN [IMULOP, INTPLUS, INTMINUS, badop27]) then begin
            opToMode[l3var2z] := 1;
        end else if (l3var2z IN [IDIVROP,badop30,badop31]) then begin
            opToMode[l3var2z] := 4;
        end else (q) begin
            opToMode[l3var2z] := 0;
            (*=z-*)exit q(*=z+*)
        end
    end;
    opToInsn[MUL] := insnTemp[AMULX];
    opToInsn[RDIVOP] := insnTemp[ADIVX];
    opToInsn[IDIVOP] := 17; (* P/DI *)
    opToInsn[IMODOP] := 11; (* P/MD *)
    opToInsn[PLUSOP] := insnTemp[ADD];
    opToInsn[MINUSOP] := insnTemp[SUB];
    opToInsn[IMULOP] := insnTemp[AMULX];
    opToInsn[SETAND] := insnTemp[AAX];
    opToInsn[SETXOR] := insnTemp[AEX];
    opToInsn[SETOR] := insnTemp[AOX];
    opToInsn[INTPLUS] := insnTemp[ADD];
    opToInsn[INTMINUS] := insnTemp[SUB];
    opToInsn[IDIVROP] := 67; (* P/IS *)
    opToInsn[badop27] := 22; (* P/II unused, undefined *)
    opToInsn[badop30] := 23; (* P/RR *)
    opToInsn[badop31] := 24; (* P/RI *)
    opToInsn[MKRANGE] := 61; (* P/PI *)
    opToInsn[SETSUB] := insnTemp[AAX];
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
    for jdx := 0 to 6 do begin
        funcInsn[jdx] := (500000B + jdx);
    end
(* 23516 *)end; (* initInsnTemplates *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure regKeywords;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure regResWord(l4arg1z: integer);
var
    kw: @kword;
    l4var2z: word;
begin
    curVal.i := l4arg1z;
    curVal.m := curVal.m * hashMask.m;
    mapai(curVal.a, curVal.i);
    l4var2z.i := l4arg1z;
    new(kw);
    with kw@ do begin
        w := l4var2z;
        sym := SY;
        op := charClass;
        next := kwordHashTabBase[curVal.i];
    end;
    kwordHashTabBase[curVal.i] := kw;
    if (charClass = NOOP) then begin
        SY := succ(SY);
    end else begin
        charClass := succ(charClass);
    end
end; (* regResWord *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* regKeywords *)
    SY := MULOP;
    charClass := AMPERS;
    regResWord(415644C(*"     AND"*));
    regResWord(445166C(*"     DIV"*));
    regResWord(555744C(*"     MOD"*));
    SY := GTSY; (* reused as NILSY *)
    charClass := NOOP;
    regResWord(565154C(*"     NIL"*));
    SY := ADDOP;
    charClass := OROP;
    regResWord(5762C(*"      OR"*));
    SY := RELOP;
    charClass := INOP;
    regResWord(5156C(*"      IN"*));
    SY := NOTSY;
    charClass := NOOP;
    regResWord(565764C(*"     NOT"*));
    SY := LABELSY;
    charClass := NOOP;
    for idx := 0 to 29 do
        regResWord(resWordNameBase[idx]);
end; (* regKeywords *)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure initArrays;
var
    l3var1z, l3var2z: word;
begin
    FcstCnt := 0;
    FcstCount := 0;
    for idx := 3 to 6 do begin
        l3var2z.i := (idx - (2));
        for jdx to l3var2z.i do
            frameRestore[idx][jdx] := 0;
    end;
    for idx to 99 do
        helperMap[idx] := 0;
end; (* initArrays *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure initSets;
begin
    skipToSet := blockBegSys + statBegSys - [CASESY];
    bigSkipSet := skipToSet + statEndSys;
end; (* initSets *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin (* initTables *)
    initArrays;
    initInsnTemplates;
    initSets;
    unpack(pasinfor.a3@, iso2text, '_052'); (* '*' *)
    rewrite(CHILD);
    for jdx to 10 do
        put(CHILD);
    for idx := 0 to 127 do begin
        symHashTabBase[idx] := NIL;
        typeHashTabBase[idx] := ;
        kwordHashTabBase[idx] := ;
    end;
    regKeywords;
    numLabList := NIL;
    totalErrors := 0;
    heapCallsCnt := 0;
    putLeft := true;
    bool102z := true;
    curFrameRegTemplate := frameRegTemplate;
    curProcNesting := 1;
end; (* initTables *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure finalize;
var
    idx, cnt, unused: integer;
    sizes: array [1..10] of @integer;
begin
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
    while not eof(FCST) do begin
        write(CHILD, FCST@);
        get(FCST);
    end;
    curVal.i := (symTabPos - 70000B) * 100000000B;
    for cnt to longSymCnt do begin
        idx := longSymTabBase[cnt];
        symTab[idx] := (symTab[idx] + (curVal.m * [9:23]));
        curVal.i := (curVal.i + 100000000B);
    end;
    symTabPos := (symTabPos - (1));
    for cnt := 74000B to symTabPos do
        write(CHILD, symTab[cnt]);
    for cnt to longSymCnt do
        write(CHILD, longSyms[cnt]);
    if (allowCompat) then begin
        write((lineCnt - 1):6, ' LINES STRUCTURE ');
        for idx to 10 do
            write(ord(sizes[idx]):0, ' ');
        writeln;
    end;
    entryPtTable[entryPtCnt] := [];
    pasinfor.entryptr@ := entryPtTable;
    pasinfor.sizes := sizes;
end; (* finalize *)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procedure initOptions;
begin
    pasinfor.startOffset := pasinfor.startOffset - 16384;
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
    curVal.m := pasinfor.flags;
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
    chain := NIL;
    litOct.i := 574364C;
    longSymCnt := 0;
    pasinfor.errors@ := true;
    extSymAdornment := 0;
    symTabCnt := 0;
end; (* initOptions *)
%
begin (* main *)
    if PASINFOR.listMode <> 0 then
        writeln(boilerplate);
    initOptions;
    curInsnTemplate := 0;
    initTables;
    programme(curInsnTemplate, hashTravPtr);
    if errors then begin
9999:   writeln(' IN ', (lineCnt-1):0, ' LINES ',
            totalErrors:0, ' ERRORS');
    end else begin
        finalize;
        PASINFOR.errors@ := false;
    end
end
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
        5441424554C             (*"   LABEL"*),
        4357566364C             (*"   CONST"*),
        64716045C               (*"    TYPE"*),
        664162C                 (*"     VAR"*),
        4665564364515756C       (*"FUNCTION"*),
        6062574345446562C       (*"PROCEDUR"*),
        634564C                 (*"     SET"*),
        604143534544C           (*"  PACKED"*),
        4162624171C             (*"   ARRAY"*),
        624543576244C           (*"  RECORD"*),
        46515445C               (*"    FILE"*),
        4245475156C             (*"   BEGIN"*),
        5146C                   (*"      IF"*),
        43416345C               (*"    CASE"*),
        624560454164C           (*"  REPEAT"*),
        6750515445C             (*"   WHILE"*),
        465762C                 (*"     FOR"*),
        67516450C               (*"    WITH"*),
        47576457C               (*"    GOTO"*),
        455644C                 (*"     END"*),
        45546345C               (*"    ELSE"*),
        6556645154C             (*"   UNTIL"*),
        5746C                   (*"      OF"*),
        4457C                   (*"      DO"*),
        6457C                   (*"      TO"*),
        445767566457C           (*"  DOWNTO"*),
        64504556C               (*"    THEN"*),
        634554454364C           (*"  SELECT"*),
        60625747624155C         (*" PROGRAM"*),
        576450456263C           (*"  OTHERS"*);
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
        6017210000000000C      (*"P/1     "*),
        6017220000000000C      (*"P/2     "*),
        6017230000000000C      (*"P/3     "*),
        6017240000000000C      (*"P/4     "*),
        6017250000000000C      (*"P/5     "*),
        6017260000000000C      (*"P/6     "*),
        6017434100000000C      (*"P/CA    "*),
        6017455700000000C      (*"P/EO    "*),
        6017636300000000C      (*"P/SS    "*),
(*10*)  6017455400000000C      (*"P/EL    "*),
        6017554400000000C      (*"P/MD    "*),
        6017555100000000C      (*"P/MI    "*),
        6017604100000000C      (*"P/PA    "*),
        6017655600000000C      (*"P/UN    "*),
        6017436000000000C      (*"P/CP    "*),
        6017414200000000C      (*"P/AB    "*),
        6017445100000000C      (*"P/DI    "*),
        6017624300000000C      (*"P/RC    "*),
        6017454100000000C      (*"P/EA    "*),
(*20*)  6017564100000000C      (*"P/NA    "*),
        6017424100000000C      (*"P/BA    "*),
        6017515100000000C      (*"P/II   u"*),
        6017626200000000C      (*"P/RR    "*),
        6017625100000000C      (*"P/RI    "*),
        6017214400000000C      (*"P/1D    "*),
        6017474400000000C      (*"P/GD    "*),
        6017450000000000C      (*"P/E     "*),
        6017454600000000C      (*"P/EF    "*),
        6017604600000000C      (*"P/PF    "*),
(*30*)  6017474600000000C      (*"P/GF    "*),
        6017644600000000C      (*"P/TF    "*),
        6017624600000000C      (*"P/RF    "*),
        6017566700000000C      (*"P/NW    "*),
        6017446300000000C      (*"P/DS    "*),
        6017506400000000C      (*"P/HT    "*),
        6017675100000000C      (*"P/WI    "*),
        6017676200000000C      (*"P/WR    "*),
        6017674300000000C      (*"P/WC    "*),
        6017412600000000C      (*"P/A6    "*),
(*40*)  6017412700000000C      (*"P/A7    "*),
        6017677000000000C      (*"P/WX    "*),
        6017675700000000C      (*"P/WO    "*),
        6017436700000000C      (*"P/CW    "*),
        6017264100000000C      (*"P/6A    "*),
        6017274100000000C      (*"P/7A    "*),
        6017675400000000C      (*"P/WL    "*),
        6017624451000000C      (*"P/RDI   "*),
        6017624462000000C      (*"P/RDR   "*),
        6017624443000000C      (*"P/RDC   "*),
(*50*)  6017624126000000C      (*"P/RA6   "*),
        6017624127000000C      (*"P/RA7   "*),
        6017627000000000C      (*"P/RX   u"*),
        6017625400000000C      (*"P/RL    "*),
        6017675754560000C      (*"P/WOLN  "*),
        6017625154560000C      (*"P/RILN  "*),
        6017626200000000C      (*"P/RR    "*),
        6017434500000000C      (*"P/CE    "*),
        6017646200000000C      (*"P/TR    "*),
        6017546600000000C      (*"P/LV    "*),
(*60*)  6017724155000000C      (*"P/ZAM  u"*),
        6017605100000000C      (*"P/PI    "*),
        6017426000000000C      (*"P/BP    "*),
        6017422600000000C      (*"P/B6    "*),
        6017604200000000C      (*"P/PB    "*),
        6017422700000000C      (*"P/B7    "*),
        6017515600000000C      (*"P/IN    "*),
        6017516300000000C      (*"P/IS    "*),
        6017444100000000C      (*"P/DA    "*),
        6017435700000000C      (*"P/CO    "*),
(*70*)  6017516400000000C      (*"P/IT    "*),
        6017435300000000C      (*"P/CK    "*),
        6017534300000000C      (*"P/KC    "*),
        6017545647604162C      (*"P/LNGPAR"*),
        6017544441620000C      (*"P/LDAR  "*),
        6017544441625156C      (*"P/LDARIN"*),
        6017202043000000C      (*"P/00C   "*),
        6017636441620000C      (*"P/STAR  "*),
        6017605544634564C      (*"P/PMDSET"*),
        6017435100000000C      (*"P/CI    "*),
(*80*)  6041514200000000C      (*"PAIB    "*),
        6017674100000000C      (*"P/WA    "*),
        6361626412000000C      (*"SQRT*   "*),
        6351561200000000C      (*"SIN*    "*),
        4357631200000000C      (*"COS*    "*),
        4162436441561200C      (*"ARCTAN* "*),
        4162436351561200C      (*"ARCSIN* "*),
        5456120000000000C      (*"LN*     "*),
        4570601200000000C      (*"EXP*    "*),
        6017456100000000C      (*"P/EQ    "*),
(*90*)  6017624100000000C      (*"P/RA    "*),
        6017474500000000C      (*"P/GE    "*),
        6017554600000000C      (*"P/MF    "*),
        6017465500000000C      (*"P/FM    "*),
        6017565600000000C      (*"P/NN    "*),
        6017634300000000C      (*"P/SC    "*),
        6017444400000000C      (*"P/DD    "*),
        6017624500000000C      (*"P/RE    "*);
    systemProcNames :=
(*0*)   606564C                (*"     PUT"*),
        474564C                (*"     GET"*),
        62456762516445C        (*" REWRITE"*),
        6245634564C            (*"   RESET"*),
        564567C                (*"     NEW"*),
        44516360576345C        (*" DISPOSE"*),
        50415464C              (*"    HALT"*),
        63645760C              (*"    STOP"*),
        6345646560C            (*"   SETUP"*),
        625754546560C          (*"  ROLLUP"*),
(*10*)  6762516445C            (*"   WRITE"*),
        67625164455456C        (*" WRITELN"*),
        62454144C              (*"    READ"*),
        624541445456C          (*"  READLN"*),
        45705164C              (*"    EXIT"*),
        4445426547C            (*"   DEBUG"*),
        42456355C              (*"    BESM"*),
        5541605141C            (*"   MAPIA"*),
        5541604151C            (*"   MAPAI"*),
        604353C                (*"     PCK"*),
(*20*)  6556604353C            (*"   UNPCK"*),
        60414353C              (*"    PACK"*),
        655660414353C          (*"  UNPACK"*),
        5760455644C            (*"   OPEND"*),
        44455444C              (*"    DELD"*),
        56456744C              (*"    NEWD"*),
        60656444C              (*"    PUTD"*),
        47456444C              (*"    GETD"*),
        55574444C              (*"    MODD"*),
        46515644C              (*"    FIND"*);
end
