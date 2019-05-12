{******************************************************************************
*                                                                             *
*                       TEST SUITE FOR BESM6 PASCAL                           *
*                                                                             *
*                               Part 1 of 3                                   *
*                                                                             *
* This program attempts to use and display the results of each feature of     *
* BESM6 pascal. It is a "positive" test in that it should compile and run     *
* error free, and thus does not check error conditions/detection.             *
*                                                                             *
* The test is based on the "PASCAL ACCEPTANCE TEST" Version 1.1,              *
* by S. A. Moore. See comments in file iso7185pat.pas for details about       *
* the original test.                                                          *
*                                                                             *
* This test was modified to match the BESM6 compiler.                         *
* Changes are listed below.                                                   *
*                                                                             *
* 1. Removed 'set of char' and 'set of 'a'..'z'' constructs, and              *
*    all dependencies. BESM-6 Pascal supports only sets of integers           *
*    in range 0..47.                                                          *
*                                                                             *
* 2. Upper bound of 'set of 1..100' reduced to 47.                            *
*                                                                             *
* 3. Removed 'array of text' constructs, and all dependencies.                *
*                                                                             *
* 4. Removed succ() and pred() with integer argument:                         *
*    not supported on BESM-6.                                                 *
*                                                                             *
* 5. Exponent in real constants limited to -18..18 range.                     *
*                                                                             *
* 6. Procedure dispose() requires a variable as argument (not expression).    *
*                                                                             *
* 7. Procedures pack() and unpack() don't accept arrays with different        *
*    types of indexes, for example array of[integer] and array of[char].      *
*                                                                             *
* 8. Upper index of FOR loop is evaluated on every iteration. Hence, the loop *
*    "for i := 1 to i" never terminates on BESM-6                             *
*                                                                             *
* 9. Test was split into three parts, to fit the limits of BESM-6 compiler.   *
*                                                                             *
******************************************************************************}

program besm6pat(output);

label
      0, 3, 9999, 0004;

const
      tcnst = 768;
      scst = 'this is a string';
      ccst = 'v';
      tsncst = -52;
      tsncst2 = -tcnst;
      tsncst3 = -tsncst;
      mmaxint = -maxint;

type
     string10 = packed array [1..10] of char;
     enum  = (one, two, three, four, five, six, seven, eight, nine, ten);
     esub  = three..six;
     subr  = 10..20;
     (* Note use of alternatives for '[' and ']'. The availablity of these
        alternates is implementation defined. *)
     arri  = array (.1..10.) of integer;
     { Note that the availability of the alternate '@' is implementation
       defined }
     iptr  = @integer;
     recs  = record

               a: integer;
               b: char

            end;
     intalias = integer;

var
    i, x, y, z: integer;
    srx, sry, srz: 0..100;
    sras, srbs, srcs, srds, sres: -100..100;
    a:  array [1..10] of integer;
    r:  record
           rx: integer;
           rc: char;
           ry: integer;
           rb: boolean;
           rs: packed array [1..10] of char;
        end;
    sa, sb, sc: packed array [1..10] of char;
    ca, cb, cc: char;
    car:   array ['a'..'z'] of integer;
    sar:   array [1..10] of packed array [1..10] of char;
    as, bs, cs, ds, es, gs, hs : integer;
    vnum: -maxint..maxint;
    esia:  array [two..six] of integer;
    pesia: packed array [two..six] of integer;
    fes:   file of esub;
    pfes:  packed file of esub;
    fs:    file of subr;
    pfs:   packed file of subr;
    fr:    file of real;
    pfr:   packed file of real;
    fst:   file of string10;
    pfst:  packed file of string10;
    fa:    file of arri;
    pfa:   packed file of arri;
    frc:   file of recs;
    pfrc:  packed file of recs;
    fp:    file of iptr;
    pfp:   packed file of iptr;
    rcastt: integer;
    rcast: record case rcastt: boolean of true: (); false: () end;
    vintalias: intalias;

procedure junk6;
begin
   goto 9999
end;

procedure part1;
begin
{******************************************************************************

                            Integers

******************************************************************************}

   writeln;
   writeln('******************* Integers *******************');
   writeln;

   { integer variables }
   x := 43; y := 78; z := y;
   writeln('Integer1:   ', x + y:1, ' s/b 121');
   writeln('Integer2:   ', y - x:1, ' s/b 35');
   writeln('Integer3:   ', x * y:1, ' s/b 3354');
   writeln('Integer4:   ', y div x:1, ' s/b 1');
   writeln('Integer5:   ', y mod x:1, ' s/b 35');
   writeln('Integer8:   ', sqr(x):1, ' s/b 1849');
   writeln('Integer9:   ', chr(y), ' s/b N');
   writeln('Integer10:  ', ord(chr(x)):1, ' s/b 43');
   writeln('Integer11:  ', odd(x):5, ' s/b true');
   writeln('Integer12:  ', odd(y):5, ' s/b false');
   writeln('Integer13:  ', z = y:5, ' s/b true');
   writeln('Integer14:  ', x = y:5, ' s/b false');
   writeln('Integer15:  ', x < y:5, ' s/b true');
   writeln('Integer16:  ', y < x:5, ' s/b false');
   writeln('Integer17:  ', y > x:5, ' s/b true');
   writeln('Integer18:  ', x > y:5, ' s/b false');
   writeln('Integer19:  ', x <> y:5, ' s/b true');
   writeln('Integer20:  ', y <> z:5, ' s/b false');
   writeln('Integer21:  ', x <= y:5, ' s/b true');
   writeln('Integer22:  ', z <= y:5, ' s/b true');
   writeln('Integer23:  ', y <= x:5, ' s/b false');
   writeln('Integer24:  ', y >= x:5, ' s/b true');
   writeln('Integer25:  ', y >= z:5, ' s/b true');
   writeln('Integer26:  ', x >= y:5, ' s/b false');

   { unsigned integer constants }
   write('Integer27:  '); i := 546; writeln(i:1, ' s/b 546');
   writeln('Integer28:  ', 56 + 34:1, ' s/b 90');
   writeln('Integer29:  ', 56 - 34:1, ' s/b 22');
   writeln('Integer30:  ', 56 * 34:1, ' s/b 1904');
   writeln('Integer31:  ', 56 div 34:1, ' s/b 1');
   writeln('Integer32:  ', 56 mod 34:1, ' s/b 22');
   writeln('Integer35:  ', sqr(7):1, ' s/b 49');
   writeln('Integer36:  ', chr(65), ' s/b A');
   writeln('Integer37:  ', ord(chr(65)):1, ' s/b 65');
   writeln('Integer38:  ', tcnst:1, ' s/b 768');
   writeln('Integer39:  ', odd(5):5, ' s/b true');
   writeln('Integer40:  ', odd(8):5, ' s/b false');
   writeln('Integer41:  ', 56 = 56:5, ' s/b true');
   writeln('Integer42:  ', 56 = 57:5, ' s/b false');
   writeln('Integer43:  ', 56 < 57:5, ' s/b true');
   writeln('Integer44:  ', 57 < 56:5, ' s/b false');
   writeln('Integer45:  ', 57 > 56:5, ' s/b true');
   writeln('Integer46:  ', 56 > 57:5, ' s/b false');
   writeln('Integer47:  ', 56 <> 57:5, ' s/b true');
   writeln('Integer48:  ', 56 <> 56:5, ' s/b false');
   writeln('Integer49:  ', 55 <= 500:5, ' s/b true');
   writeln('Integer50:  ', 67 <= 67:5, ' s/b true');
   writeln('Integer51:  ', 56 <= 33:5, ' s/b false');
   writeln('Integer52:  ', 645 >= 4:5, ' s/b true');
   writeln('Integer53:  ', 23 >= 23:5, ' s/b true');
   writeln('Integer54:  ', 45 >= 123:5, ' s/b false');

   { signed integer variables }
   as := -14;
   bs := -32;
   cs := -14;
   ds := 20;
   es := -15;
   gs := maxint;
   hs := mmaxint;
   vnum := -maxint;
   writeln('Integer55:  ', as + ds:1, ' s/b 6');
   writeln('Integer56:  ', ds + as:1, ' s/b 6');
   writeln('Integer57:  ', bs + ds:1, ' s/b -12');
   writeln('Integer58:  ', as + bs:1, ' s/b -46');
   writeln('Integer59:  ', ds - as:1, ' s/b 34');
   writeln('Integer60:  ', bs - ds:1, ' s/b -52');
   writeln('Integer61:  ', bs - as:1, ' s/b -18');
   writeln('Integer62:  ', ds * as:1, ' s/b -280');
   writeln('Integer63:  ', as * ds:1, ' s/b -280');
   writeln('Integer64:  ', as * bs:1, ' s/b 448');
   writeln('Integer65:  ', ds div as:1, ' s/b -1');
   writeln('Integer66:  ', bs div ds:1, ' s/b -1');
   writeln('Integer67:  ', bs div as:1, ' s/b 2');
   writeln('Integer70: ', sqr(as):1, ' s/b 196');
   writeln('Integer71:  ', odd(as):5, ' s/b false');
   writeln('Integer72:  ', odd(es):5, ' s/b true');
   writeln('Integer73:  ', as = cs:5, ' s/b true');
   writeln('Integer74:  ', as = bs:5, ' s/b false');
   writeln('Integer75:  ', as <> bs:5, ' s/b true');
   writeln('Integer76:  ', as <> cs:5, ' s/b false');
   writeln('Integer77:  ', as < ds:5, ' s/b true');
   writeln('Integer78:  ', bs < as:5, ' s/b true');
   writeln('Integer79:  ', ds < as:5, ' s/b false');
   writeln('Integer80:  ', as < bs:5, ' s/b false');
   writeln('Integer81:  ', ds > as:5, ' s/b true');
   writeln('Integer82:  ', as > bs:5, ' s/b true');
   writeln('Integer83:  ', as > ds:5, ' s/b false');
   writeln('Integer84:  ', bs > as:5, ' s/b false');
   writeln('Integer85:  ', as <= ds:5, ' s/b true');
   writeln('Integer86:  ', bs <= as:5, ' s/b true');
   writeln('Integer87:  ', as <= cs:5, ' s/b true');
   writeln('Integer88:  ', ds <= as:5, ' s/b false');
   writeln('Integer89:  ', as <= bs:5, ' s/b false');
   writeln('Integer90:  ', ds >= as:5, ' s/b true');
   writeln('Integer91:  ', as >= bs:5, ' s/b true');
   writeln('Integer92:  ', as >= cs:5, ' s/b true');
   writeln('Integer93:  ', as >= ds:5, ' s/b false');
   writeln('Integer94:  ', bs >= as:5, ' s/b false');
   writeln('Integer95:  ', abs(as):1, ' s/b 14');
   writeln('Integer96:  ', gs+hs:1, ' s/b 0');
   writeln('Integer97:  ', gs-maxint:1, ' s/b 0');
   writeln('Integer98:  ', gs+vnum:1, ' s/b 0');
end;

procedure part2;
begin
   { signed integer constants }
   writeln('Integer99:  ', 45 + (-30):1, ' s/b 15');
   writeln('Integer100:  ', -25 + 70:1, ' s/b 45');
   writeln('Integer101: ', -62 + 23:1, ' s/b -39');
   writeln('Integer102: ', -20 + (-15):1, ' s/b -35');
   writeln('Integer103: ', 20 - (-14):1, ' s/b 34');
   writeln('Integer104: ', -34 - 14:1, ' s/b -48');
   writeln('Integer105: ', -56 - (-12):1, ' s/b -44');
   writeln('Integer106: ', 5 * (-4):1, ' s/b -20');
   writeln('Integer107: ', (-18) * 7:1, ' s/b -126');
   writeln('Integer108: ', (-40) * (-13):1, ' s/b 520');
   writeln('Integer109: ', 30 div (-5):1, ' s/b -6');
   writeln('Integer110: ', (-50) div 2:1, ' s/b -25');
   writeln('Integer111: ', (-20) div (-4):1, ' s/b 5');
   writeln('Integer115: ', sqr(-8):1, ' s/b 64');
   writeln('Integer117: ', odd(-20):5, ' s/b false');
   writeln('Integer118: ', odd(-15):5, ' s/b true');
   writeln('Integer119: ', -5 = -5:5, ' s/b true');
   writeln('Integer120: ', -5 = 5:5, ' s/b false');
   writeln('Integer121: ', -21 <> -40:5, ' s/b true');
   writeln('Integer122: ', -21 <> -21:5, ' s/b false');
   writeln('Integer123: ', -3 < 5:5, ' s/b true');
   writeln('Integer124: ', -32 < -20:5, ' s/b true');
   writeln('Integer125: ', 20 < -20:5, ' s/b false');
   writeln('Integer126: ', -15 < -40:5, ' s/b false');
   writeln('Integer127: ', 70 > -4:5, ' s/b true');
   writeln('Integer128: ', -23 > -34:5, ' s/b true');
   writeln('Integer129: ', -5 > 5:5, ' s/b false');
   writeln('Integer130: ', -60 > -59:5, ' s/b false');
   writeln('Integer131: ', -12 <= 4:5, ' s/b true');
   writeln('Integer132: ', -14 <= -5:5, ' s/b true');
   writeln('Integer133: ', -7 <= -7:5, ' s/b true');
   writeln('Integer134: ', 5 <= -5:5, ' s/b false');
   writeln('Integer135: ', -10 <= -20:5, ' s/b false');
   writeln('Integer136: ', 9 >= -3:5, ' s/b true');
   writeln('Integer137: ', -4 >= -10:5, ' s/b true');
   writeln('Integer138: ', -13 >= -13:5, ' s/b true');
   writeln('Integer139: ', -6 >= 6:5, ' s/b false');
   writeln('Integer140: ', -20 >= -10:5, ' s/b false');
   writeln('Integer141: ', abs(-6):1, ' s/b 6');
   writeln('Integer142: ', tsncst:1, ' s/b -52');
   writeln('Integer143: ', -tsncst:1, ' s/b 52');
   writeln('Integer144: ', tsncst2:1, ' s/b -768');
   writeln('Integer145: ', tsncst3:1, ' s/b 52');
   writeln('Integer146: ', maxint+mmaxint:1, ' s/b 0');
end;

procedure part3;
begin
{******************************************************************************

                            Subranges

******************************************************************************}

   writeln;
   writeln('******************* Subranges *******************');
   writeln;

   { subrange unsigned variables }
   srx := 43; sry := 78; srz := sry;
   writeln('Subrange1:   ', srx + sry:1, ' s/b 121');
   writeln('Subrange2:   ', sry - srx:1, ' s/b 35');
   writeln('Subrange3:   ', srx * sry:1, ' s/b 3354');
   writeln('Subrange4:   ', sry div srx:1, ' s/b 1');
   writeln('Subrange5:   ', sry mod srx:1, ' s/b 35');
   writeln('Subrange8:   ', chr(sry), ' s/b N');
   writeln('Subrange9:   ', ord(chr(srx)):1, ' s/b 43');
   writeln('Subrange10:  ', odd(srx):5, ' s/b true');
   writeln('Subrange11:  ', odd(sry):5, ' s/b false');
   writeln('Subrange12:  ', srz = sry:5, ' s/b true');
   writeln('Subrange13:  ', srx = sry:5, ' s/b false');
   writeln('Subrange14:  ', srx < sry:5, ' s/b true');
   writeln('Subrange15:  ', sry < srx:5, ' s/b false');
   writeln('Subrange16:  ', sry > srx:5, ' s/b true');
   writeln('Subrange17:  ', srx > sry:5, ' s/b false');
   writeln('Subrange18:  ', srx <> sry:5, ' s/b true');
   writeln('Subrange19:  ', sry <> srz:5, ' s/b false');
   writeln('Subrange20:  ', srx <= sry:5, ' s/b true');
   writeln('Subrange21:  ', srz <= sry:5, ' s/b true');
   writeln('Subrange22:  ', sry <= srx:5, ' s/b false');
   writeln('Subrange23:  ', sry >= srx:5, ' s/b true');
   writeln('Subrange24:  ', sry >= srz:5, ' s/b true');
   writeln('Subrange25:  ', srx >= sry:5, ' s/b false');

   { signed subrange variables }
   sras := -14;
   srbs := -32;
   srcs := -14;
   srds := 20;
   sres := -15;
   writeln('Subrange26:  ', sras + srds:1, ' s/b 6');
   writeln('Subrange27:  ', srds + sras:1, ' s/b 6');
   writeln('Subrange28:  ', srbs + srds:1, ' s/b -12');
   writeln('Subrange29:  ', sras + srbs:1, ' s/b -46');
   writeln('Subrange30:  ', srds - sras:1, ' s/b 34');
   writeln('Subrange31:  ', srbs - srds:1, ' s/b -52');
   writeln('Subrange32:  ', srbs - sras:1, ' s/b -18');
   writeln('Subrange33:  ', srds * sras:1, ' s/b -280');
   writeln('Subrange34:  ', sras * srds:1, ' s/b -280');
   writeln('Subrange35:  ', sras * srbs:1, ' s/b 448');
   writeln('Subrange36:  ', srds div sras:1, ' s/b -1');
   writeln('Subrange37:  ', srbs div srds:1, ' s/b -1');
   writeln('Subrange38:  ', srbs div sras:1, ' s/b 2');
   writeln('Subrange41:  ', odd(sras):5, ' s/b false');
   writeln('Subrange42:  ', odd(sres):5, ' s/b true');
   writeln('Subrange43:  ', sras = srcs:5, ' s/b true');
   writeln('Subrange44:  ', sras = srbs:5, ' s/b false');
   writeln('Subrange45:  ', sras <> srbs:5, ' s/b true');
   writeln('Subrange46:  ', sras <> srcs:5, ' s/b false');
   writeln('Subrange47:  ', sras < srds:5, ' s/b true');
   writeln('Subrange48:  ', srbs < sras:5, ' s/b true');
   writeln('Subrange49:  ', srds < sras:5, ' s/b false');
   writeln('Subrange50:  ', sras < srbs:5, ' s/b false');
   writeln('Subrange51:  ', srds > sras:5, ' s/b true');
   writeln('Subrange52:  ', sras > srbs:5, ' s/b true');
   writeln('Subrange53:  ', sras > srds:5, ' s/b false');
   writeln('Subrange54:  ', srbs > sras:5, ' s/b false');
   writeln('Subrange55:  ', sras <= srds:5, ' s/b true');
   writeln('Subrange56:  ', srbs <= sras:5, ' s/b true');
   writeln('Subrange57:  ', sras <= srcs:5, ' s/b true');
   writeln('Subrange58:  ', srds <= sras:5, ' s/b false');
   writeln('Subrange59:  ', sras <= srbs:5, ' s/b false');
   writeln('Subrange60:  ', srds >= sras:5, ' s/b true');
   writeln('Subrange61:  ', sras >= srbs:5, ' s/b true');
   writeln('Subrange62:  ', sras >= srcs:5, ' s/b true');
   writeln('Subrange63:  ', sras >= srds:5, ' s/b false');
   writeln('Subrange64:  ', srbs >= sras:5, ' s/b false');
   writeln('Subrange65:  ', abs(sras):1, ' s/b 14');

{******************************************************************************

                         Characters

******************************************************************************}

   writeln;
   writeln('******************* Characters*******************');
   writeln;

   { character variables }
   ca := 'g'; cb := 'g'; cc := 'u';
   writeln('Character1:   ', ca, ' ', cb, ' ', cc, ' s/b g g u');
   writeln('Character2:   ', succ(ca), ' s/b h');
   writeln('Character3:   ', pred(cb), ' s/b f');
   writeln('Character4:   ', ord(ca):1, ' s/b 103');
   writeln('Character5:   ', chr(ord(cc)), ' s/b u');
   writeln('Character6:   ', ca = cb:5, ' s/b true');
   writeln('Character7:   ', ca = cc:5, ' s/b false');
   writeln('Character8:   ', ca < cc:5, ' s/b true');
   writeln('Character9:   ', cc < ca:5, ' s/b false');
   writeln('Character10:  ', cc > ca:5, ' s/b true');
   writeln('Character11:  ', ca > cc:5, ' s/b false');
   writeln('Character12:  ', ca <> cc:5, ' s/b true');
   writeln('Character13:  ', ca <> cb:5, ' s/b false');
   writeln('Character14:  ', ca <= cc:5, ' s/b true');
   writeln('Character15:  ', ca <= cb:5, ' s/b true');
   writeln('Character16:  ', cc <= ca:5, ' s/b false');
   writeln('Character17:  ', cc >= cb:5, ' s/b true');
   writeln('Character18:  ', cb >= ca:5, ' s/b true');
   writeln('Character19:  ', cb >= cc:5, ' s/b false');
   sa := 'porker    '; sb := 'porker    '; sc := 'parker    ';
   writeln('Character20:  ', sa, sb, sc,
      ' s/b porker    porker    parker');
   writeln('Character21:  ', sa = sb:5, ' s/b true');
   writeln('Character22:  ', sa = sc:5, ' s/b false');
   writeln('Character23:  ', sc < sa:5, ' s/b true');
   writeln('Character24:  ', sa < sc:5, ' s/b false');
   writeln('Character25:  ', sa > sc:5, ' s/b true');
   writeln('Character26:  ', sc > sa:5, ' s/b false');
   writeln('Character27:  ', sa <> sc:5, ' s/b true');
   writeln('Character28:  ', sa <> sb:5, ' s/b false');
   writeln('Character29:  ', sc <= sa:5, ' s/b true');
   writeln('Character30:  ', sa <= sb:5, ' s/b true');
   writeln('Character40:  ', sa <= sc:5, ' s/b false');
   writeln('Character41:  ', sa >= sc:5, ' s/b true');
   writeln('Character42:  ', sa >= sb:5, ' s/b true');
   writeln('Character43:  ', sc >= sa:5, ' s/b false');
   write('Character44:  ');
   for ca := 'a' to 'z' do write(ca);
   writeln(' s/b abcdefghijklmnopqrstuvwxyz');
   write('Character45:  ');
   for ca := 'z' downto 'a' do write(ca);
   writeln(' s/b zyxwvutsrqponmlkjihgfedcba');
   write('Character46:  ');
   x := 0;
   for ca := 'a' to 'z' do begin car[ca] := x; x := x + 1 end;
   for ca := 'z' downto 'a' do write(car[ca]:1, ' ');
   writeln;
   writeln('Character46: s/b 25 24 23 22 21 20 19 18 17 16 15',
      ' 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0');
   r.rc := 'n'; writeln('Character47: ', r.rc, ' s/b n');
   r.rs := 'junky01234'; writeln('Character48: ', r.rs,
                           ' s/b junky01234');
end;

procedure part4;
begin
   for i := 1 to 10 do sar[i] := '0123456789';
   sar[1] := 'trash     ';
   sar[2] := 'finnork   ';
   sar[10] := 'crapola   ';
   writeln('Character49:  ');
   for i := 10 downto 1 do writeln(sar[i]);
   writeln('Character49: s/b');
   writeln('crapola');
   writeln('0123456789');
   writeln('0123456789');
   writeln('0123456789');
   writeln('0123456789');
   writeln('0123456789');
   writeln('0123456789');
   writeln('0123456789');
   writeln('finnork');
   writeln('trash');
   writeln('Character50:  ');
   for ca := '0' to '9' do
   begin
     case ca of
       '5': write('five ');
       '3': write('three ');
       '6': write('six ');
       '8': write('eight ');
       '0': write('zero ');
       '9': write('nine ');
       '7': write('seven ');
       '4': write('four ');
       '1': write('one ');
       '2': write('two ');
     end
   end;
   writeln;
   writeln(' s/b zero one two three four five six ',
           'seven eight nine');

   { character constants }
   writeln('Character51:  ', 'a', ' s/b a');
   writeln('Character52:  ', succ('a'), ' s/b b');
   writeln('Character53:  ', pred('z'), ' s/b y');
   writeln('Character54:  ', ord('c'):1, ' s/b 99');
   writeln('Character55:  ', chr(ord('g')), ' s/b g');
   writeln('Character56:  ', 'q' = 'q':5, ' s/b true');
   writeln('Character57:  ', 'r' = 'q':5, ' s/b false');
   writeln('Character58:  ', 'b' < 't':5, ' s/b true');
   writeln('Character59:  ', 'g' < 'c':5, ' s/b false');
   writeln('Character60:  ', 'f' > 'e':5, ' s/b true');
   writeln('Character61:  ', 'f' > 'g':5, ' s/b false');
   writeln('Character62:  ', 'h' <> 'l':5, ' s/b true');
   writeln('Character63:  ', 'i' <> 'i':5, ' s/b false');
   writeln('Character64:  ', 'v' <= 'y':5, ' s/b true');
   writeln('Character65:  ', 'y' <= 'y':5, ' s/b true');
   writeln('Character66:  ', 'z' <= 'y':5, ' s/b false');
   writeln('Character67:  ', 'l' >= 'b':5, ' s/b true');
   writeln('Character68:  ', 'l' >= 'l':5, ' s/b true');
   writeln('Character69:  ', 'l' >= 'm':5, ' s/b false');
   writeln('Character70:  ', 'finnork' = 'finnork':5, ' s/b true');
   writeln('Character71:  ',
      'finoork' = 'finnork':5, ' s/b false');
   writeln('Character72:  ', 'oliab' < 'olibb':5, ' s/b true');
   writeln('Character73:  ', 'olibb' < 'oliab':5, ' s/b false');
   writeln('Character74:  ', 'olibb' > 'oliab':5, ' s/b true');
   writeln('Character75:  ', 'oliab' > 'olibb':5, ' s/b false');
   writeln('Character76:  ', 'fark ' <> 'farks':5, ' s/b true');
   writeln('Character77:  ', 'farks' <> 'farks':5, ' s/b false');
   writeln('Character78:  ', 'farka' <= 'farkz':5, ' s/b true');
   writeln('Character79:  ', 'farks' <= 'farks':5, ' s/b true');
   writeln('Character80:  ', 'farkz' <= 'farks':5, ' s/b false');
   writeln('Character81:  ', 'topnat' >= 'topcat':5, ' s/b true');
   writeln('Character82:  ', 'topcat' >= 'topcat':5, ' s/b true');
   writeln('Character83:  ', 'topcat' >= 'topzat':5, ' s/b false');
   writeln('Character84:  ', scst, ' s/b this is a string');
   writeln('Character85:  ', ccst, ' s/b v');
   writeln('Character86:  ');
   for i := 15 downto 1 do writeln('hello, world': i);
   writeln('Character86:  s/b:');
   writeln('   hello, world');
   writeln('  hello, world');
   writeln(' hello, world ');
   writeln('hello, world');
   writeln('hello, worl');
   writeln('hello, wor');
   writeln('hello, wo');
   writeln('hello, w');
   writeln('hello, ');
   writeln('hello,');
   writeln('hello');
   writeln('hell');
   writeln('hel');
   writeln('he');
   writeln('h');
end;

begin

   write('****************************************************************');
   writeln('***************');
   writeln;
   writeln('                 TEST SUITE FOR ISO 7185 PASCAL');
   writeln;
   write('                 Copyright (C) 1995 S. A. Moore - All rights ');
   writeln('reserved');
   writeln;
   write('****************************************************************');
   writeln('***************');
   writeln;

{******************************************************************************

                          Reference dangling defines

******************************************************************************}

{ unused declarations are always a problem, because it is always concievable
  that there is a compiler test that will reveal they are not used. We use
  assign to references here because a simple read of a variable could fault
  on an undefined reference. Its also possible that a never used fault could
  occur (written, but never used), in which case the code would have to be
  more complex. The best solution, of course, is to write a real test that
  uses the variables. }

   a[1] :=  1;
   esia[two] := 1;
   pesia[two] := 1;
   rewrite(fes);
   rewrite(pfes);
   rewrite(fs);
   rewrite(pfs);
   rewrite(fr);
   rewrite(pfr);
   rewrite(fst);
   rewrite(pfst);
   rewrite(fa);
   rewrite(pfa);
   rewrite(frc);
   rewrite(pfrc);
   rewrite(fp);
   rewrite(pfp);
   rcastt := 1;
   rcast.rcastt := true;
   vintalias := 1;

{******************************************************************************

                                 Metering

******************************************************************************}

   writeln('The following are implementation defined characteristics');
   writeln;
   writeln('Maxint: ', maxint:1);
   i := maxint;
   x := 0;
   while i > 0 do begin i := i div 2;  x := x+1 end;
   writeln('Bit length of integer without sign bit appears to be: ', x:1);
   writeln('Integer default output field');
   writeln('         1111111111222222222233333333334');
   writeln('1234567890123456789012345678901234567890');
   writeln(1);
   writeln('Real default output field');
   writeln('         1111111111222222222233333333334');
   writeln('1234567890123456789012345678901234567890');
   writeln(1.2);
   writeln('Note that the exponent character ''e'' or ''E'' is implementation');
   writeln('defined as well as the number of exponent digits');
   writeln('Boolean default output field');
   writeln('         1111111111222222222233333333334');
   writeln('1234567890123456789012345678901234567890');
   writeln(false);
   writeln(true);
   writeln('Note that the upper or lower case state of the characters in');
   writeln('''true'' and ''false'' are implementation defined');
   writeln('Char default output field');
   writeln('         1111111111222222222233333333334');
   writeln('1234567890123456789012345678901234567890');
   writeln('a');
   if (ord('a') = 97) and (ord('(') = 40) and (ord('^') = 94) then
      writeln('Appears to be ASCII')
   else
      writeln('Appears to not be ASCII');

{******************************************************************************

                           Control structures

******************************************************************************}

   writeln;
   writeln('******************* Control structures tests *******************');
   writeln;
   write('Control1: ');
   for i := 1 to 10 do write(i:1, ' ');
   writeln('s/b 1 2 3 4 5 6 7 8 9 10');
   write('Control2: ');
   for i := 10 downto 1 do write(i:1, ' ');
   writeln('s/b 10 9 8 7 6 5 4 3 2 1');
   write('Control3: ');
   i := 1;
   while i <=10 do begin write(i:1, ' '); i := i + 1 end;
   writeln('s/b 1 2 3 4 5 6 7 8 9 10');
   write('Control4: ');
   i := 1; repeat write(i:1, ' '); i := i + 1 until i > 10;
   writeln('s/b 1 2 3 4 5 6 7 8 9 10');
   write('Control5: ');
   i := 1;
   0: write(i:1, ' '); i := i + 1; if i <= 10 then goto 0;
   writeln('s/b 1 2 3 4 5 6 7 8 9 10');
   write('Control6: ');
   if true then write('yes') else write('no');
   writeln(' s/b yes');
   write('Control7: ');
   if false then write('no') else write('yes');
   writeln(' s/b yes');
   write('Control8: ');
   if true then write('yes '); write('stop');
   writeln(' s/b yes stop');
   write('Control9: ');
   if false then write('no '); write('stop');
   writeln(' s/b stop');
   write('Control10: ');
   for i := 1 to 10 do
      case i of
         1:     write('one ');
         2:     write('two ');
         3:     write('three ');
         4:     write('four ');
         5:     write('five ');
         6:     write('six ');
         7:     write('seven ');
         8:     write('eight ');
         9, 10: write('nine-ten ')

      end;
   writeln;
   write('Control10: s/b ');
   write('one two three four five ');
   writeln('six seven eight nine-ten nine-ten');
   write('Control11: start ');
   junk6;
   write('!! BAD !!');
   9999: writeln('stop s/b start stop');
   write('Control12: start ');
   goto 003;
   write('!! BAD !!');
   3: writeln('stop s/b start stop');
   write('Control13: start ');
   { self defined fors }
   i := 10;
   {for i := 1 to i do write(i:3); -- Runs forever on BESM-6 }
   for i := 1 to 10 do write(i:3);
   writeln(' s/b start  1  2  3  4  5  6  7  8  9 10');
   write('Control14: start ');
   { self defined fors }
   i := 10;
   for i := i downto 1 do write(i:3);
   writeln(' s/b start 10  9  8  7  6  5  4  3  2  1');
   write('Control15: start ');
   { for against 0 }
   for i := 0 to 9 do write(i:2);
   writeln(' s/b start 0 1 2 3 4 5 6 7 8 9');
   write('Control16: start ');
   { for against 0 }
   for i := 9 downto 0 do write(i:2);
   writeln(' s/b start 9 8 7 6 5 4 3 2 1 0');
   { wide spread of case statements }
   write('Control17: start ');
   i := 10000;
   case i of
      1: write('*** bad ***');
      10000: write('good')
   end;
   writeln(' start s/b start good');
   write('Control18: start ');
   repeat
      goto 004;
      write('!! BAD !!');
      4: writeln('stop s/b start stop');
      i := 0;
      if i <> 0 then goto 04;
   until true;

   part1;
   part2;
   part3;
   part4;

   writeln;
   writeln('******************* Finish *******************');
   writeln;
end.
