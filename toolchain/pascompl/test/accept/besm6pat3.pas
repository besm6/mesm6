{******************************************************************************
*                                                                             *
*                       TEST SUITE FOR BESM6 PASCAL                           *
*                                                                             *
*                               Part 3 of 3                                   *
*                                                                             *
******************************************************************************}

program besm6pat(output);

const
      { flags to control run }

      { the pointer torture test takes time and isn't run for interpreted
        systems }
      doptrtortst = false;

      testfile = true;

type
     string10 = packed array [1..10] of char;
     enum  = (one, two, three, four, five, six, seven, eight, nine, ten);
     esub  = three..six;
     subr  = 10..20;
     (* Note use of alternatives for '[' and ']'. The availablity of these
        alternates is implementation defined. *)
     arri  = array (.1..10.) of integer;
     arrim = array [1..2, 1..2] of array [1..2, 1..2, 1..2, 1..2] of integer;
     { Note that the availability of the alternate '@' is implementation
       defined }
     iptr  = @integer;
     recs  = record

               a: integer;
               b: char

            end;
     rec = record

              i:   integer;
              b:   boolean;
              c:   char;
              e:   enum;
              es:  esub;
              s:   subr;
              r:   real;
              st:  string10;
              a:   arri;
              rc:  recs;
              p:   iptr

           end;
     prec = packed record

              i:   integer;
              b:   boolean;
              c:   char;
              e:   enum;
              es:  esub;
              s:   subr;
              r:   real;
              st:  string10;
              a:   arri;
              rc:  recs;
              p:   iptr

           end;
     recv = record

               a: integer;
               b: char;
               case c: boolean of

                  false: (d: string10);
                  true:  (e: enum)

               { end }

            end;
     arrr = array [1..10] of recs;
     vart = (vti, vtb, vtc, vte, vtes, vts, vtr, vtst, vta, vtrc, vtp);

var
    i, x, y, z, q, n, t: integer;
    srx, sry: 0..100;
    da:    array [1..10, 1..10] of integer;
    ca, cc: char;
    ba:    boolean;
    s:     string10;
    ra, rb: real;
    ci:    char;
    ei, ea: enum;
    ai:    arri;
    arec:  rec;
    parec: prec;
    vrec:  recv;
    ip:    iptr;
    avi:   arri;
    avi2:  arri;
    pavi:  packed array [1..10] of 0..32767;
    avis:  array [1..10] of 10..20;
    pavis: packed array [1..10] of 10..20;
    avb:   array [1..10] of boolean;
    pavb:  packed array [1..10] of boolean;
    avr:   array [1..10] of real;
    pavr:  packed array [1..10] of real;
    avc:   array [1..10] of char;
    pavc:  packed array [1..10] of char;
    avcs:  array [1..10] of 'g'..'p';
    pavcs: packed array [1..10] of 'g'..'p';
    ave:   array [1..10] of enum;
    pave:  packed array [1..10] of enum;
    aves:  array [1..10] of esub;
    paves: packed array [1..10] of esub;
    avrc:  array [1..10] of recs;
    pavrc: packed array [1..10] of recs;
    avp:   array [1..10] of iptr;
    pavp:  packed array [1..10] of iptr;
    bia:   array [boolean] of integer;
    pbia:  packed array [boolean] of integer;
    cia:   array [char] of integer;
    pcia:  packed array [char] of integer;
    csia:  array ['a'..'z'] of integer;
    pcsia: packed array ['a'..'z'] of integer;
    eia:   array [enum] of integer;
    peia:  packed array [enum] of integer;
    mdar:  arrim;
    mdar2: arrim;
    vra:   record

              i: integer;
              case vt: vart of

                 vti:   (vdi:   integer;  a: integer);
                 vtb:   (vdb:   boolean;  b: integer);
                 vtc:   (vdc:   char;     c: integer);
                 vte:   (vde:   enum;     d: integer);
                 vtes:  (vdes:  esub;     e: integer);
                 vts:   (vds:   subr;     f: integer);
                 vtr:   (vdr:   real;     g: integer);
                 vtst:  (vdst:  string10; h: integer);
                 vta:   (vda:   arri;     j: integer);
                 vtrc:  (vdrc:  recs;     k: integer);
                 vtp:   (vdp:   iptr;     m: integer)

              { end }

           end;
    vvrs:  record

              case vt: subr of

                 10, 11, 12, 13, 14, 15: (vi: integer);
                 16, 17, 18, 19, 20: (vb: boolean)

              { end }

           end;
    vvrb:  record

              case vt:boolean of

                 true: (vi: integer);
                 false: (vb: boolean)

              { end }

           end;
    vvre:  record

              case vt: enum of

                 one, two, three, four, five: (vi: integer);
                 six, seven, eight, nine, ten: (vb: boolean)

              { end }

           end;
    vvres: record

              case vt: esub of

                 three, four: (vi: integer);
                 five, six: (vb: boolean)

              { end }

           end;
    nvr:   record

              i: integer;
              r: record

                 i: integer;
                 r: record

                    i: integer;
                    r: record

                       i: integer;
                       r: record

                          i: integer;
                          r: record

                             i: integer;
                             r: record

                                i: integer;
                                r: record

                                   i: integer;
                                   r: record

                                      i: integer;
                                      r: record

                                         i: integer

                                      end

                                   end

                                end

                             end

                          end

                       end

                    end

                 end

              end

           end;
    rpa:   ^rec;
    ara:   arrr;
    fi:    file of integer;
    pfi:   packed file of integer;
    fb:    file of boolean;
    pfb:   packed file of boolean;
    fc:    file of char;
    pfc:   packed file of char;
    fe:    file of enum;
    pfe:   packed file of enum;
    ft:    text;
    pti, pti1: ^integer;
    ptb:   ^boolean;
    ptc:   ^char;
    pte:   ^enum;
    ptes:  ^esub;
    pts:   ^subr;
    ptr:   ^real;
    ptst:  ^string10;
    pta:   ^arri;
    ptrc:  ^recs;
    ptp:   ^iptr;
    ipa,       ipb, ipc, ipd, ipe: ^integer;
    iap:       array [1..100] of ^integer;
    rndseq:    integer;
    cnt, cnt2: integer;
    rn:        integer;

procedure junk1(z, q : integer);
begin
   write(z:1, ' ', q:1);
end;

procedure junk2(var z : integer);
begin
   z := z + 1
end;

procedure junk3(var p : string10);
begin
   write(p)
end;

procedure junk4(p : string10);
begin
   p[5] := '?';
   write(p)
end;

function junk5(x : integer) : integer;
begin
   junk5 := x + 1
end;

function junk7(a, b, c: integer): integer; forward;

function junk7;
    var x, y, z: integer;
begin
   x := 1;
   y := 2;
   z := 3;
   write(a:1, ' ', b:1, ' ', c:1, ' ');
   a := 4;
   b := 5;
   c := 6;
   write(c:1, ' ', b:1, ' ', a:1, ' ', z:1, ' ', y:1, ' ', x:1);
   junk7 := 78
end;

procedure junk8(a: integer; b: boolean; c: char; e: enum; es: esub; s: subr;
                r: real; st: string10; ar: arri; rc: rec; rv: recv;
                p: iptr);
    var i:  integer;
        ci: char;
begin
   writeln(a:1, ' ', b:5, ' ', c:1, ' ', ord(e):1, ' ', ord(es):1, ' ', s:1, ' ',
           r:15, ' ', st);
   for i := 1 to 10 do write(ar[i]:1, ' '); writeln;
   writeln(rc.i:1, ' ', rc.b:5, ' ', rc.c:1, ' ', ord(rc.e):1, ' ', ord(rc.es):1,
           ' ', rc.s:1, ' ', rc.r:15, ' ', rc.st);
   for i := 1 to 10 do write(rc.a[i]:1, ' '); writeln;
   writeln(rc.rc.a:1, ' ', rc.rc.b:1);
   writeln(rc.p^:1);
   writeln(rv.a:1, ' ', rv.b:1, ' ', rv.c:5);
   if rv.c then writeln(ord(rv.e):1) else writeln(rv.d);
   writeln(p^:1)
end;

function random (low, hi : integer) : integer;
    const a = 16807;
          m = 2147483647;
    var gamma: integer;
begin
    gamma := a*(rndseq mod (m div a))-(m mod a)*(rndseq div (m div a));
    if gamma > 0 then
        rndseq := gamma
    else
        rndseq := gamma+m;
    random := rndseq div (maxint div (hi-low+1))+low
end {of random};

procedure part9;
begin
{******************************************************************************

                            Pointers

******************************************************************************}

   writeln;
   writeln('******************* Pointers ******************************');
   writeln;

   { pointers to types }
   write('Pointer1:   ');
   new(pti);
   pti^ := 4594;
   writeln(pti^:1, ' s/b 4594');
   write('Pointer2:   ');
   new(ptb);
   ptb^ := true;
   writeln(ptb^:5, ' s/b  true');
   write('Pointer3:   ');
   new(ptb);
   ptb^ := false;
   writeln(ptb^:5, ' s/b false');
   write('Pointer4:   ');
   new(ptc);
   ptc^ := 'p';
   writeln(ptc^, ' s/b p');
   write('Pointer5:   ');
   new(pte);
   pte^ := six;
   writeln(ord(pte^):1, ' s/b 5');
   write('Pointer6:   ');
   new(ptes);
   ptes^ := four;
   writeln(ord(ptes^):1, ' s/b 3');
   write('Pointer7:   ');
   new(pts);
   pts^ := 17;
   writeln(pts^:1, ' s/b 17');
   write('Pointer8:   ');
   new(ptr);
   ptr^ := 1234.5678;
   writeln(ptr^:1:4, ' s/b 1234.5678');
   write('Pointer9:   ');
   new(ptst);
   ptst^ := 'my word is';
   writeln(ptst^, ' s/b my word is');
   write('Pointer10:  ');
   new(pta);
   for i := 1 to 10 do pta^[i] := i+10;
   for i := 10 downto 1 do write(pta^[i]:1, ' ');
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');
   write('Pointer11:   ');
   new(ptrc);
   ptrc^.a := 7234;
   ptrc^.b := 'y';
   writeln(ptrc^.a:1, ' ', ptrc^.b, ' s/b 7234 y');
   write('Pointer13:  ');
   new(ptp);
   new(ptp^);
   ptp^^ := 3732;
   writeln(ptp^^:1, ' s/b 3732');

   { equality/inequality, nil }
   write('Pointer14:  ');
   pti := nil;
   writeln(pti = nil:5, ' s/b  true');
   write('Pointer15:  ');
   new(pti);
   writeln(pti = nil:5, ' s/b false');
   write('Pointer16:  ');
   pti1 := pti;
   writeln(pti = pti1:5, ' s/b true');
   write('Pointer17:  ');
   pti1 := pti;
   writeln(pti <> pti1:5, ' s/b false');
   write('Pointer18:  ');
   new(pti1);
   writeln(pti = pti1:5, ' s/b false');
   write('Pointer19:  ');
   writeln(pti <> pti1:5, ' s/b  true');

   { dynamic allocation stress tests }

   { allocate top to bottom, then free from top to bottom }
   write('Pointer20:  ');
   new(ipa);
   new(ipb);
   new(ipc);
   dispose(ipa);
   dispose(ipb);
   dispose(ipc);
   writeln('done s/b done');

   { allocate top to bottom, then free from bottom to top }

   write('Pointer21:  ');
   new(ipa);
   new(ipb);
   new(ipc);
   dispose(ipc);
   dispose(ipb);
   dispose(ipa);

   { free 2 middle blocks to test coalesce }

   write('Pointer22:  ');
   new(ipa);
   new(ipb);
   new(ipc);
   new(ipd);
   dispose(ipb);
   dispose(ipc);
   dispose(ipa);
   dispose(ipd);
   writeln('done s/b done');

   { free 3 middle blocks to test coalesce }
   write('Pointer23:  ');
   new(ipa);
   new(ipb);
   new(ipc);
   new(ipd);
   new(ipe);
   dispose(ipb);
   dispose(ipd);
   dispose(ipc);
   dispose(ipa);
   dispose(ipe);
   writeln('done s/b done');

   if doptrtortst then begin

      { linear torture test }
      writeln('Pointer24:  ');
      for cnt := 1 to 100 do begin

         write(cnt:3, ' '); if (cnt mod 10) = 0 then writeln;
         for i := 1 to 100 do iap[i] := nil;
         for i := 1 to 100 do begin new(iap[i]); iap[i]^ := i end;
         for i := 1 to 100 do if iap[i] = nil then
            writeln('*** bad allocation of block');
         for i := 100 downto 1 do if iap[i]^ <> i then
            writeln('*** bad block content');
         for i := 1 to 100 do begin

            dispose(iap[i]);
            iap[i] := nil;
            for x := 1 to 100 do if iap[x] <> nil then
               if iap[x]^ <> x then
                  writeln('*** bad block content')

         end;

         for i := 1 to 100 do iap[i] := nil;
         for i := 1 to 100 do begin new(iap[i]); iap[i]^ := i end;
         for i := 1 to 100 do if iap[i] = nil then
            writeln('*** bad allocation of block');
         for i := 100 downto 1 do if iap[i]^ <> i then
            writeln('*** bad block content');
         for i := 100 downto 1 do begin

            dispose(iap[i]);
            iap[i] := nil;
            for x := 1 to 100 do if iap[x] <> nil then
               if iap[x]^ <> x then
                  writeln('*** bad block content')

         end

      end;
      writeln;
      writeln('s/b');
      writeln;
      writeln('  1   2   3   4   5   6   7   8   9  10');
      writeln(' 11  12  13  14  15  16  17  18  19  20');
      writeln(' 21  22  23  24  25  26  27  28  29  30');
      writeln(' 31  32  33  34  35  36  37  38  39  40');
      writeln(' 41  42  43  44  45  46  47  48  49  50');
      writeln(' 51  52  53  54  55  56  57  58  59  60');
      writeln(' 61  62  63  64  65  66  67  68  69  70');
      writeln(' 71  72  73  74  75  76  77  78  79  80');
      writeln(' 81  82  83  84  85  86  87  88  89  90');
      writeln(' 91  92  93  94  95  96  97  98  99  100');

   end else begin

      { keep listing equal for compare }
      writeln('Pointer24:  ');
      writeln('  1   2   3   4   5   6   7   8   9  10 ');
      writeln(' 11  12  13  14  15  16  17  18  19  20 ');
      writeln(' 21  22  23  24  25  26  27  28  29  30 ');
      writeln(' 31  32  33  34  35  36  37  38  39  40 ');
      writeln(' 41  42  43  44  45  46  47  48  49  50 ');
      writeln(' 51  52  53  54  55  56  57  58  59  60 ');
      writeln(' 61  62  63  64  65  66  67  68  69  70 ');
      writeln(' 71  72  73  74  75  76  77  78  79  80 ');
      writeln(' 81  82  83  84  85  86  87  88  89  90 ');
      writeln(' 91  92  93  94  95  96  97  98  99 100 ');
      writeln;
      writeln('s/b');
      writeln;
      writeln('  1   2   3   4   5   6   7   8   9  10');
      writeln(' 11  12  13  14  15  16  17  18  19  20');
      writeln(' 21  22  23  24  25  26  27  28  29  30');
      writeln(' 31  32  33  34  35  36  37  38  39  40');
      writeln(' 41  42  43  44  45  46  47  48  49  50');
      writeln(' 51  52  53  54  55  56  57  58  59  60');
      writeln(' 61  62  63  64  65  66  67  68  69  70');
      writeln(' 71  72  73  74  75  76  77  78  79  80');
      writeln(' 81  82  83  84  85  86  87  88  89  90');
      writeln(' 91  92  93  94  95  96  97  98  99  100');

   end;

   if doptrtortst then begin

      rndseq := 1;

      { random block torture test }
      writeln('Pointer25:  ');
      for i := 1 to 100 do iap[i] := nil;
      for cnt2 := 1 to 100 do begin

         write(cnt2:3, ' '); if (cnt2 mod 10) = 0 then writeln;
         for cnt := 1 to 100 do begin

            { allocate random }
            rn := random(1, 100); { choose random pointer }
            new(iap[rn]); { allocate }
            iap[rn]^ := rn; { set number }
            for i := 1 to 100 do if iap[i] <> nil then
               if iap[i]^ <> i then
                  writeln('*** bad block content');

            { deallocate random }
            rn := random(1, 100); { choose random pointer }
            if iap[rn] <> nil then dispose(iap[rn]); { deallocate }
            iap[rn] := nil;
            for i := 1 to 100 do if iap[i] <> nil then
               if iap[i]^ <> i then
                  writeln('*** bad block content');

         end

      end;
      writeln;
      writeln('s/b');
      writeln;
      writeln('  1   2   3   4   5   6   7   8   9  10');
      writeln(' 11  12  13  14  15  16  17  18  19  20');
      writeln(' 21  22  23  24  25  26  27  28  29  30');
      writeln(' 31  32  33  34  35  36  37  38  39  40');
      writeln(' 41  42  43  44  45  46  47  48  49  50');
      writeln(' 51  52  53  54  55  56  57  58  59  60');
      writeln(' 61  62  63  64  65  66  67  68  69  70');
      writeln(' 71  72  73  74  75  76  77  78  79  80');
      writeln(' 81  82  83  84  85  86  87  88  89  90');
      writeln(' 91  92  93  94  95  96  97  98  99  100');

   end else begin

      { keep listing equal for comparision }
      writeln('Pointer25:  ');
      writeln('  1   2   3   4   5   6   7   8   9  10 ');
      writeln(' 11  12  13  14  15  16  17  18  19  20 ');
      writeln(' 21  22  23  24  25  26  27  28  29  30 ');
      writeln(' 31  32  33  34  35  36  37  38  39  40 ');
      writeln(' 41  42  43  44  45  46  47  48  49  50 ');
      writeln(' 51  52  53  54  55  56  57  58  59  60 ');
      writeln(' 61  62  63  64  65  66  67  68  69  70 ');
      writeln(' 71  72  73  74  75  76  77  78  79  80 ');
      writeln(' 81  82  83  84  85  86  87  88  89  90 ');
      writeln(' 91  92  93  94  95  96  97  98  99 100 ');
      writeln;
      writeln('s/b');
      writeln;
      writeln('  1   2   3   4   5   6   7   8   9  10');
      writeln(' 11  12  13  14  15  16  17  18  19  20');
      writeln(' 21  22  23  24  25  26  27  28  29  30');
      writeln(' 31  32  33  34  35  36  37  38  39  40');
      writeln(' 41  42  43  44  45  46  47  48  49  50');
      writeln(' 51  52  53  54  55  56  57  58  59  60');
      writeln(' 61  62  63  64  65  66  67  68  69  70');
      writeln(' 71  72  73  74  75  76  77  78  79  80');
      writeln(' 81  82  83  84  85  86  87  88  89  90');
      writeln(' 91  92  93  94  95  96  97  98  99  100');

   end;
end;

procedure part10;
begin
{******************************************************************************

                            Arrays

******************************************************************************}

   writeln;
   writeln('******************* arrays ******************************');
   writeln;

   { single demension, integer index }
   write('Array1:   ');
   for i := 1 to 10 do avi[i] := i+10;
   for i := 10 downto 1 do write(avi[i]:1, ' ');
   writeln(' s/b 20 19 18 17 16 15 14 13 12 11');
   write('Array2:   ');
   for i := 1 to 10 do pavi[i] := i+10;
   for i := 10 downto 1 do write(pavi[i]:1, ' ');
   writeln(' s/b 20 19 18 17 16 15 14 13 12 11');
   write('Array3:   ');
   for i := 1 to 10 do avis[i] := i+10;
   for i := 10 downto 1 do write(avis[i]:1, ' ');
   writeln(' s/b 20 19 18 17 16 15 14 13 12 11');
   write('Array4:   ');
   for i := 1 to 10 do pavis[i] := i+10;
   for i := 10 downto 1 do write(pavis[i]:1, ' ');
   writeln(' s/b 20 19 18 17 16 15 14 13 12 11');
   write('Array5:   ');
   for i := 1 to 10 do avb[i] := odd(i);
   for i := 10 downto 1 do write(avb[i]:5, ' ');
   writeln;
   writeln('    s/b:   false  true false  true false  true false  true false',
           '  true');
   write('Array6:   ');
   for i := 1 to 10 do pavb[i] := odd(i);
   for i := 10 downto 1 do write(pavb[i]:5, ' ');
   writeln;
   writeln('    s/b:   false  true false  true false  true false  true false',
           '  true');
   write('Array7:   ');
   for i := 1 to 10 do avr[i] := i+10+0.12;
   for i := 10 downto 1 do write(avr[i]:1:2, ' ');
   writeln;
   writeln('    s/b:   20.12 19.12 18.12 17.12 16.12 15.12 14.12 ',
           '13.12 12.12 11.12');
   write('Array8:   ');
   for i := 1 to 10 do pavr[i] := i+10+0.12;
   for i := 10 downto 1 do write(pavr[i]:1:2, ' ');
   writeln;
   writeln('    s/b:   20.12 19.12 18.12 17.12 16.12 15.12 14.12 ',
           '13.12 12.12 11.12');
   write('Array9:   ');
   for i := 1 to 10 do avc[i] := chr(i+ord('a'));
   for i := 10 downto 1 do write(avc[i]:1, ' ');
   writeln('s/b k j i h g f e d c b');
   write('Array10:  ');
   for i := 1 to 10 do pavc[i] := chr(i+ord('a'));
   for i := 10 downto 1 do write(pavc[i]:1, ' ');
   writeln('s/b k j i h g f e d c b');
   write('Array11:  ');
   for i := 1 to 10 do avcs[i] := chr(i+ord('f'));
   for i := 10 downto 1 do write(avcs[i]:1, ' ');
   writeln('s/b p o n m l k j i h g');
   write('Array12:  ');
   for i := 1 to 10 do pavcs[i] := chr(i+ord('f'));
   for i := 10 downto 1 do write(pavcs[i]:1, ' ');
   writeln('s/b p o n m l k j i h g');
   write('Array13:  ');
   for ei := one to ten do ave[ord(ei)+1] := ei;
   for ei := ten downto one do write(ord(ave[ord(ei)+1]):1, ' ');
   writeln('s/b 9 8 7 6 5 4 3 2 1 0');
   write('Array14:  ');
   for ei := one to ten do pave[ord(ei)+1] := ei;
   for ei := ten downto one do write(ord(ave[ord(ei)+1]):1, ' ');
   writeln('s/b 9 8 7 6 5 4 3 2 1 0');
   write('Array15:  ');
   for ei := three to six do aves[ord(ei)+1] := ei;
   for ei := six downto three do write(ord(aves[ord(ei)+1]):1, ' ');
   writeln('s/b 5 4 3 2');
   write('Array16:  ');
   for ei := three to six do paves[ord(ei)+1] := ei;
   for ei := six downto three do write(ord(paves[ord(ei)+1]):1, ' ');
   writeln('s/b 5 4 3 2');
   write('Array19:  ');
   for i := 1 to 10 do
      begin avrc[i].a := i+10; avrc[i].b := chr(i+ord('a')) end;
   for i := 10 downto 1 do write(avrc[i].a:1, ' ', avrc[i].b, ' ');
   writeln;
   writeln('     s/b:  20 k 19 j 18 i 17 h 16 g 15 f 14 e 13 d 12 c 11 b');
   write('Array20:  ');
   for i := 1 to 10 do
      begin pavrc[i].a := i+10; pavrc[i].b := chr(i+ord('a')) end;
   for i := 10 downto 1 do write(pavrc[i].a:1, ' ', pavrc[i].b, ' ');
   writeln;
   writeln('     s/b:  20 k 19 j 18 i 17 h 16 g 15 f 14 e 13 d 12 c 11 b');
   write('Array23:  ');
   for i := 1 to 10 do begin new(avp[i]); avp[i]^ := i+10 end;
   for i := 10 downto 1 do write(avp[i]^:1, ' ');
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');
   write('Array24:  ');
   for i := 1 to 10 do begin new(pavp[i]); pavp[i]^ := i+10 end;
   for i := 10 downto 1 do write(pavp[i]^:1, ' ');
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');

   { indexing tests }
   write('Array25:  ');
   for ba := false to true do bia[ba] := ord(ba)+10;
   for ba := true downto false do write(bia[ba]:1, ' ');
   writeln(' s/b 11 10');
   write('Array26:  ');
   for ba := false to true do pbia[ba] := ord(ba)+10;
   for ba := true downto false do write(pbia[ba]:1, ' ');
   writeln(' s/b 11 10');
   write('Array27:  ');
   for ci := 'a' to 'j' do cia[ci] := ord(ci);
   for ci := 'j' downto 'a' do write(chr(cia[ci]), ' ');
   writeln(' s/b  j i h g f e d c b a');
   write('Array28:  ');
   for ci := 'a' to 'j' do pcia[ci] := ord(ci);
   for ci := 'j' downto 'a' do write(chr(pcia[ci]), ' ');
   writeln(' s/b  j i h g f e d c b a');
   write('Array29:  ');
   for ci := 'a' to 'j' do csia[ci] := ord(ci);
   for ci := 'j' downto 'a' do write(chr(csia[ci]), ' ');
   writeln(' s/b  j i h g f e d c b a');
   write('Array30:  ');
   for ci := 'a' to 'j' do pcsia[ci] := ord(ci);
   for ci := 'j' downto 'a' do write(chr(pcsia[ci]), ' ');
   writeln(' s/b  j i h g f e d c b a');
   write('Array31:  ');
   for ei := one to ten do eia[ei] := ord(ei);
   for ei := ten downto one do write(eia[ei]:1, ' ');
   writeln(' s/b  9 8 7 6 5 4 3 2 1 0');
   write('Array32:  ');
   for ei := one to ten do peia[ei] := ord(ei);
   for ei := ten downto one do write(peia[ei]:1, ' ');
   writeln(' s/b  9 8 7 6 5 4 3 2 1 0');
   write('Array33:  ');
   for ei := two to six do eia[ei] := ord(ei);
   for ei := six downto two do write(eia[ei]:1, ' ');
   writeln(' s/b  5 4 3 2 1');
   write('Array34:  ');
   for ei := two to six do peia[ei] := ord(ei);
   for ei := six downto two do write(peia[ei]:1, ' ');
   writeln(' s/b  5 4 3 2 1');
end;

procedure part11;
begin
   { multidementional arrays }
   writeln('Array35:');
   z := 0;
   for x := 1 to 10 do
      for y := 1 to 10 do begin da[y, x] := z; z := z + 1 end;
   for x := 1 to 10 do
   begin
      for y := 1 to 10 do write(da[x][y]:2, ' ');
      writeln;
   end;
   writeln('s/b');
   writeln('0 10 20 30 40 50 60 70 80 90');
   writeln('1 11 21 31 41 51 61 71 81 91');
   writeln('2 12 22 32 42 52 62 72 82 92');
   writeln('3 13 23 33 43 53 63 73 83 93');
   writeln('4 14 24 34 44 54 64 74 84 94');
   writeln('5 15 25 35 45 55 65 75 85 95');
   writeln('6 16 26 36 46 56 66 76 86 96');
   writeln('7 17 27 37 47 57 67 77 87 97');
   writeln('8 18 28 38 48 58 68 78 88 98');
   writeln('9 19 29 39 49 59 69 79 89 99');
   writeln('Array36: ');
   t := 0;
   for i := 1 to 2 do
      for x := 1 to 2 do
         for y := 1 to 2 do
            for z := 1 to 2 do
               for q := 1 to 2 do
                  for n := 1 to 2 do
                     begin mdar[i][x, y, z][q][n] := t; t := t+1 end;
   for i := 2 downto 1 do
      for x := 2 downto 1 do
         for y := 2 downto 1 do begin

            for z := 2 downto 1 do
               for q := 2 downto 1 do
                  for n := 2 downto 1 do write(mdar[i, x][y, z][q][n]:2, ' ');
            writeln;

         end;
   writeln('s/b:');
   writeln('63 62 61 60 59 58 57 56');
   writeln('55 54 53 52 51 50 49 48');
   writeln('47 46 45 44 43 42 41 40');
   writeln('39 38 37 36 35 34 33 32');
   writeln('31 30 29 28 27 26 25 24');
   writeln('23 22 21 20 19 18 17 16');
   writeln('15 14 13 12 11 10  9  8');
   writeln(' 7  6  5  4  3  2  1  0');

   { assignments }
   writeln('Array37: ');
   pavc := 'hello, guy';
   writeln(pavc, ' s/b hello, guy');
   writeln('Array38: ');
   for i := 1 to 10 do avi[i] := i+10;
   avi2 := avi;
   for i := 10 downto 1 do write(avi2[i]:1, ' ');
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');
   writeln('Array39: ');
   t := 0;
   for i := 1 to 2 do
      for x := 1 to 2 do
         for y := 1 to 2 do
            for z := 1 to 2 do
               for q := 1 to 2 do
                  for n := 1 to 2 do
                     begin mdar[i][x, y, z][q][n] := t; t := t+1 end;
   mdar2 := mdar;
   for i := 2 downto 1 do
      for x := 2 downto 1 do
         for y := 2 downto 1 do begin

            for z := 2 downto 1 do
               for q := 2 downto 1 do
                  for n := 2 downto 1 do write(mdar2[i, x][y, z][q][n]:2, ' ');
            writeln;

         end;
   writeln('s/b:');
   writeln('63 62 61 60 59 58 57 56');
   writeln('55 54 53 52 51 50 49 48');
   writeln('47 46 45 44 43 42 41 40');
   writeln('39 38 37 36 35 34 33 32');
   writeln('31 30 29 28 27 26 25 24');
   writeln('23 22 21 20 19 18 17 16');
   writeln('15 14 13 12 11 10  9  8');
   writeln(' 7  6  5  4  3  2  1  0');

   { transfer procedures }
   writeln('Array40: ');
   for i := 1 to 10 do pavi[i] := i+10;
   unpack(pavi, avi, 1);
   for i := 10 downto 1 do write(avi[i]:1, ' ');
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');
   writeln('Array41: ');
   for i := 1 to 10 do avi[i] := i+20;
   pack(avi, 1, pavi);
   for i := 10 downto 1 do write(pavi[i]:1, ' ');
   writeln('s/b 30 29 28 27 26 25 24 23 22 21');

{******************************************************************************

                            Records

******************************************************************************}

   writeln;
   writeln('******************* records ******************************');
   writeln;

   { types in records }
   writeln('Record1:   ');
   arec.i := 64;
   arec.b := false;
   arec.c := 'j';
   arec.e := two;
   arec.es := four;
   arec.s := 12;
   arec.r := 4545.12e-16;
   arec.st := 'what ? who';
   for i := 1 to 10 do arec.a[i] := i+20;
   arec.rc.a := 2324;
   arec.rc.b := 'y';
   new(arec.p);
   arec.p^ := 8454;
   writeln(arec.i:1, ' ', arec.b:5, ' ', arec.c:1, ' ', ord(arec.e):1, ' ',
           ord(arec.es):1,
           ' ', arec.s:1, ' ', arec.r:15, ' ', arec.st);
   for i := 1 to 10 do write(arec.a[i]:1, ' '); writeln;
   writeln(arec.rc.a:1, ' ', arec.rc.b:1);
   writeln(arec.p^:1);
   writeln('s/b:');
   writeln('64 false j 1 3 12  4.54512000e-15 what ? who');
   writeln('21 22 23 24 25 26 27 28 29 30');
   writeln('2324 y');
   writeln('_bcde___i_');
   writeln('8454');
   writeln('Record2:   ');
   parec.i := 64;
   parec.b := false;
   parec.c := 'j';
   parec.e := two;
   parec.es := four;
   parec.s := 12;
   parec.r := 4545.12e-16;
   parec.st := 'what ? who';
   for i := 1 to 10 do parec.a[i] := i+20;
   parec.rc.a := 2324;
   parec.rc.b := 'y';
   new(parec.p);
   parec.p^ := 8454;
   writeln(parec.i:1, ' ', parec.b:5, ' ', parec.c:1, ' ', ord(parec.e):1, ' ',
           ord(parec.es):1,
           ' ', parec.s:1, ' ', parec.r:15, ' ', parec.st);
   for i := 1 to 10 do write(parec.a[i]:1, ' '); writeln;
   writeln(parec.rc.a:1, ' ', parec.rc.b:1);
   writeln(parec.p^:1);
   writeln('s/b:');
   writeln('64 false j 1 3 12  4.54512000e-15 what ? who');
   writeln('21 22 23 24 25 26 27 28 29 30');
   writeln('2324 y');
   writeln('_bcde___i_');
   writeln('8454');

   { types in variants, and border clipping }
   write('Record3:   ');
   vra.i := 873;
   vra.vt := vti;
   vra.a := 427;
   vra.vdi := 235;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdi:1, ' ', vra.a:1);
   writeln(' s/b 873 0 235 427');
   write('Record4:   ');
   vra.i := 873;
   vra.vt := vtb;
   vra.b := 427;
   vra.vdb := true;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdb:5, ' ', vra.b:1);
   writeln(' s/b 873 1  true 427');
   write('Record5:   ');
   vra.i := 873;
   vra.vt := vtc;
   vra.c := 427;
   vra.vdc := 'f';
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdc, ' ', vra.c:1);
   writeln(' s/b 873 2 f 427');
   write('Record6:   ');
   vra.i := 873;
   vra.vt := vte;
   vra.d := 427;
   vra.vde := nine;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', ord(vra.vde):1, ' ', vra.d:1);
   writeln(' s/b 873 3 8 427');
   write('Record7:   ');
   vra.i := 873;
   vra.vt := vtes;
   vra.e := 427;
   vra.vdes := four;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', ord(vra.vdes):1, ' ', vra.e:1);
   writeln(' s/b 873 4 3 427');
   write('Record8:   ');
   vra.i := 873;
   vra.vt := vts;
   vra.f := 427;
   vra.vds := 12;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vds:1, ' ', vra.f:1);
   writeln(' s/b 873 5 12 427');
   write('Record9:   ');
   vra.i := 873;
   vra.vt := vtr;
   vra.g := 427;
   vra.vdr := 8734.8389;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdr:1:4, ' ', vra.g:1);
   writeln(' s/b 873 6 8734.8389 427');
   write('Record10:  ');
   vra.i := 873;
   vra.vt := vtst;
   vra.h := 427;
   vra.vdst := 'this one ?';
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdst, ' ', vra.h:1);
   writeln(' s/b 873 7 this one ? 427');
   write('Record11:  ');
   vra.i := 873;
   vra.vt := vta;
   vra.j := 427;
   for i := 1 to 10 do vra.vda[i] := i+10;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ');
   for i := 10 downto 1 do write(vra.vda[i]:1, ' ');
   writeln(vra.j:1);
   writeln('      s/b:  873 8 20 19 18 17 16 15 14 13 12 11 427');
   write('Record12:  ');
   vra.i := 873;
   vra.vt := vtrc;
   vra.k := 427;
   vra.vdrc.a := 2387;
   vra.vdrc.b := 't';
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdrc.a:1, ' ', vra.vdrc.b, ' ',
         vra.k:1);
   writeln(' s/b:  873 9 2387 t 427');
   write('Record14:  ');
   vra.i := 873;
   vra.vt := vtp;
   vra.m := 427;
   new(vra.vdp);
   vra.vdp^ := 2394;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdp^:1, ' ', vra.m:1);
   writeln(' s/b 873 11 2394 427');
end;

procedure part12;
begin
   { types of variant tags }
   write('Record15:  ');
   vvrs.vt := 10;
   vvrs.vi := 2343;
   write(vvrs.vt:1, ' ', vvrs.vi:1);
   writeln(' s/b 10 2343');
   write('Record16:  ');
   vvrs.vt := 19;
   vvrs.vb := true;
   write(vvrs.vt:1, ' ', vvrs.vb:5);
   writeln(' s/b 19  true');
   write('Record17:  ');
   vvrb.vt := true;
   vvrb.vi := 2343;
   write(vvrb.vt:5, ' ', vvrb.vi:1);
   writeln(' s/b  true 2343');
   write('Record18:  ');
   vvrb.vt := false;
   vvrb.vb := true;
   write(vvrb.vt:5, ' ', vvrb.vb:5);
   writeln(' s/b false  true');
   write('Record19:  ');
   vvre.vt := three;
   vvre.vi := 2343;
   write(ord(vvre.vt):1, ' ', vvre.vi:1);
   writeln(' s/b 2 2343');
   write('Record20:  ');
   vvre.vt := eight;
   vvre.vb := true;
   write(ord(vvre.vt):1, ' ', vvre.vb:5);
   writeln(' s/b 7  true');
   write('Record21:  ');
   vvres.vt := four;
   vvres.vi := 2343;
   write(ord(vvres.vt):1, ' ', vvres.vi:1);
   writeln(' s/b 3 2343');
   write('Record22:  ');
   vvres.vt := five;
   vvres.vb := true;
   write(ord(vvres.vt):1, ' ', vvres.vb:5);
   writeln(' s/b 4  true');
   { change to another tag constant in same variant }
   write('Record23:  ');
   vvrs.vt := 10;
   vvrs.vi := 42;
   i := vvrs.vi;
   vvrs.vt := 11;
   i := vvrs.vi;
   writeln(i:1, ' s/b 42');

   { nested records }
   write('Record24:  ');
   nvr.i := 1;
   nvr.r.i := 2;
   nvr.r.r.i := 3;
   nvr.r.r.r.i := 4;
   nvr.r.r.r.r.i := 5;
   nvr.r.r.r.r.r.i := 6;
   nvr.r.r.r.r.r.r.i := 7;
   nvr.r.r.r.r.r.r.r.i := 8;
   nvr.r.r.r.r.r.r.r.r.i := 9;
   nvr.r.r.r.r.r.r.r.r.r.i := 10;
   writeln(nvr.i:1, ' ',
           nvr.r.i:1, ' ',
           nvr.r.r.i:1, ' ',
           nvr.r.r.r.i:1, ' ',
           nvr.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.r.r.i:1, ' ',
           's/b 1 2 3 4 5 6 7 8 9 10');

   { 'with' statements }
   write('Record25:  ');
   with nvr do begin

      i := 10;
      with r do begin

         i := 9;
         with r do begin

            i := 8;
            with r do begin

               i := 7;
               with r do begin

                  i := 6;
                  with r do begin

                     i := 5;
                     with r do begin

                        i := 4;
                        with r do begin

                           i := 3;
                           with r do begin

                              i := 2;
                              with r do begin

                                 i := 2;
                                 with r do begin

                                    i := 1

                                 end

                              end

                           end

                        end

                     end

                  end

               end

            end

         end

      end

   end;
   writeln(nvr.i:1, ' ',
           nvr.r.i:1, ' ',
           nvr.r.r.i:1, ' ',
           nvr.r.r.r.i:1, ' ',
           nvr.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.r.r.i:1, ' ',
           's/b 10 9 8 7 6 5 4 3 2 1');
   write('Record26:  ');
   with nvr, r, r, r, r, r, r, r, r, r do i := 76;
   writeln(nvr.i:1, ' ',
           nvr.r.i:1, ' ',
           nvr.r.r.i:1, ' ',
           nvr.r.r.r.i:1, ' ',
           nvr.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.r.r.i:1, ' ',
           's/b 10 9 8 7 6 5 4 3 2 76');
   write('Record27:  ');
   new(rpa);
   with rpa^ do begin

      i := 1;
      with rc do b := 'g'

   end;
   writeln(rpa^.i:1, ' ', rpa^.rc.b, ' s/b 1 g');
   write('Record28:  ');
   for i := 1 to 10 do with ara[i] do a := i+10;
   for i := 10 downto 1 do with ara[i] do write(a:1, ' ');
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');

{******************************************************************************

                            Files

******************************************************************************}

if testfile then begin

   writeln;
   writeln('******************* files ******************************');
   writeln;

   { file base types }
   write('File1:   ');
   rewrite(fi);
   for i := 1 to 10 do write(fi, i+10);
   reset(fi);
   for i := 1 to 10 do begin read(fi, x); write(x:1, ' ') end;
   writeln('s/b 11 12 13 14 15 16 17 18 19 20');
   write('File2:   ');
   rewrite(pfi);
   for i := 1 to 10 do write(pfi, i+10);
   reset(pfi);
   for i := 1 to 10 do begin read(pfi, x); write(x:1, ' ') end;
   writeln('s/b 11 12 13 14 15 16 17 18 19 20');
   write('File3:   ');
   rewrite(fb);
   for i := 1 to 10 do write(fb, odd(i));
   reset(fb);
   for i := 1 to 10 do begin read(fb, ba); write(ba:5, ' ') end;
   writeln;
   writeln('   s/b:    true false  true false  true false  true false  true ',
           'false');
   write('File4:   ');
   rewrite(pfb);
   for i := 1 to 10 do write(pfb, odd(i));
   reset(pfb);
   for i := 1 to 10 do begin read(pfb, ba); write(ba:5, ' ') end;
   writeln;
   writeln('   s/b:    true false  true false  true false  true false  true ',
           'false');
   write('File5:   ');
   rewrite(fc);
   for ci := 'a' to 'j' do write(fc, ci);
   reset(fc);
   for ci := 'a' to 'j' do begin read(fc, ca); write(ca, ' ') end;
   writeln('s/b a b c d e f g h i j');
   write('File6:   ');
   rewrite(pfc);
   for ci := 'a' to 'j' do write(pfc, ci);
   reset(pfc);
   for ci := 'a' to 'j' do begin read(pfc, ca); write(ca, ' ') end;
   writeln('s/b a b c d e f g h i j');
   write('File7:   ');
   rewrite(fe);
   for ei := one to ten do write(fe, ei);
   reset(fe);
   for ei := one to ten do begin read(fe, ea); write(ord(ea):1, ' ') end;
   writeln('s/b 0 1 2 3 4 5 6 7 8 9');
   write('File8:   ');
   rewrite(pfe);
   for ei := one to ten do write(pfe, ei);
   reset(pfe);
   for ei := one to ten do begin read(pfe, ea); write(ord(ea):1, ' ') end;
   writeln('s/b 0 1 2 3 4 5 6 7 8 9');

   { types written to text }
   writeln('File9:');
   rewrite(ft);
   x := 7384;
   writeln(ft, x:1);
   writeln(ft, 8342:1);
   ba := true;
   writeln(ft, ba:5);
   writeln(ft, false:5);
   ca := 'm';
   writeln(ft, ca);
   writeln(ft, 'q');
   ra := 1234.5678e-3;
   writeln(ft, ra:15);
   writeln(ft, ra:1:7);
   writeln(ft, 5689.4321e-2:15);
   writeln(ft, 9383.7632e-4:1:8);
   s := 'hi there !';
   writeln(ft, s);
   writeln(ft, s:5);
   writeln(ft, s:15);
   reset(ft); get(ft); cc := ft^; reset(ft);
   while not eof(ft) do begin

      if eoln(ft) then begin

         readln(ft);
         writeln

      end else begin

         read(ft, ci);
         write(ci)

      end

   end;
   writeln('s/b:');
   writeln('7384');
   writeln('8342');
   writeln(' true');
   writeln('false');
   writeln('m');
   writeln('q');
   writeln(' 1.2345678000e+00');
   writeln('1.2345678');
   writeln(' 5.6894321000e+01');
   writeln('0.93837632');
   writeln('hi there !');
   writeln('hi th');
   writeln('     hi there !');

   { types read from text }
   writeln('file10:');
   reset(ft);
   readln(ft, y);
   writeln(y:1);
   readln(ft, y);
   writeln(y:1);
   readln(ft);
   readln(ft);
   readln(ft, ci);
   writeln(ci);
   readln(ft, ci);
   writeln(ci);
   readln(ft, rb);
   writeln(rb:15);
   readln(ft, rb);
   writeln(rb:15);
   readln(ft, rb);
   writeln(rb:15);
   readln(ft, rb);
   writeln(rb:15);
   writeln('s/b:');
   writeln('7384');
   writeln('8342');
   writeln('m');
   writeln('q');
   writeln(' 1.2345678000e+00');
   writeln(' 1.2345678000e+00');
   writeln(' 5.6894321000e+01');
   writeln(' 9.3837632000e-01');

   { line and file endings in text }
   writeln('file11:');
   rewrite(ft);
   writeln(ft, 'how now');
   writeln(ft, 'brown cow');
   reset(ft);
   write('''');
   while not eof(ft) do begin

      if eoln(ft) then write('<eoln>');
      read(ft, ca);
      write(ca)

   end;
   write('''');
   writeln(' s/b ''how now<eoln> brown cow<eoln> ''');
   writeln('file12:');
   rewrite(ft);
   writeln(ft, 'too much');
   write(ft, 'too soon');
   reset(ft);
   write('''');
   while not eof(ft) do begin

      if eoln(ft) then write('<eoln>');
      read(ft, ca);
      write(ca)

   end;
   write('''');
   writeln(' s/b ''too much<eoln> too soon<eoln> ''');

   { get/put and buffer variables }
   write('File13:   ');
   rewrite(fi);
   for i := 1 to 10 do begin fi^ := i+10; put(fi) end;
   reset(fi);
   for i := 1 to 10 do begin x := fi^; get(fi); write(x:1, ' ') end;
   writeln('s/b 11 12 13 14 15 16 17 18 19 20');
   write('File14:   ');
   rewrite(pfi);
   for i := 1 to 10 do begin pfi^ := i+10; put(pfi) end;
   reset(pfi);
   for i := 1 to 10 do begin x := pfi^; get(pfi); write(x:1, ' ') end;
   writeln('s/b 11 12 13 14 15 16 17 18 19 20');
   write('File15:   ');
   rewrite(fb);
   for i := 1 to 10 do begin fb^ := odd(i); put(fb) end;
   reset(fb);
   for i := 1 to 10 do begin ba := fb^; get(fb); write(ba:5, ' ') end;
   writeln;
   writeln('   s/b:    true false  true false  true false  true false  true ',
           'false');
   write('File16:   ');
   rewrite(pfb);
   for i := 1 to 10 do begin pfb^ := odd(i); put(pfb) end;
   reset(pfb);
   for i := 1 to 10 do begin ba := pfb^; get(pfb); write(ba:5, ' ') end;
   writeln;
   writeln('   s/b:    true false  true false  true false  true false  true ',
           'false');
   write('File17:   ');
   rewrite(fc);
   for ci := 'a' to 'j' do begin fc^ := ci; put(fc) end;
   reset(fc);
   for ci := 'a' to 'j' do begin ca := fc^; get(fc); write(ca, ' ') end;
   writeln('s/b a b c d e f g h i j');
   write('File18:   ');
   rewrite(pfc);
   for ci := 'a' to 'j' do begin pfc^ := ci; put(pfc) end;
   reset(pfc);
   for ci := 'a' to 'j' do begin ca := pfc^; get(pfc); write(ca, ' ') end;
   writeln('s/b a b c d e f g h i j');
   write('File19:   ');
   rewrite(fe);
   for ei := one to ten do begin fe^ := ei; put(fe) end;
   reset(fe);
   for ei := one to ten do begin ea := fe^; get(fe); write(ord(ea):1, ' ') end;
   writeln('s/b 0 1 2 3 4 5 6 7 8 9');
   write('File20:   ');
   rewrite(pfe);
   for ei := one to ten do begin pfe^ := ei; put(pfe) end;
   reset(pfe);
   for ei := one to ten do begin ea := pfe^; get(pfe); write(ord(ea):1, ' ') end;
   writeln('s/b 0 1 2 3 4 5 6 7 8 9');
   write('File21:   ');
   rewrite(ft);
   writeln(ft, '50');
   reset(ft);
   read(ft, srx);
   write(srx:1);
   writeln(' s/b ', 50:1);
   write('File22:   ');
   rewrite(ft);
   writeln(eof(ft), ' s/b true');

end;
end;

procedure part13;
begin
{******************************************************************************

                         Procedures and functions

******************************************************************************}

   writeln;
   writeln('************ Procedures and functions ******************');
   writeln;
   write('ProcedureFunction1:   ');
   x := 45; y := 89;
   junk1(x, y);
   writeln(' s/b 45 89');
   write('ProcedureFunction2:   ');
   x := 45; junk2(x);
   writeln(x:1, ' s/b 46');
   write('ProcedureFunction3:   ');
   s := 'total junk';
   junk3(s);
   writeln(' s/b total junk');
   write('ProcedureFunction4:   ');
   s := 'total junk';
   junk4(s);
   writeln(' s/b tota? junk');
   writeln('                      ', s, ' s/b total junk');
   write('ProcedureFunction5:   ');
   writeln(junk5(34):1, ' s/b 35');
   write('ProcedureFunction6:   ');
   i := junk7(10, 9, 8);
   writeln(' ', i:1);
   writeln('s/b:   10 9 8 6 5 4 3 2 1 78');
   writeln('ProcedureFunction7:');
   for i := 1 to 10 do ai[i] := i+10;
   arec.i := 64;
   arec.b := false;
   arec.c := 'j';
   arec.e := two;
   arec.es := four;
   arec.s := 12;
   arec.r := 4545.12e-16;
   arec.st := 'what ? who';
   for i := 1 to 10 do arec.a[i] := i+20;
   arec.rc.a := 2324;
   arec.rc.b := 'y';
   new(arec.p);
   arec.p^ := 8454;
   vrec.a := 23487;
   vrec.b := 'n';
   vrec.c := false;
   vrec.d := 'help me123';
   new(ip);
   ip^ := 734;
   junk8(93, true, 'k', eight, five, 10, 3.1414, 'hello, guy', ai, arec, vrec,
         ip);
   writeln('s/b:');
   writeln('93  true k 7 4 10  3.14140000e+00 hello, guy');
   writeln('11 12 13 14 15 16 17 18 19 20');
   writeln('64 false j 1 3 12  4.54512000e-15 what ? who');
   writeln('21 22 23 24 25 26 27 28 29 30');
   writeln('2324 y');
   writeln('_bcde___i_');
   writeln('8454');
   writeln('23487 n false');
   writeln('help me123');
   writeln('abcd___h__');
   writeln('734');
end;

begin
   part9;
   part10;
   part11;
   part12;
   part13;
end.
