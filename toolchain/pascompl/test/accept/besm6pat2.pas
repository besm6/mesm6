{******************************************************************************
*                                                                             *
*                       TEST SUITE FOR BESM6 PASCAL                           *
*                                                                             *
*                               Part 2 of 3                                   *
*                                                                             *
******************************************************************************}

program besm6pat(output);

const
      rcnst = 43.33;
      rscst = -84.22;
      rscst2 = -rcnst;
      rscst3 = -rscst;
      cone = 1;

type
     enum  = (one, two, three, four, five, six, seven, eight, nine, ten);

var
    i, x:  integer;
    srx, sry: 0..100;
    ba, bb, bc : boolean;
    sva, svb, svc : (mon, tue, wed, thur, fri, sat, sun);
    ra, rb, rc, rd, re: real;
    sta,   stb, stc, std: set of 1..47;
    ste:   set of 1..10;
    stf:   packed set of 1..10;
    stg:   packed set of 1..20;
    sena,  senb, senc, send: set of enum;
    sene:  set of one..five;
    senf:  packed set of enum;
    seng:  packed set of one..seven;
    ei:    enum;
    sba,   sbb, sbc, sbd: set of boolean;
    sbe:   set of false..true;
    sbf:   packed set of boolean;
    sbg:   packed set of false..true;
    pi1, pi2: ^integer;

procedure part5;
begin
{******************************************************************************

                            Booleans

******************************************************************************}

   writeln;
   writeln('******************* Booleans *******************');
   writeln;

   { boolean variables }
   ba := true; bb := false; bc := true;
   writeln('Boolean1:   ', ba:5, ' ', bb:5, ' s/b true false');
   writeln('Boolean2:   ', succ(bb):5, ' s/b true');
   writeln('Boolean3:   ', pred(ba):5, ' s/b false');
   writeln('Boolean4:   ', ord(bb):1, ' s/b 0');
   writeln('Boolean5:   ', ord(ba):1, ' s/b 1');
   writeln('Boolean6:   ', ba = bc:5, ' s/b true');
   writeln('Boolean7:   ', bb = bb:5, ' s/b true');
   writeln('Boolean8:   ', ba = bb:5, ' s/b false');
   writeln('Boolean9:   ', bb < ba:5, ' s/b true');
   writeln('Boolean10:  ', ba < bb:5, ' s/b false');
   writeln('Boolean11:  ', ba > bb:5, ' s/b true');
   writeln('Boolean12:  ', bb > ba:5, ' s/b false');
   writeln('Boolean13:  ', ba <> bb:5, ' s/b true');
   writeln('Boolean14:  ', ba <> bc:5, ' s/b false');
   writeln('Boolean15:  ', bb <= ba:5, ' s/b true');
   writeln('Boolean16:  ', ba <= bc:5, ' s/b true');
   writeln('Boolean17:  ', ba <= bb:5, ' s/b false');
   writeln('Boolean18:  ', ba >= bb:5, ' s/b true');
   writeln('Boolean19:  ', bb >= bb:5, ' s/b true');
   writeln('Boolean20:  ', bb >= ba:5, ' s/b false');
   write('Boolean21:  ');
   for ba := false to true do write(ba:5, ' ');
   writeln('s/b false true');
   write('Boolean22:  ');
   for bb := true downto false do write(bb:5, ' ');
   writeln('s/b true false');
   write('Boolean23:  ');
   ba := 1 > 0; writeln(ba:5, ' s/b true');
   write('Boolean24:  ');
   ba := 1 < 0; writeln(ba:5, ' s/b false');

   { boolean constants }
   writeln('Boolean25:  ', true:5, ' ', false:5, ' s/b true false');
   writeln('Boolean26:  ', succ(false):5, ' s/b true');
   writeln('Boolean27:  ', pred(true):5, ' s/b false');
   writeln('Boolean28:  ', ord(false):1, ' s/b 0');
   writeln('Boolean29:  ', ord(true):1, ' s/b 1');
   writeln('Boolean30:  ', true = true:5, ' s/b true');
   writeln('Boolean31:  ', false = false:5, ' s/b true');
   writeln('Boolean32:  ', true = false:5, ' s/b false');
   writeln('Boolean33:  ', false < true:5, ' s/b true');
   writeln('Boolean34:  ', true < false:5, ' s/b false');
   writeln('Boolean35:  ', true > false:5, ' s/b true');
   writeln('Boolean36:  ', false > true:5, ' s/b false');
   writeln('Boolean37:  ', true <> false:5, ' s/b true');
   writeln('Boolean38:  ', true <> true:5, ' s/b false');
   writeln('Boolean39:  ', false <= true:5, ' s/b true');
   writeln('Boolean40:  ', true <= true:5, ' s/b true');
   writeln('Boolean41:  ', true <= false:5, ' s/b false');
   writeln('Boolean42:  ', true >= false:5, ' s/b true');
   writeln('Boolean43:  ', false >= false:5, ' s/b true');
   writeln('Boolean44:  ', false >= true:5, ' s/b false');
   writeln('Boolean45:');
   for i := 10 downto 1 do writeln(false:i);
   writeln('Boolean45: s/b:');
   writeln('     false');
   writeln('    false');
   writeln('   false');
   writeln('  false');
   writeln(' false');
   writeln('false');
   writeln('fals');
   writeln('fal');
   writeln('fa');
   writeln('f');
   writeln('Boolean46:');
   for i := 10 downto 1 do writeln(true:i);
   writeln('Boolean46: s/b:');
   writeln('      true');
   writeln('     true');
   writeln('    true');
   writeln('   true');
   writeln('  true');
   writeln(' true');
   writeln('true');
   writeln('tru');
   writeln('tr');
   writeln('t');

{******************************************************************************

                            Scalar variables

******************************************************************************}

   writeln;
   writeln('******************* Scalar *******************');
   writeln;

   { scalar variables }
   sva := wed; svb := mon; svc := wed;
   writeln('Scalar1:   ', succ(svb) = tue:5, ' s/b true');
   writeln('Scalar2:   ', pred(sva) = tue:5, ' s/b true');
   writeln('Scalar3:   ', ord(svb):1, ' s/b 0');
   writeln('Scalar4:   ', ord(sva):1, ' s/b 2');
   writeln('Scalar5:   ', sva = svc:5, ' s/b true');
   writeln('Scalar6:   ', svb = svb:5, ' s/b true');
   writeln('Scalar7:   ', sva = svb:5, ' s/b false');
   writeln('Scalar8:   ', svb < sva:5, ' s/b true');
   writeln('Scalar9:   ', sva < svb:5, ' s/b false');
   writeln('Scalar10:  ', sva > svb:5, ' s/b true');
   writeln('Scalar11:  ', svb > sva:5, ' s/b false');
   writeln('Scalar12:  ', sva <> svb:5, ' s/b true');
   writeln('Scalar13:  ', sva <> svc:5, ' s/b false');
   writeln('Scalar14:  ', svb <= sva:5, ' s/b true');
   writeln('Scalar15:  ', sva <= svc:5, ' s/b true');
   writeln('Scalar16:  ', sva <= svb:5, ' s/b false');
   writeln('Scalar17:  ', sva >= svb:5, ' s/b true');
   writeln('Scalar18:  ', svb >= svb:5, ' s/b true');
   writeln('Scalar19:  ', svb >= sva:5, ' s/b false');
   write('Scalar20:  ');
   for sva := mon to sun do write(ord(sva):1, ' ');
   writeln('s/b 0 1 2 3 4 5 6');
   write('Scalar21:  ');
   for svb := sun downto mon do write(ord(svb):1, ' ');
   writeln('s/b 6 5 4 3 2 1 0');

   { scalar constants }
   writeln('Scalar20:   ', succ(mon) = tue:5, ' s/b true');
   writeln('Scalar21:   ', pred(fri) = thur:5, ' s/b true');
   writeln('Scalar22:   ', ord(wed):1, ' s/b 2');
   writeln('Scalar23:   ', ord(sun):1, ' s/b 6');
   writeln('Scalar24:   ', thur = thur:5, ' s/b true');
   writeln('Scalar25:   ', fri = fri:5, ' s/b true');
   writeln('Scalar26:   ', tue = wed:5, ' s/b false');
   writeln('Scalar27:   ', mon < wed:5, ' s/b true');
   writeln('Scalar28:   ', fri < fri:5, ' s/b false');
   writeln('Scalar29:  ', sun > sat:5, ' s/b true');
   writeln('Scalar30:  ', fri > sun:5, ' s/b false');
   writeln('Scalar31:  ', thur <> tue:5, ' s/b true');
   writeln('Scalar32:  ', wed <> wed:5, ' s/b false');
   writeln('Scalar33:  ', mon <= fri:5, ' s/b true');
   writeln('Scalar34:  ', fri <= fri:5, ' s/b true');
   writeln('Scalar35:  ', sat <= fri:5, ' s/b false');
   writeln('Scalar36:  ', fri >= tue:5, ' s/b true');
   writeln('Scalar37:  ', tue >= tue:5, ' s/b true');
   writeln('Scalar38:  ', tue >= sat:5, ' s/b false');
end;

procedure part6;
begin
{******************************************************************************

                            Reals

******************************************************************************}

   writeln;
   writeln('******************* Reals ******************************');
   writeln;

   { formats, input (compiler) and output }
   writeln('Real1:   ', 1.554:15, ' s/b  1.554000e+00');
   writeln('Real2:   ', 0.00334:15, ' s/b  3.340000e-03');
   writeln('Real3:   ', 0.00334e-13:15, ' s/b  3.34000e-16');
   writeln('Real4:   ', 4e-18:15, ' s/b  4.000000e-18');
   writeln('Real5:   ', -5.565:15, ' s/b -5.565000e+00');
   writeln('Real6:   ', -0.00944:15, ' s/b -9.440000e-03');
   writeln('Real7:   ', -0.006364E+18:15, ' s/b -6.364000e+15');
   writeln('Real8:   ', -2e-14:15, ' s/b -2.000000e-14');
   writeln('Real9:');
   writeln('         11111111112222222222333333333344444444445');
   writeln('12345678901234567890123456789012345678901234567890');
   for i := 1 to 20 do writeln(1.23456789012345678901234567890:i);
   writeln('s/b (note precision dropoff at right):');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.23e+000');
   writeln(' 1.234e+000');
   writeln(' 1.2345e+000');
   writeln(' 1.23456e+000');
   writeln(' 1.234567e+000');
   writeln(' 1.2345678e+000');
   writeln(' 1.23456789e+000');
   writeln(' 1.234567890e+000');
   writeln(' 1.2345678901e+000');
   writeln(' 1.23456789012e+000');
   writeln(' 1.234567890123e+000');
   writeln('Real10:');
   writeln('         11111111112222222222333333333344444444445');
   writeln('12345678901234567890123456789012345678901234567890');
   for i := 1 to 20 do writeln(i+0.23456789012345678901234567890:1:i);
   writeln('s/b (note precision dropoff at right):');
   writeln('1.2');
   writeln('2.23');
   writeln('3.234');
   writeln('4.2345');
   writeln('5.23456');
   writeln('6.234567');
   writeln('7.2345678');
   writeln('8.23456789');
   writeln('9.234567890');
   writeln('10.2345678901');
   writeln('11.23456789012');
   writeln('12.234567890123');
   writeln('13.2345678901234');
   writeln('14.23456789012345');
   writeln('15.234567890123456');
   writeln('16.2345678901234567');
   writeln('17.23456789012345678');
   writeln('18.234567890123456789');
   writeln('19.2345678901234567890');
   writeln('20.23456789012345678901');

   { unsigned variables }
   ra := 435.23;
   rb := 983.67;
   rc := rb;
   rd := 0.3443;
   writeln('Real11:  ', ra + rb:15, ' s/b  1.418900e+03');
   writeln('Rea112:  ', rb - ra:15, ' s/b  5.484399e+02');
   writeln('Real13:  ', ra * rb:15, ' s/b  4.281227e+05');
   writeln('Real14:  ', rb / ra:15, ' s/b  2.260115e+00');
   writeln('Real15:  ', rc = rb:5, ' s/b true');
   writeln('Real16:  ', ra = rb:5, ' s/b false');
   writeln('Real17:  ', ra < rb:5, ' s/b true');
   writeln('Real18:  ', rb < ra:5, ' s/b false');
   writeln('Real19:  ', rb > ra:5, ' s/b true');
   writeln('Real20:  ', ra > rb:5, ' s/b false');
   writeln('Real21:  ', ra <> rb:5, ' s/b true');
   writeln('Real22:  ', rb <> rc:5, ' s/b false');
   writeln('Real23:  ', ra <= rb:5, ' s/b true');
   writeln('Real24:  ', rc <= rb:5, ' s/b true');
   writeln('Real25:  ', rb <= ra:5, ' s/b false');
   writeln('Real26:  ', rb >= ra:5, ' s/b true');
   writeln('Real27:  ', rb >= rc:5, ' s/b true');
   writeln('Real28:  ', ra >= rb:5, ' s/b false');
   writeln('Real29:  ', abs(ra):15, ' s/b  4.35230e+02');
   writeln('Real30:  ', sqr(ra):15, ' s/b  1.89425e+05');
   writeln('Real31:  ', sqrt(rb):15, ' s/b  3.13635e+01');
   writeln('Real32:  ', sin(rb):15, ' s/b -3.44290e-01');
   writeln('Real33:  ', arctan(ra):15, ' s/b  1.56850e+00');
   writeln('Real34:  ', exp(rd):15, ' s/b  1.41100e+00');
   writeln('Real35:  ', ln(ra):15, ' s/b  6.07587e+00');
   writeln('Real36:  ', trunc(ra):1, ' s/b 435');
   writeln('Real37:  ', round(rb):1, ' s/b 984');
   writeln('Real38:  ', round(ra):1, ' s/b 435');

   { unsigned constants }
   writeln('Real39:  ', 344.939 + 933.113:15, ' s/b  1.278052e+03');
   writeln('Real40:  ', 883.885 - 644.939:15, ' s/b  2.389460e+02');
   writeln('Real41:  ', 754.74 * 138.75:15, ' s/b  1.047202e+05');
   writeln('Real42:  ', 634.3 / 87373.99:15, ' s/b  7.259598e-03');
   writeln('Real43:  ', 77.44 = 77.44:5, ' s/b true');
   writeln('Real44:  ', 733.9 = 959.2:5, ' s/b false');
   writeln('Real45:  ', 883.22 < 8383.33:5, ' s/b true');
   writeln('Real46:  ', 475.322 < 234.93:5, ' s/b false');
   writeln('Real47:  ', 7374.3 > 6442.34:5, ' s/b true');
   writeln('Real48:  ', 985.562 > 1001.95:5, ' s/b false');
   writeln('Real49:  ', 030.11 <> 0938.44:5, ' s/b true');
   writeln('Real50:  ', 1.233 <> 1.233:5, ' s/b false');
   writeln('Real51:  ', 8484.002 <= 9344.003:5, ' s/b true');
   writeln('Real52:  ', 9.11 <= 9.11:5, ' s/b true');
   writeln('Real53:  ', 93.323 <= 90.323:5, ' s/b false');
   writeln('Real54:  ', 6543.44 >= 5883.33:5, ' s/b true');
   writeln('Real55:  ', 3247.03 >= 3247.03:5, ' s/b true');
   writeln('Real56:  ', 28343.22 >= 30044.45:5, ' s/b false');
   writeln('Real57:  ', abs(34.93):15, ' s/b  3.493000e+01');
   writeln('Real58:  ', sqr(2.34):15, ' s/b  5.475600e+00');
   writeln('Real59:  ', sqrt(9454.32):15, ' s/b  9.723333e+01');
   writeln('Real60:  ', sin(34.22):15, ' s/b  3.311461e-01');
   writeln('Real61:  ', arctan(343.2):15, ' s/b  1.567883e+00');
   writeln('Real62:  ', exp(0.332):15, ' s/b  1.393753e+00');
   writeln('Real63:  ', ln(83.22):15, ' s/b  4.421488e+00');
   writeln('Real64:  ', trunc(24.344):1, ' s/b 24');
   writeln('Real65:  ', round(74.56):1, ' s/b 75');
   writeln('Real66:  ', round(83.24):1, ' s/b 83');
   writeln('Real67:  ', rcnst:15, ' s/b  4.333000e+01');

   { signed variables }
   ra := -734.2;
   rb := -7634.52;
   rc := ra;
   rd := 1034.54;
   re := -0.38483;
   writeln('Real68:  ', ra + rd:15, ' s/b  3.003400e+02');
   writeln('Real69:  ', rd + ra:15, ' s/b  3.003400e+02');
   writeln('Real70:  ', rb + rd:15, ' s/b -6.599980e+03');
   writeln('Real71:  ', ra + rb:15, ' s/b -8.368720e+03');
   writeln('Real72:  ', rd - ra:15, ' s/b  1.768740e+03');
   writeln('Real73:  ', rb - rd:15, ' s/b -8.669061e+03');
   writeln('Real74:  ', rb - ra:15, ' s/b -6.900320e+03');
   writeln('Real75:  ', rd * ra:15, ' s/b -7.595593e+05');
   writeln('Real76:  ', ra * rd:15, ' s/b -7.595593e+05');
   writeln('Real77:  ', ra * rb:15, ' s/b  5.605265e+06');
   writeln('Real78:  ', rd / ra:15, ' s/b -1.409071e+00');
   writeln('Real79:  ', rb / rd:15, ' s/b -7.379627e+00');
   writeln('Real80:  ', rb / ra:15, ' s/b  1.039842e+01');
   writeln('Real81:  ', ra = rc:5, ' s/b true');
   writeln('Real82:  ', ra = rb:5, ' s/b false');
   writeln('Real83:  ', ra <> rb:5, ' s/b true');
   writeln('Real84:  ', ra <> rc:5, ' s/b false');
   writeln('Real85:  ', ra < rd:5, ' s/b true');
   writeln('Real86:  ', rb < ra:5, ' s/b true');
   writeln('Real87:  ', rd < ra:5, ' s/b false');
   writeln('Real88:  ', ra < rb:5, ' s/b false');
   writeln('Real89:  ', rd > ra:5, ' s/b true');
   writeln('Real90:  ', ra > rb:5, ' s/b true');
   writeln('Real91:  ', ra > rd:5, ' s/b false');
   writeln('Real92:  ', rb > ra:5, ' s/b false');
   writeln('Real93:  ', ra <= rd:5, ' s/b true');
   writeln('Real94:  ', rb <= ra:5, ' s/b true');
   writeln('Real95:  ', ra <= rc:5, ' s/b true');
   writeln('Real96:  ', rd <= ra:5, ' s/b false');
   writeln('Real97:  ', ra <= rb:5, ' s/b false');
   writeln('Real98:  ', rd >= ra:5, ' s/b true');
   writeln('Real99:  ', ra >= rb:5, ' s/b true');
end;

procedure part7;
begin
   writeln('Real100: ', ra >= rc:5, ' s/b true');
   writeln('Real101: ', ra >= rd:5, ' s/b false');
   writeln('Real102: ', rb >= ra:5, ' s/b false');
   writeln('Real103: ', abs(ra):15, ' s/b  7.34200e+02');
   writeln('Real104: ', sqr(ra):15, ' s/b  5.39050e+05');
   writeln('Real105: ', sin(rb):15, ' s/b -4.34850e-01');
   writeln('Real106: ', arctan(ra):15, ' s/b -1.56943e+00');
   writeln('Real107: ', exp(re):15, ' s/b  6.80566e-01');
   writeln('Real108: ', trunc(ra):15, ' s/b -734');
   writeln('Real109: ', round(rb):15, ' s/b -7635');
   writeln('Real110: ', round(ra):15, ' s/b -734');

   { signed constants }
   writeln('Real111: ', 45.934 + (-30.834):15, ' s/b  1.510000e+01');
   writeln('Real112: ', -25.737 + 70.87:15, ' s/b  4.513300e+01');
   writeln('Real113: ', -62.63 + 23.99:15, ' s/b -3.864000e+01');
   writeln('Real114: ', -20.733 + (-15.848):15, ' s/b -3.658100e+01');
   writeln('Real115: ', 20.774 - (-14.774):15, ' s/b  3.554800e+01');
   writeln('Real116: ', -34.523 - 14.8754:15, ' s/b -4.939840e+01');
   writeln('Real117: ', -56.664 - (-12.663):15, ' s/b -4.400100e+01');
   writeln('Real118: ', 5.663 * (-4.664):15, ' s/b -2.641223e+01');
   writeln('Real119: ', (-18.62) * 7.997:15, ' s/b -1.489041e+02');
   writeln('Real120: ', (-40.552) * (-13.774):15, ' s/b  5.585632e+02');
   writeln('Real121: ', 30.6632 / (-5.874):15, ' s/b -5.220157e+00');
   writeln('Real122: ', (-50.636) / 2.8573:15, ' s/b -1.772163e+01');
   writeln('Real123: ', (-20.7631) / (-4.85734):15, ' s/b  4.274582e+00');
   writeln('Real124: ', -5.775 = -5.775:5, ' s/b true');
   writeln('Real125: ', -5.6364 = 5.8575:5, ' s/b false');
   writeln('Real126: ', -21.6385 <> -40.764:5, ' s/b true');
   writeln('Real127: ', -21.772 <> -21.772:5, ' s/b false');
   writeln('Real128: ', -3.512 < 5.8467:5, ' s/b true');
   writeln('Real129: ', -32.644 < -20.9074:5, ' s/b true');
   writeln('Real130: ', 20.763 < -20.743:5, ' s/b false');
   writeln('Real131: ', -15.663 < -40.784:5, ' s/b false');
   writeln('Real132: ', 70.766 > -4.974:5, ' s/b true');
   writeln('Real133: ', -23.6532 > -34.774:5, ' s/b true');
   writeln('Real134: ', -5.773 > 5.9874:5, ' s/b false');
   writeln('Real135: ', -60.663 > -59.78:5, ' s/b false');
   writeln('Real136: ', -12.542 <= 4.0848:5, ' s/b true');
   writeln('Real137: ', -14.8763 <= -5.0847:5, ' s/b true');
   writeln('Real138: ', -7.8373 <= -7.8373:5, ' s/b true');
   writeln('Real139: ', 5.4564 <= -5.4564:5, ' s/b false');
   writeln('Real140: ', -10.72633 <= -20.984:5, ' s/b false');
   writeln('Real141: ', 9.834 >= -3.9383:5, ' s/b true');
   writeln('Real142: ', -4.562 >= -10.74:5, ' s/b true');
   writeln('Real143: ', -13.63 >= -13.63:5, ' s/b true');
   writeln('Real144: ', -6.74 >= 6.74:5, ' s/b false');
   writeln('Real145: ', -20.7623 >= -10.574:5, ' s/b false');
   writeln('Real146: ', abs(-6.823):15, ' s/b  6.823000e+00');
   writeln('Real147  ', sqr(-348.22):15, ' s/b  1.212572e+05');
   writeln('Real148: ', sin(-733.22):15, ' s/b  9.421146e-01');
   writeln('Real149: ', arctan(-8387.22):15, ' s/b -1.570677e+00');
   writeln('Real150: ', exp(-0.8743):15, ' s/b  4.171539e-01');
   writeln('Real151: ', trunc(-33.422):1, ' s/b -33');
   writeln('Real152: ', round(-843.22):1, ' s/b -843');
   writeln('Real153: ', round(-6243.76):1, ' s/b -6244');
   writeln('Real154: ', rscst:15, ' s/b -8.422000e+01');
   writeln('Real155: ', -rscst:15, ' s/b  8.422000e+01');
   writeln('Real156:  ', rscst2:15, ' s/b -4.333000e+01');
   writeln('Real157: ', rscst3:15, ' s/b  8.422000e+01');

{******************************************************************************

                            Sets

******************************************************************************}

   writeln;
   writeln('******************* sets ******************************');
   writeln;

   { sets of integers }
   write('Set1:  ');
   sta := [];
   for i := 1 to 10 do if odd(i) then sta := sta+[i, i+10];
   for i := 1 to 20 do if i in sta then write('1') else write('0');
   write(' s/b ');
   writeln('10101010101010101010');
   write('Set2:  ');
   sta := [1, 4, 5];
   stb := [2, 6, 10];
   for i := 1 to 10 do if i in sta+stb then write('1') else write('0');
   write(' s/b ');
   writeln('1101110001');
   write('Set3:  ');
   sta := [1, 2, 6, 5, 7];
   stb := [2, 6, 10];
   for i := 1 to 10 do if i in sta*stb then write('1') else write('0');
   write(' s/b ');
   writeln('0100010000');
   write('Set4:  ');
   sta := [2, 4, 7, 8];
   stb := [1, 3, 4, 8, 10];
   for i := 1 to 10 do if i in sta-stb then write('1') else write('0');
   write(' s/b ');
   writeln('0100001000');
   sta := [4, 6, 8, 9];
   stb := [1, 4, 5, 9];
   stc := [4, 6, 8, 9];
   writeln('Set5:  ', sta = stb:5, ' s/b false');
   writeln('Set6:  ', sta = stc:5, ' s/b true');
   writeln('Set7:  ', sta <> stb:5, ' s/b true');
   writeln('Set8:  ', sta <> stc:5, ' s/b false');
   sta := [1, 2, 5, 7, 10];
   stb := [1, 5, 10];
   stc := [1, 5, 10, 6];
   std := [1, 2, 5, 7, 10];
   writeln('Set9:  ', stb <= sta:5, ' s/b true');
   writeln('Set10: ', stb <= std:5, ' s/b true');
   writeln('Set11: ', stc <= sta:5, ' s/b false');
   writeln('Set12: ', sta >= stb:5, ' s/b true');
   writeln('Set13: ', std >= stb:5, ' s/b true');
   writeln('Set14: ', sta >= stc:5, ' s/b false');
   write('Set15: ');
   i := 2;
   x := 4;
   sta := [i, x, i+x];
   for i := 1 to 10 do if i in sta then write('1') else write('0');
   write(' s/b ');
   writeln('0101010000');
   { these are just compile time tests }
   ste := std;
   stf := [1, 2, 5, 7];
   stg := stf;
   i := 10;
   writeln('Set16: ', 5 in [cone..i], ' s/b true');

   { sets of enumerated }
   write('Set32: ');
   sena := [];
   for ei := one to ten do if odd(ord(ei)) then sena := sena+[ei];
   for ei := one to ten do if ei in sena then write('1') else write('0');
   write(' s/b ');
   writeln('0101010101');
   write('Set33: ');
   sena := [one, four, five];
   senb := [two, six, ten];
   for ei := one to ten do if ei in sena+senb then write('1') else write('0');
   write(' s/b ');
   writeln('1101110001');
   write('Set34: ');
   sena := [one, two, six, five, seven];
   senb := [two, six, ten];
   for ei := one to ten do if ei in sena*senb then write('1') else write('0');
   write(' s/b ');
   writeln('0100010000');
   write('Set35: ');
   sena := [two, four, seven, eight];
   senb := [one, three, four, eight, ten];
   for ei := one to ten do if ei in sena-senb then write('1') else write('0');
   write(' s/b ');
   writeln('0100001000');
   sena := [four, six, eight, nine];
   senb := [one, four, five, nine];
   senc := [four, six, eight, nine];
   writeln('Set36: ', sena = senb:5, ' s/b false');
   writeln('Set37: ', sena = senc:5, ' s/b true');
   writeln('Set38: ', sena <> senb:5, ' s/b true');
   writeln('Set39: ', sena <> senc:5, ' s/b false');
   sena := [one, two, five, seven, ten];
   senb := [one, five, ten];
   senc := [one, five, ten, six];
   send := [one, two, five, seven, ten];
   writeln('Set40: ', senb <= sena:5, ' s/b true');
   writeln('Set41: ', senb <= send:5, ' s/b true');
   writeln('Set42: ', senc <= sena:5, ' s/b false');
   writeln('Set43: ', sena >= senb:5, ' s/b true');
   writeln('Set44: ', send >= senb:5, ' s/b true');
   writeln('Set45: ', sena >= senc:5, ' s/b false');
   write('Set46: ');
   ei := two;
   sena := [ei, succ(ei)];
   for ei := one to ten do if ei in sena then write('1') else write('0');
   write(' s/b ');
   writeln('0110000000');
   { these are just compile time tests }
   send := [one, two, five];
   sene := send;
   senf := [one, two, five, seven];
   seng := senf;
end;

procedure part8;
begin
   { sets of boolean }
   write('Set47: ');
   sba := [];
   for ba := false to true do if odd(ord(ba)) then sba := sba+[ba];
   for ba := false to true do if ba in sba then write('1') else write('0');
   write(' s/b ');
   writeln('01');
   write('Set48: ');
   sba := [false];
   sbb := [true];
   for ba := false to true do if ba in sba+sbb then write('1') else write('0');
   write(' s/b ');
   writeln('11');
   write('Set49: ');
   sba := [false, true];
   sbb := [false];
   for ba := false to true do if ba in sba*sbb then write('1') else write('0');
   write(' s/b ');
   writeln('10');
   write('Set50: ');
   sba := [true, false];
   sbb := [true];
   for ba := false to true do if ba in sba-sbb then write('1') else write('0');
   write(' s/b ');
   writeln('10');
   sba := [true];
   sbb := [false];
   sbc := [true];
   writeln('Set51: ', sba = sbb:5, ' s/b false');
   writeln('Set52: ', sba = sbc:5, ' s/b true');
   writeln('Set53: ', sba <> sbb:5, ' s/b true');
   writeln('Set54: ', sba <> sbc:5, ' s/b false');
   sba := [true, false];
   sbb := [false];
   sbc := [true];
   sbd := [false];
   writeln('Set55: ', sbb <= sba:5, ' s/b true');
   writeln('Set56: ', sbb <= sbd:5, ' s/b true');
   writeln('Set57: ', sbc <= sbb:5, ' s/b false');
   writeln('Set58: ', sba >= sbb:5, ' s/b true');
   writeln('Set59: ', sbd >= sbb:5, ' s/b true');
   writeln('Set60: ', sbb >= sbc:5, ' s/b false');
   write('Set61: ');
   ba := false;
   sba := [ba, succ(ba)];
   for ba := false to true do if ba in sba then write('1') else write('0');
   write(' s/b ');
   writeln('11');
   { these are just compile time tests }
   sbe := sbd;
   sbf := [true];
   sbg := sbf;
   write('set62: ');
   new(pi1);
   new(pi2);
   pi1^ := 3;
   pi2^ := 5;
   write([pi1^..pi2^] = [3..5]:5);
   writeln(' s/b true');
   write('set63: ');
   srx := 1;
   sry := 10;
   for i := 1 to 10 do if i in [srx,sry] then write('1') else write('0');
   writeln(' s/b 1000000001');
end;

begin
   part5;
   part6;
   part7;
   part8;
end.
