program procparam(output);

procedure junk9(procedure junk9(junk9, b: integer; c: char);
                function y(a: integer): integer);
begin
    junk9(9834, 8383, 'j');
    write(' ', y(743):1);
end;

procedure junk10(x, y: integer; junk10: char);
begin
    write(x:1, ' ', y:1, ' ', junk10:1)
end;

function junk11(x: integer): integer;
begin
    junk11 := succ(x)
end;

procedure junk12(procedure xq(function yq(z: integer): integer);
                 function q(n: integer): integer);
begin
    xq(q)
end;

procedure junk13(function xz(z: integer): integer);
begin
    write(xz(941):1)
end;

procedure junk14;
    var i, x: integer;

    procedure junk15;
    begin
        write(i:1, ' ', x:1)
    end;
begin
    i := 62;
    x := 76;
    junk15
end;

procedure junk16; begin end;

procedure junk17(procedure x; i: integer);
    procedure junk18;
    begin
        write(i:1)
    end;
begin
    x;
    if i=52 then junk17(junk18, 83)
end;

{ test preference of pointer bonding to current scope }

procedure junk19;
    type pt = ^intalias;
         intalias = char;
    var p: pt;
begin
   new(p);
   p^ := 'a';
   write(p^);
   dispose(p)
end;

{ test ability to assign function result to nested function }

function junk20: integer;
    var i: integer;

    function inner: integer;
    begin
        inner := 12;
        junk20 := 37
    end;
begin
    i := inner
end;

begin
    write('ProcedureFunction8:   ');
    junk9(junk10, junk11);
    writeln(' s/b 9834 8383 j 744');
    write('ProcedureFunction9:   ');
    junk12(junk13, junk11);
    writeln(' s/b 942');
    write('ProcedureFunction10:   ');
    junk14;
    writeln(' s/b 62 76');
    write('ProcedureFunction11:   ');
    junk17(junk16, 52);
    writeln(' s/b 52');
    write('ProcedureFunction12:   ');
    junk19;
    writeln(' s/b a');
    write('ProcedureFunction13:   ');
    writeln(junk20:1, ' s/b 37');
end.
