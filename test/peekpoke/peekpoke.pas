{=P-    Disable debug information and crash dump}
{=T-    Disable range checks}
{=S8    Disable checking for stack overflow}

program test();

{
|  Use external routines written in assembler.
}
function peek(addr: integer): integer;
    fortran;
procedure poke(val: integer; addr: integer);
    fortran;

begin
    repeat
        poke(peek(77755B), 77756B)
    until false;
end.
