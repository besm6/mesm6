{=P-    Disable debug information and crash dump}
{=T-    Disable range checks}
{=S8    Disable checking for stack overflow}

program test();
var
    src, dst: @integer;
begin
    src := ptr(77755B);
    dst := ptr(77756B);
    repeat
        dst@ := src@
    until false;
end.
