(*P-, T-, S8, U-, Y+*)
program hello(output, pastel);
var
    pastel: array [0..30B] of integer;
    procedure dump; fortran;
begin
    (* Dump the binary image. *)
    dump;

    (* Disable 'what?' prompt. *)
    pastel[3] := 0C;

    writeln('Hello, World!');
end.
