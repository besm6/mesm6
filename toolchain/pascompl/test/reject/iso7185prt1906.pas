{

PRT test 1905: While statement

    The condition part of a while statement must have boolean type.
	ISO 7185 6.8.3.8

}

program iso7185prt1905(output);

type myBoolean = (myFalse, myTrue);

var b: myBoolean;

begin

   b := myFalse;
   while b do
      ;
   writeln('error not detected');

end.
