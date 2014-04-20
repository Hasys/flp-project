{ Example 6: Duplicate variable name ERROR }
var a: integer, a: string;
begin
	writeln('Zadejte int value:');
	readln(a);
	writeln('Zadejte string value:');
	readln(a);

	writeln('');
	writeln('Jste zadal hodnoty:');
	writeln(a);
	writeln(a)
end.