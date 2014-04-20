{ Example 3: Read/write variables }
var i: integer, _s: string, d0: double;
begin
	writeln('Zadejte int value:');
	readln(i);
	writeln('Zadejte string value:');
	readln(_s);
	writeln('Zadejte double value:');
	readln(d0);

	writeln('');
	writeln('Jste zadal hodnoty:');
	writeln(i);
	writeln(_s);
	writeln(d0)
end.