{ Example 9: Porovnaní }
var a: integer, b: double;
begin
	writeln('Porovnaní hodnot:');

	if 5 > 4 then 
		writeln('5 > 4')
	else 
		writeln('5 < 4');

	if 7 < 6 then 
		writeln('5 < 4')
	else 
		writeln('5 > 4');

	if 5.1 > 4.9 then 
		writeln('5.1 > 4.9')
	else 
		writeln('5.1 < 4.9');

	if 9.2 < 9.21 then 
		writeln('9.2 < 9.21')
	else 
		writeln('9.2 > 9.21');

	if 19.001 > 19 then 
		writeln('19.001 > 19')
	else 
		writeln('19.001 < 19');

	if 19.001 < 19 then 
		writeln('19.001 < 19')
	else 
		writeln('19.001 > 19');

	a := 5;
	b := 0.1;
	writeln('');
	writeln('Od 5 do 1');
	while a >= b do begin
		writeln(a);
		a := a - 1
	end;

	a := 100;
	b := 95;
	writeln('');
	writeln('Od 95 do 100');
	while b <= a do begin
		writeln(b);
		b := b + 1
	end

end.