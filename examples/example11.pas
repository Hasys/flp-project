{ Example 11: Ruzné děklarace funkcii ERROR }
var res: integer, s: string;

function helloWorld (s: string) : integer;
function helloWorld (s: string) : integer;
	begin
		writeln('Hello this ' + s + ' world');
		helloWorld := 0
	end

function helloWorld (s: string) : integer;
	begin
		writeln('Mega hello this ' + s + ' world');
		goodbueWorld := 0
	end

begin
	s := 'cruel';
	res := helloWorld(s);	
	res := helloWorld(s)
end.