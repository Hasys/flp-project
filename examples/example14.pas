{ Example 14: Call function as operator }
var s: string;

function helloWorld (s: string) : string;
function helloWorld (s: string) : string;
	begin
		helloWorld := 'Hello this ' + s + ' world'
	end

begin
	s := 'cruel';
	writeln(helloWorld(s) + '!')
end.