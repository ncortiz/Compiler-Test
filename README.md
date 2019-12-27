# Single-File-Compiler-From-Scratch-Recursive-Descent-LR-and-NASM-asm-gen

<img src="view.png">

<p>
C-like to NASM assembly compiler in one file with combined Lexer-> Recursive Descent Parser->Code Gen. written in C++ with only standard libraries, parsers and a nice command line coloring system!
All in only 1328 lines of code!
</p>

<h2> Grammar: </h2>
<code>
 NUM-LIT: [0-9]+ ['b'|'q']? 'u'?
 
 CHAR-LIT: '\'' [\0-\255] '\''
 
 typespec: 'unsigned'? ('byte' | 'word' | 'dword' | 'qword' | 'void' ) 'ptr'?
 
 typespec-list: typespec (',' typespec)*
 
 expr-list: expr (',' expr)*
 
 param-list : IDENT ':' typespec (',' IDENT ':' typespec)*
 

 atom-expr: NUM-LIT
 
		  : CHAR-LIT
		  
		  : IDENT ('++'| '--' | '(' expr-list? ')' | '<' typespec-list '>' )?
		  
		  : ('++' | '--') IDENT
		  
		  : ('-' atom | '~' atom | '!' atom | '&' atom | '*' atom)
		  
		  : '(' expr ')'
		  


	mul-div-mod-expr:   atom-expr (('*' | '/' | '%') atom-expr)*
	
	add-sub-expr:       mul-div-mod-expr (('+' | '-') mul-div-mod-expr)*
	
	shift-expr:         add-sub-expr (('<<'|'>>') add-sub-expr)*
	
	lt-gt-lte-gte-expr: shift-expr(('<?' | '>?' | '<=' | '>=') shift-expr)*
	
	ee-ne-expr:         lt-gt-lte-gte-expr(('=' | '!=')  lt-gt-lte-gte-expr)*
	
	bit-and-expr:       ee-ne-expr('&' ee-ne-expr)*
	
	bit-xor-expr:       bit-and-expr('^' bit-and-expr)*
	
	bit-or-expr:        bit-xor-expr('|' bit-xor-expr)*
	
	log-and-expr:       bit-or-expr('&&' bit-or-expr)*
	
	log-or-expr:        log-expr('||' log-and-expr)*
	
	stmt_expr		   : 'ref' expr '=' expr
	
					   : 'let' IDENT ('='|'=>') expr
					   
					   : 'def' IDENT '(' param-list? ')' ':' typespec '{' expr_block '}'
					   
					   : 'call' expr '(' param-list? ')'
					   
					   : 'ret' expr
					   
             : if '(' expr ')' '{' expr_block '}' ('else' '{' expr_block '}' )?
</code>      

<h2> Summary: <h2>
	
<h3> Types are: </h3>
<p>
byte, word, dword, qword, void,
signed/unsigned versions of these and pointer versions of these.

A value that has 'u' at the end of it e.g. 18u is unsigned, if it then has ('b', 'w' or 'q') then it is a byte, word, qword value 
respectively if it doesn't specify, it's a dword. Lastly if it has a 'p' then it's a constant pointer memory address.

If a value is entered as 'x' with x being an ascii character between 0-255 then the corresponding ascii value will be read into a byte value.
</p>

<h4>
For e.g.:
</h4>
<code>
15u : unsigned dword,
15ub : unsigned byte,
15ubp : unsigned byte pointer,

15b : signed byte,
15bp : signed byte ptr,
'a' : signed byte,
'@' : signed byte,
....
</code>
	
<h3> Statements: </h3> 
<code>
'if' and 'else' do what you would expect,
'let' allows you to set/define variables, '=' would set and '=>' would define a variable.
'ref' references a variable by pointer basically this in C++: *(ptr) = x;
'&' gets the address (returns ptr values).
'def' defines function.
</code>
<h4>Syntax:</h4>
<code>
if ( expr ) { body },
if (expr) { body } else { body },
let identifier = expr,
let identifier => expr,
ref expr = expr,
def identifier ( (param-name : param-type)* ) : type { body },
& expr,
</code>

<h3> Function overloads </h3>
<p>
When using 'def' to define a function, this function will not be indexed within the symbol table only using its identifier or name but using its complete signature, that is, a name composed of its identifier as well as a list of the types of its parameters in order. (for e.g. a function called 'test' that takes two bytes and a char would be 'test(byte,byte,char)' in the symbol table. Therefore you can have as many functions with the same name as you'd like as long as they have different parameter types or combinations thereof.
</p>

<h4>
Then, in order to call a specific overload you use '<' and '>' as follows:
</h4>

<code>
test<byte>(x) : calls test's byte overload with x,
test<char>(x) : calls test's char overload with x,
test<char, byte, dword>(x) : calls test's (char, byte, dword) overload with x,
</code>
<h4> Syntax: </h4>
<code> function-name < param-type-list >(args); </code>

<h3> Finally </h3>
<p>
Always end a statement with a ';', it doesn't matter if two or more statements are on the same line as long as they each end in ';',
(kinda free-form)....
</p>
<h2> Examples: </h2>

<h3> This works: </h3>

<code>
let x => 5;
let y => x * 2;
let z => x + y * (x * 4 + (2 + x));
let x = 67;
def test(x : dword) : dword { let a => x * x; ret a + x; }
test(x + z);

if (z >= 2) { let z = 4; };
else { let z = 6; };

def test(x : dword) : dword 
{ 
  if (x >= 2) { 
    ret 1; 
  };
  else { 
    ret -1; 
  }; 
}

test(55);
</code>

<h3> Kinda works but not entirely (function overloading): </h3>
<code>
def test(x : byte) : byte { let a => x * x; ret a + x; }
call test<byte>(x + z);
call test<dword>(x + z);
</code>
	
<h3> Doesn't compile properly (type-checking doesn't work but I'm too lazy to fix it): </h3>
<code>
def pt => &x;
ref pt => 6;
</code>

<p>
Basically most features work except for pointers (these don't compile/transpile properly to NASM assembly) and function overloading.
This is a little personal project I don't intend to finish it but it's interesting nonetheless so I'm posting the full source code.
</p>


<h2> Note </h2>
<p> I now that the source code is organized terribly that is due to this being a one-day project I worked on. It's anything but a serious project. It's meant to be organized kinda like a c-program but using C++ for strings and other features found therein. </p>
