# Single-File-Compiler-From-Scratch-Recursive-Descent-LR-and-NASM-asm-gen

<img src="view1.png">

<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/3.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-nd/3.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/3.0/">Creative Commons Attribution-NonCommercial-NoDerivs 3.0 Unported License</a>.

C-like to NASM assembly compiler in one file with combined Lexer- Recursive Descent Parser-Compiler written in C++ with only standard libraries, parsers and a nice command line coloring system!

Grammar:

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
             
Examples:

- This works 100%:
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

- Kinda works but not entirely (function overloading):
def test(x : byte) : byte { let a => x * x; ret a + x; }
call test<byte>(x + z);
call test<dword>(x + z);
  
- Doesn't compile properly (type-checking doesn't work but I'm too lazy to fix it): 
def pt => &x;
ref pt => 6;

Basically most features work except for pointers (these don't compile/transpile properly to NASM assembly) and function overloading.
This is a little personal project I don't intend to finish it but it's interesting nonetheless so I'm posting the full source code.

Types are:
byte, word, dword, qword, void,
signed/unsigned versions of these and pointer versions of these.

Summary:
Basically, if and else do what you would expect,
let allows you to set/define variables, '=' would set and '=>' would define a variable.
Ref references a variable by pointer basically this in C++: *(ptr) = x;
& gets the address (returns ptr values).
a value that has 'u' at the end of it e.g. 18u is unsigned, if it then has ('b', 'w' or 'q') then it is a byte, word, qword value respectively if it doesn't specify, it's a dword. Lastly if it has a 'p' then it's a constant pointer memory address.

For e.g.:

15u: unsigned dword
15ub: unsigned byte
15ubp: unsigned byte pointer

15b: signed byte
15bp: signed byte ptr.
....

If a value is entered as 'x' with x being an ascii character between 0-255 then the corresponding ascii value will be read into a byte value.

'<' and '>' allows you to call a function overload as follows:

test<byte>(x) : calls test's byte overload with x
test<char>(x) : calls test's char overload with x
test<char, byte, dword...>(x) : calls test's char, byte, dword.... overload with x
  
function_name<param_type_list>(args);

a function is defined using def:

def function_name (parameter_name : parameter_type, ...) : return_type { body }
where function_name is the identifier for your function,
parameter_name is a parameter's name, parameter type is a parameter's type (byte, word, dword, qword...unsigned dword ptr...., return_type is the return type (byte, word, dword, qword...unsigned dword ptr....),
and finally the body is the body which is a list of statements separated by ';'

always end a statement with a ';', it doesn't matter if two or more statements are on the same line as long as they each end in ';',
(kinda free-form)....
