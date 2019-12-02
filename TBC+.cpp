#include <stdio.h>
#include <vector>
#include <map>
#include <algorithm>
#include <tuple>
#include <string>

//for clr
#include <Windows.h>

//for title
#include <tchar.h>

using namespace std;

/*
 NUM-LIT: [0-9]+ ['b'|'q']? 'u'?
 CHAR-LIT: '\'' [\0-\255] '\''
 typespec: 'unsigned'? ('type_byte' | 'type_word' | 'type_dword' | 'type_qword' | 'void' ) 'ptr'?
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
	lt-gt-lte-gte-expr: shift-expr(('<' | '>' | '<=' | '>=') shift-expr)*
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
*/

enum value_type
{
	type_qword,
	type_dword,
	type_word,
	type_byte,
	type_null,
	type_error,
	type_no_error,
};

char* program = NULL;

#define type_null_symbol symbol{type_null, false, false, false, 0, "", 0}
#define type_error_symbol symbol{type_error, false, false, false, 0, "", 0}
#define placeholder_symbol(type) symbol{type, false, false, false, 0, "", 0}
#define type_no_error_symbol symbol{type_no_error, false, false, false, 0, "", 0}

unsigned char color = 226;
#define begin_output_block() {output_main_section += "\033[38;5;" + to_string(color) + "m"; color = (unsigned char)(rand()%(231-4+1)+4); }
#define end_output_block() output_main_section += "\033[0m"

#define begin_out_block() {output_full += "\033[38;5;" + to_string(color) + "m"; color = (unsigned char)(rand()%(231-4+1)+4); }
#define end_out_block() output_full += "\033[0m"

#define is_whitespace(c) \
	((c) == ' ' || (c) == '\n' || (c) == '\t')

#define is_digit(c) \
	((c) >= '0' && (c) <= '9')

#define is_letter(c) \
	(((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z') || (c) == '_')

#define REMOVE_WHITESPACE() \
	while(is_whitespace(*program)) program++

#define EXPECT(condition, expected_tip, type_error_return, pass_adjust) \
	REMOVE_WHITESPACE();\
	if(condition) { printf("Error: Expected %s but found '%c'\n", expected_tip, *program); return type_error_return;} \
	program += pass_adjust; REMOVE_WHITESPACE();

string output_main_section = "";
string output_function_section = "";
size_t cur_offset = 0;
size_t cur_label_idx = 0;

struct symbol
{
	value_type type{ type_null };
	bool is_unsigned{ false };
	bool is_ptr{ false };
	bool is_func{ false };
	size_t offset{ false };
	string const_value{ false };
	unsigned long long const_value_lit{ 0 };

	std::string get_size_spec ()
	{
		if (is_ptr)
			return "qword";

		switch (type)
		{
		case type_qword:
			return "qword";
		case type_dword:
			return "dword";
		case type_word:
			return "word";
		case type_byte:
			return "byte";
		default:
			printf ("Error: Unknown type for register size-spec");
			return "type_error-unk-sz";
		}
	}

	std::string get_reg (int idx)
	{
		if (is_ptr)
			return idx == 0 ? "rax" : idx == 1 ? "rbx" : idx == 2 ? "rcx" : "rdx";

		switch (type)
		{
		case type_qword:
			return idx == 0 ? "rax" : idx == 1 ? "rbx" : idx == 2 ? "rcx" : "rdx";
		case type_dword:
			return idx == 0 ? "eax" : idx == 1 ? "ebx" : idx == 2 ? "ecx" : "edx";
		case type_word:
			return idx == 0 ? "ax" : idx == 1 ? "bx" : idx == 2 ? "cx" : "dx";
		case type_byte:
			return idx == 0 ? "al" : idx == 1 ? "bl" : idx == 2 ? "cl" : "dl";
		default:
			printf ("Error: Unknown type for register name");
			return "type_error-unk-type";
		}
	}

	string get_reference (bool override_imm = false)
	{
		if (const_value.size ())
		{
			if (override_imm)
			{
				output_main_section += "mov " + get_reg (0) + ", " + const_value + "\n    ";
				return get_reg (0);
			}
			else
				return const_value;
		}
		else
			return get_size_spec () + " [rbp-" + to_string (offset) + "]";
	}

	size_t get_padding ()
	{
		if (is_ptr)
			return 8;

		switch (type)
		{
		case type_qword:
			return 8;
		case type_dword:
			return 4;
		case type_word:
			return 2;
		case type_byte:
			return 1;
		default:
			printf ("Error: Unknown type for register padding");
			return 0;
		}
	}

	std::string sym_type_str ()
	{
		return string (is_unsigned ? "unsigned " : "") +
			(type == type_qword ? "qword" : type == type_dword ? "dword" : type == type_word ? "word" : type == type_byte ? "byte" : "unk-type") +
			string (is_ptr ? " pointer" : "");
	}

	std::string get_multiplier ()
	{
		if (is_ptr)
			return to_string (get_padding ());

		return "1";
	}
};

map<string, symbol> symbol_table;

bool cmp_symbol_types (const symbol& a, const symbol& b)
{
	return ((int)a.type) == ((int)b.type) && a.is_unsigned == b.is_unsigned;
}

value_type calc_size_spec (size_t type_bytes)
{
	switch (type_bytes)
	{
	case 8:
		return type_qword;
	case 4:
		return type_dword;
	case 2:
		return type_word;
	case 1:
		return type_byte;
	default:
		printf ("Error: Invalid size for size-spec");
		return (value_type)0;
	}
}

bool match_pattern (const std::string& pattern)
{
	bool match = true;
	for (size_t idx = 0; idx < pattern.size (); ++idx)
		match &= program[idx] == pattern[idx];
	return match;
}

bool match_any (const std::string& set)
{
	for (size_t idx = 0; idx < set.size (); ++idx)
		if (*program == set[idx])
			return true;

	return false;
}

tuple<value_type, bool, bool, bool, bool> parse_type_expr ()
{
	REMOVE_WHITESPACE ();

	bool is_unsigned = match_pattern ("unsigned");
	if (is_unsigned)
		program += 8;

	REMOVE_WHITESPACE ();

	value_type type = type_dword;
	if (match_pattern ("byte"))
	{
		type = type_byte;
		program += 4;
	}
	else if (match_pattern ("word"))
	{
		type = type_word;
		program += 4;
	}
	else if (match_pattern ("dword"))
	{
		type = type_dword;
		program += 5;
	}
	else if (match_pattern ("qword"))
	{
		type = type_qword;
		program += 5;
	}
	else
	{
		printf ("Error: Expected 'byte', 'word', 'dword' or 'qword' but found '%c'\n", *program);
		return make_tuple (type_error, false, false, false, false);
	}
	REMOVE_WHITESPACE ();

	bool is_ptr = match_pattern ("ptr");
	if (is_ptr)
		program += 3;

	REMOVE_WHITESPACE ();

	bool is_func = match_pattern ("func");
	if (is_func)
		program += 4;

	return make_tuple (type, is_unsigned, is_ptr, is_func, true);
}

symbol arithmetic_logic_bitwise_expr ();

symbol atom_expr ()
{
	REMOVE_WHITESPACE ();

	if (is_digit (*program))
	{
		unsigned long long accumulator = 0;
		while (is_digit (*program))
		{
			accumulator *= 10;
			accumulator += (*(program++)) - '0';
		}

		bool is_unsigned = *program == 'u';
		if (is_unsigned)
			program++;

		value_type type = type_dword;
		if (*program == 'b')
		{
			++program;
			type = type_byte;
		}
		else if (*program == 'w')
		{
			++program;
			type = type_word;
		}
		else if (*program == 'q')
		{
			++program;
			type = type_qword;
		}

		bool is_ptr = *program == 'p';
		if (is_ptr)
			program += 3;

		auto new_symbol = symbol{ type, is_unsigned, is_ptr, false, 0, "", accumulator };
		new_symbol.const_value = new_symbol.get_size_spec () + " " + to_string (accumulator);

		return new_symbol;
	}
	else if (*program == '\'')
	{
		auto offset = cur_offset++;
		char char_value = *program;
		program++;
		EXPECT (*program != '\'', "'''", type_error_symbol, 1);
		return symbol{ type_byte, true, false, false, 0, "type_byte " + char_value, (unsigned long long)char_value };
	}
	else if (is_letter (*program))
	{
		string ident = "";
		while (is_letter (*program) || is_digit (*program))
			ident += *(program++);

		REMOVE_WHITESPACE ();

		if (*program == '(')
		{
			program++;
			auto arg = arithmetic_logic_bitwise_expr ();
			string param_sign = "(";

			if (arg.type == type_error)
				return type_error_symbol;
			else if (arg.type != type_null)
			{
				auto offset = cur_offset;
				cur_offset += arg.get_padding ();
				begin_output_block ();
				output_main_section += "mov " + arg.get_size_spec () + " PTR [rbp-" + to_string (offset) + "], " + arg.get_reference () + "\n    "; \
					param_sign += arg.sym_type_str ();
				while (*program == ',')
				{
					program++;
					auto arg = arithmetic_logic_bitwise_expr ();
					if (arg.type == type_error)
						return type_error_symbol;
					if (arg.type == type_null)
					{
						printf ("Error: Expected expression but found '%c'\n", *program);
						return type_error_symbol;
					}

					auto offset = cur_offset;
					cur_offset += arg.get_padding ();
					output_main_section += "mov " + arg.get_size_spec () + " PTR [rbp-" + to_string (offset) + "], " + arg.get_reference () + "\n    "; \
						param_sign += "," + arg.sym_type_str ();
				}
			}

			EXPECT (*program != ')', "')' or ','", type_error_symbol, 1);

			param_sign += ")";
			param_sign = ident + param_sign;

			map<string, symbol>::iterator search = symbol_table.find (param_sign);

			if (search == symbol_table.end ())
			{
				printf ("Error: Function '%s' is undefined\n", param_sign.c_str ());
				return type_error_symbol;
			}
			auto func_val = search->second;
			if (!func_val.is_func)
			{
				printf ("Error: Identifier lookup must result in function value for func-call.\n");
				return type_error_symbol;
			}

			output_main_section += "call " + param_sign + "\n    ";

			auto offset = cur_offset;
			cur_offset += func_val.get_padding ();
			output_main_section += "mov " + func_val.get_size_spec () + " PTR [rbp-" + to_string (offset) + "], " + func_val.get_reg (0) + "\n ";
			end_output_block ();
			return symbol{ func_val.type, func_val.is_unsigned, func_val.is_ptr, false, offset, func_val.const_value };
		}
		else if (*program == '<' && *(program + 1) != '?' && *(program + 1) != '<' && *(program + 1) != '=')
		{
			program++;
			REMOVE_WHITESPACE ();
			auto arg1 = parse_type_expr ();
			if (!get<4> (arg1))
				return type_error_symbol;

			auto param_sym = symbol{ get<0> (arg1), get<1> (arg1), get<2> (arg1), get<3> (arg1), cur_offset, "" };
			string param_sign = "(" + param_sym.sym_type_str ();
			REMOVE_WHITESPACE ();
			while (*program == ',')
			{
				REMOVE_WHITESPACE ();
				program++;
				auto arg = parse_type_expr ();
				REMOVE_WHITESPACE ();
				if (!get<4> (arg))
					return type_error_symbol;

				auto param_sym = symbol{ get<0> (arg), get<1> (arg), get<2> (arg), get<3> (arg1), cur_offset, "" };
				param_sign += "," + param_sym.sym_type_str ();
			}

			REMOVE_WHITESPACE ();
			EXPECT (*program != '>', "'>' or ','", type_error_symbol, 1);

			param_sign += ")";
			ident += param_sign;
		}
		else if (match_pattern ("++") || match_pattern ("--"))
		{
			auto op = *program;
			program += 2;

			map<string, symbol>::iterator search = symbol_table.find (ident);
			if (search == symbol_table.end ())
			{
				printf ("Error: Identifier '%s' is undefined\n", ident.c_str ());
				return type_error_symbol;
			}

			symbol original = search->second;

			begin_output_block ();
			output_main_section += "mov " + original.get_reg (0) + ", " + original.get_reference () + "\n    ";
			output_main_section += string (op == '+' ? "add " : "sub ") + original.get_reg (0) + ", " + original.get_multiplier () + "\n    ";
			auto offset = cur_offset;
			cur_offset += original.get_padding ();
			output_main_section += "mov " + original.get_size_spec () + " [rbp-" + to_string (offset) + "]," + original.get_reg (0) + "\n    ";

			symbol_table[ident] = symbol{ original.type, original.is_unsigned, original.is_ptr, original.is_func, offset, "" };

			auto offset2 = cur_offset;
			cur_offset += original.get_padding ();
			output_main_section += "mov " + original.get_size_spec () + " [rbp-" + to_string (offset2) + "]," + original.get_reference () + "\n    ";
			end_output_block ();
			return symbol{ original.type, original.is_unsigned, original.is_ptr, original.is_func, offset2, "" };
		}

		map<string, symbol>::iterator search = symbol_table.find (ident);

		if (search == symbol_table.end ())
		{
			printf ("Error: Function '%s' is undefined\n", ident.c_str ());
			return type_error_symbol;
		}

		return search->second;
	}
	else if (match_pattern ("++") || match_pattern ("--"))
	{
		auto op = *program;
		program += 2;

		REMOVE_WHITESPACE ();

		if (is_letter (*program))
		{
			string ident = "";
			while (is_letter (*program) || is_digit (*program))
				ident += *(program++);

			map<string, symbol>::iterator search = symbol_table.find (ident);
			if (search == symbol_table.end ())
			{
				printf ("Error: Identifier '%s' is undefined\n", ident.c_str ());
				return type_error_symbol;
			}

			symbol original = search->second;

			if (original.is_func)
			{
				printf ("Error: Cannot perform unary operation on function value\n");
				return type_error_symbol;
			}

			begin_output_block ();
			output_main_section += "mov " + original.get_reg (0) + ", " + original.get_reference () + "\n    ";
			output_main_section += string (op == '+' ? "add " : "sub ") + original.get_reg (0) + ", " + original.get_multiplier () + "\n    ";
			auto offset = cur_offset;
			cur_offset += original.get_padding ();
			output_main_section += "mov " + original.get_size_spec () + " [rbp-" + to_string (offset) + "]," + original.get_reg (0) + "\n    ";
			end_output_block ();
			symbol_table[ident] = symbol{ original.type, original.is_unsigned, original.is_ptr, original.is_func, offset, "" };
			return symbol_table[ident];
		}

		printf ("Error: Expected identifier but found '%c'\n", *program);
		return type_error_symbol;
	}
	else if (match_any ("-~!&*"))
	{
		auto op = *(program++);

		REMOVE_WHITESPACE ();

		auto expr = atom_expr ();
		if (expr.type == type_null)
		{
			printf ("Error: Expected atomic expression but found '%c'\n", *program);
			return type_error_symbol;
		}
		else if (expr.type == type_error)
			return type_error_symbol;

		auto offset = cur_offset;
		symbol new_symbol = expr;
		new_symbol.offset = offset;

		if (expr.is_func)
		{
			printf ("Error: Cannot perform unary operation on function value\n");
			return type_error_symbol;
		}

		begin_output_block ();
		if (op == '!')
		{
			output_main_section += "cmp " + expr.get_reference (true) + ", 0\n    ";
			output_main_section += "sete al\n    ";
			output_main_section += "movzx eax, al\n    ";
			output_main_section += "mov " + expr.get_size_spec () + " [rbp-" + to_string (offset) + "], " + expr.get_reg (0) + "\n    ";
		}
		else if (op == '&')
		{
			if (expr.const_value.size ())
			{
				printf ("Error: Cannot get address of constant\n");
				return type_error_symbol;
			}

			new_symbol.is_ptr = true;
			output_main_section += "lea rax, " + expr.get_reference () + "\n    ";
			output_main_section += "mov type_qword [rbp-" + to_string (offset) + "], rax\n    ";
		}
		else if (op == '*')
		{
			if (!expr.is_ptr)
			{
				printf ("Error: Cannot dereference non-ptr expression\n");
				return type_error_symbol;
			}

			new_symbol.is_ptr = false;
			output_main_section += "mov rax, type_qword [rbp-" + to_string (expr.offset) + "]\n    ";
			output_main_section += "mov rax, " + new_symbol.get_size_spec () + " [rax]\n    ";
			output_main_section += "mov " + new_symbol.get_size_spec () + " [rbp-" + to_string (offset) + "], rax\n    ";
		}
		else
		{
			new_symbol.is_unsigned = !new_symbol.is_unsigned;
			output_main_section += "mov " + expr.get_reg (0) + ", " + expr.get_reference () + "\n    ";
			if (op == '-')
			{
				output_main_section += "neg " + expr.get_reg (0) + "\n    ";
			}
			else if (op == '~')
			{
				output_main_section += "not " + expr.get_reg (0) + "\n    ";
			}

			output_main_section += "mov " + expr.get_size_spec () + " [rbp-" + to_string (offset) + "], " + expr.get_reg (0) + "\n    ";
		}
		end_output_block ();
		cur_offset += new_symbol.get_padding ();
		return new_symbol;
	}
	else if (*program == '(')
	{
		program++;

		REMOVE_WHITESPACE ();

		auto expr = arithmetic_logic_bitwise_expr ();
		if (expr.type == type_null)
		{
			printf ("Error: Expected expression but found '%c'\n", *program);
			return type_error_symbol;
		}
		else if (expr.type == type_error)
			return type_error_symbol;

		REMOVE_WHITESPACE ();

		EXPECT (*program != ')', "')'", type_error_symbol, 1);
		return expr;
	}

	return type_null_symbol;
}

#define CREATE_PRECEDENCE_SET(name, upper, condition, condition_offset, generator)\
symbol name() {\
	auto left = upper();\
	if(left.type == type_null) return left;\
	else if(left.type == type_error) return type_error_symbol;\
	REMOVE_WHITESPACE();\
	bool condition_check = condition;\
	auto offset = cur_offset;\
	if(condition_check){\
		begin_output_block (); \
		cur_offset += left.get_padding();\
		output_main_section += "mov " + left.get_reg(0) + ", " + left.get_reference() + "\n    ";\
		output_main_section += "mov " + left.get_size_spec()  + " [rbp-" + to_string(offset) + "], " + left.get_reg(0) + "\n    ";\
		end_output_block();}\
	while(condition) {	\
		string op = ""; \
		REMOVE_WHITESPACE();\
		size_t condition_offset_buffer = condition_offset;\
		for(size_t idx = 0; idx < condition_offset_buffer; ++idx)\
			op += *(program++);\
		auto right = upper();\
		REMOVE_WHITESPACE();\
		if(right.type == type_null)\
		{ printf("Error: Expected expression but found '%c'\n", *program); return type_error_symbol;}\
		else if(right.type == type_error) return type_error_symbol;\
		if(left.is_func || right.is_func)\
		{\
			printf("Error: Cannot perform binary operation with a function value\n");\
			return type_error_symbol;\
		}\
		if(!cmp_symbol_types(left, right))\
		{ printf("Error: Cannot apply binary operation with value of type '%s' to value of different type '%s'\n",\
			left.sym_type_str().c_str(), right.sym_type_str().c_str());\
			return type_error_symbol; }\
		begin_output_block();\
		output_main_section += "mov " + left.get_reg (0) + ", " + left.get_size_spec () + " [rbp-" + to_string (offset) + "]\n    ";  \
		generator\
		output_main_section += "mov " + left.get_size_spec () + " [rbp-" + to_string (offset) + "], " + left.get_reg (0) + "\n    "; \
		end_output_block ();\
	}\
	return symbol{left.type, left.is_unsigned, left.is_ptr, left.is_func,\
			condition_check? offset : left.offset,\
			condition_check? "" : left.const_value};\
}

CREATE_PRECEDENCE_SET (mul_div_mod_expr, atom_expr, *program == '*' || *program == '/' || *program == '%', 1,
	switch (op[0])
	{
	case '*':
		output_main_section += (left.is_unsigned ? "mul " : "imul ") + left.get_reg (0) + ", " + right.get_reference () + "\n    ";
		break;
	case '/':
		if (left.is_unsigned) output_main_section += "mov edx, 0\n    ";
		else output_main_section += "cdq\n    ";
		output_main_section += (left.is_unsigned ? "div " : "idiv ") + right.get_reference (true) + "\n    ";
		break;
	case '%':
		if (left.is_unsigned) output_main_section += "mov edx, 0\n    ";
		else output_main_section += "cdq\n    ";
		output_main_section += (left.is_unsigned ? "div " : "idiv ") + right.get_reference (true) + "\n    ";
		output_main_section += "mov " + left.get_reg (0) + ", " + left.get_reg (3) + "\n    ";
		break;
	default:
		printf ("Internal Error: Compiler cannot identify operator\n");
	}
);

CREATE_PRECEDENCE_SET (add_sub_expr, mul_div_mod_expr, *program == '+' || *program == '-', 1,
	switch (op[0])
	{
	case '+':
		output_main_section += "add " + left.get_reg (0) + ", " + right.get_reference () + "\n    ";
		break;
	case '-':
		output_main_section += "sub " + left.get_reg (0) + ", " + right.get_reference () + "\n    ";
		break;
	default:
		printf ("Internal Error: Compiler cannot identify operator\n");
	}
);

CREATE_PRECEDENCE_SET (shift_expr, add_sub_expr, match_pattern ("<<") || match_pattern (">>"), 2,
	switch (op[0])
	{
	case '<':
		output_main_section += "mov " + left.get_reg (2) + ", " + right.get_reference () + "\n    ";
		output_main_section += "sal " + left.get_reg (0) + ", cl\n    ";
		break;
	case '>':
	{
		output_main_section += "mov " + left.get_reg (2) + ", " + right.get_reference () + "\n    ";
		output_main_section += "shr " + left.get_reg (0) + ", cl\n    ";
		break;
	}
	default:
		printf ("Internal Error: Compiler cannot identify operator\n");
	}
);

CREATE_PRECEDENCE_SET (lt_gt_lte_gte_expr, shift_expr, match_pattern ("<=") || match_pattern (">=") || match_pattern ("<?") || match_pattern (">?"),
	2,

	if (op == "<=")
	{
		output_main_section += "cmp " + left.get_reg (0) + ", " + right.get_reference () + "\n    ";
		output_main_section += "setbe al\n    ";
		output_main_section += "movzx eax, al\n    ";
		break;
	}
	else if (op == ">=")
	{
		output_main_section += "cmp " + left.get_reg (0) + ", " + right.get_reference () + "\n    ";
		output_main_section += "setnb al\n    ";
		output_main_section += "movzx eax, al\n    ";
		break;
	}
	else if (op[0] == '<')
	{
		output_main_section += "cmp " + left.get_reg (0) + ", " + right.get_reference () + "\n    ";
		output_main_section += "setb al\n    ";
		output_main_section += "movzx eax, al\n    ";
		break;
	}
	else if (op[0] == '>')
	{
		output_main_section += "cmp " + left.get_reg (0) + ", " + right.get_reference () + "\n    ";
		output_main_section += "seta al\n    ";
		output_main_section += "movzx eax, al\n    ";
		break;
	}
	else
		printf ("Internal Error: Compiler cannot identify operator\n");
);

symbol arithmetic_logic_bitwise_expr ()
{
	return lt_gt_lte_gte_expr ();
}

tuple<string, bool> param_list_expr ()
{
	REMOVE_WHITESPACE ();

	string identifier = "";

	if (is_letter (*program))
	{
		while (is_letter (*program) || is_digit (*program))
			identifier += *(program++);
	}

	string parameter_signature = "(";

	if (identifier.size ())
	{
		REMOVE_WHITESPACE ();

		EXPECT (*program != ':', "':'", make_tuple ("", false), 1);

		tuple<value_type, bool, bool, bool, bool> type_expr = parse_type_expr ();
		if (!get<4> (type_expr))
			return make_tuple ("", false);

		parameter_signature += placeholder_symbol (get<0> (type_expr)).sym_type_str ();
		auto parameter_symbol = symbol{ get<0> (type_expr), get<1> (type_expr), get<2> (type_expr), get<3> (type_expr), cur_offset, "" };
		symbol_table[identifier] = parameter_symbol;
		cur_offset += parameter_symbol.get_padding ();

		while (*program == ',')
		{
			program++;
			REMOVE_WHITESPACE ();

			string identifier = "";
			if (is_letter (*program))
			{
				while (is_letter (*program) || is_digit (*program))
					identifier += *(program++);
			}
			else
			{
				printf ("Error: Expected identifier but found '%c'\n", *program);
				return make_tuple ("", false);
			}
			REMOVE_WHITESPACE ();

			EXPECT (*program != ':', "':'", make_tuple ("", false), 1);

			auto type_expr = parse_type_expr ();
			if (!get<4> (type_expr))
				return make_tuple ("", false);
			parameter_signature += "," + placeholder_symbol (get<0> (type_expr)).sym_type_str ();
			auto param_sym = symbol{ get<0> (type_expr), get<1> (type_expr), get<2> (type_expr), get<3> (type_expr), cur_offset, "" };
			symbol_table[identifier] = param_sym;
			cur_offset += param_sym.get_padding ();
		}
	}
	return make_tuple (parameter_signature + ")", true);
}

symbol expression_block (bool global_ctx);

symbol stmt_expr (bool global_ctx = false)
{
	REMOVE_WHITESPACE ();

	if (match_pattern ("ref"))
	{
		program += 3;
		auto ptr_expr = arithmetic_logic_bitwise_expr ();

		if (ptr_expr.type == type_null)
		{
			printf ("Error: Expected expression but found '%c'\n", *program);
			return type_error_symbol;
		}
		else if (ptr_expr.type == type_error)
			return type_error_symbol;

		if (!ptr_expr.is_ptr)
		{
			printf ("Error: Left hand expression must result in pointer value for ref-set.\n");
			return type_error_symbol;
		}

		EXPECT (*program != '=', "'='", type_error_symbol, 1);

		auto expr = arithmetic_logic_bitwise_expr ();

		if (!cmp_symbol_types (ptr_expr, expr))
		{
			printf ("Error: Cannot set value of type '%s' to expr of type '%s'\n", ptr_expr.sym_type_str ().c_str (), expr.sym_type_str ().c_str ());
			return type_error_symbol;
		}

		begin_output_block ();
		output_main_section += "mov rax, type_qword [rbp-" + to_string (ptr_expr.offset) + "]\n    ";
		output_main_section += "mov " + expr.get_reg (1) + ", " + expr.get_size_spec () + " [rbp-" + to_string (expr.offset) + "]\n    ";
		output_main_section += "mov " + expr.get_size_spec () + " [rax], " + expr.get_reg (1) + "\n    ";
		end_output_block ();
	}
	else if (match_pattern ("let"))
	{
		program += 3;
		string identifier = "";

		REMOVE_WHITESPACE ();

		if (is_letter (*program))
		{
			while (is_letter (*program) || is_digit (*program))
				identifier += *(program++);
		}

		if (!identifier.size ())
		{
			printf ("Error: Expected identifier but found '%c'\n", *program);
			return type_error_symbol;
		}

		REMOVE_WHITESPACE ();

		if (match_pattern ("=>"))
		{
			program += 2;
			if (symbol_table.find (identifier) != symbol_table.end ())
			{
				printf ("Error: Identifier '%s' is already defined\n", identifier.c_str ());
				return type_error_symbol;
			}

			REMOVE_WHITESPACE ();
			auto expr = arithmetic_logic_bitwise_expr ();
			if (expr.type == type_null)
			{
				printf ("Error: Expected expression but found '%c'\n", *program);
				return type_error_symbol;
			}
			else if (expr.type == type_error)
				return type_error_symbol;

			if (expr.const_value.size ())
			{
				output_main_section += "mov " + expr.get_size_spec () + " [rbp-" + to_string (cur_offset) + "], " + expr.get_reference () + "\n    ";
				expr.offset = cur_offset;
				expr.const_value = "";
				cur_offset += expr.get_padding ();
			}
			symbol_table[identifier] = expr;
			return expr;
		}
		else if (*program == '=')
		{
			program++;
			if (symbol_table.find (identifier) == symbol_table.end ())
			{
				printf ("Error: Identifier '%s' is undefined\n", identifier.c_str ());
				return type_error_symbol;
			}

			REMOVE_WHITESPACE ();
			auto expr = arithmetic_logic_bitwise_expr ();
			if (expr.type == type_null)
			{
				printf ("Error: Expected expression but found '%c'\n", *program);
				return type_error_symbol;
			}
			else if (expr.type == type_error)
				return type_error_symbol;

			auto existing_symbol = symbol_table[identifier];
			if (!cmp_symbol_types (existing_symbol, expr))
			{
				printf ("Error: Cannot set value of type '%s' to expr of type '%s'\n", existing_symbol.sym_type_str ().c_str (), expr.sym_type_str ().c_str ());
				return type_error_symbol;
			}

			auto new_ex = symbol{ expr.type, expr.is_unsigned, expr.is_ptr, expr.is_func, cur_offset, expr.const_value };
			output_main_section += "mov " + new_ex.get_size_spec () + " [rbp-" + to_string (cur_offset) + "], " + new_ex.get_reference () + "\n    ";
			new_ex.offset = cur_offset;
			new_ex.const_value = "";
			cur_offset += new_ex.get_padding ();
			symbol_table[identifier] = new_ex;
			return new_ex;
		}
		else
		{
			printf ("Error: Expected '=>' or '=' but found '%c'\n", *program);
			return type_error_symbol;
		}
	}
	else if (match_pattern ("call"))
	{
		program += 4;
		REMOVE_WHITESPACE ();
		auto function_expr = arithmetic_logic_bitwise_expr ();

		if (function_expr.type == type_null)
		{
			printf ("Error: Expected expression but found '%c'\n", *program);
			return type_error_symbol;
		}
		else if (function_expr.type == type_error)
			return type_error_symbol;

		EXPECT (!function_expr.is_func, "left-hand expression with function value", type_error_symbol, 0);

		EXPECT (*program != '(', "'('", type_error_symbol, 1);

		auto arg = arithmetic_logic_bitwise_expr ();
		string param_sign = "(";

		begin_output_block ();
		if (arg.type == type_error)
			return type_error_symbol;
		else if (arg.type != type_null)
		{
			auto offset = cur_offset;
			cur_offset += arg.get_padding ();
			output_main_section += "mov " + arg.get_size_spec () + " [rbp-" + to_string (offset) + "], " + arg.get_reference () + "\n    "; \
				param_sign += arg.sym_type_str ();
			while (*program == ',')
			{
				program++;
				auto arg = arithmetic_logic_bitwise_expr ();
				if (arg.type == type_error)
					return type_error_symbol;
				if (arg.type == type_null)
				{
					printf ("Error: Expected expression but found '%c'\n", *program);
					return type_error_symbol;
				}

				auto offset = cur_offset;
				cur_offset += arg.get_padding ();
				output_main_section += "mov " + arg.get_size_spec () + " [rbp-" + to_string (offset) + "], " + arg.get_reference () + "\n    "; \
					param_sign += "," + arg.sym_type_str ();
			}
		}

		EXPECT (*program != ')', "')'", type_error_symbol, 1);

		param_sign += ")";

		param_sign = function_expr.const_value;
		output_main_section += "call " + param_sign + "\n    ";

		auto offset = cur_offset;
		cur_offset += function_expr.get_padding ();
		output_main_section += "mov " + function_expr.get_size_spec () + " [rbp-" + to_string (offset) + "], " + function_expr.get_reg (0) + "\n    ";
		return symbol{ function_expr.type, function_expr.is_unsigned, function_expr.is_ptr, false, offset, function_expr.const_value };
		end_output_block ();
	}
	else if (match_pattern ("def") && global_ctx)
	{
		program += 3;
		string identifier = "";

		REMOVE_WHITESPACE ();

		if (is_letter (*program))
		{
			while (is_letter (*program) || is_digit (*program))
				identifier += *(program++);
		}

		if (!identifier.size ())
		{
			printf ("Error: Expected identifier but found '%c'\n", *program);
			return type_error_symbol;
		}

		EXPECT (*program != '(', "'('", type_error_symbol, 1);

		auto offset_buffer = cur_offset;
		cur_offset = 0;
		auto parameter_descriptor = param_list_expr ();

		if (!get<1> (parameter_descriptor))
			return type_error_symbol;

		EXPECT (*program != ')', "')'", type_error_symbol, 1);
		EXPECT (*program != ':', "':'", type_error_symbol, 1);

		auto return_type = parse_type_expr ();

		if (!get<4> (return_type))
			return type_error_symbol;

		auto function_signature = identifier + get<0> (parameter_descriptor);

		std::string output_buffer = "";
		auto symbol_table_buffer = symbol_table;
		output_buffer += output_main_section;
		output_main_section = "";

		output_main_section += "\033[38;5;172m" + function_signature + "\033[0m:\n    ";

		begin_output_block ();
		output_main_section += "push rbp\n    ";
		output_main_section += "mov rbp, rsp\n    ";
		end_output_block ();

		REMOVE_WHITESPACE ();

		EXPECT (*program != '{', "'{'", type_error_symbol, 1);

		REMOVE_WHITESPACE ();

		auto block = expression_block (false);

		if (block.type == type_error)
			return type_error_symbol;

		EXPECT (*program != '}', "'}'", type_error_symbol, 1);

		begin_output_block ();
		output_main_section += "pop rbp\n    ";
		output_main_section += "ret\n";
		end_output_block ();

		output_function_section += output_main_section;
		output_main_section = output_buffer;
		cur_offset = offset_buffer;
		auto function_symbol_table_buffer = symbol_table;
		symbol_table = symbol_table_buffer;

		map<string, symbol>::iterator it;
		for (auto& s : symbol_table)
			s.second = function_symbol_table_buffer[s.first];

		symbol_table[function_signature] = symbol{ get<0> (return_type), get<1> (return_type), get<2> (return_type), true, 0, function_signature };
		return symbol_table[function_signature];
	}
	else if (match_pattern ("ret"))
	{
		program += 3;
		auto expr = arithmetic_logic_bitwise_expr ();
		begin_output_block ();
		if (expr.const_value == "0")
			output_main_section += "xor eax, eax\n    ";
		else
			output_main_section += "mov eax, " + expr.get_reference () + "\n    ";
		end_output_block ();
		return expr;
	}
	else if (match_pattern ("if"))
	{
		program += 2;
		EXPECT (*program != '(', "'('", type_error_symbol, 1);

		auto cond_expr = arithmetic_logic_bitwise_expr ();

		if (cond_expr.type == type_null)
		{
			printf ("Error: Expected expression but found '%c'\n", *program);
			return type_error_symbol;
		}
		else if (cond_expr.type == type_error)
			return type_error_symbol;

		EXPECT (*program != ')', "')'", type_error_symbol, 1);
		
		EXPECT (cond_expr.is_func || cond_expr.is_ptr, "conditional expression", type_error_symbol, 0);

		output_main_section += "cmp " + cond_expr.get_reference (true) + ", 0\n    ";
		output_main_section += "je .L" + to_string (cur_label_idx) + "\n    ";

		EXPECT (*program != '{', "'{'", type_error_symbol, 1);

		auto block = expression_block (false);

		if (block.type == type_error)
			return type_error_symbol;

		EXPECT (*program != '}', "'}'", type_error_symbol, 1);
		
		size_t 	else_label = cur_label_idx++;
		string else_cnt = "";
		if (match_pattern ("else"))
		{
			program += 4;

			auto output_buff = output_main_section;
			output_main_section = "";

			EXPECT (*program != '{', "'{'", type_error_symbol, 1);

			REMOVE_WHITESPACE ();

			auto block = expression_block (false);

			if (block.type == type_error)
				return type_error_symbol;

			else_cnt = output_main_section;
			output_main_section = output_buff;

			EXPECT (*program != '}', "'}'", type_error_symbol, 1);
		}

		output_main_section += "jmp .L" + to_string (cur_label_idx) + "\n    ";
		auto skip_label = cur_label_idx++;

		output_main_section += ".L" + to_string (else_label) + ":\n    ";
		output_main_section += else_cnt;

		output_main_section += ".L" + to_string (skip_label) + ":\n    ";
	}

	return arithmetic_logic_bitwise_expr ();
}

symbol expression_block (bool is_global_scope)
{
	if (*program == '\0')
		return type_null_symbol;

	auto expr = stmt_expr (is_global_scope);

	for (;; expr = stmt_expr (is_global_scope))
	{
		if (expr.type == type_error)
			return type_error_symbol;
		else if (expr.type == type_null)
		{
			if (*program == ';')
			{
				program++;
				continue;
			}

			break;
		}

		EXPECT (*program != ';', "';'", type_error_symbol, 1);
	}

	return type_no_error_symbol;
}

int main (int argc, char** argv)
{
	SetConsoleTitle (_T("TBC+ Compiler"));
begin:
	printf ("\033[44;42m\033[38;5;0m");

	printf(
		"     ___________  _____        _____                       _ _                 \n"
		"    |_   _| ___ \\/  __ \\ _    /  __ \\                     (_) |                \n"
		"      | | | |_/ /| /  \\/| |_  | /  \\/ ___  _ __ ___  _ __  _| | ___ _ __       \n"
		"      | | | ___ \\| |  |_   _| | |    / _ \\| '_ ` _ \\| '_ \\| | |/ _ \\ '__|      \n"
		"      | | | |_/ /| \\__/\\|_|   | \\__/\\ (_) | | | | | | |_) | | |  __/ |         \n"
		"      \\_/ \\____/  \\____/       \\____/\\___/|_| |_| |_| .__/|_|_|\\___|_|         \n"
		"                                                    | |                        \n"
		"                                                    |_|                        \n");
	printf("\033[0m\n");
	printf ("\033[38;5;45mCopyright(C) 2019 Nicolas O. S.\033[0m\n");
start:
	std::string buffer = "";//
	printf ("\033[38;5;208m>>>\033[0m\033[38;5;216m");

	for (char c = getchar (); c != '\n'; c = getchar ())
		buffer += c;

	printf ("\b\033[0m\n");

	program = &buffer[0];

	if (buffer == "clr\0")
	{
		cur_offset = 0;
		output_main_section = "";
		symbol_table.clear ();
		output_function_section = "";
		goto start;
	}
	else if (buffer == "cls\0")
	{
		COORD topLeft = { 0, 0 };
		HANDLE console = GetStdHandle (STD_OUTPUT_HANDLE);
		CONSOLE_SCREEN_BUFFER_INFO screen;
		DWORD written;

		GetConsoleScreenBufferInfo (console, &screen);
		FillConsoleOutputCharacterA (
			console, ' ', screen.dwSize.X * screen.dwSize.Y, topLeft, &written
		);
		FillConsoleOutputAttribute (
			console, FOREGROUND_GREEN | FOREGROUND_RED | FOREGROUND_BLUE,
			screen.dwSize.X * screen.dwSize.Y, topLeft, &written
		);
		SetConsoleCursorPosition (console, topLeft);

		goto begin;
	}

	auto output_buffer = output_main_section;
	auto output_funcs_buffer = output_function_section;
	auto symbol_table_buffer = symbol_table;
	auto offset_buffer = cur_offset;

	auto res = expression_block (true);

	if (res.type == type_error)
	{
		output_main_section = output_buffer;
		output_function_section = output_funcs_buffer;
		symbol_table = symbol_table_buffer;
		cur_offset = offset_buffer;
		goto start;
	}
	else if (res.type == type_null)
		goto start;

	else if (*program != '\0' && res.type != type_null)
	{
		REMOVE_WHITESPACE ();

		printf ("Error: Expected EOF but found '%c'\n", *program);

		output_main_section = output_buffer;
		output_function_section = output_funcs_buffer;
		symbol_table = symbol_table_buffer;
		cur_offset = offset_buffer;
		goto start;
	}

	string output_full;

	begin_out_block ();
	output_full += "section .text\n";
	end_out_block ();

	output_full += output_function_section;

	begin_out_block ();
	output_full += "global main\n";
	end_out_block ();

	begin_out_block ();
	output_full += "main:\n    ";
	end_out_block ();

	begin_out_block ();
	output_full += "push rbp\n    ";
	end_out_block ();

	output_full += output_main_section;

	begin_out_block ();
	output_full += "pop rbp\n    ";
	output_full += "ret\n";
	end_out_block ();

	printf ("%s\n", output_full.c_str ());

	goto start;

	return 0;
}