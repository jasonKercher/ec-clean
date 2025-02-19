package ecclean

import    "core:fmt"
import    "core:bytes"
import os "core:os/os2"
import    "core:strings"
import    "core:strconv"

import "getargs"

main :: proc() {
	ap := getargs.make_getargs()
	getargs.add_arg(&ap, "h", "help", .None)
	getargs.add_arg(&ap, "p", "prev", .Required)
	getargs.add_arg(&ap, "o", "output", .Required)
	getargs.read_args(&ap, os.args)

	if getargs.get_flag(&ap, "h") {
		fmt.println("no help for you!")
		os.exit(0)
	}

	output_name, found := getargs.get_payload(&ap, "o")
	if !found {
		output_name = ""
	}

	prev_name: string
	prev_name, found = getargs.get_payload(&ap, "p")
	if found {
		unimplemented("--prev not implemented")
	}

	if ap.arg_idx >= len(os.args) {
		fmt.eprintln("Expected asm file input")
		os.exit(1)
	}
	input_file := os.args[ap.arg_idx]

	input, err := os.read_entire_file(input_file, context.allocator)
	if err != nil {
		os.print_error(os.stderr, err, input_file)
		os.exit(1)
	}
	defer delete(input)

	output_file: ^os.File = os.stdout
	if output_name != "" {
		output_file, err = os.open(output_name, {.Read, .Write, .Trunc, .Create}, 0o664)
		if err != nil {
			os.print_error(os.stderr, err, output_name)
		}
	}

	_init_maps()
	tokens := _tokenize(string(input))

}

_instruction_map: map[string]Instruction
_register_map:    map[string]Register

BANK_JUMP_0 :: 0x4e00
BANK_JUMP_1 :: 0x4e14
BANK_JUMP_2 :: 0x4e28
BANK_JUMP_3 :: 0x4e3c  // unused...

LJMP_TO_BANK_JUMP_0 :: "024e00"
LJMP_TO_BANK_JUMP_1 :: "024e14"
LJMP_TO_BANK_JUMP_2 :: "024e28"
LJMP_TO_BANK_JUMP_3 :: "024e3c"  // unused...

Token_Kind :: enum u8 {
	Ignore,
	Whitespace,
	Line_End,
	Data_Hex,
	Data_Ascii,
	Name,
	Label,
	Label_Sym,
	Comma,
	Bit_Address,
	Deref,
	Deref_Label,
	Deref_Label_Sym,
	Comment,
	Arg_Label,
	Location,
	Location_Label,
	Location_Bytes,
	Immediate,
	Register,
	Bit_Suffix,
	Instruction,
	Trailing_Space,
	Label_Offset,
	Register_Offset,
}

Token :: struct {
	start:  i32,
	length: u8,
	kind:   Token_Kind,
	using _: struct #raw_union {
		value:       u16,
		instruction: Instruction,
		register:    Register,
	}
}

#assert(size_of(Token) == 8)

Instruction :: enum u8 {
	Invalid,
	ACALL,
	ADD,
	ADDC,
	AJMP,
	ANL,
	CJNE,
	CLR,
	CPL,
	DA,
	DEC,
	DIV,
	DJNZ,
	INC,
	JB,
	JBC,
	JC,
	JMP,
	JNB,
	JNC,
	JNZ,
	JZ,
	LCALL,
	LJMP,
	MOV,
	MOVC,
	MOVX,
	MUL,
	NOP,
	ORL,
	POP,
	PUSH,
	RET,
	RETI,
	RL,
	RLC,
	RR,
	RRC,
	SETB,
	SJMP,
	SUBB,
	SWAP,
	XCH,
	XCHD,
	XRL,
}

Register :: enum u8 {
	A,
	ACC = A,
	B,
	DPH,
	DPL,
	DPTR,
	IE,
	IP,
	P0,
	P1,
	P2,
	P3,
	PCON,
	PSW,
	PSW1,
	SCON,
	SBUF,
	SP,
	TMOD,
	TCON,
	TL0,
	TH0,
	TL1,
	TH1,
	TL2,
	TH2,
	R0,
	R1,
	R2,
	R3,
	R4,
	R5,
	R6,
	R7,
}

_init_maps :: proc() {
	_instruction_map["ACALL"] = .ACALL
	_instruction_map["ADD"]   = .ADD
	_instruction_map["ADDC"]  = .ADDC
	_instruction_map["AJMP"]  = .AJMP
	_instruction_map["ANL"]   = .ANL
	_instruction_map["CJNE"]  = .CJNE
	_instruction_map["CLR"]   = .CLR
	_instruction_map["CPL"]   = .CPL
	_instruction_map["DA"]    = .DA
	_instruction_map["DEC"]   = .DEC
	_instruction_map["DIV"]   = .DIV
	_instruction_map["DJNZ"]  = .DJNZ
	_instruction_map["INC"]   = .INC
	_instruction_map["JB"]    = .JB
	_instruction_map["JBC"]   = .JBC
	_instruction_map["JC"]    = .JC
	_instruction_map["JMP"]   = .JMP
	_instruction_map["JNB"]   = .JNB
	_instruction_map["JNC"]   = .JNC
	_instruction_map["JNZ"]   = .JNZ
	_instruction_map["JZ"]    = .JZ
	_instruction_map["LCALL"] = .LCALL
	_instruction_map["LJMP"]  = .LJMP
	_instruction_map["MOV"]   = .MOV
	_instruction_map["MOVC"]  = .MOVC
	_instruction_map["MOVX"]  = .MOVX
	_instruction_map["MUL"]   = .MUL
	_instruction_map["NOP"]   = .NOP
	_instruction_map["ORL"]   = .ORL
	_instruction_map["POP"]   = .POP
	_instruction_map["PUSH"]  = .PUSH
	_instruction_map["RET"]   = .RET
	_instruction_map["RETI"]  = .RETI
	_instruction_map["RL"]    = .RL
	_instruction_map["RLC"]   = .RLC
	_instruction_map["RR"]    = .RR
	_instruction_map["RRC"]   = .RRC
	_instruction_map["SETB"]  = .SETB
	_instruction_map["SJMP"]  = .SJMP
	_instruction_map["SUBB"]  = .SUBB
	_instruction_map["SWAP"]  = .SWAP
	_instruction_map["XCH"]   = .XCH
	_instruction_map["XCHD"]  = .XCHD
	_instruction_map["XRL"]   = .XRL

	_register_map["A"]    = .A
	_register_map["ACC"]  = .ACC
	_register_map["B"]    = .B
	_register_map["DPH"]  = .DPH
	_register_map["DPL"]  = .DPL
	_register_map["DPTR"] = .DPTR
	_register_map["IE"]   = .IE
	_register_map["IP"]   = .IP
	_register_map["P0"]   = .P0
	_register_map["P1"]   = .P1
	_register_map["P2"]   = .P2
	_register_map["P3"]   = .P3
	_register_map["PCON"] = .PCON
	_register_map["PSW"]  = .PSW
	_register_map["PSW1"] = .PSW1
	_register_map["SCON"] = .SCON
	_register_map["SBUF"] = .SBUF
	_register_map["SP"]   = .SP
	_register_map["TMOD"] = .TMOD
	_register_map["TCON"] = .TCON
	_register_map["TL0"]  = .TL0
	_register_map["TH0"]  = .TH0
	_register_map["TL1"]  = .TL1
	_register_map["TH1"]  = .TH1
	_register_map["TL2"]  = .TL2
	_register_map["TH2"]  = .TH2
	_register_map["R0"]   = .R0
	_register_map["R1"]   = .R1
	_register_map["R2"]   = .R2
	_register_map["R3"]   = .R3
	_register_map["R4"]   = .R4
	_register_map["R5"]   = .R5
	_register_map["R6"]   = .R6
	_register_map["R7"]   = .R7
}

_tokenize :: proc(input: string) -> []Token {
	tokens: [dynamic]Token

	is_alpha :: proc(ch: u8) -> bool {
		ch := ch & ~u8(0x20)
		return ch >= 'A' && ch <= 'Z'
	}
	is_upper_alpha :: proc(ch: u8) -> bool {
		return ch >= 'A' && ch <= 'Z'
	}
	is_digit :: proc(ch: u8) -> bool {
		return ch >= '0' && ch <= '9'
	}
	is_hex :: proc(ch: u8) -> bool {
		ch_upper := ch & ~u8(0x20)
		return ch_upper >= 'A' && ch_upper <= 'F' || is_digit(ch)
	}

	// consumption operates on entire input
	consume_token :: proc(tokens: ^[dynamic]Token, kind: Token_Kind, start: ^int, length: int, value: uint = 0) {
		assert(length < 256)
		assert(length > 0)
		token := Token {
			kind   = kind,
			start  = i32(start^),
			length = u8(length),
			value  = u16(value),
		}
		append(tokens, token)
		start^ += length
	}

	consume_instruction :: proc(tokens: ^[dynamic]Token, s: string, idx: ^int) {
		start := idx^
		for ; idx^ < len(s) && s[idx^] != ' ' && s[idx^] != '\n'; idx^ += 1 { }

		instruction_str := s[start:idx^]
		instruction, found := _instruction_map[instruction_str]
		if !found {
			instruction = .Invalid
		}

		assert(idx^ - start < 256)
		assert(idx^ - start > 0)
		token := Token {
			kind        = .Instruction,
			start       = i32(start),
			length      = u8(idx^ - start),
			instruction = instruction,
		}
		append(tokens, token)
	}

	consume_name :: proc(tokens: ^[dynamic]Token, s: string, idx: ^int, allow_dot: bool = false) -> Token_Kind {
		start := idx^
		kind: Token_Kind = .Name
		if s[start] == '@' {
			kind = .Deref
			idx^ += 1
		}
		end := get_name(s[idx^:], allow_dot)

		reg, is_reg := _register_map[s[idx^:idx^+end]]
		if kind == .Deref {
			fmt.assertf(is_reg, "unknown register %s", s[idx^:idx^+end])
		} else if is_reg {
			kind = .Register
		}

		idx^ += end

		assert(end < 256)
		assert(end > 0)
		token := Token {
			kind   = kind,
			start  = i32(start),
			length = u8(end),
		}
		if is_reg {
			token.register = reg
		}
		append(tokens, token)

		return kind
	}

	consume_immediate :: proc(tokens: ^[dynamic]Token, s: string, idx: ^int) {
		start := idx^
		assert(s[idx^] == '#')
		assert(s[idx^+1] == '0')
		assert(s[idx^+2] == 'x')
		idx^ += 3

		i: int
		for ; is_hex(s[idx^ + i]); i += 1 { }

		n: int
		val, ok := strconv.parse_uint(s[idx^:idx^ + i], 16, &n)
		assert(ok)
		assert(i == n)
		assert(idx^ - start > 0)

		idx^ += n
		token := Token {
			kind   = .Immediate,
			start  = i32(start),
			length = u8(idx^ - start),
			value  = u16(val),
		}
		append(tokens, token)
	}

	consume_data :: proc(tokens: ^[dynamic]Token, s: string, idx: ^int) {
		i := idx^
		for ; i < len(s) && s[i] != '\n' && s[i] != ' '; i += 1 { }

		assert(i - idx^ == 3)
		assert(s[i-1] == 'h')

		val, ok := strconv.parse_uint(s[idx^:i - 1], 16)
		assert(ok)

		consume_token(tokens, .Data_Hex, idx, 3, val)

		if s[i] == '\n' {
			return
		}

		n := get_spaces(s[idx^:])
		if n == 0 {
			return
		}

		if idx^ + n >= len(s) || s[idx^ + n] == '\n' {
			consume_token(tokens, .Trailing_Space, idx, n)
			return
		}
		consume_token(tokens, .Whitespace, idx, n)
		if s[idx^] == ';' {
			return
		}

		if s[idx^] == '"' && s[idx^ + 1] == '"' {
			consume_token(tokens, .Data_Ascii, idx, 2)
		} else {
			consume_token(tokens, .Data_Ascii, idx, 1)
		}
	}

	consume_args :: proc(tokens: ^[dynamic]Token, s: string, idx: ^int) {
		arg_count: int
		for idx^ < len(s) {
			switch s[idx^] {
			case ';', ' ', '\n':
				return
			case '@':
				kind := consume_name(tokens, s, idx)
				assert(kind == .Deref)

				if s[idx^] == '+' {
					consume_token(tokens, .Register_Offset, idx, 1)
					consume_name(tokens, s, idx)
				}

				if s[idx^] == '=' && s[idx^+1] == '>' {
					consume_token(tokens, .Deref_Label_Sym, idx, 2)

					kind = consume_name(tokens, s, idx)
					assert(kind == .Name)
					if s[idx^] == ':' {
						tokens[len(tokens) - 1].kind = .Arg_Label
						consume_token(tokens, .Label_Sym, idx, 1)

						kind = consume_name(tokens, s, idx)
						assert(kind == .Name)
					}
					tokens[len(tokens) - 1].kind = .Deref_Label
				}
			case '=':
				assert(s[idx^ + 1] == '>')
				consume_token(tokens, .Deref_Label_Sym, idx, 2)
			case '#':
				consume_immediate(tokens, s, idx)
			case ',':
				consume_token(tokens, .Comma, idx, 1)
			case '.':
				val := uint(s[idx^ + 1] - '0')
				assert(val >= 0 && val <= 7)
				consume_token(tokens, .Bit_Suffix, idx, 2, val)
			case '+':
				assert(s[idx^ + 1] == '1' || s[idx^ + 1] == '2')
				consume_token(tokens, .Label_Offset, idx, 2)
			case:
				start := idx^
				kind := consume_name(tokens, s, idx)
				#partial switch kind {
				case .Name:
					if s[idx^] == ':' {
						tokens[len(tokens) - 1].kind = .Arg_Label
						consume_token(tokens, .Label_Sym, idx, 1)
					}
				}
			}
		}
	}

	get_name :: proc(s: string, allow_dot: bool = false) -> int {
		i: int
		for i < len(s) {
			if is_alpha(s[i]) || is_digit(s[i]) {
				i += 1
				continue
			}
			switch s[i] {
			case '_', '-', '/':
				i += 1
				continue
			case '.':
				if allow_dot {
					i += 1
					continue
				}
				// single dot could be bit address
				if i + 2 < len(s) && s[i + 1] == '.' && s[i + 2] == '.' {
					i += 3
					continue
				}
			}
			break
		}
		return i
	}

	get_spaces :: proc(s: string) -> int {
		i: int
		for ; i < len(s) && s[i] == ' '; i += 1 { }
		return i
	}

	get_hex :: proc(s: string) -> int {
		i: int
		for ; i < len(s) && is_hex(s[i]); i += 1 { }
		return i
	}

	get_to_eol :: proc(s: string) -> int {
		i: int
		for ; i < len(s) && s[i] != '\n'; i += 1 { }
		return i
	}

	idx:   int
	for idx < len(input) {
		if is_alpha(input[idx]) {
			end := strings.index_byte(input[idx:], ':')
			assert(end != -1)

			switch input[idx:idx+end] {
			case "CODE","B0","B1","B2":
			case:
				// memory label.. exclude
				idx += get_to_eol(input[idx:])
				continue
			}
			consume_token(&tokens, .Location_Label, &idx, end)
			consume_token(&tokens, .Label_Sym, &idx, 1)

			if is_upper_alpha(input[idx]) {
				skip := strings.index_byte(input[idx:], ':')
				assert(skip != -1)
				idx += skip + 1

				// double ::
				if input[idx] == ':' {
					idx += 1
				}
			}

			end = strings.index_byte(input[idx:], ' ')
			assert(end != -1)
			v, ok := strconv.parse_uint(input[idx:idx+end], 16)
			assert(ok)

			consume_token(&tokens, .Location, &idx, end, v)

			end = get_spaces(input[idx:])
			consume_token(&tokens, .Whitespace, &idx, end)

			end = get_hex(input[idx:])
			consume_token(&tokens, .Location_Bytes, &idx, end)

			end = get_spaces(input[idx:])
			consume_token(&tokens, .Whitespace, &idx, end)

			consume_instruction(&tokens, input, &idx)
			instruction := tokens[len(tokens) - 1].instruction

			end = get_spaces(input[idx:])
			if end != 0 {
				consume_token(&tokens, .Whitespace, &idx, end)
			}

			if instruction == .Invalid {
				consume_data(&tokens, input, &idx)
			} else {
				consume_args(&tokens, input, &idx)
			}

		} else if input[idx] == ' ' {
			end := get_spaces(input[idx:])
			consume_token(&tokens, .Whitespace, &idx, end)

			if is_alpha(input[idx]) || is_digit(input[idx]) {
				kind := consume_name(&tokens, input, &idx, true)
				assert(kind == .Name || kind == .Register)

				assert(input[idx] == ':')
				tokens[len(tokens) - 1].kind = .Label
				consume_token(&tokens, .Label_Sym, &idx, 1)
			}
		}

		// trailng space or comments
		end := get_spaces(input[idx:])
		if input[idx+end] == ';' {
			if end > 0 {
				consume_token(&tokens, .Whitespace, &idx, end)
			}
			end = get_to_eol(input[idx:])
			consume_token(&tokens, .Comment, &idx, end)
		} else if input[idx+end] == '\n' && end > 0 {
			consume_token(&tokens, .Trailing_Space, &idx, end)
		}

		assert(input[idx] == '\n')
		consume_token(&tokens, .Line_End, &idx, 1)
	}

	return tokens[:]
}

