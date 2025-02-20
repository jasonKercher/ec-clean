package ecclean

import    "core:fmt"
import    "core:bytes"
import os "core:os/os2"
import    "core:strings"
import    "core:strconv"
import    "core:bufio"

import "getargs"

main :: proc() {
	args := getargs.make_getargs()
	defer getargs.destroy(&args)
	getargs.add_arg(&args, "h", "help", .None)
	getargs.add_arg(&args, "p", "prev", .Required)
	getargs.add_arg(&args, "o", "output", .Required)
	getargs.read_args(&args, os.args)

	if getargs.get_flag(&args, "h") {
		fmt.println("no help for you!")
		os.exit(0)
	}

	output_file: ^os.File = os.stdout
	if output_name, found := getargs.get_payload(&args, "o"); found {
		err: os.Error
		output_file, err = os.open(output_name, {.Read, .Write, .Trunc, .Create}, 0o664)
		if err != nil {
			os.print_error(os.stderr, err, output_name)
			os.exit(1)
		}
	}

	if prev_name, found := getargs.get_payload(&args, "p"); found {
		unimplemented("--prev not implemented")
	}

	if args.arg_idx >= len(os.args) {
		fmt.eprintln("Expected asm file input")
		os.exit(1)
	}
	input_file := os.args[args.arg_idx]

	input, err := os.read_entire_file(input_file, context.allocator)
	if err != nil {
		os.print_error(os.stderr, err, input_file)
		os.exit(1)
	}
	defer delete(input)

	data_init()
	tokens, offsets := _tokenize(string(input))

	_remove_useless_comments(tokens)

	_write_output(output_file, input, tokens)
	os.close(output_file)
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
	Null,        // edited out
	Whitespace,
	Eol,
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
	Comment_Useless,
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
	Memory_Info,
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

Edit :: struct {
	text:       string,
	insert_idx: i32,
	_:          i32,
}

_tokenize :: proc(input: string) -> ([]Token, []i32) {
	tokens: [dynamic]Token

	offsets := make([]i32, 1024 * 128)

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

			// offset to make the labeled address correct
			address_offset: uint
			switch input[idx:idx+end] {
			case "CODE","B0":
			case "B1":
				address_offset = 0x8000
			case "B2":
				address_offset = 0x10000
			case:
				line_len := get_to_eol(input[idx:])
				consume_token(&tokens, .Memory_Info, &idx, line_len)
				continue
			}
			offset := len(tokens)

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

			offsets[v + address_offset] = i32(offset)

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

			comment := input[idx:idx+end]
			comment_kind : Token_Kind = .Comment

			if len(comment) >= 2 && comment[:2] == ";*" {
				comment_kind = .Comment_Useless
			} else if len(comment) >= 5 && comment[:5] == ";XREF" {
				comment_kind = .Comment_Useless
			} else if len(comment) >= 15 && comment[:14] == ";             " {
				comment_kind = .Comment_Useless
			} else if len(comment) == 5 && comment == ";= ??" {
				comment_kind = .Comment_Useless
			} else if len(comment) >= 10 && comment[:10] == ";undefined" {
				comment_kind = .Comment_Useless
			}
			consume_token(&tokens, comment_kind, &idx, end)
		} else if input[idx+end] == '\n' && end > 0 {
			consume_token(&tokens, .Trailing_Space, &idx, end)
		}

		assert(input[idx] == '\n')
		consume_token(&tokens, .Eol, &idx, 1)
	}

	return tokens[:], offsets
}

_remove_useless_comments :: proc(tokens: []Token) {
	line_begin := 0
	for i := 0; i < len(tokens); i += 1 {
		if tokens[i].kind == .Eol {
			line_begin = i + 1
			continue
		}
		if tokens[i].kind == .Comment_Useless {
			if i >= line_begin && tokens[i-1].kind == .Whitespace {
				tokens[i-1].kind = .Null
				tokens[i].kind = .Null
				if i - 1 == line_begin && tokens[i + 1].kind == .Eol {
					tokens[i+1].kind = .Null
					line_begin = i + 2
				}
			}
		}
	}
}

_write_output :: proc(f: ^os.File, input: []u8, tokens: []Token) {
	stream := os.to_stream(f)
	w: bufio.Writer
	bufio.writer_init(&w, stream)

	for t in tokens {
		data := input[t.start:t.start+i32(t.length)]
		#partial switch t.kind {
		case .Trailing_Space:
		case .Null:
			continue
		case .Data_Ascii:
			if t.kind == .Data_Ascii && t.length == 1 && data[0] == '"' {
				q := "\"\""
				bufio.writer_write_string(&w, q)
				continue
			}
		}

		bufio.writer_write(&w, data)
	}

	bufio.writer_flush(&w)
}
