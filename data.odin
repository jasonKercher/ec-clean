package ecclean

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

data_init :: proc() {
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

