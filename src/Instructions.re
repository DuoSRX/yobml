open Printf
open Registers

// Used to decode some ocodes.
// Order is important.
let regs = [|B, C, D, E, H, L, F, A|];

type storage =
  | Register(register)
  | Register16(register16)
  | Pointer(register16)

type instruction =
  | Adc(register)
  | Adc_d8
  | Adc_hl
  | Add(register)
  | Add_d8
  | Add_hl
  | Add_hl_r16(register16)
  | Add_sp_e8
  | And_d8
  | And(register)
  | And_hl
  | Bit(int, register)
  | Call
  | CallCond(Cpu.cpu_flag, bool)
  | Ccf
  | Cp(register)
  | Cp_hl
  | Cp_n
  | Cpl
  | Daa
  | Dec(register)
  | Dec16(register16)
  | Dec_hl
  | Di
  | Ei
  | Halt
  | Inc(register)
  | Inc16(register16)
  | Inc_hl
  | Jp
  | JpCond(Cpu.cpu_flag, bool)
  | Jp_hl
  | Jr_e8
  | Jr(Cpu.cpu_flag, bool)
  | Ld_sp
  | Ld_r16_a(register16)
  | Ld_rr(register, register)
  | Ld_read_io_n
  | Ld_write_io_n
  | Ld_read_io_c
  | Ld_write_io_c
  | Ld_hl_r(register)
  | Ld_r_hl(register)
  | Ldd_hl_a
  | Ldi_a_hl
  | Ldi_hl_a
  | Ld_a16_a
  | Ld_a16_sp
  | Ld_sp_hl
  | Ld_a_a16
  | Ld_a_r16(register16)
  | Ld_n(register)
  | Ld_hl_d8
  | Ld_hl_sp_e8
  | Ld_nn(register16)
  | Or(register)
  | Or_d8
  | Or_hl
  | Nop
  | Pop16(register16)
  | Pop_af
  // | Push(register)
  | Push16(register16)
  | Res(int, register)
  | Res_hl(int)
  | Ret
  | Reti
  | RetCond(Cpu.cpu_flag, bool)
  | Rlca
  | Rra
  | Rr(storage)
  | Rst(int)
  | Sbc(register)
  | Sbc_d8
  | Scf
  | Sla(register)
  | Srl(register)
  | Srl_hl
  | Sub(register)
  | Sub_d8
  | Swap(register)
  | Swap_hl
  | Xor(register)
  | Xor_d8
  | Xor_hl

exception OpcodeNotImplemented(string);
exception CBOpcodeNotImplemented(string);

let decode = (opcode) => switch(opcode) {
  | 0x00 => Nop
  | 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x47 // ld b,r
  | 0x48 | 0x49 | 0x4A | 0x4B | 0x4C | 0x4D | 0x4F // ld c,r
  | 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x57 // ld d,r
  | 0x58 | 0x59 | 0x5A | 0x5B | 0x5C | 0x5D | 0x5F // ld e,r
  | 0x60 | 0x61 | 0x62 | 0x63 | 0x64 | 0x65 | 0x67 // ld h,r
  | 0x68 | 0x69 | 0x6A | 0x6B | 0x6C | 0x6D | 0x6F // ld l,r
  | 0x78 | 0x79 | 0x7A | 0x7B | 0x7C | 0x7D | 0x7F // ld a,r
    => {
      let dst = regs[opcode lsr 3 land 7];
      let src = regs[opcode land 7];
      Ld_rr(dst, src);
    }
  | 0x01 => Ld_nn(BC)
  | 0x02 => Ld_r16_a(BC)
  | 0x03 => Inc16(BC)
  | 0x04 => Inc(B)
  | 0x05 => Dec(B)
  | 0x06 => Ld_n(B)
  | 0x07 => Rlca
  | 0x08 => Ld_a16_sp
  | 0x09 => Add_hl_r16(BC)
  | 0x0A => Ld_a_r16(BC)
  | 0x0B => Dec16(BC)
  | 0x0C => Inc(C)
  | 0x0D => Dec(C)
  | 0x0E => Ld_n(C)
  | 0x11 => Ld_nn(DE)
  | 0x12 => Ld_r16_a(DE)
  | 0x13 => Inc16(DE)
  | 0x14 => Inc(D)
  | 0x16 => Ld_n(D)
  | 0x18 => Jr_e8
  | 0x19 => Add_hl_r16(DE)
  | 0x1A => Ld_a_r16(DE)
  | 0x1B => Dec16(DE)
  | 0x1C => Inc(E)
  | 0x1D => Dec(E)
  | 0x1F => Rra
  | 0x1E => Ld_n(E)
  | 0x20 => Jr(Z, false)
  | 0x21 => Ld_nn(HL)
  | 0x22 => Ldi_hl_a
  | 0x23 => Inc16(HL)
  | 0x24 => Inc(H)
  | 0x25 => Dec(H)
  | 0x26 => Ld_n(H)
  | 0x27 => Daa
  | 0x28 => Jr(Z, true)
  | 0x29 => Add_hl_r16(HL)
  | 0x2A => Ldi_a_hl
  | 0x2B => Dec16(HL)
  | 0x2C => Inc(L)
  | 0x2D => Dec(L)
  | 0x2E => Ld_n(L)
  | 0x2F => Cpl
  | 0x30 => Jr(C, false)
  | 0x31 => Ld_sp
  | 0x32 => Ldd_hl_a
  | 0x33 => Inc16(SP)
  | 0x34 => Inc_hl
  | 0x35 => Dec_hl
  | 0x36 => Ld_hl_d8
  | 0x37 => Scf
  | 0x38 => Jr(C, true)
  | 0x39 => Add_hl_r16(SP)
  | 0x3B => Dec16(SP)
  | 0x3C => Inc(A)
  | 0x3D => Dec(A)
  | 0x3E => Ld_n(A)
  | 0x3F => Ccf
  | 0x46 => Ld_r_hl(B)
  | 0x4E => Ld_r_hl(C)
  | 0x56 => Ld_r_hl(D)
  | 0x5E => Ld_r_hl(E)
  | 0x66 => Ld_r_hl(H)
  | 0x6E => Ld_r_hl(L)
  | 0x70 => Ld_hl_r(B)
  | 0x71 => Ld_hl_r(C)
  | 0x72 => Ld_hl_r(D)
  | 0x73 => Ld_hl_r(E)
  | 0x74 => Ld_hl_r(H)
  | 0x75 => Ld_hl_r(L)
  | 0x76 => Halt
  | 0x77 => Ld_hl_r(A)
  | 0x7E => Ld_r_hl(A)
  | 0x80 => Add(B)
  | 0x81 => Add(C)
  | 0x82 => Add(D)
  | 0x83 => Add(E)
  | 0x84 => Add(H)
  | 0x85 => Add(L)
  | 0x86 => Add_hl
  | 0x87 => Add(A)
  | 0x88 => Adc(B)
  | 0x89 => Adc(C)
  | 0x8A => Adc(D)
  | 0x8B => Adc(E)
  | 0x8C => Adc(H)
  | 0x8D => Adc(L)
  | 0x8E => Adc_hl
  | 0x8F => Adc(A)
  | 0x90 => Sub(B)
  | 0x91 => Sub(C)
  | 0x92 => Sub(D)
  | 0x93 => Sub(E)
  | 0x94 => Sub(H)
  | 0x95 => Sub(L)
  // | 0x96 => Sub_hl
  | 0x97 => Sub(A)
  | 0x98 => Sbc(B)
  | 0x99 => Sbc(C)
  | 0x9A => Sbc(D)
  | 0x9B => Sbc(E)
  | 0x9C => Sbc(H)
  | 0x9D => Sbc(L)
  // | 0x9E => Sbc_hl
  | 0x9F => Sbc(A)
  | 0xA0 => And(B)
  | 0xA1 => And(C)
  | 0xA2 => And(D)
  | 0xA3 => And(E)
  | 0xA4 => And(H)
  | 0xA5 => And(L)
  // | 0xA6 => And_hl
  | 0xA7 => And(A)
  | 0xA8 => Xor(B)
  | 0xA9 => Xor(C)
  | 0xAA => Xor(D)
  | 0xAB => Xor(E)
  | 0xAC => Xor(H)
  | 0xAD => Xor(L)
  | 0xAE => Xor_hl
  | 0xAF => Xor(A)
  | 0xB0 => Or(B)
  | 0xB1 => Or(C)
  | 0xB2 => Or(D)
  | 0xB3 => Or(E)
  | 0xB4 => Or(H)
  | 0xB5 => Or(L)
  | 0xB6 => Or_hl
  | 0xB7 => Or(A)
  | 0xB8 => Cp(B)
  | 0xB9 => Cp(C)
  | 0xBA => Cp(D)
  | 0xBB => Cp(E)
  | 0xBC => Cp(H)
  | 0xBD => Cp(L)
  | 0xBE => Cp_hl
  | 0xBF => Cp(A)
  | 0xC0 => RetCond(Z, false)
  | 0xC1 => Pop16(BC)
  | 0xC2 => JpCond(Z, false)
  | 0xC3 => Jp
  | 0xC4 => CallCond(Z, false)
  | 0xC5 => Push16(BC)
  | 0xC6 => Add_d8
  | 0xC7 => Rst(0)
  | 0xC8 => RetCond(Z, true)
  | 0xC9 => Ret
  | 0xCA => JpCond(Z, true)
  | 0xCC => CallCond(Z, true)
  | 0xCE => Adc_d8
  | 0xCF => Rst(0x8)
  | 0xCD => Call
  | 0xD0 => RetCond(C, false)
  | 0xD1 => Pop16(DE)
  | 0xD2 => JpCond(C, false)
  | 0xD4 => CallCond(C, false)
  | 0xD5 => Push16(DE)
  | 0xD6 => Sub_d8
  | 0xD7 => Rst(0x10)
  | 0xD8 => RetCond(C, true)
  | 0xD9 => Reti
  | 0xDA => JpCond(C, true)
  | 0xDC => CallCond(C, true)
  | 0xDE => Sbc_d8
  | 0xDF => Rst(0x18)
  | 0xE0 => Ld_write_io_n
  | 0xE1 => Pop16(HL)
  | 0xE2 => Ld_write_io_c
  | 0xE5 => Push16(HL)
  | 0xE6 => And_d8
  | 0xE7 => Rst(0x20)
  | 0xE8 => Add_sp_e8
  | 0xE9 => Jp_hl
  | 0xEA => Ld_a16_a
  | 0xEE => Xor_d8
  | 0xEF => Rst(0x28)
  | 0xF0 => Ld_read_io_n
  | 0xF1 => Pop_af
  | 0xF2 => Ld_read_io_c
  | 0xF3 => Di
  | 0xF5 => Push16(AF)
  | 0xF6 => Or_d8
  | 0xF7 => Rst(0x30)
  | 0xF8 => Ld_hl_sp_e8
  | 0xF9 => Ld_sp_hl
  | 0xFA => Ld_a_a16
  | 0xFB => Ei
  | 0xFE => Cp_n
  | 0xFF => Rst(0x38)
  // | n => Js.log(sprintf("Opcode not implemented: %02X", n)); Nop
  | n => raise(OpcodeNotImplemented(sprintf("0x%02X", n)))
}

let decode_cb = (opcode) => switch(opcode) {
  | 0x18 => Rr(Register(B))
  | 0x19 => Rr(Register(C))
  | 0x1A => Rr(Register(D))
  | 0x1B => Rr(Register(E))
  | 0x1C => Rr(Register(H))
  | 0x1D => Rr(Register(L))
  | 0x1E => Rr(Pointer(HL))
  | 0x1F => Rr(Register(A))
  | 0x20 => Sla(B)
  | 0x21 => Sla(C)
  | 0x22 => Sla(D)
  | 0x23 => Sla(E)
  | 0x24 => Sla(H)
  | 0x25 => Sla(L)
  // | 0x26 => Sla_hl
  | 0x27 => Sla(A)
  | 0x30 => Swap(B)
  | 0x31 => Swap(C)
  | 0x32 => Swap(D)
  | 0x33 => Swap(E)
  | 0x34 => Swap(H)
  | 0x35 => Swap(L)
  | 0x36 => Swap_hl
  | 0x37 => Swap(A)
  | 0x38 => Srl(B)
  | 0x39 => Srl(C)
  | 0x3A => Srl(D)
  | 0x3B => Srl(E)
  | 0x3C => Srl(H)
  | 0x3D => Srl(L)
  | 0x3E => Srl_hl
  | 0x3F => Srl(A)
  | 0x86 => Res_hl(0)
  | 0x8E => Res_hl(1)
  | 0x96 => Res_hl(2)
  | 0x9E => Res_hl(3)
  | 0xA6 => Res_hl(4)
  | 0xAE => Res_hl(5)
  | 0xB6 => Res_hl(6)
  | 0xBE => Res_hl(7)
  | _ when opcode >= 0x40 && opcode <= 0x7F => {
      let reg = regs[opcode land 7];
      let bit = opcode lsr 3 land 7;
      Bit(bit, reg)
    }
  | _ when opcode >= 0x80 && opcode <= 0xBF => {
      let reg = regs[opcode land 7];
      let bit = opcode lsr 3 land 7;
      Res(bit, reg)
    }
  | n => raise(CBOpcodeNotImplemented(sprintf("0x%02X", n)))
}

let pretty = (instruction) => switch(instruction) {
  | Adc(r) => sprintf("ADC A, %s", to_string(r))
  | Adc_d8 => "ADC d8"
  | Adc_hl => "ADC A, (HL)"
  | Add(r) => sprintf("ADD A, %s", to_string(r))
  | Add_d8 => "ADD A, d8"
  | Add_hl => "ADD A, (HL)"
  | Add_hl_r16(r) => sprintf("ADD HL, %s", to_string16(r))
  | Add_sp_e8 => "ADD SP, r8"
  | And_d8 => "AND d8"
  | And(r) => sprintf("AND %s", to_string(r))
  | And_hl => "NAD (HL)"
  | Bit(n,r) => sprintf("BIT %d, %s", n, to_string(r))
  | Call => "CALL d16"
  | CallCond(C, true) => "CALL C, d16"
  | CallCond(C, false) => "CALL NC, d16"
  | CallCond(Z, true) => "CALL Z, d16"
  | CallCond(Z, false) => "CALL NZ, d16"
  | CallCond(_,_) => "unreachable"
  | Ccf => "CCF"
  | Cp(r) => sprintf("CP %s", to_string(r))
  | Cp_hl => "CP (HL)"
  | Cp_n => "CP n"
  | Cpl => "CPL"
  | Daa => "DAA"
  | Dec(r) => sprintf("DEC %s", to_string(r))
  | Dec16(r) => sprintf("DEC %s", to_string16(r))
  | Dec_hl => "DEC (HL)"
  | Di => "DI"
  | Ei => "EI"
  | Halt => "HALT"
  | Inc(r) => sprintf("INC %s", to_string(r))
  | Inc16(r) => sprintf("INC %s", to_string16(r))
  | Inc_hl => "INC (HL)"
  | Jp => "JP nn"
  | Jp_hl => "JP (HL)"
  | JpCond(C, true) => "JP C, a16"
  | JpCond(C, false) => "JP NC, a16"
  | JpCond(Z, true) => "JP Z, a16"
  | JpCond(Z, false) => "JP NZ, a16"
  | JpCond(_,_) => "JP unreachable"
  | Jr_e8 => "JR e8"
  | Jr(Z, false) => "JR NZ, nn"
  | Jr(C, false) => "JR NC, nn"
  | Jr(Z, true) => "JR Z, nn"
  | Jr(C, true) => "JR C, nn"
  | Jr(_, _) => "Unreachable"
  | Ld_sp => "LD sp, nn"
  | Ld_n(r) => sprintf("LD %s, n", to_string(r))
  | Ld_r_hl(r) => sprintf("LD %s, (HL)", to_string(r))
  | Ld_hl_r(r) => sprintf("LD (HL), %s", to_string(r))
  | Ld_hl_d8 => "LD n, (HL)"
  | Ld_hl_sp_e8 => "LD HL, sp+e8"
  | Ld_nn(r) => sprintf("LD %s, nn", to_string16(r))
  | Ld_rr(r1, r2) => sprintf("LD %s, %s", to_string(r1), to_string(r2))
  | Ld_r16_a(r) => sprintf("LD [%s], A", to_string16(r))
  | Ld_a_r16(r) => sprintf("LD A, [%s]", to_string16(r))
  | Ldi_hl_a => "LDI (HL), A"
  | Ld_sp_hl => "LD SP, HL"
  | Ldi_a_hl => "LDI A, (HL)"
  | Ldd_hl_a => "LDD (HL), A"
  | Ld_a_a16 => "LD A,(a16)"
  | Ld_a16_a => "LD (a16), A"
  | Ld_a16_sp => "LD (a16), SP"
  | Ld_read_io_n => "LDH A, (FF00+n)"
  | Ld_write_io_n => "LDH (FF00+n), A"
  | Ld_read_io_c => "LD A, (FF00+C)"
  | Ld_write_io_c => "LD (FF00+C), A"
  | Or(r) => sprintf("OR %s", to_string(r))
  | Or_d8 => "OR d8"
  | Or_hl => "OR (HL)"
  | Pop16(r) => sprintf("POP %s", to_string16(r))
  | Pop_af => "POP AF"
  // | Push(r) => sprintf("PUSH %s", to_string(r))
  | Push16(r) => sprintf("PUSH %s", to_string16(r))
  | Res(n, r) => sprintf("RES %d, %s", n, to_string(r))
  | Res_hl(n) => sprintf("RES %d, (HL)", n)
  | Ret => "RET"
  | Reti => "RETI"
  | RetCond(Z, false) => "RET NZ"
  | RetCond(C, false) => "RET NC"
  | RetCond(Z, true) => "RET Z"
  | RetCond(C, true) => "RET C"
  | RetCond(_,_) => "Unreachable RET"
  | Rlca => "RLCA"
  | Rr(Register(r)) => sprintf("RR %s", to_string(r))
  | Rr(Register16(r)) => sprintf("RR %s", to_string16(r))
  | Rr(Pointer(r)) => sprintf("RR (%s)", to_string16(r))
  | Rra => "RRA"
  | Rst(n) => sprintf("RST %02XH", n)
  | Scf => "SCF"
  | Sla(r) => sprintf("SLA %s", to_string(r))
  | Srl(r) => sprintf("SRL %s", to_string(r))
  | Srl_hl => "SRL (HL)"
  | Sub(r) => sprintf("SUB %s", to_string(r))
  | Sbc(r) => sprintf("SBC A,%s", to_string(r))
  | Sbc_d8 => "SBC A, d8"
  | Sub_d8 => "SUB d8"
  | Swap(r) => sprintf("SWAP %s", to_string(r))
  | Swap_hl => "SWAP (HL)"
  | Xor(r) => sprintf("XOR %s", to_string(r))
  | Xor_d8 => "XOR d8"
  | Xor_hl => "XOR A, (HL)"
  | Nop => "NOP"
  // | _ => "???"
}
