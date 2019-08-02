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
  | Adc_d8
  | Add_d8
  | And_d8
  // | And(register)
  | Call
  | CallCond(Cpu.cpu_flag, bool)
  | Cp_n
  | Dec(register)
  | Di
  | Ld_sp
  | Ld_r16_a(register16)
  | Ld_rr(register, register)
  | Inc(register)
  | Inc16(register16)
  | Jp
  | Jr_e8
  | Jr(Cpu.cpu_flag, bool)
  | Ld_read_io_n
  | Ld_write_io_n
  // | Ld_read_io_c
  // | Ld_write_io_c
  | Ld_hl_r(register)
  | Ld_r_hl(register)
  | Ldd_hl_a
  | Ldi_a_hl
  | Ldi_hl_a
  | Ld_a16_a
  | Ld_a_a16
  | Ld_a_r16(register16)
  | Ld_n(register)
  | Ld_hl_d8
  | Ld_nn(register16)
  | Or(register)
  | Or_hl
  | Nop
  | Pop16(register16)
  // | Push(register)
  | Push16(register16)
  | Ret
  | RetCond(Cpu.cpu_flag, bool)
  | Rra
  | Rr(storage)
  | Srl(register)
  | Srl_hl
  | Sub_d8
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
  | 0x03 => Inc16(BC)
  | 0x05 => Dec(B)
  | 0x06 => Ld_n(B)
  | 0x0A => Ld_a_r16(BC)
  | 0x11 => Ld_nn(DE)
  | 0x12 => Ld_r16_a(DE)
  | 0x13 => Inc16(DE)
  | 0x14 => Inc(D)
  | 0x16 => Ld_n(D)
  | 0x18 => Jr_e8
  | 0x1A => Ld_a_r16(DE)
  | 0x1C => Inc(E)
  | 0x1F => Rra
  | 0x0C => Inc(C)
  | 0x0D => Dec(C)
  | 0x0E => Ld_n(C)
  | 0x1E => Ld_n(E)
  | 0x20 => Jr(Z, false)
  | 0x21 => Ld_nn(HL)
  | 0x22 => Ldi_hl_a
  | 0x23 => Inc16(HL)
  | 0x24 => Inc(H)
  | 0x25 => Dec(H)
  | 0x26 => Ld_n(H)
  | 0x28 => Jr(Z, true)
  | 0x2A => Ldi_a_hl
  | 0x2C => Inc(L)
  | 0x2D => Dec(L)
  | 0x2E => Ld_n(L)
  | 0x30 => Jr(C, false)
  | 0x31 => Ld_sp
  | 0x32 => Ldd_hl_a
  | 0x36 => Ld_hl_d8
  | 0x3D => Dec(A)
  | 0x3E => Ld_n(A)
  | 0x46 => Ld_r_hl(B)
  | 0x4E => Ld_r_hl(E)
  | 0x56 => Ld_r_hl(D)
  | 0x70 => Ld_hl_r(B)
  | 0x71 => Ld_hl_r(C)
  | 0x72 => Ld_hl_r(D)
  | 0x73 => Ld_hl_r(E)
  | 0x74 => Ld_hl_r(H)
  | 0x75 => Ld_hl_r(L)
  // 0x75 => HALT
  | 0x77 => Ld_hl_r(A)
  // | 0x41 => Ld_rr(B, C)
  // | 0x42 => Ld_rr(B, D)
  // | 0x47 => Ld_rr(B, A)
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
  | 0xC0 => RetCond(Z, false)
  | 0xC1 => Pop16(BC)
  | 0xC3 => Jp
  | 0xC4 => CallCond(Z, false)
  | 0xC5 => Push16(BC)
  | 0xC6 => Add_d8
  | 0xC8 => RetCond(Z, true)
  | 0xC9 => Ret
  | 0xCE => Adc_d8
  | 0xCD => Call
  | 0xD0 => RetCond(C, false)
  | 0xD1 => Pop16(DE)
  | 0xD5 => Push16(DE)
  | 0xD6 => Sub_d8
  | 0xD8 => RetCond(C, true)
  | 0xE0 => Ld_write_io_n
  | 0xE1 => Pop16(HL)
  | 0xE5 => Push16(HL)
  | 0xE6 => And_d8
  | 0xEA => Ld_a16_a
  | 0xEE => Xor_d8
  | 0xF1 => Pop16(AF)
  | 0xF5 => Push16(AF)
  | 0xFA => Ld_a_a16
  | 0xFE => Cp_n
  // | 0xE2 => Ld_write_io_c
  | 0xF0 => Ld_read_io_n
  // | 0xF2 => Ld_read_io_c
  | 0xF3 => Di
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
  | 0x38 => Srl(B)
  | 0x39 => Srl(C)
  | 0x3A => Srl(D)
  | 0x3B => Srl(E)
  | 0x3C => Srl(H)
  | 0x3D => Srl(L)
  | 0x3E => Srl_hl
  | 0x3F => Srl(A)
  | n => raise(CBOpcodeNotImplemented(sprintf("0x%02X", n)))
}

let pretty = (instruction) => switch(instruction) {
  | Adc_d8 => "ADC d8"
  | Add_d8 => "ADD A, d8"
  | And_d8 => "AND d8"
  | Call => "CALL d16"
  | CallCond(C, true) => "CALL C, d16"
  | CallCond(C, false) => "CALL NC, d16"
  | CallCond(Z, true) => "CALL Z, d16"
  | CallCond(Z, false) => "CALL NZ, d16"
  | CallCond(_,_) => "unreachable"
  | Cp_n => "CP n"
  | Di => "Di"
  | Ld_sp => "LD sp, nn"
  | Ld_n(r) => sprintf("LD %s, n", to_string(r))
  | Ld_r_hl(r) => sprintf("LD %s, (HL)", to_string(r))
  | Ld_hl_r(r) => sprintf("LD (HL), %s", to_string(r))
  | Ld_hl_d8 => "LD n, (HL)"
  | Ld_nn(r) => sprintf("LD %s, nn", to_string16(r))
  | Ld_rr(r1, r2) => sprintf("LD %s, %s", to_string(r1), to_string(r2))
  | Ld_r16_a(r) => sprintf("LD [%s], A", to_string16(r))
  | Ld_a_r16(r) => sprintf("LD A, [%s]", to_string16(r))
  | Ldi_hl_a => "LDI (HL), A"
  | Ldi_a_hl => "LDI A, (HL)"
  | Ldd_hl_a => "LDD (HL), A"
  | Ld_a_a16 => "LD A,(a16)"
  | Ld_a16_a => "LD (a16), A"
  | Ld_read_io_n => "LDH A, (FF00+n)"
  | Ld_write_io_n => "LDH (FF00+n), A"
  // | Ld_read_io_c => "LD A, (FF00+C)"
  // | Ld_write_io_c => "LD (FF00+c), A"
  | Inc(r) => sprintf("INC %s", to_string(r))
  | Inc16(r) => sprintf("INC %s", to_string16(r))
  | Dec(r) => sprintf("DEC %s", to_string(r))
  | Jp => "JP nn"
  | Jr_e8 => "JR e8"
  | Jr(Z, false) => "JR NZ, nn"
  | Jr(C, false) => "JR NC, nn"
  | Jr(Z, true) => "JR Z, nn"
  | Jr(C, true) => "JR C, nn"
  | Jr(_, _) => "Unreachable"
  | Or(r) => sprintf("OR %s", to_string(r))
  | Or_hl => "OR (HL)"
  | Pop16(r) => sprintf("POP %s", to_string16(r))
  // | Push(r) => sprintf("PUSH %s", to_string(r))
  | Push16(r) => sprintf("PUSH %s", to_string16(r))
  | Ret => "RET"
  | RetCond(Z, false) => "RET NZ"
  | RetCond(C, false) => "RET NC"
  | RetCond(Z, true) => "RET Z"
  | RetCond(C, true) => "RET C"
  | RetCond(_,_) => "Unreachable RET"
  | Rr(Register(r)) => sprintf("RR %s", to_string(r))
  | Rr(Register16(r)) => sprintf("RR %s", to_string16(r))
  | Rr(Pointer(r)) => sprintf("RR (%s)", to_string16(r))
  | Rra => "RRA"
  | Srl(r) => sprintf("SRL %s", to_string(r))
  | Srl_hl => "SRL (HL)"
  | Sub_d8 => "SUB d8"
  | Xor(r) => sprintf("XOR %s", to_string(r))
  | Xor_d8 => "XOR d8"
  | Xor_hl => "XOR A, (HL)"
  | Nop => "NOP"
  // | _ => "???"
}
