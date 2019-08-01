open Printf
open Registers

type instruction =
  | Cp_n
  | Dec(register)
  | Di
  | Ld_rr(register, register)
  | Inc(register)
  | Jp
  | Jr(Cpu.cpu_flag, bool)
  | Ld_read_io_n
  | Ld_write_io_n
  // | Ld_read_io_c
  // | Ld_write_io_c
  | Ldd_hl_a
  | Ldi_a_hl
  | Ld_n(register)
  | Ld_n_hl
  | Ld_nn(register16)
  | Nop
  | Xor(register, register)

exception OpcodeNotImplement(string);

// FE 7E 28 A7
let decode = (opcode) => switch(opcode) {
  | 0x00 => Nop
  | 0x01 => Ld_nn(BC)
  | 0x05 => Dec(B)
  | 0x06 => Ld_n(B)
  | 0x11 => Ld_nn(DE)
  | 0x0C => Inc(C)
  | 0x0D => Dec(C)
  | 0x0E => Ld_n(C)
  | 0x20 => Jr(Z, false)
  | 0x21 => Ld_nn(HL)
  | 0x2A => Ldi_a_hl
  | 0x32 => Ldd_hl_a
  | 0x36 => Ld_n_hl
  | 0x3E => Ld_n(A)
  | 0x41 => Ld_rr(B, C)
  | 0x42 => Ld_rr(B, D)
  | 0x47 => Ld_rr(B, A)
  | 0xAF => Xor(A, A)
  | 0xC3 => Jp
  | 0xE0 => Ld_write_io_n
  | 0xFE => Cp_n
  // | 0xE2 => Ld_write_io_c
  | 0xF0 => Ld_read_io_n
  // | 0xF2 => Ld_read_io_c
  | 0xF3 => Di
  // | n => Js.log(sprintf("Opcode not implemented: %02X", n)); Nop
  | n => raise(OpcodeNotImplement(sprintf("0x%02X", n)))
}

let pretty = (instruction) => switch(instruction) {
  | Cp_n => "CP n"
  | Di => "Di"
  | Ld_n(r) => Format.sprintf("LD %s, n", to_string(r))
  | Ld_n_hl => "LD n, (HL)"
  | Ld_nn(r) => Format.sprintf("LD %s, nn", to_string16(r))
  | Ld_rr(r1, r2) => Format.sprintf("LD %s, %s", to_string(r1), to_string(r2))
  | Ldi_a_hl => "LDI A, (HL)"
  | Ldd_hl_a => "LDD (HL), A"
  | Ld_read_io_n => "LDH A, (FF00+n)"
  | Ld_write_io_n => "LDH (FF00+n), A"
  // | Ld_read_io_c => "LD A, (FF00+C)"
  // | Ld_write_io_c => "LD (FF00+c), A"
  | Inc(r) => Format.sprintf("INC %s", to_string(r))
  | Dec(r) => Format.sprintf("DEC %s", to_string(r))
  | Jp => "JP nn"
  | Jr(Z, false) => "JR NZ, nn"
  | Jr(C, false) => "JR NC, nn"
  | Jr(Z, true) => "JR Z, nn"
  | Jr(C, true) => "JR C, nn"
  | Jr(_, _) => "Unreachable"
  | Xor(r1, r2) => Format.sprintf("XOR %s %s", to_string(r1), to_string(r2))
  | Nop => "NOP"
  // | _ => "???"
}
