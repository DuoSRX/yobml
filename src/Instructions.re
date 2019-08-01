open Printf
open Registers

// Used to decode some ocodes.
// Order is important.
let regs = [|B, C, D, E, H, L, F, A|];

type instruction =
  | Cp_n
  | Dec(register)
  | Di
  | Ld_r16_a(register16)
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
  | 0x05 => Dec(B)
  | 0x06 => Ld_n(B)
  | 0x11 => Ld_nn(DE)
  | 0x12 => Ld_r16_a(DE)
  | 0x14 => Inc(D)
  | 0x1C => Inc(E)
  | 0x0C => Inc(C)
  | 0x0D => Dec(C)
  | 0x0E => Ld_n(C)
  | 0x20 => Jr(Z, false)
  | 0x21 => Ld_nn(HL)
  | 0x2A => Ldi_a_hl
  | 0x32 => Ldd_hl_a
  | 0x36 => Ld_n_hl
  | 0x3E => Ld_n(A)
  // | 0x41 => Ld_rr(B, C)
  // | 0x42 => Ld_rr(B, D)
  // | 0x47 => Ld_rr(B, A)
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
  | Ld_r16_a(r) => Format.sprintf("LD [%s], A", to_string16(r))
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
