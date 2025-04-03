
# Floating point rounding mode, bits 5-7 in the FCSR
class FP_RNDMode:
    #  Used in F,D,Q,H extensions, current standard says always set as RNE when encoding the 'rm' fields
    # CSR reg bits[5,8] 3 bits sub 5,6,7
    # TODO: Convert into enum when py version supports
    RNE = '000'  # round nearest
    RTZ = '001'  # round towards zero
    RDN = '010'  # round down
    RUP = '011'  # round up
    RMM = '100'  # round nearest, ties to max magnitude
    RSV0 = '101'  # Reserved
    RSV1 = '101'  # Reserved
    DYN = '111'  # Dynamic - Rounding mode set by target machine via fp control register

# floatign point flags which are bits 0-4 in the FCSR
FP_FCSR_FLAGS = (
    # fCSR bits  0,1,2,3,4
    (0, "NX"),  # inexact
    (1, "UF"),  # Underflow
    (2, "OF"),  # Overflow
    (3, "DZ"),  # Divide by zero
    (4, "NV"),  # Invalid
)


# floating point classification rd. Set (in rd) by 'fclass.s' a 10 bit mask will have 1 bit set as the classification
FP_CLASS_MASK_VALUES = (
    (0, "−inf"),  # bit[0] set i.e. the first bit
    (1, "negative normal number"),
    (2, "negative subnormal number"),
    (3, "−0"),
    (4, "+0"),
    (5, "positive subnormal number"),
    (6, "positive normal number"),
    (7, "+inf"),
    (8, "signaling NaN"),
    (9, "quiet NaN"),  # 10th bit set i.e. bits[9]
)


class FP_CSR_REGISTERS:
    FLAGS = 0x001  # name for flags portion of the csr
    RM = 0x002    # name for rounding mode portion of the csr
    FCSR = 0x003  # name for FSR (floating point status reg) of the csr i.e. Flags+RM bits 0-7


#  instruction: (name, opcode, func3, func7, itype, ext)
INSTRUCTION_MAP = {
    'add': ('ADD', '0110011', '000', '0000000', 'R', 'i'),
    'sub': ('SUB', '0110011', '000', '0100000', 'R', 'i'),
    'sll': ('SLL', '0110011', '001', '0000000', 'R', 'i'),
    'slt': ('SLT', '0110011', '010', '0000000', 'R', 'i'),
    'sltu': ('SLTU', '0110011', '011', '0000000', 'R', 'i'),
    'xor': ('XOR', '0110011', '100', '0000000', 'R', 'i'),
    'srl': ('SRL', '0110011', '101', '0000000', 'R', 'i'),
    'sra': ('SRA', '0110011', '101', '0100000', 'R', 'i'),
    'or': ('OR', '0110011', '110', '0000000', 'R', 'i'),
    'and': ('AND', '0110011', '111', '0000000', 'R', 'i'),
    'addw': ('ADDW', '0111011', '000', '0000000', 'R', 'i'),
    'subw': ('SUBW', '0111011', '000', '0100000', 'R', 'i'),
    'sllw': ('SLLW', '0111011', '001', '0000000', 'R', 'i'),
    'slrw': ('SLRW', '0111011', '101', '0000000', 'R', 'i'),
    'sraw': ('SRAW', '0111011', '101', '0100000', 'R', 'i'),
    'addi': ('ADDI', '0010011', '000', None, 'I', 'i'),
    'lb': ('LB', '0000011', '000', None, 'IL', 'i'),
    'lh': ('LH', '0000011', '001', None, 'IL', 'i'),
    'lw': ('LW', '0000011', '010', None, 'IL', 'i'),
    'ld': ('LD', '0000011', '011', None, 'IL', 'i'),
    'lbu': ('LBU', '0000011', '100', None, 'IL', 'i'),
    'lhu': ('LHU', '0000011', '101', None, 'IL', 'i'),
    'lwu': ('LWU', '0000011', '110', None, 'IL', 'i'),
    'slli': ('SLLI', '0010011', '001', '0000000', 'I', 'i'),
    'slti': ('SLTI', '0010011', '010', None, 'I', 'i'),
    'sltiu': ('SLTIU', '0010011', '011', None, 'I', 'i'),
    'xori': ('XORI', '0010011', '100', None, 'I', 'i'),
    'srai': ('SRAI', '0010011', '101', '0100000', 'I', 'i'),
    'srli': ("SRLI", "0010011", "101", "0000000", 'I', 'i'),  # Shift Right Logical
    'ori': ('ORI', '0010011', '110', None, 'I', 'i'),
    'andi': ('ANDI', '0010011', '111', None, 'I', 'i'),
    'addiw': ('ADDIW', '0011011', '000', None, 'I', 'i'),
    'slliw': ('SLLIW', '0011011', '001', '0000000', 'I', 'i'),
    'srliw': ('SRLIW', '0011011', '101', '0000000', 'I', 'i'),
    'sraiw': ('SRAIW', '0011011', '101', '0100000', 'I', 'i'),
    'jalr': ('JALR', '1100111', '000', None, 'I', 'i'),
    'sw': ('SW', '0100011', '010', None, 'S', 'i'),
    'sb': ('SB', '0100011', '000', None, 'S', 'i'),
    'sh': ('SH', '0100011', '001', None, 'S', 'i'),
    'sd': ('SD', '0100011', '011', None, 'S', 'i'),
    'beq': ('BEQ', '1100011', '000', None, 'SB', 'i'),
    'bne': ('BNE', '1100011', '001', None, 'SB', 'i'),
    'blt': ('BLT', '1100011', '100', None, 'SB', 'i'),
    'bge': ('BGE', '1100011', '101', None, 'SB', 'i'),
    'bltu': ('BLTU', '1100011', '110', None, 'SB', 'i'),
    'bgeu': ('BGEU', '1100011', '111', None, 'SB', 'i'),
    'auipc': ('AUIPC', '0010111', None, None, 'U', 'i'),
    'lui': ('LUI', '0110111', None, None, 'U', 'i'),
    'jal': ('JAL', '1101111', None, None, 'UJ', 'i'),
    'mul': ('MUL', '0110011', '000', '0000001', 'R', 'm'),
    'mulh': ('MULH', '0110011', '001', '0000001', 'R', 'm'),
    'mulhsu': ('MULHSU', '0110011', '010', '0000001', 'R', 'm'),
    'mulhu': ('MULHU', '0110011', '011', '0000001', 'R', 'm'),
    'div': ('DIV', '0110011', '100', '0000001', 'R', 'm'),
    'divu': ('DIVU', '0110011', '101', '0000001', 'R', 'm'),
    'rem': ('REM', '0110011', '110', '0000001', 'R', 'm'),
    'remu': ('REMU', '0110011', '111', '0000001', 'R', 'm'),
    'andn': ('ANDN', '0110011', '111', '0100000', 'R', 'b'),
    'orn': ('ORN', '0110011', '110', '0100000', 'R', 'b'),
    'xnor': ('XNOR', '0110011', '100', '0100000', 'R', 'b'),
    'clz': ('CLZ', '0010011', '001', '0110000', 'I', 'b'),
    'ctz': ('CTZ', '0010011', '001', '0000001', 'I', 'b'),
    'pcnt': ('PCNT', '0010011', '001', '0000010', 'I', 'b'),
    'rol': ('ROL', '0110011', '001', '0110000', 'R', 'b'),
    'ror': ('ROR', '0110011', '101', '0110000', 'R', 'b'),
    'rev8': ('REV8', '0110011', '110', '0110100', 'R', 'b'),
    'clmul': ('CLMUL', '0110011', '001', '0000101', 'R', 'b'),
    'clmulr': ('CLMULR', '0110011', '101', '0000101', 'R', 'b'),
    'clmulh': ('CLMULH', '0110011', '011', '0000101', 'R', 'b'),
    'bclr': ('BCLR', '0110011', '001', '0100100', 'R', 'b'),
    'bset': ('BSET', '0110011', '001', '0010100', 'R', 'b'),
    'binv': ('BINV', '0110011', '001', '0110100', 'R', 'b'),
    'bext': ('BEXT', '0110011', '101', '0100100', 'R', 'b'),
    'bdep': ('BDEP', '0110011', '101', '0110100', 'R', 'b'),
    'ecall': ('ECALL', '1110011', '000', '000000000000', 'I', 'zifencei'),
    'ebreak': ('EBREAK', '1110011', '000', '000000000001', 'I', 'zifencei'),
    'fence': ('FENCE', '0001111', '000', None, 'I', 'zifencei'),
    'fence.i': ('FENCE.I', '0001111', '001', None, 'I', 'zifencei'),
    'csrrw': ('CSRRW', '1110011', '001', None, 'I', 'zicsr'),  # csrrs
    'csrrs': ('CSRRS', '1110011', '010', None, 'I', 'zicsr'),
    'csrrc': ('CSRRC', '1110011', '011', None, 'I', 'zicsr'),
    'csrrwi': ('CSRRWI', '1110011', '101', None, 'I', 'zicsr'),
    'csrrsi': ('CSRRSI', '1110011', '110', None, 'I', 'zicsr'),
    'csrrci': ('CSRRCI', '1110011', '111', None, 'I', 'zicsr'),

    # Note: F instruction Rounding Mode (RM) set by the "func3" field. This may change.
    # NO R4 : https://five-embeddev.com/riscv-user-isa-manual/Priv-v1.12/f.html#sec:single-float-compute
    'fadd.s': ('FADD.S', '1010011', FP_RNDMode.RNE, '0000000', 'R', 'f'),
    'fsub.s': ('FSUB.S', '1010011', FP_RNDMode.RNE, '0000100', 'R', 'f'),
    'fmul.s': ('FMUL.S', '1010011', FP_RNDMode.RNE, '0001000', 'R', 'f'),
    'fdiv.s': ('FDIV.S', '1010011', FP_RNDMode.RNE, '0001100', 'R', 'f'),
    'fsqrt.s': ('FSQRT.S', '1010011', FP_RNDMode.RNE, '0101100', 'R', 'f'),
    'fmin.s': ('FMIN.S', '1010011', '000', '0000000', 'R', 'f'),
    'fmax.s': ('FMAX.S', '1010011', '001', '0000100', 'R', 'f'),
    'fcvt.w.s': ('FCVT.W.S', '1010011', FP_RNDMode.RNE, '1100000', 'R', 'f'),
    'fcvt.wu.s': ('FCVT.WU.S', '1010011', FP_RNDMode.RNE, '1100001', 'R', 'f'),
    'fcvt.s.w': ('FCVT.S.W', '1010011', FP_RNDMode.RNE, '1101000', 'R', 'f'),
    'fcvt.s.wu': ('FCVT.S.WU', '1010011', FP_RNDMode.RNE, '1101001', 'R', 'f'),
    'feq.s': ('FEQ.S', '1010011', '010', '1010000', 'R', 'f'),
    'flt.s': ('FLT.S', '1010011', '001', '1010000', 'R', 'f'),
    'fle.s': ('FLE.S', '1010011', '000', '1010000', 'R', 'f'),
    'fsgnj.s': ('FSGNJ.S', '1010011', '000', '0010000', 'R', 'f'),
    'fsgnjn.s': ('FSGNJN.S', '1010011', '001', '0010000', 'R', 'f'),
    'fsgnjx.s': ('FSGNJX.S', '1010011', '010', '0010000', 'R', 'f'),
}

# map pseudo instruction name --> implementation with arg placeholders
PSEUDO_INSTRUCTION_MAP = {
    "mv": ["addi %arg0, %arg1, 0"],  # move
    "nop": ["addi x0, x0, 0"],   # no op
    "not": ["xori %arg0, %arg1, -1"],  # One's complement
    "neg": ["sub %arg0, x0, %arg1"],  # Two's complement
    "seqz": ["sltiu %arg0, %arg1, 1"],  # Set if = zero
    "li": ["addi %arg0, x0, %arg1"],
    "call": ["jal x1, %arg0"],   # invoke subroutines

    # jumps/returns
    "j": ["jal x0, %arg0"],    # Jump
    "jr": ["jalr x0, %arg0, 0"],  # Jump register
    "ret": ["jalr x0, x1, 0"],  # Return from subroutine

    # branching
    "beqz": ["beq %arg0, x0, %arg1"],  # branch eq zero
    "bnez": ["bne %arg0, x0, %arg1"],  # branch not eq zero
    "blez": ["bge x0, %arg0, %arg1"],  # Branch if ≤ zero
    "bgez": ["bge %arg0, x0, %arg1"],  # Branch if ≥ zero
    "bltz": ["blt %arg0, x0, %arg1"],  # Branch if < zero
    "bgtz": ["blt x0, %arg0, %arg1"],  # Branch if > zero
    # Note: args pos change
    "bgt": ["blt %arg1, %arg0, %arg2"],  # Branch if >
    "ble": ["bge %arg1, %arg0, %arg2"],  # Branch if ≤
    "bgtu": ["bltu %arg1, %arg0, %arg2"],  # Branch if >, unsigned
    "bleu": ["bgeu %arg1, %arg0, %arg2"],  # Branch if ≤, unsigned
    # mult instructs returned
    "la": ["lui %arg0, %hi(%arg1)", "addi %arg0, %arg0, %lo(%arg1)"],
    # b ext
    "snez": ["sltu %arg0, x0, %arg1"],  # Set rd to 1 if rs1 is non-zero, else 0
    "sltz": ["slt %arg0, %arg1, x0"],  # Set rd to 1 if rs1 < 0, else 0
    "sgtz": ["slt %arg0, x0, %arg1"],  # Set rd to 1 if rs1 > 0, else 0
    # f ext
    "fmv.s": ["fsgnj.s %arg0, %arg1, %arg1"],  # Move a floating-point value
    "fabs.s": ["fsgnjx.s %arg0, %arg1, %arg1"],   # Absolute value of a floating-point number
    "fneg.s": ["fsgnjn.s %arg0, %arg1, %arg1"],   # Negate a floating-point number

    "frflags": [f"csrrs %arg0, {FP_CSR_REGISTERS.FLAGS}, x0"],  # read flags "register" into rd, x0 no mods
    "fsflags": [f"csrrw x0, {FP_CSR_REGISTERS.FLAGS}, %arg0"],  # set flags "register"
    "fsflagsi": [f"csrrwi %arg0, {FP_CSR_REGISTERS.FLAGS}, %arg1"],

    "frrm": [f"csrrs %arg0, {FP_CSR_REGISTERS.RM}, x0"],   # read rounding mode "register"
    "fsrm": [f"csrrw x0, {FP_CSR_REGISTERS.RM}, %arg0"],
    "fsrmi": [f"csrrwi %arg0, {FP_CSR_REGISTERS.RM}, %arg1"],

    "fscsr": [f"csrrw %arg0, {FP_CSR_REGISTERS.FCSR}, x0"],
    "frcsr": [f"csrrs x0, {FP_CSR_REGISTERS.FCSR}, %arg0"],
}


# list of pseudo instructions
pseudo_instr_list = list(PSEUDO_INSTRUCTION_MAP.keys())


# registers  "lookup": ('name', int)
STANDARD_REGISTER_MAP = {
    'x0': ('x0', 0), 'zero': ('x0', 0), 'x1': ('x1', 1), 'ra': ('x1', 1), 'x2': ('x2', 2), 'sp': ('x2', 2),
    'x3': ('x3', 3), 'gp': ('x3', 3), 'x4': ('x4', 4), 'tp': ('x4', 4), 'x5': ('x5', 5), 't0': ('x5', 5),
    'x6': ('x6', 6), 't1': ('x6', 6), 'x7': ('x7', 7), 't2': ('x7', 7), 'x8': ('x8', 8),
    'x9': ('x9', 9), 's1': ('x9', 9), 'x10': ('x10', 10), 'a0': ('x10', 10), 'x11': ('x11', 11),
    'a1': ('x11', 11), 'x12': ('x12', 12), 'a2': ('x12', 12), 'x13': ('x13', 13), 'a3': ('x13', 13),
    'x14': ('x14', 14), 'a4': ('x14', 14), 'x15': ('x15', 15), 'a5': ('x15', 15), 'x16': ('x16', 16),
    'a6': ('x16', 16), 'x17': ('x17', 17), 'a7': ('x17', 17), 'x18': ('x18', 18), 's2': ('x18', 18),
    'x19': ('x19', 19), 's3': ('x19', 19), 'x20': ('x20', 20), 's4': ('x20', 20), 'x21': ('x21', 21),
    's5': ('x21', 21), 'x22': ('x22', 22), 's6': ('x22', 22), 'x23': ('x23', 23), 's7': ('x23', 23),
    'x24': ('x24', 24), 's8': ('x24', 24), 'x25': ('x25', 25), 's9': ('x25', 25), 'x26': ('x26', 26),
    's10': ('x26', 26), 'x27': ('x27', 27), 's11': ('x27', 27), 'x28': ('x28', 28), 't3': ('x28', 28),
    'x29': ('x29', 29), 't4': ('x29', 29), 'x30': ('x30', 30), 't5': ('x30', 30), 'x31': ('x31', 31),
    't6': ('x31', 31), 's0': ('x8', 8), 'fp': ('x8', 8)
}

F_EXT_REGISTER_MAP = {
      "ft0": ["f0", 0], "f0": ["f0", 0], "ft1": ["f1", 1], "f1": ["f1", 1], "ft2": ["f2", 2],
      "f2": ["f2", 2], "ft3": ["f3", 3], "f3": ["f3", 3], "ft4": ["f4", 4], "f4": ["f4", 4],
      "ft5": ["f5", 5], "f5": ["f5", 5], "ft6": ["f6", 6], "f6": ["f6", 6], "ft7": ["f7", 7],
      "f7": ["f7", 7], "fs0": ["f8", 8], "f8": ["f8", 8], "fs1": ["f9", 9], "f9": ["f9", 9],
      "fa0": ["f10", 10], "f10": ["f10", 10], "fa1": ["f11", 11], "f11": ["f11", 11],
      "fa2": ["f12", 12], "f12": ["f12", 12], "fa3": ["f13", 13], "f13": ["f13", 13],
      "fa4": ["f14", 14], "f14": ["f14", 14], "fa5": ["f15", 15], "f15": ["f15", 15],
      "fa6": ["f16", 16], "f16": ["f16", 16], "fa7": ["f17", 17], "f17": ["f17", 17],
      "fs2": ["f18", 18], "f18": ["f18", 18], "fs3": ["f19", 19], "f19": ["f19", 19],
      "fs4": ["f20", 20], "f20": ["f20", 20], "fs5": ["f21", 21], "f21": ["f21", 21],
      "fs6": ["f22", 22], "f22": ["f22", 22], "fs7": ["f23", 23], "f23": ["f23", 23],
      "fs8": ["f24", 24], "f24": ["f24", 24], "fs9": ["f25", 25], "f25": ["f25", 25],
      "fs10": ["f26", 26], "f26": ["f26", 26], "fs11": ["f27", 27], "f27": ["f27", 27],
      "ft8": ["f28", 28], "f28": ["f28", 28], "ft9": ["f29", 29], "f29": ["f29", 29],
      "ft10": ["f30", 30], "f30": ["f30", 30], "ft11": ["f31", 31], "f31": ["f31", 31],
}
# all registers
REGISTER_MAP = {**STANDARD_REGISTER_MAP, **F_EXT_REGISTER_MAP}
