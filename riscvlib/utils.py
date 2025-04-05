
ZERO_BYTES32 = b"\x00" * 32


def extend_bitstr(bit_str:str, ext_bit:str= '0', bit_len:int=12):
    """
    Extend a bitstring to 'bit_len' length using 'extend_bit' as the extra padding
    """
    return (ext_bit * (bit_len - len(bit_str))) + bit_str


def twos_complement_str(bit_str):
    """
    convert a bitstring -> 2's complement of bitstring
    :param bit_str: str - bitstring
    :return: str - 2's complement of input string
    """
    # if passed a leading '-', replace with a zero, it's a python format thing
    bit_str = bit_str.replace("-", "0", 1)
    bit_str = bit_str.replace('0b', "", 1)

    # Convert the bit string to an integer
    unsigned_value = int(bit_str, 2)
    bit_len = len(bit_str)

    # Calculate the 2's complement
    inverted_value = ~unsigned_value
    twos_complement_value = (inverted_value + 1) & ((1 << bit_len) - 1)

    # Convert back to a bit string
    twos_complement_string = format(twos_complement_value, f"0{bit_len}b")
    return twos_complement_string
