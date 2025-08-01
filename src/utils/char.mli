include module type of Base.Char

val is_binary : char -> bool
val is_octal : char -> bool
val is_newline : char -> bool
val is_special_symbol : char -> bool
val is_eof : char -> bool
val is_nonascii : char -> bool
val is_valid_name_symbol : char -> bool
val is_word_separator : char -> bool
val of_hex_digits_le : char -> char -> char option
