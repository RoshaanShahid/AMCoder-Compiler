import re

class Token:
    def __init__(self, type, value, line, column):
        self.type = type
        self.value = value
        self.line = line
        self.column = column

class Lexer:
    def __init__(self):
        self.token_patterns = [
            ('//.*', 'COMMENT'),                     # Single-line comment
            ('/\*[\s\S]*?\*/', 'MULTI_LINE_COMMENT'), # Multi-line comment
            ('AMint|AMchar|AMfloat|AMbool|AMstring|AMdouble', 'TYPE'),
            ('AMfor|AMwhile|AMdo|startIf|otherIf|otherwise|shift', 'KEYWORD'),
            ('[a-zA-Z_][a-zA-Z0-9_]*', 'IDENTIFIER'),
            ('\+\+|--|\*\*|%%', 'ARITHMETIC_OPERATOR'),
            ('-|\?|\^|~|\^-|~-', 'COMPARISON_OPERATOR'),
            ('==', 'ASSIGNMENT_OPERATOR'),
            ('//|\\\\|\?\?', 'LOGICAL_OPERATOR'),
            ('##|\$\$', 'INCREMENT_DECREMENT_OPERATOR'),
            ('@|\|', 'POINTER_OPERATOR'),
            (';', 'DELIMITER'),
            ('Static|Dynamic|Const', 'TYPE_CAST_OPERATOR'),
            ('[0-9]+', 'INTEGER_LITERAL'),
            ('[{}()\[\]]', 'BRACKET'),
            ('&', 'TERMINATOR'),
            ('\s+', 'WHITESPACE'),
            ('.', 'UNKNOWN')
        ]
        self.token_regex = '|'.join(f'(?P<{name}>{pattern})' for pattern, name in self.token_patterns)
        self.master_pattern = re.compile(self.token_regex, re.MULTILINE)
        self.symbol_table = {}

    def tokenize(self, code):
        line_num = 1
        line_start = 0

        for mo in self.master_pattern.finditer(code):
            kind = mo.lastgroup
            value = mo.group(kind)

            if kind == 'WHITESPACE':
                if '\n' in value:
                    line_num += value.count('\n')
                    line_start = mo.end()
                continue

            if kind == 'COMMENT':
                continue

            if kind == 'UNKNOWN':
                print(f"Lexical Error: Unrecognized symbol '{value}' at Line {line_num}, Column {mo.start() - line_start}")
                continue

            if kind == 'IDENTIFIER':
                if value not in self.symbol_table:
                    self.symbol_table[value] = {'type': None, 'line': line_num, 'column': mo.start() - line_start}

            column = mo.start() - line_start
            yield Token(kind, value, line_num, column)

    def print_symbol_table(self):
        print("\nSymbol Table:")
        for identifier, info in self.symbol_table.items():
            print(f"Identifier: {identifier}, Type: {info['type']}, Line: {info['line']}, Column: {info['column']}")

def main():
    # Ask user for input method
    print("Choose an option:")
    print("1. Read source code from a file.")
    print("2. Enter AMCoder code manually (type 'END' to finish).")
    input_method = input("Enter your choice (1 or 2): ").strip()

    lexer = Lexer()

    if input_method == '1':
        # Read code from a file
        filename = input("Enter the filename of the source code: ").strip()
        try:
            with open(filename, "r") as file:
                code = file.read()
        except FileNotFoundError:
            print(f"Error: File '{filename}' not found.")
            return
    elif input_method == '2':
        # Read code from user input until 'END'
        print("Enter AMCoder code (type 'END' on a new line to finish):")
        code_lines = []
        while True:
            line = input()
            if line.strip() == 'END':
                break
            code_lines.append(line)
        code = '\n'.join(code_lines)
    else:
        print("Invalid option. Please select 1 or 2.")
        return

    # Process the code
    print("\nTokens:")
    tokens = list(lexer.tokenize(code))
    for token in tokens:
        print(f"TOKEN: {token.value:<10} TYPE: {token.type}")

    lexer.print_symbol_table()

if __name__ == "__main__":
    main()






