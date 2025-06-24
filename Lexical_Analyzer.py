import re

class Token:
    def __init__(self, type, value, line, column):
        self.type = type
        self.value = value
        self.line = line
        self.column = column

    def __repr__(self):
        return f"Token({self.type}, '{self.value}', L{self.line}, C{self.column})"

class Lexer:
    def __init__(self):
        # The order of these patterns is CRUCIAL. Longer matches must come before shorter ones.
        # All token names (the second element of the tuple) must be valid Python identifiers.
        self.token_patterns = [
            # Comments and Whitespace (to be ignored)
            (r'//.*', 'COMMENT'),
            (r'/\*[\s\S]*?\*/', 'MULTI_LINE_COMMENT'),
            (r'\s+', 'WHITESPACE'),

            # Keywords and Types
            (r'AMint|AMchar|AMfloat|AMbool|AMstring|AMdouble', 'DATA_TYPE'),
            (r'AMfor|AMwhile|AMdo|startIf|otherIf|otherwise|shift|case|break|default', 'KEYWORD'),
            (r'Static|Dynamic|Const', 'TYPE_CAST_OPERATOR'),
            
            # Literals
            (r'[a-zA-Z_][a-zA-Z0-9_]*', 'IDENTIFIER'),
            (r'[0-9]+', 'INTEGER_LITERAL'),
            (r'"(?:\\.|[^"\\])*"', 'STRING_LITERAL'), # Handles escaped quotes

            # Operators (Combined patterns to avoid redefinition error. Longest alts first.)
            (r'==|!=|<=|>=|[<>]', 'COMPARISON_OPERATOR'),
            (r'\+\+|--', 'INCREMENT_DECREMENT_OPERATOR'),
            (r'\*\*|%%|[\+\-\*/]', 'ARITHMETIC_OPERATOR'),
            (r'=', 'ASSIGNMENT_OPERATOR'),
            (r'[@&|]', 'POINTER_OPERATOR'),

            # Delimiters and Brackets
            (r'\(', 'BRACKET_OPEN_PAREN'),
            (r'\)', 'BRACKET_CLOSE_PAREN'),
            (r'\{', 'BRACKET_OPEN_CURLY'),
            (r'\}', 'BRACKET_CLOSE_CURLY'),
            (r'\[', 'BRACKET_OPEN_SQUARE'),
            (r'\]', 'BRACKET_CLOSE_SQUARE'),
            (r';', 'DELIMITER'),
            (r',', 'SEPARATOR'),

            # Fallback for any other character
            (r'.', 'UNKNOWN')
        ]
        # Compile the master regex
        self.token_regex = '|'.join(f'(?P<{name}>{pattern})' for pattern, name in self.token_patterns)
        self.master_pattern = re.compile(self.token_regex)
        self.symbol_table = {}

    def tokenize(self, code):
        line_num = 1
        line_start = 0

        for mo in self.master_pattern.finditer(code):
            kind = mo.lastgroup
            value = mo.group(kind)
            column = mo.start() - line_start

            if kind in ('COMMENT', 'MULTI_LINE_COMMENT'):
                if '\n' in value:
                    line_num += value.count('\n')
                    line_start = mo.end() - value.rfind('\n') -1
                continue
            
            if kind == 'WHITESPACE':
                if '\n' in value:
                    line_num += value.count('\n')
                    line_start = mo.end() - value.rfind('\n') - 1
                continue

            if kind == 'UNKNOWN':
                print(f"Lexical Error: Unrecognized symbol '{value}' at Line {line_num}, Column {column + 1}")
                continue

            if kind == 'IDENTIFIER':
                if value not in self.symbol_table:
                    self.symbol_table[value] = {'type': None, 'line': line_num, 'column': column + 1}
            
            yield Token(kind, value, line_num, column + 1)

    def print_symbol_table(self):
        print("\nSymbol Table:")
        if not self.symbol_table:
            print("  (empty)")
            return
        for identifier, info in self.symbol_table.items():
            print(f"  Identifier: {identifier}, Type: {info['type']}, Line: {info['line']}, Column: {info['column']}")

def main():
    print("Choose an option:")
    print("1. Read source code from a file.")
    print("2. Enter AMCoder code manually (type 'END' to finish).")
    input_method = input("Enter your choice (1 or 2): ").strip()

    lexer = Lexer()

    if input_method == '1':
        filename = input("Enter the filename of the source code: ").strip()
        try:
            with open(filename, "r") as file:
                code = file.read()
        except FileNotFoundError:
            print(f"Error: File '{filename}' not found.")
            return
    elif input_method == '2':
        print("Enter AMCoder code (type 'END' on a new line to finish):")
        code_lines = []
        while True:
            line = input()
            if line.strip().upper() == 'END':
                break
            code_lines.append(line)
        code = '\n'.join(code_lines)
    else:
        print("Invalid option. Please select 1 or 2.")
        return

    print("\nTokens:")
    tokens = list(lexer.tokenize(code))
    for token in tokens:
        print(f"TOKEN: {token.value:<15} TYPE: {token.type}")

    lexer.print_symbol_table()

if __name__ == "__main__":
    main()
