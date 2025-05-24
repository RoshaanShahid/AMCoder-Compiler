import re
from Lexical_Analyzer import Token, Lexer

class SyntaxError(Exception):
    def __init__(self, message, token):
        self.message = message
        self.token = token
        super().__init__(f"Syntax Error at Line {token.line}, Column {token.column}: {message} (Found '{token.value}', Type: {token.type})")

class Node:
    def __init__(self, node_type, value=None, children=None, token=None):
        self.node_type = node_type
        self.value = value
        self.children = children if children is not None else []
        self.token = token

    def __repr__(self):
        if self.value is not None:
            return f"<{self.node_type}: '{self.value}'>"
        return f"<{self.node_type}>"

    def print_ast(self, indent=0):
        prefix = "  " * indent
        print(f"{prefix}{self}")
        for child in self.children:
            child.print_ast(indent + 1)

class Parser:
    def __init__(self, tokens):
        self.tokens = list(tokens)
        self.pos = 0
        self.errors = []
        self.synchronize_tokens = {
            'DELIMITER', 'BRACKET_OPEN_CURLY', 'BRACKET_CLOSE_CURLY',
            'TYPE', 'KEYWORD', 'IDENTIFIER', 'TERMINATOR'
        }

    def current_token(self):
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        last_token_line = self.tokens[-1].line if self.tokens else 1
        last_token_col = (self.tokens[-1].column + len(self.tokens[-1].value)) if self.tokens else 0
        return Token('EOF', '', last_token_line, last_token_col)

    def advance(self):
        if self.pos < len(self.tokens):
            self.pos += 1
        return self.previous_token()

    def previous_token(self):
        return self.tokens[self.pos - 1] if self.pos > 0 else None

    def match(self, *token_types):
        if self.current_token().type in token_types:
            self.advance()
            return True
        return False

    def expect(self, *token_types):
        current = self.current_token()
        if current.type in token_types:
            self.advance()
            return current
        else:
            expected_str = ", ".join(token_types)
            error_msg = f"Expected one of [{expected_str}], but found '{current.value}' (Type: {current.type})"
            self.errors.append(SyntaxError(error_msg, current))
            self.synchronize()
            return None

    def synchronize(self):
        while self.current_token().type != 'EOF':
            if self.previous_token() and self.previous_token().type == 'DELIMITER':
                return
            if self.current_token().type in self.synchronize_tokens:
                return
            self.advance()

    def parse(self):
        program_node = Node('PROGRAM')
        while self.current_token().type != 'EOF':
            try:
                statement = self.parse_statement()
                if statement:
                    program_node.children.append(statement)
            except Exception as e:
                self.errors.append(SyntaxError(f"Unexpected internal error during parsing: {e}", self.current_token()))
                self.synchronize()
        return program_node

    def parse_statement(self):
        token_type = self.current_token().type
        token_value = self.current_token().value

        if token_type == 'TYPE':
            return self.parse_declaration()
        elif token_type == 'IDENTIFIER':
            if self.pos + 1 < len(self.tokens) and self.tokens[self.pos + 1].type == 'ASSIGNMENT_OPERATOR':
                return self.parse_assignment()
            else:
                return self.parse_expression_statement()
        elif token_type == 'KEYWORD':
            if token_value == 'startIf':
                return self.parse_if_statement()
            elif token_value == 'AMfor':
                return self.parse_for_loop()
            elif token_value == 'AMwhile':
                return self.parse_while_loop()
            elif token_value == 'AMdo':
                return self.parse_do_while_loop()
            elif token_value == 'shift':
                return self.parse_shift_statement()
            else:
                self.errors.append(SyntaxError(f"Unexpected keyword '{token_value}' at start of statement.", self.current_token()))
                self.advance()
                self.synchronize()
                return None
        elif token_type == 'BRACKET_OPEN_CURLY':
            return self.parse_block()
        elif token_type == 'DELIMITER':
            term_token = self.advance()
            return Node('EMPTY_STATEMENT', token=term_token)
        elif token_type == 'TERMINATOR':
            term_token = self.advance()
            return Node('EMPTY_STATEMENT', token=term_token)
        elif token_type == 'EOF':
            return None
        else:
            self.errors.append(SyntaxError(f"Unexpected token at start of statement: '{self.current_token().value}'", self.current_token()))
            self.advance()
            self.synchronize()
            return None

    def parse_declaration(self):
        type_token = self.expect('TYPE')
        if not type_token: return None

        identifier_token = self.expect('IDENTIFIER')
        if not identifier_token: return None

        delimiter_token = self.expect('DELIMITER')
        if not delimiter_token: return None

        return Node('DECLARATION', token=type_token, children=[
            Node('TYPE', type_token.value, token=type_token),
            Node('IDENTIFIER', identifier_token.value, token=identifier_token)
        ])

    def parse_assignment(self):
        identifier_token = self.expect('IDENTIFIER')
        if not identifier_token: return None

        assign_op_token = self.expect('ASSIGNMENT_OPERATOR')
        if not assign_op_token: return None

        expression_node = self.parse_expression()
        if not expression_node: return None

        delimiter_token = self.expect('DELIMITER')
        if not delimiter_token: return None

        return Node('ASSIGNMENT', token=assign_op_token, children=[
            Node('IDENTIFIER', identifier_token.value, token=identifier_token),
            expression_node
        ])

    def parse_expression_statement(self):
        expr_node = self.parse_expression()
        if not expr_node: return None

        delimiter_token = self.expect('DELIMITER')
        if not delimiter_token: return None

        return Node('EXPRESSION_STATEMENT', token=expr_node.token, children=[expr_node])

    def parse_expression(self):
        return self.parse_logical()

    def parse_logical(self):
        node = self.parse_comparison()
        if not node: return None

        while self.current_token().type == 'LOGICAL_OPERATOR':
            op_token = self.advance()
            right_node = self.parse_comparison()
            if not right_node: return None
            node = Node('BINARY_EXPR', op_token.value, token=op_token, children=[node, right_node])
        return node

    def parse_comparison(self):
        node = self.parse_arithmetic()
        if not node: return None

        while self.current_token().type == 'COMPARISON_OPERATOR':
            op_token = self.advance()
            right_node = self.parse_arithmetic()
            if not right_node: return None
            node = Node('BINARY_EXPR', op_token.value, token=op_token, children=[node, right_node])
        return node

    def parse_arithmetic(self):
        node = self.parse_factor()
        if not node: return None

        while self.current_token().type == 'ARITHMETIC_OPERATOR' and \
              self.current_token().value not in ('++', '--', '**', '%%'):
            op_token = self.advance()
            right_node = self.parse_factor()
            if not right_node: return None
            node = Node('BINARY_EXPR', op_token.value, token=op_token, children=[node, right_node])
        return node

    def parse_factor(self):
        token = self.current_token()

        if token.type == 'TYPE_CAST_OPERATOR':
            op_token = self.advance()
            expr_node = self.parse_factor()
            if not expr_node: return None
            return Node('UNARY_PREFIX_EXPR', op_token.value, token=op_token, children=[expr_node])
        elif token.type == 'POINTER_OPERATOR':
            op_token = self.advance()
            identifier_token = self.expect('IDENTIFIER')
            if not identifier_token: return None
            return Node('POINTER_EXPR', op_token.value, token=op_token, children=[Node('IDENTIFIER', identifier_token.value, token=identifier_token)])

        elif token.type == 'INTEGER_LITERAL':
            self.advance()
            return Node('INTEGER_LITERAL', token.value, token=token)
        elif token.type == 'STRING_LITERAL':
            self.advance()
            return Node('STRING_LITERAL', token.value[1:-1], token=token)
        elif token.type == 'IDENTIFIER':
            self.advance()
            identifier_node = Node('IDENTIFIER', token.value, token=token)
            if self.current_token().type == 'INCREMENT_DECREMENT_OPERATOR':
                op_token = self.advance()
                return Node('UNARY_POSTFIX_EXPR', op_token.value, token=op_token, children=[identifier_node])
            elif self.current_token().type == 'ARITHMETIC_OPERATOR' and \
                 self.current_token().value in ('++', '--', '**', '%%'):
                op_token = self.advance()
                return Node('UNARY_POSTFIX_EXPR', op_token.value, token=op_token, children=[identifier_node])
            return identifier_node
        elif token.type == 'BRACKET_OPEN_PAREN':
            self.advance()
            expr_node = self.parse_expression()
            if not expr_node: return None
            self.expect('BRACKET_CLOSE_PAREN')
            return Node('PARENTHESIZED_EXPR', token=token, children=[expr_node])
        else:
            self.errors.append(SyntaxError(f"Expected expression, but found '{token.value}'", token))
            self.advance()
            return None

    def parse_block(self):
        block_start_token = self.expect('BRACKET_OPEN_CURLY')
        if not block_start_token: return None

        statements = []
        while self.current_token().type != 'BRACKET_CLOSE_CURLY' and self.current_token().type != 'EOF':
            stmt = self.parse_statement()
            if stmt:
                statements.append(stmt)
            elif self.current_token().type not in ('BRACKET_CLOSE_CURLY', 'EOF'):
                self.advance()

        block_end_token = self.expect('BRACKET_CLOSE_CURLY')
        if not block_end_token: return None

        return Node('BLOCK', token=block_start_token, children=statements)

    def parse_if_statement(self):
        if_token = self.expect_keyword('startIf')
        if not if_token: return None

        self.expect('BRACKET_OPEN_PAREN')
        condition = self.parse_expression()
        if not condition: return None
        self.expect('BRACKET_CLOSE_PAREN')

        if_block = self.parse_block()
        if not if_block: return None

        if_node = Node('IF_STATEMENT', token=if_token, children=[condition, if_block])

        while self.current_token().type == 'KEYWORD' and self.current_token().value == 'otherIf':
            self.advance()
            self.expect('BRACKET_OPEN_PAREN')
            else_if_condition = self.parse_expression()
            if not else_if_condition: break
            self.expect('BRACKET_CLOSE_PAREN')
            else_if_block = self.parse_block()
            if not else_if_block: break
            if_node.children.append(Node('ELSE_IF_CLAUSE', children=[else_if_condition, else_if_block]))

        if self.current_token().type == 'KEYWORD' and self.current_token().value == 'otherwise':
            self.advance()
            else_block = self.parse_block()
            if not else_block: return None
            if_node.children.append(Node('ELSE_CLAUSE', children=[else_block]))

        return if_node

    def parse_for_loop(self):
        for_token = self.expect_keyword('AMfor')
        if not for_token: return None

        self.expect('BRACKET_OPEN_PAREN')

        init_node = Node('EMPTY_INIT', token=self.current_token())
        if self.current_token().type == 'TYPE':
            init_node = self.parse_declaration()
        elif self.current_token().type == 'IDENTIFIER' and \
             self.pos + 1 < len(self.tokens) and \
             self.tokens[self.pos + 1].type == 'ASSIGNMENT_OPERATOR':
            init_node = self.parse_assignment()
        else:
            self.expect('DELIMITER')

        condition_node = self.parse_expression()
        if not condition_node: return None
        self.expect('DELIMITER')

        increment_node = Node('EMPTY_INCREMENT', token=self.current_token())
        if self.current_token().type != 'BRACKET_CLOSE_PAREN':
            if self.current_token().type == 'IDENTIFIER' and \
               self.pos + 1 < len(self.tokens) and \
               self.tokens[self.pos + 1].type == 'ASSIGNMENT_OPERATOR':
                increment_node = self.parse_assignment()
            else:
                increment_node = self.parse_expression()

        self.expect('BRACKET_CLOSE_PAREN')

        loop_body = self.parse_block()
        if not loop_body: return None

        return Node('FOR_LOOP', token=for_token, children=[
            init_node, condition_node, increment_node, loop_body
        ])

    def parse_while_loop(self):
        while_token = self.expect_keyword('AMwhile')
        if not while_token: return None

        self.expect('BRACKET_OPEN_PAREN')
        condition = self.parse_expression()
        if not condition: return None
        self.expect('BRACKET_CLOSE_PAREN')

        loop_body = self.parse_block()
        if not loop_body: return None

        return Node('WHILE_LOOP', token=while_token, children=[condition, loop_body])

    def parse_do_while_loop(self):
        do_token = self.expect_keyword('AMdo')
        if not do_token: return None

        loop_body = self.parse_block()
        if not loop_body: return None

        self.expect_keyword('AMwhile')
        self.expect('BRACKET_OPEN_PAREN')
        condition = self.parse_expression()
        if not condition: return None
        self.expect('BRACKET_CLOSE_PAREN')
        self.expect('DELIMITER')

        return Node('DO_WHILE_LOOP', token=do_token, children=[loop_body, condition])

    def parse_shift_statement(self):
        shift_token = self.expect_keyword('shift')
        if not shift_token: return None

        self.expect('BRACKET_OPEN_PAREN')
        expression = self.parse_expression()
        if not expression: return None
        self.expect('BRACKET_CLOSE_PAREN')

        self.expect('BRACKET_OPEN_CURLY')

        cases = []
        default_case = None

        while self.current_token().type != 'BRACKET_CLOSE_CURLY' and self.current_token().type != 'EOF':
            if self.current_token().value == 'case':
                case_token = self.advance()
                case_expr = self.parse_expression()
                if not case_expr: break
                self.expect('DELIMITER')
                case_block = self.parse_block()
                if not case_block: break
                self.expect_keyword('break')
                self.expect('DELIMITER')
                cases.append(Node('CASE_CLAUSE', token=case_token, children=[case_expr, case_block]))
            elif self.current_token().value == 'default':
                default_token = self.advance()
                self.expect('DELIMITER')
                default_block = self.parse_block()
                if not default_block: break
                self.expect_keyword('break')
                self.expect('DELIMITER')
                default_case = Node('DEFAULT_CLAUSE', token=default_token, children=[default_block])
            else:
                self.errors.append(SyntaxError(f"Expected 'case' or 'default' inside 'shift' statement, but found '{self.current_token().value}'", self.current_token()))
                self.advance()
                self.synchronize()

        self.expect('BRACKET_CLOSE_CURLY')

        shift_node = Node('SHIFT_STATEMENT', token=shift_token, children=[expression] + cases)
        if default_case:
            shift_node.children.append(default_case)
        return shift_node

    def expect_keyword(self, keyword_value):
        token = self.current_token()
        if token.type == 'KEYWORD' and token.value == keyword_value:
            self.advance()
            return token
        else:
            error_msg = f"Expected keyword '{keyword_value}', but found '{token.value}' (Type: {token.type})"
            self.errors.append(SyntaxError(error_msg, token))
            self.synchronize()
            return None


def read_source_code(filename):
    try:
        with open(filename, "r") as file:
            return file.read()
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.")
        return None

def main():
    print("--- AMCoder Compiler (Lexical and Syntax Analyzer) ---")
    print("Choose an option:")
    print("1. Read source code from a file.")
    print("2. Enter AMCoder code manually (type 'END' to finish).")
    input_method = input("Enter your choice (1 or 2): ").strip()

    code = ""
    if input_method == '1':
        filename = input("Enter the filename of the source code: ").strip()
        code = read_source_code(filename)
        if code is None:
            return
    elif input_method == '2':
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

    lexer = Lexer()
    print("\n--- Lexical Analysis ---")
    print("Tokens:")
    tokens_stream = list(lexer.tokenize(code))
    for token in tokens_stream:
        print(token)
    lexer.print_symbol_table()

    print("\n--- Syntax Analysis ---")
    parser = Parser(tokens_stream)
    ast = parser.parse()

    if not parser.errors:
        print("Parsing completed successfully!")
        print("\nAbstract Syntax Tree (AST):")
        if ast and ast.children:
            ast.print_ast()
        else:
            print("  (Empty AST - no statements parsed or only comments/whitespace)")
    else:
        print("Parsing completed with errors:")
        for error in parser.errors:
            print(error)
        print("\nAST (partial, due to errors):")
        if ast and ast.children:
            ast.print_ast()
        else:
            print("  (No AST generated or very partial due to early errors)")


if __name__ == "__main__":
    main()