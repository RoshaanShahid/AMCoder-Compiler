import re
# Import Token and Lexer from your separate lexical analyzer file
from Lexical_Analyzer import Token, Lexer

# --- Syntax Analyzer (Parser) ---

class SyntaxError(Exception):
    """Custom exception for syntax errors, including location and message."""
    def __init__(self, message, token):
        self.message = message
        self.token = token
        super().__init__(f"Syntax Error at Line {token.line}, Column {token.column}: {message} (Found '{token.value}', Type: {token.type})")

class Node:
    """
    Represents a node in the Abstract Syntax Tree (AST).
    Each node has a type, an optional value, a list of child nodes,
    and a reference to the token that generated it for better error context.
    """
    def __init__(self, node_type, value=None, children=None, token=None):
        self.node_type = node_type
        self.value = value
        self.children = children if children is not None else []
        self.token = token # Store the token that generated this node for better error reporting

    def __repr__(self):
        # String representation for debugging and printing AST nodes
        if self.value is not None:
            return f"<{self.node_type}: '{self.value}'>"
        return f"<{self.node_type}>"

    def print_ast(self, indent=0):
        """Recursively prints the AST structure with indentation."""
        prefix = "  " * indent
        print(f"{prefix}{self}")
        for child in self.children:
            child.print_ast(indent + 1)

class Parser:
    """
    Parses a stream of tokens to build an Abstract Syntax Tree (AST).
    Implements a recursive descent parser with panic-mode error recovery.
    """
    def __init__(self, tokens):
        self.tokens = list(tokens) # Ensure tokens are a list for indexing
        self.pos = 0 # Current position in the token list
        self.errors = [] # List to collect all syntax errors

        # Define synchronizing tokens for error recovery
        # These are tokens that typically mark the start or end of a statement or block
        self.synchronize_tokens = {
            'DELIMITER', 'BRACKET_OPEN_CURLY', 'BRACKET_CLOSE_CURLY',
            'TYPE', 'KEYWORD', 'IDENTIFIER', 'TERMINATOR'
        }

    def current_token(self):
        """Returns the token at the current position, or a dummy EOF token if at end."""
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        # Return a dummy EOF token for consistent handling at the end of input
        # Provide line/column for EOF based on the last token or default to 1,0
        last_token_line = self.tokens[-1].line if self.tokens else 1
        last_token_col = (self.tokens[-1].column + len(self.tokens[-1].value)) if self.tokens else 0
        return Token('EOF', '', last_token_line, last_token_col)

    def advance(self):
        """Consumes the current token and moves to the next."""
        if self.pos < len(self.tokens):
            self.pos += 1
        return self.previous_token()

    def previous_token(self):
        """Returns the token that was just consumed."""
        return self.tokens[self.pos - 1] if self.pos > 0 else None

    def match(self, *token_types):
        """
        Checks if the current token's type matches any of the given types.
        If it matches, consumes the token and returns True. Otherwise, False.
        """
        if self.current_token().type in token_types:
            self.advance()
            return True
        return False

    def expect(self, *token_types):
        """
        Expects the current token to be one of the given types.
        If it matches, consumes the token and returns it.
        If not, records a SyntaxError and attempts panic-mode recovery.
        """
        current = self.current_token()
        if current.type in token_types:
            self.advance()
            return current
        else:
            expected_str = ", ".join(token_types)
            error_msg = f"Expected one of [{expected_str}], but found '{current.value}' (Type: {current.type})"
            self.errors.append(SyntaxError(error_msg, current))
            self.synchronize() # Attempt error recovery
            return None # Indicate that the expected token was not found

    def synchronize(self):
        """
        Implements panic-mode error recovery.
        Skips tokens until a synchronizing token is found, or EOF.
        """
        while self.current_token().type != 'EOF':
            # If the previous token was a delimiter, we might be at the end of a statement
            if self.previous_token() and self.previous_token().type == 'DELIMITER':
                return
            # If the current token is a synchronizing token, we can try to resume parsing
            if self.current_token().type in self.synchronize_tokens:
                return
            self.advance() # Skip token

    def parse(self):
        """
        Main parsing method. Parses the entire program (sequence of statements).
        Returns a single AST node representing the program.
        """
        program_node = Node('PROGRAM')
        while self.current_token().type != 'EOF':
            # Use a try-except block to catch SyntaxErrors for graceful recovery
            try:
                statement = self.parse_statement()
                if statement:
                    program_node.children.append(statement)
                # If parse_statement returns None (e.g., after error recovery),
                # we still need to ensure progress to avoid infinite loop on bad tokens.
                # The synchronize() in expect() should usually handle this by advancing.
            except Exception as e:
                # Catch any unexpected errors that might not be SyntaxErrors
                self.errors.append(SyntaxError(f"Unexpected internal error during parsing: {e}", self.current_token()))
                self.synchronize() # Attempt to recover and continue parsing

        return program_node

    def parse_statement(self):
        """Parses a single statement based on the current token."""
        token_type = self.current_token().type
        token_value = self.current_token().value

        if token_type == 'TYPE':
            return self.parse_declaration()
        elif token_type == 'IDENTIFIER':
            # Could be assignment or expression statement
            # Look ahead to differentiate
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
            # Add other keywords if they start statements (e.g., 'shift' for switch)
            # For 'shift', assuming it's a switch-like statement
            elif token_value == 'shift':
                return self.parse_shift_statement()
            else:
                # Unexpected keyword, treat as error and synchronize
                self.errors.append(SyntaxError(f"Unexpected keyword '{token_value}' at start of statement.", self.current_token()))
                self.advance() # Consume the unexpected keyword
                self.synchronize()
                return None
        elif token_type == 'BRACKET_OPEN_CURLY':
            return self.parse_block()
        elif token_type == 'DELIMITER': # Handle empty statements like just ';'
            term_token = self.advance()
            return Node('EMPTY_STATEMENT', token=term_token)
        elif token_type == 'TERMINATOR': # Handle AMCoder's custom terminator '&' as an empty statement
            term_token = self.advance()
            return Node('EMPTY_STATEMENT', token=term_token)
        elif token_type == 'EOF':
            return None # End of file, no more statements

        else:
            # Unrecognized start of a statement
            self.errors.append(SyntaxError(f"Unexpected token at start of statement: '{self.current_token().value}'", self.current_token()))
            self.advance() # Consume the bad token
            self.synchronize() # Attempt to recover
            return None

    def parse_declaration(self):
        """Parses a variable declaration: TYPE IDENTIFIER DELIMITER"""
        type_token = self.expect('TYPE')
        if not type_token: return None # Error already reported and recovered

        identifier_token = self.expect('IDENTIFIER')
        if not identifier_token: return None

        delimiter_token = self.expect('DELIMITER')
        if not delimiter_token: return None

        return Node('DECLARATION', token=type_token, children=[
            Node('TYPE', type_token.value, token=type_token),
            Node('IDENTIFIER', identifier_token.value, token=identifier_token)
        ])

    def parse_assignment(self):
        """Parses an assignment: IDENTIFIER ASSIGNMENT_OPERATOR Expression DELIMITER"""
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
        """Parses an expression followed by a delimiter, e.g., `x++;`"""
        expr_node = self.parse_expression()
        if not expr_node: return None

        delimiter_token = self.expect('DELIMITER')
        if not delimiter_token: return None

        return Node('EXPRESSION_STATEMENT', token=expr_node.token, children=[expr_node])

    def parse_expression(self):
        """
        Parses an expression.
        This implementation uses a simple recursive descent for binary operations
        with a basic level of operator precedence (comparison/logical lowest, then arithmetic, then factor).
        """
        return self.parse_logical()

    def parse_logical(self):
        """Parses logical expressions (//, \\\\, ??)."""
        node = self.parse_comparison()
        if not node: return None

        while self.current_token().type == 'LOGICAL_OPERATOR':
            op_token = self.advance()
            right_node = self.parse_comparison()
            if not right_node: return None
            node = Node('BINARY_EXPR', op_token.value, token=op_token, children=[node, right_node])
        return node

    def parse_comparison(self):
        """Parses comparison expressions (-, ?, ^, ~, ^-, ~-)."""
        node = self.parse_arithmetic()
        if not node: return None

        while self.current_token().type == 'COMPARISON_OPERATOR':
            op_token = self.advance()
            right_node = self.parse_arithmetic()
            if not right_node: return None
            node = Node('BINARY_EXPR', op_token.value, token=op_token, children=[node, right_node])
        return node

    def parse_arithmetic(self):
        """Parses arithmetic expressions (++, --, **, %%, +, -, *, /)."""
        node = self.parse_factor()
        if not node: return None

        # Handle basic arithmetic operators (+, -, *, /)
        while self.current_token().type == 'ARITHMETIC_OPERATOR' and \
              self.current_token().value not in ('++', '--', '**', '%%'): # Exclude custom ops handled by factor
            op_token = self.advance()
            right_node = self.parse_factor()
            if not right_node: return None
            node = Node('BINARY_EXPR', op_token.value, token=op_token, children=[node, right_node])
        return node

    def parse_factor(self):
        """Parses the most basic elements of an expression: literals, identifiers, parenthesized expressions, unary ops."""
        token = self.current_token()

        # Unary operators (prefix) or type cast
        if token.type == 'TYPE_CAST_OPERATOR':
            op_token = self.advance()
            expr_node = self.parse_factor() # Recurse for the operand
            if not expr_node: return None
            return Node('UNARY_PREFIX_EXPR', op_token.value, token=op_token, children=[expr_node])
        elif token.type == 'POINTER_OPERATOR':
            op_token = self.advance()
            identifier_token = self.expect('IDENTIFIER')
            if not identifier_token: return None
            return Node('POINTER_EXPR', op_token.value, token=op_token, children=[Node('IDENTIFIER', identifier_token.value, token=identifier_token)])

        # Literals
        elif token.type == 'INTEGER_LITERAL':
            self.advance()
            return Node('INTEGER_LITERAL', token.value, token=token)
        elif token.type == 'STRING_LITERAL':
            self.advance()
            # Remove quotes from string literal value for AST
            return Node('STRING_LITERAL', token.value[1:-1], token=token)
        elif token.type == 'IDENTIFIER':
            self.advance()
            identifier_node = Node('IDENTIFIER', token.value, token=token)
            # Check for postfix increment/decrement
            if self.current_token().type == 'INCREMENT_DECREMENT_OPERATOR':
                op_token = self.advance()
                return Node('UNARY_POSTFIX_EXPR', op_token.value, token=op_token, children=[identifier_node])
            # Check for other custom arithmetic operators as postfix
            elif self.current_token().type == 'ARITHMETIC_OPERATOR' and \
                 self.current_token().value in ('++', '--', '**', '%%'):
                op_token = self.advance()
                return Node('UNARY_POSTFIX_EXPR', op_token.value, token=op_token, children=[identifier_node])
            return identifier_node
        elif token.type == 'BRACKET_OPEN_PAREN':
            self.advance() # Consume '('
            expr_node = self.parse_expression()
            if not expr_node: return None
            self.expect('BRACKET_CLOSE_PAREN') # Expect ')'
            return Node('PARENTHESIZED_EXPR', token=token, children=[expr_node])
        else:
            self.errors.append(SyntaxError(f"Expected expression, but found '{token.value}'", token))
            self.advance() # Consume the unexpected token for recovery
            return None

    def parse_block(self):
        """Parses a block of statements enclosed in curly braces: { Statement* }"""
        block_start_token = self.expect('BRACKET_OPEN_CURLY')
        if not block_start_token: return None

        statements = []
        while self.current_token().type != 'BRACKET_CLOSE_CURLY' and self.current_token().type != 'EOF':
            stmt = self.parse_statement()
            if stmt:
                statements.append(stmt)
            # If parse_statement failed and synchronized, it would have advanced past the bad token.
            # If it didn't advance, and we are not at the end of block/file,
            # it means there's an unparsed token, so advance to prevent infinite loop.
            elif self.current_token().type not in ('BRACKET_CLOSE_CURLY', 'EOF'):
                self.advance() # Skip token if a statement wasn't parsed

        block_end_token = self.expect('BRACKET_CLOSE_CURLY')
        if not block_end_token: return None

        return Node('BLOCK', token=block_start_token, children=statements)

    def parse_if_statement(self):
        """
        Parses an if-else if-else statement:
        startIf ( Expression ) Block (otherIf ( Expression ) Block)* (otherwise Block)?
        """
        if_token = self.expect_keyword('startIf')
        if not if_token: return None

        self.expect('BRACKET_OPEN_PAREN')
        condition = self.parse_expression()
        if not condition: return None
        self.expect('BRACKET_CLOSE_PAREN')

        if_block = self.parse_block()
        if not if_block: return None

        if_node = Node('IF_STATEMENT', token=if_token, children=[condition, if_block])

        # Parse 'otherIf' (else if) clauses
        while self.current_token().type == 'KEYWORD' and self.current_token().value == 'otherIf':
            self.advance() # Consume 'otherIf'
            self.expect('BRACKET_OPEN_PAREN')
            else_if_condition = self.parse_expression()
            if not else_if_condition: break # Error in condition, break loop
            self.expect('BRACKET_CLOSE_PAREN')
            else_if_block = self.parse_block()
            if not else_if_block: break # Error in block, break loop
            if_node.children.append(Node('ELSE_IF_CLAUSE', children=[else_if_condition, else_if_block]))

        # Parse 'otherwise' (else) clause
        if self.current_token().type == 'KEYWORD' and self.current_token().value == 'otherwise':
            self.advance() # Consume 'otherwise'
            else_block = self.parse_block()
            if not else_block: return None # Error in block
            if_node.children.append(Node('ELSE_CLAUSE', children=[else_block]))

        return if_node

    def parse_for_loop(self):
        """
        Parses a for loop:
        AMfor ( (Declaration | Assignment | EmptyStatement) Expression DELIMITER (Assignment | ExpressionStatement | EmptyStatement) ) Block
        """
        for_token = self.expect_keyword('AMfor')
        if not for_token: return None

        self.expect('BRACKET_OPEN_PAREN')

        # Initialization (can be declaration, assignment, or empty)
        init_node = Node('EMPTY_INIT', token=self.current_token()) # Default to empty
        if self.current_token().type == 'TYPE':
            init_node = self.parse_declaration()
        elif self.current_token().type == 'IDENTIFIER' and \
             self.pos + 1 < len(self.tokens) and \
             self.tokens[self.pos + 1].type == 'ASSIGNMENT_OPERATOR':
            init_node = self.parse_assignment()
        else:
            self.expect('DELIMITER') # Expect a delimiter if init is empty or not handled

        # Condition
        condition_node = self.parse_expression()
        if not condition_node: return None
        self.expect('DELIMITER')

        # Increment/Decrement (can be assignment, expression statement, or empty)
        increment_node = Node('EMPTY_INCREMENT', token=self.current_token()) # Default to empty
        # Check if there's an expression/assignment before the closing paren
        if self.current_token().type != 'BRACKET_CLOSE_PAREN':
            if self.current_token().type == 'IDENTIFIER' and \
               self.pos + 1 < len(self.tokens) and \
               self.tokens[self.pos + 1].type == 'ASSIGNMENT_OPERATOR':
                increment_node = self.parse_assignment()
            else:
                increment_node = self.parse_expression() # Just an expression, e.g., i++

        self.expect('BRACKET_CLOSE_PAREN')

        loop_body = self.parse_block()
        if not loop_body: return None

        return Node('FOR_LOOP', token=for_token, children=[
            init_node, condition_node, increment_node, loop_body
        ])

    def parse_while_loop(self):
        """Parses a while loop: AMwhile ( Expression ) Block"""
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
        """Parses a do-while loop: AMdo Block AMwhile ( Expression ) DELIMITER"""
        do_token = self.expect_keyword('AMdo')
        if not do_token: return None

        loop_body = self.parse_block()
        if not loop_body: return None

        self.expect_keyword('AMwhile') # Expect 'AMwhile' keyword
        self.expect('BRACKET_OPEN_PAREN')
        condition = self.parse_expression()
        if not condition: return None
        self.expect('BRACKET_CLOSE_PAREN')
        self.expect('DELIMITER') # Do-while requires a delimiter after condition

        return Node('DO_WHILE_LOOP', token=do_token, children=[loop_body, condition])

    def parse_shift_statement(self):
        """
        Parses a 'shift' (switch-like) statement.
        Assuming a structure like: shift (Expression) { case Expression: Block break; ... default: Block break; }
        This is a simplified example.
        """
        shift_token = self.expect_keyword('shift')
        if not shift_token: return None

        self.expect('BRACKET_OPEN_PAREN')
        expression = self.parse_expression()
        if not expression: return None
        self.expect('BRACKET_CLOSE_PAREN')

        self.expect('BRACKET_OPEN_CURLY') # Start of switch body

        cases = []
        default_case = None

        while self.current_token().type != 'BRACKET_CLOSE_CURLY' and self.current_token().type != 'EOF':
            if self.current_token().value == 'case':
                case_token = self.advance() # Consume 'case'
                case_expr = self.parse_expression() # The value to match
                if not case_expr: break
                self.expect('DELIMITER') # Assuming ':' is represented as DELIMITER
                case_block = self.parse_block() # The code block for this case
                if not case_block: break
                self.expect_keyword('break') # Assuming 'break' is a keyword
                self.expect('DELIMITER') # After break;
                cases.append(Node('CASE_CLAUSE', token=case_token, children=[case_expr, case_block]))
            elif self.current_token().value == 'default':
                default_token = self.advance() # Consume 'default'
                self.expect('DELIMITER') # Assuming ':' is represented as DELIMITER
                default_block = self.parse_block()
                if not default_block: break
                self.expect_keyword('break')
                self.expect('DELIMITER')
                default_case = Node('DEFAULT_CLAUSE', token=default_token, children=[default_block])
            else:
                self.errors.append(SyntaxError(f"Expected 'case' or 'default' inside 'shift' statement, but found '{self.current_token().value}'", self.current_token()))
                self.advance() # Skip bad token
                self.synchronize()

        self.expect('BRACKET_CLOSE_CURLY') # End of switch body

        shift_node = Node('SHIFT_STATEMENT', token=shift_token, children=[expression] + cases)
        if default_case:
            shift_node.children.append(default_case)
        return shift_node

    def expect_keyword(self, keyword_value):
        """Helper to expect a specific keyword."""
        token = self.current_token()
        if token.type == 'KEYWORD' and token.value == keyword_value:
            self.advance()
            return token
        else:
            error_msg = f"Expected keyword '{keyword_value}', but found '{token.value}' (Type: {token.type})"
            self.errors.append(SyntaxError(error_msg, token))
            self.synchronize()
            return None


# --- Main Execution Logic ---

def read_source_code(filename):
    """Reads source code from a file."""
    try:
        with open(filename, "r") as file:
            return file.read()
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.")
        return None

def main():
    """Main function to run the lexical and syntax analysis."""
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

    # --- Lexical Analysis ---
    # Instantiate the Lexer from Lexical_Analyzer.py
    lexer = Lexer()
    print("\n--- Lexical Analysis ---")
    print("Tokens:")
    # Get the token stream from the lexer
    tokens_stream = list(lexer.tokenize(code)) # Convert generator to list for parser
    for token in tokens_stream:
        print(token)
    lexer.print_symbol_table()

    # --- Syntax Analysis ---
    print("\n--- Syntax Analysis ---")
    parser = Parser(tokens_stream)
    ast = parser.parse() # Start parsing

    if not parser.errors:
        print("Parsing completed successfully!")
        print("\nAbstract Syntax Tree (AST):")
        if ast and ast.children: # Only print AST if it contains statements
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
