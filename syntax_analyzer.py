import re
import sys
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
            if isinstance(child, Node):
                child.print_ast(indent + 1)
            else:
                print(f"{prefix}  - Invalid child node: {child}")


# The Parser class implements a Recursive Descent Parser.
# Each `parse_...` method corresponds to a rule in the language's grammar.
# The grammar is defined by how these methods call each other.
class Parser:
    def __init__(self, tokens):
        self.tokens = list(tokens)
        self.pos = 0
        self.errors = []
        # FIX: Changed 'TYPE' to 'DATA_TYPE' for error recovery
        self.synchronize_tokens = {
            'DELIMITER', 'BRACKET_OPEN_CURLY', 'BRACKET_CLOSE_CURLY',
            'DATA_TYPE', 'KEYWORD', 'IDENTIFIER', 'TERMINATOR'
        }

    def current_token(self):
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        last_token_line = self.tokens[-1].line if self.tokens else 1
        val_len = len(str(self.tokens[-1].value)) if self.tokens and self.tokens[-1].value is not None else 0
        last_token_col = (self.tokens[-1].column + val_len) if self.tokens else 0
        return Token('EOF', '', last_token_line, last_token_col)

    def advance(self):
        if self.pos < len(self.tokens):
            self.pos += 1
        return self.previous_token()

    def previous_token(self):
        return self.tokens[self.pos - 1] if self.pos > 0 else None

    def expect(self, *token_types):
        current = self.current_token()
        if current.type in token_types:
            return self.advance()
        else:
            expected_str = ", ".join(token_types)
            error_msg = f"Expected one of [{expected_str}]"
            self.errors.append(SyntaxError(error_msg, current))
            return None

    def synchronize(self):
        self.advance() # Consume the token that caused the error
        while self.current_token().type != 'EOF':
            if self.previous_token() and self.previous_token().type == 'DELIMITER':
                return
            if self.current_token().type in self.synchronize_tokens:
                return
            self.advance()

    # Grammar rule: program ::= statement*
    # This is the top-level rule. A program is a sequence of zero or more statements.
    def parse(self):
        program_node = Node('PROGRAM')
        while self.current_token().type != 'EOF':
            try:
                if len(self.errors) > 10: # Safety break
                    self.errors.append(SyntaxError("Too many errors, aborting parse.", self.current_token()))
                    break
                statement = self.parse_statement()
                if statement:
                    program_node.children.append(statement)
                elif not self.errors and self.current_token().type != 'EOF':
                    self.synchronize() # Move past a token that couldn't form a statement
            except Exception as e:
                self.errors.append(SyntaxError(f"Unexpected internal parser error: {e}", self.current_token()))
                self.synchronize()
        return program_node

    # Grammar rule: statement ::= declaration | assignment | expression_statement | if_statement | block | ... | ';'
    # This method determines what kind of statement is next based on the current token.
    def parse_statement(self):
        token_type = self.current_token().type
        
        if token_type == 'DATA_TYPE':
            return self.parse_declaration()
        elif token_type == 'IDENTIFIER':
            if self.pos + 1 < len(self.tokens) and self.tokens[self.pos + 1].type == 'ASSIGNMENT_OPERATOR':
                return self.parse_assignment()
            else:
                return self.parse_expression_statement()
        elif token_type == 'KEYWORD':
            # FIX: Added handling for the 'AMwhile' keyword
            if self.current_token().value == 'startIf':
                return self.parse_if_statement()
            elif self.current_token().value == 'AMwhile':
                return self.parse_while_loop()
            # ... other keywords for other statements (for, etc.) would go here ...
            else:
                self.errors.append(SyntaxError(f"Keyword '{self.current_token().value}' cannot start a statement.", self.current_token()))
                self.synchronize()
                return None
        elif token_type == 'BRACKET_OPEN_CURLY':
            return self.parse_block()
        elif self.current_token().type == 'DELIMITER':
            self.advance()
            return Node('EMPTY_STATEMENT')
        else:
            if self.current_token().type != 'EOF':
                self.errors.append(SyntaxError("Unexpected token at start of statement", self.current_token()))
                self.synchronize()
            return None

    # Grammar rule: declaration ::= DATA_TYPE IDENTIFIER ';'
    def parse_declaration(self):
        type_token = self.expect('DATA_TYPE')
        if not type_token: return None

        identifier_token = self.expect('IDENTIFIER')
        if not identifier_token: return None

        if not self.expect('DELIMITER'): return None

        return Node('DECLARATION', token=type_token, children=[
            Node('DATA_TYPE', type_token.value, token=type_token),
            Node('IDENTIFIER', identifier_token.value, token=identifier_token)
        ])

    # Grammar rule: assignment ::= IDENTIFIER '=' expression ';'
    def parse_assignment(self):
        identifier_token = self.expect('IDENTIFIER')
        if not identifier_token: return None

        assign_op_token = self.expect('ASSIGNMENT_OPERATOR')
        if not assign_op_token: return None

        expression_node = self.parse_expression()
        if not expression_node:
            self.errors.append(SyntaxError("Missing expression after assignment operator", assign_op_token))
            return None

        if not self.expect('DELIMITER'): return None

        return Node('ASSIGNMENT', token=assign_op_token, children=[
            Node('IDENTIFIER', identifier_token.value, token=identifier_token),
            expression_node
        ])
        
    # Grammar rule: expression_statement ::= expression ';'
    def parse_expression_statement(self):
        expr_node = self.parse_expression()
        if not expr_node: return None
        if not self.expect('DELIMITER'): return None
        return Node('EXPRESSION_STATEMENT', children=[expr_node])

    # --- Expression Parsing with Precedence ---
    # Grammar rule: expression ::= additive_expr
    def parse_expression(self):
        return self.parse_additive_expr()

    # Grammar rule: additive_expr ::= multiplicative_expr (('+' | '-') multiplicative_expr)*
    def parse_additive_expr(self):
        node = self.parse_multiplicative_expr()
        while self.current_token().type == 'ARITHMETIC_OPERATOR' and self.current_token().value in ('+', '-'):
            op_token = self.advance()
            right_node = self.parse_multiplicative_expr()
            if not right_node:
                self.errors.append(SyntaxError("Missing expression after '+' or '-'", op_token))
                return None
            node = Node('BINARY_EXPR', op_token.value, token=op_token, children=[node, right_node])
        return node

    # Grammar rule: multiplicative_expr ::= primary_expr (('*' | '/') primary_expr)*
    def parse_multiplicative_expr(self):
        node = self.parse_primary_expr()
        while self.current_token().type == 'ARITHMETIC_OPERATOR' and self.current_token().value in ('*', '/'):
            op_token = self.advance()
            right_node = self.parse_primary_expr()
            if not right_node:
                self.errors.append(SyntaxError("Missing expression after '*' or '/'", op_token))
                return None
            node = Node('BINARY_EXPR', op_token.value, token=op_token, children=[node, right_node])
        return node

    # Grammar rule: primary_expr ::= INTEGER_LITERAL | IDENTIFIER | '(' expression ')'
    def parse_primary_expr(self):
        token = self.current_token()
        if token.type == 'INTEGER_LITERAL':
            self.advance()
            return Node('INTEGER_LITERAL', value=int(token.value), token=token)
        elif token.type == 'IDENTIFIER':
            self.advance()
            return Node('IDENTIFIER', value=token.value, token=token)
        elif token.type == 'BRACKET_OPEN_PAREN':
            self.advance()
            expr = self.parse_expression()
            if not self.expect('BRACKET_CLOSE_PAREN'):
                return None
            return expr
        else:
            self.errors.append(SyntaxError("Invalid expression: Expected a number, variable, or parentheses", token))
            return None
            
    # --- Other Parsing Methods ---

    # Grammar rule: block ::= '{' statement* '}'
    def parse_block(self):
        block_start_token = self.expect('BRACKET_OPEN_CURLY')
        if not block_start_token: return None
        statements = []
        while self.current_token().type not in ('BRACKET_CLOSE_CURLY', 'EOF'):
            stmt = self.parse_statement()
            if stmt:
                statements.append(stmt)
            else:
                break # Error in statement parsing, exit block
        if not self.expect('BRACKET_CLOSE_CURLY'): return None
        return Node('BLOCK', token=block_start_token, children=statements)

    # Grammar rule: if_statement ::= 'startIf' '(' expression ')' block ('otherwise' block)?
    def parse_if_statement(self):
        if_token = self.expect('KEYWORD') # Assumes 'startIf'
        if not self.expect('BRACKET_OPEN_PAREN'): return None
        condition = self.parse_expression()
        if not condition: return None
        if not self.expect('BRACKET_CLOSE_PAREN'): return None
        if_block = self.parse_block()
        if not if_block: return None
        
        children = [condition, if_block]
        
        if self.current_token().value == 'otherwise':
            self.advance()
            else_block = self.parse_block()
            if else_block:
                children.append(Node('ELSE_CLAUSE', children=[else_block]))
        
        return Node('IF_STATEMENT', token=if_token, children=children)

    # FIX: Added a full implementation for parsing while loops.
    # Grammar rule: while_statement ::= 'AMwhile' '(' expression ')' block
    def parse_while_loop(self):
        while_token = self.expect('KEYWORD') # Assumes 'AMwhile'
        if not self.expect('BRACKET_OPEN_PAREN'): return None
        condition = self.parse_expression()
        if not condition: return None
        if not self.expect('BRACKET_CLOSE_PAREN'): return None
        loop_body = self.parse_block()
        if not loop_body: return None
        return Node('WHILE_LOOP', token=while_token, children=[condition, loop_body])

    # Placeholders for other control structures
    def parse_for_loop(self): return None
    def parse_do_while_loop(self): return None
    def parse_shift_statement(self): return None

# The main function is for standalone testing of the parser.
if __name__ == "__main__":
    print("--- AMCoder Syntax Analyzer Testbed ---")
    print("Enter your AMCoder code below. Type 'ENDCODE' on a new line to parse.")
    print("Type 'EXIT' to quit.")
    
    while True:
        print("\n" + "="*50)
        code_lines = []
        while True:
            try:
                line = input("> ")
                if line.strip().upper() == 'ENDCODE':
                    break
                if line.strip().upper() == 'EXIT':
                    print("Exiting syntax analyzer testbed.")
                    sys.exit(0)
                code_lines.append(line)
            except EOFError:
                print("\nExiting...")
                sys.exit(0)
        
        source_code = "\n".join(code_lines)
        if not source_code.strip():
            print("No code entered. Please try again.")
            continue

        print("\n--- Parsing Your Code ---")

        # 1. Lexical Analysis
        lexer = Lexer()
        tokens = list(lexer.tokenize(source_code))
        
        # 2. Syntax Analysis
        parser = Parser(tokens)
        ast = parser.parse()

        if not parser.errors:
            print("\nParsing completed successfully!")
            print("\nAbstract Syntax Tree (AST):")
            if ast and ast.children:
                ast.print_ast()
            else:
                print("  (Empty AST - no statements parsed)")
        else:
            print("\nParsing completed with errors:")
            for error in parser.errors:
                print(f"  - {error}")
        
        print("\nReady for new input...")
