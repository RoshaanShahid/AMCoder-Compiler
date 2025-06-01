import re 
from Lexical_Analyzer import Token, Lexer 
from syntax_analyzer import Node, Parser  
class SemanticError(Exception):
    def __init__(self, message, token):
        self.message = message
        self.token = token
        line = getattr(token, 'line', 'N/A')
        column = getattr(token, 'column', 'N/A')
        value = getattr(token, 'value', '')
        ttype = getattr(token, 'type', 'UNKNOWN')
        super().__init__(f"Semantic Error at Line {line}, Column {column}: {message} (Token: '{value}', Type: {ttype})")

class SymbolTableEntry:
    def __init__(self, name, type, kind, declaration_token, params=None, return_type=None):
        self.name = name
        self.type = type
        self.kind = kind
        self.declaration_token = declaration_token
        self.params = params if params is not None else []
        self.return_type = return_type

    def __repr__(self):
        return f"Entry(Name: {self.name}, Type: {self.type}, Kind: {self.kind}, Declared: L{self.declaration_token.line} C{self.declaration_token.column})"


class SymbolTable:
    def __init__(self):
        self.scopes = [{}]
        self.current_scope_level = 0

    def enter_scope(self):
        self.scopes.append({})
        self.current_scope_level += 1

    def exit_scope(self):
        if len(self.scopes) > 1:
            self.scopes.pop()
            self.current_scope_level -= 1
        else:
            pass # Avoid printing warning for simple interactive tests

    def declare(self, name, entry):
        current_scope = self.scopes[-1]
        if name in current_scope:
            prev_entry = current_scope[name]
            raise SemanticError(
                f"Identifier '{name}' already declared in this scope at Line {prev_entry.declaration_token.line}, Column {prev_entry.declaration_token.column}",
                entry.declaration_token
            )
        current_scope[name] = entry

    def lookup(self, name, token_for_error):
        for i in range(len(self.scopes) - 1, -1, -1):
            if name in self.scopes[i]:
                return self.scopes[i][name]
        raise SemanticError(f"Identifier '{name}' not declared", token_for_error)

    def __repr__(self):
        return f"SymbolTable(Scopes: {self.scopes})"


class SemanticAnalyzer:
    def __init__(self):
        self.symbol_table = SymbolTable()
        self.errors = []
        self.current_function_return_type = None
        self.defined_types = {'AMint', 'AMchar', 'AMfloat', 'AMbool', 'AMstring', 'AMdouble'}


    def add_error(self, message, token):
        try:
            if not isinstance(token, Token):
                # print(f"Warning: Invalid token passed to add_error for message: {message}. Using generic token.")
                token = Token('UNKNOWN', 'error_location', 0, 0)
            err = SemanticError(message, token)
            self.errors.append(err)
        except Exception as e:
            self.errors.append(f"Internal Error in error reporting: {e} for message: {message}")


    def analyze(self, ast_root):
        self.errors = []
        self.symbol_table = SymbolTable() 
        analysis_successful = True
        try:
            if ast_root: 
                self.visit(ast_root)
            else:
              
                pass 
            
            if self.errors: 
                analysis_successful = False

        except SemanticError as e:
            if e not in self.errors: 
                 self.errors.append(e)
            analysis_successful = False
        except Exception as e:
            fallback_token = Token('UNKNOWN', str(e), 0, 0)
            if hasattr(ast_root, 'token') and isinstance(ast_root.token, Token):
                fallback_token = ast_root.token
            elif ast_root and hasattr(ast_root, 'children') and ast_root.children and \
                 hasattr(ast_root.children[0], 'token') and isinstance(ast_root.children[0].token, Token):
                 fallback_token = ast_root.children[0].token
            self.add_error(f"Unexpected analysis error: {type(e).__name__} - {e}", fallback_token)
            analysis_successful = False

        if not self.errors and ast_root and (hasattr(ast_root, 'children') and ast_root.children): # Only print success if there was something to analyze
            print("Semantic analysis completed successfully.")
        elif self.errors:
            print("\nSemantic analysis completed with errors:")
            for error in self.errors:
                print(error)
        # If no errors and ast_root was None or empty, don't print success/failure from here.
        # Let the caller (main loop) decide what to print for empty valid inputs.
        return analysis_successful

    def visit(self, node):
        if not isinstance(node, Node): 
            self.add_error(f"Attempted to visit an invalid node object: {type(node)}", Token('INTERNAL', 'INVALID_NODE', 0,0))
            return "ERROR_TYPE"
        method_name = f'visit_{node.node_type}'
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        for child in node.children:
            self.visit(child)
        return "UNKNOWN_TYPE" 

    def visit_PROGRAM(self, node):
        for child_statement in node.children:
            self.visit(child_statement)
        return "VOID_PROGRAM_TYPE"

    def visit_DECLARATION(self, node):
        type_node = node.children[0]
        identifier_node = node.children[1]
        var_type = type_node.value
        var_name = identifier_node.value
        declaration_token = identifier_node.token
        if var_type not in self.defined_types:
            self.add_error(f"Unknown type '{var_type}'", type_node.token)
            return "ERROR_TYPE"
        entry = SymbolTableEntry(var_name, var_type, "variable", declaration_token)
        try:
            self.symbol_table.declare(var_name, entry)
        except SemanticError as e:
            self.errors.append(e) 
        return "VOID_DECLARATION_TYPE"

    def visit_ASSIGNMENT(self, node):
        identifier_node = node.children[0]
        expression_node = node.children[1]
        var_name = identifier_node.value
        assignment_token = node.token 
        try:
            var_entry = self.symbol_table.lookup(var_name, identifier_node.token)
            if var_entry.kind == "function": 
                 self.add_error(f"Cannot assign to function '{var_name}'", identifier_node.token)
                 return "ERROR_TYPE"
            lhs_type = var_entry.type
            rhs_type = self.visit(expression_node)
            if rhs_type == "ERROR_TYPE": 
                return "ERROR_TYPE"
            if lhs_type == rhs_type: pass
            elif lhs_type == "AMfloat" and rhs_type == "AMint": pass
            elif lhs_type == "AMdouble" and (rhs_type == "AMint" or rhs_type == "AMfloat"): pass
            elif lhs_type == "AMbool" and rhs_type == "AMbool": pass
            else:
                self.add_error(f"Type mismatch: Cannot assign type '{rhs_type}' to variable '{var_name}' of type '{lhs_type}'", assignment_token or identifier_node.token)
                return "ERROR_TYPE"
        except SemanticError as e:
            self.errors.append(e)
            return "ERROR_TYPE"
        return "VOID_ASSIGNMENT_TYPE"

    def visit_EXPRESSION_STATEMENT(self, node):
        self.visit(node.children[0])
        return "VOID_EXPRESSION_STMT_TYPE"

    def visit_IDENTIFIER(self, node):
        var_name = node.value
        try:
            entry = self.symbol_table.lookup(var_name, node.token)
            return entry.type
        except SemanticError as e:
            self.errors.append(e)
            return "ERROR_TYPE"

    def visit_INTEGER_LITERAL(self, node):
        return "AMint"

    def visit_STRING_LITERAL(self, node): 
        return "AMstring"
    
    def visit_BINARY_EXPR(self, node):
        op = node.value
        op_token = node.token
        if len(node.children) < 2:
            self.add_error(f"Binary expression '{op}' is missing operands", op_token)
            return "ERROR_TYPE"
        left_type = self.visit(node.children[0])
        right_type = self.visit(node.children[1])
        if left_type == "ERROR_TYPE" or right_type == "ERROR_TYPE":
            return "ERROR_TYPE"
        numeric_types = {"AMint", "AMfloat", "AMdouble"}
        if op in {'+', '-', '*', '/', '**', '%%'}: 
            if left_type in numeric_types and right_type in numeric_types:
                if "AMdouble" in (left_type, right_type): return "AMdouble"
                if "AMfloat" in (left_type, right_type): return "AMfloat"
                return "AMint"
            else:
                self.add_error(f"Type mismatch: Arithmetic operator '{op}' requires numeric operands, got '{left_type}' and '{right_type}'", op_token)
                return "ERROR_TYPE"
        elif node.token.type == 'COMPARISON_OPERATOR':
            if (left_type in numeric_types and right_type in numeric_types) or \
               (left_type == "AMstring" and right_type == "AMstring") or \
               (left_type == "AMchar" and right_type == "AMchar") or \
               (left_type == "AMbool" and right_type == "AMbool"): 
                return "AMbool" 
            else:
                self.add_error(f"Type mismatch: Comparison operator '{op}' cannot compare '{left_type}' and '{right_type}'", op_token)
                return "ERROR_TYPE"
        elif node.token.type == 'LOGICAL_OPERATOR':
            if left_type == "AMbool" and right_type == "AMbool":
                return "AMbool"
            else:
                self.add_error(f"Type mismatch: Logical operator '{op}' requires boolean operands, got '{left_type}' and '{right_type}'", op_token)
                return "ERROR_TYPE"
        else:
            self.add_error(f"Unknown or unsupported binary operator '{op}' with token type '{node.token.type}'", op_token)
            return "ERROR_TYPE"

    def visit_UNARY_PREFIX_EXPR(self, node):
        op = node.value
        op_token = node.token
        if len(node.children) < 1:
            self.add_error(f"Unary prefix expression '{op}' is missing an operand", op_token)
            return "ERROR_TYPE"
        expr_type = self.visit(node.children[0])
        if expr_type == "ERROR_TYPE":
            return "ERROR_TYPE"
        if op_token.type == "TYPE_CAST_OPERATOR" and op in {"Static", "Dynamic", "Const"}:
            return expr_type 
        else:
            self.add_error(f"Unknown or unsupported unary prefix operator '{op}'", op_token)
            return "ERROR_TYPE"

    def visit_UNARY_POSTFIX_EXPR(self, node):
        op = node.value
        op_token = node.token
        if len(node.children) < 1:
            self.add_error(f"Unary postfix expression '{op}' is missing an operand", op_token)
            return "ERROR_TYPE"
        operand_node = node.children[0]
        if operand_node.node_type != 'IDENTIFIER':
            self.add_error(f"Operand for postfix operator '{op}' must be a variable identifier", operand_node.token or op_token)
            return "ERROR_TYPE"
        var_name = operand_node.value
        try:
            var_entry = self.symbol_table.lookup(var_name, operand_node.token)
            operand_type = var_entry.type
            if op_token.type == "INCREMENT_DECREMENT_OPERATOR" or \
               (op_token.type == "ARITHMETIC_OPERATOR" and op in {'++', '--', '**', '%%'}): 
                if operand_type in {"AMint", "AMfloat", "AMdouble"}: 
                    return operand_type 
                else:
                    self.add_error(f"Type mismatch: Postfix operator '{op}' requires a numeric variable, got type '{operand_type}' for '{var_name}'", op_token)
                    return "ERROR_TYPE"
            else:
                self.add_error(f"Unknown or unsupported unary postfix operator '{op}'", op_token)
                return "ERROR_TYPE"
        except SemanticError as e:
            self.errors.append(e)
            return "ERROR_TYPE"

    def visit_POINTER_EXPR(self, node):
        op = node.value
        op_token = node.token
        if len(node.children) < 1 or node.children[0].node_type != 'IDENTIFIER':
             self.add_error(f"Pointer operator '{op}' requires an identifier operand.", op_token)
             return "ERROR_TYPE"
        identifier_node = node.children[0]
        var_name = identifier_node.value
        try:
            var_entry = self.symbol_table.lookup(var_name, identifier_node.token)
            self.add_error(f"Pointer operations ('{op}') semantics are not fully defined for type checking in this version. Variable '{var_name}' has type '{var_entry.type}'.", op_token)
            return "UNKNOWN_POINTER_OP_TYPE" 
        except SemanticError as e:
            self.errors.append(e)
            return "ERROR_TYPE"

    def visit_PARENTHESIZED_EXPR(self, node):
        if not node.children:
            self.add_error("Parenthesized expression is empty.", node.token or Token('SYNTAX', '()',0,0))
            return "ERROR_TYPE"
        return self.visit(node.children[0])

    def visit_BLOCK(self, node):
        self.symbol_table.enter_scope()
        for statement_node in node.children:
            self.visit(statement_node)
        self.symbol_table.exit_scope()
        return "VOID_BLOCK_TYPE"

    def visit_IF_STATEMENT(self, node):
        condition_node = node.children[0]
        if_block_node = node.children[1]
        condition_type = self.visit(condition_node)
        if condition_type != "AMbool" and condition_type != "ERROR_TYPE":
            self.add_error(f"Condition for 'startIf' statement must be of type AMbool, got '{condition_type}'", getattr(condition_node, 'token', None) or node.token)
        self.visit(if_block_node) 
        current_child_index = 2
        while current_child_index < len(node.children):
            clause_node = node.children[current_child_index]
            if clause_node.node_type == 'ELSE_IF_CLAUSE':
                else_if_condition_node = clause_node.children[0]
                else_if_block_node = clause_node.children[1]
                else_if_cond_type = self.visit(else_if_condition_node)
                if else_if_cond_type != "AMbool" and else_if_cond_type != "ERROR_TYPE":
                     self.add_error(f"Condition for 'otherIf' clause must be of type AMbool, got '{else_if_cond_type}'", getattr(else_if_condition_node, 'token', None) or clause_node.token)
                self.visit(else_if_block_node)
            elif clause_node.node_type == 'ELSE_CLAUSE':
                else_block_node = clause_node.children[0]
                self.visit(else_block_node)
            current_child_index += 1
        return "VOID_IF_STMT_TYPE"

    def visit_FOR_LOOP(self, node):
        self.symbol_table.enter_scope() 
        init_node = node.children[0]
        condition_node = node.children[1]
        increment_node = node.children[2] 
        loop_body_node = node.children[3]
        if init_node.node_type != 'EMPTY_INIT':
            self.visit(init_node)
        if condition_node.node_type != 'EMPTY_EXPR': 
            condition_type = self.visit(condition_node)
            if condition_type != "AMbool" and condition_type != "ERROR_TYPE":
                 self.add_error(f"Condition for 'AMfor' loop must be of type AMbool, got '{condition_type}'", getattr(condition_node, 'token', None) or node.token)
        if increment_node.node_type != 'EMPTY_INCREMENT':
            self.visit(increment_node)
        self.visit(loop_body_node) 
        self.symbol_table.exit_scope() 
        return "VOID_FOR_LOOP_TYPE"

    def visit_WHILE_LOOP(self, node):
        condition_node = node.children[0]
        loop_body_node = node.children[1]
        condition_type = self.visit(condition_node)
        if condition_type != "AMbool" and condition_type != "ERROR_TYPE":
            self.add_error(f"Condition for 'AMwhile' loop must be of type AMbool, got '{condition_type}'", getattr(condition_node, 'token', None) or node.token)
        self.visit(loop_body_node) 
        return "VOID_WHILE_LOOP_TYPE"

    def visit_DO_WHILE_LOOP(self, node):
        loop_body_node = node.children[0]
        condition_node = node.children[1]
        self.visit(loop_body_node) 
        condition_type = self.visit(condition_node)
        if condition_type != "AMbool" and condition_type != "ERROR_TYPE":
            self.add_error(f"Condition for 'AMdo-AMwhile' loop must be of type AMbool, got '{condition_type}'", getattr(condition_node, 'token', None) or node.token)
        return "VOID_DO_WHILE_LOOP_TYPE"

    def visit_SHIFT_STATEMENT(self, node):
        shift_expr_node = node.children[0]
        shift_expr_type = self.visit(shift_expr_node)
        if shift_expr_type == "ERROR_TYPE":
            return "VOID_SHIFT_STMT_TYPE" 
        allowed_shift_types = {"AMint", "AMchar", "AMstring"} 
        if shift_expr_type not in allowed_shift_types:
            self.add_error(f"'shift' expression type must be one of {allowed_shift_types}, got '{shift_expr_type}'", getattr(shift_expr_node, 'token', None) or node.token)
        case_values_seen = set() 
        for i in range(1, len(node.children)):
            clause_node = node.children[i]
            if clause_node.node_type == 'CASE_CLAUSE':
                case_expr_node = clause_node.children[0]
                case_block_node = clause_node.children[1]
                case_expr_type = self.visit(case_expr_node)
                if case_expr_type != "ERROR_TYPE" and shift_expr_type != "ERROR_TYPE" and \
                   case_expr_type != shift_expr_type:
                    self.add_error(f"'case' expression type '{case_expr_type}' does not match 'shift' expression type '{shift_expr_type}'", getattr(case_expr_node, 'token', None) or clause_node.token)
                if case_expr_node.node_type in {'INTEGER_LITERAL', 'STRING_LITERAL', 'CHAR_LITERAL'}: 
                    case_val = case_expr_node.value 
                    if case_val in case_values_seen:
                        self.add_error(f"Duplicate case value '{case_val}'", case_expr_node.token)
                    case_values_seen.add(case_val)
                self.visit(case_block_node)
            elif clause_node.node_type == 'DEFAULT_CLAUSE':
                default_block_node = clause_node.children[0]
                self.visit(default_block_node)
        return "VOID_SHIFT_STMT_TYPE"

    def visit_EMPTY_STATEMENT(self, node): 
        return "VOID_EMPTY_STMT_TYPE"
        
    def visit_EMPTY_INIT(self, node): 
        return "VOID_TYPE" 
    def visit_EMPTY_INCREMENT(self, node): 
        return "VOID_TYPE"
    def visit_EMPTY_EXPR(self, node): 
        return "AMbool" 


if __name__ == "__main__":
    # Ensure Lexer and Parser are imported from your files
    # from Lexical_Analyzer import Lexer, Token # Token is already imported at top
    # from syntax_analyzer import Parser, Node # Node is already imported at top

    amcoder_lexer = Lexer()
    amcoder_parser = Parser([]) # Parser needs an initial token list, can be empty
    semantic_analyzer = SemanticAnalyzer()

    print("AMCoder Interactive Semantic Analyzer (Type 'ENDCODE' on a new line to finish input, 'EXIT' to quit)")
    while True:
        print("-" * 50)
        print("Enter your AMCoder code:")
        code_lines = []
        while True:
            try:
                line = input("> ")
                if line.strip().upper() == 'ENDCODE':
                    break
                if line.strip().upper() == 'EXIT':
                    print("Exiting interactive analyzer.")
                    exit()
                code_lines.append(line)
            except EOFError: # Handle Ctrl+D or similar
                print("Exiting interactive analyzer due to EOF.")
                exit()
        
        source_code = "\n".join(code_lines)
        if not source_code.strip():
            print("No code entered.")
            continue

        print("\n--- Lexical Analysis ---")
        tokens = []
        try:
            # The Lexer in the user's provided code is a generator.
            # It also prints lexical errors directly.
            # We need to collect tokens and also see if it printed errors.
            # For simplicity, we'll just collect. A more robust way would be for lexer to return errors.
            tokens = list(amcoder_lexer.tokenize(source_code))
            print("Tokens generated:")
            if not tokens:
                print("(No tokens were generated - possibly only comments or whitespace, or lexer error)")
            for token in tokens:
                print(f"  TOKEN: {token.value:<15} TYPE: {token.type:<25} L:{token.line} C:{token.column}")
            amcoder_lexer.print_symbol_table() # Lexer's symbol table
        except Exception as e:
            print(f"Error during lexical analysis: {e}")
            continue # Skip to next input

        print("\n--- Syntax Analysis ---")
        amcoder_parser.tokens = tokens # Re-initialize parser with new tokens
        amcoder_parser.pos = 0
        amcoder_parser.errors = [] # Clear previous parser errors
        
        ast_root = None
        try:
            ast_root = amcoder_parser.parse()
        except Exception as e:
            print(f"Critical error during parsing: {e}")
            # If parser itself raises an unhandled exception
            if hasattr(amcoder_parser, 'current_token'):
                 tok = amcoder_parser.current_token()
                 print(f"  Error occurred near token: {tok.value} (Type: {tok.type}) at L:{tok.line} C:{tok.column}")
            continue

        if amcoder_parser.errors:
            print("Syntax errors found:")
            for error in amcoder_parser.errors:
                print(error)
            if ast_root and (hasattr(ast_root, 'children') and ast_root.children):
                print("\nPartial AST (due to syntax errors):")
                ast_root.print_ast() # Assuming Node has print_ast
            else:
                print("(No AST or very partial AST generated due to syntax errors)")
        elif not ast_root or not (hasattr(ast_root, 'children') and ast_root.children):
             print("Parsing completed. No statements found or AST is empty (e.g. only comments/whitespace).")
        else:
            print("Parsing completed successfully!")
            if hasattr(ast_root, 'print_ast'):
                print("\nAbstract Syntax Tree (AST):")
                ast_root.print_ast()
            else:
                print("(AST generated, but Node class has no print_ast method)")

            print("\n--- Semantic Analysis ---")
            semantic_analyzer.analyze(ast_root) # This will print its own success/error messages
