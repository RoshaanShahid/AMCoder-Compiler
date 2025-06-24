import sys
import re
from syntax_analyzer import Node, Parser
from Lexical_Analyzer import Lexer, Token
from semantic_analyzer import SemanticAnalyzer

class ThreeAddressCode:
    """
    A class to manage the generation of Three-Address Code (TAC).
    It holds the list of instructions and manages temporary variables and labels.
    """
    def __init__(self):
        self.instructions = []
        self.temp_count = 0
        self.label_count = 0

    def new_temp(self):
        """Generates a new temporary variable name."""
        self.temp_count += 1
        return f"t{self.temp_count}"

    def new_label(self):
        """Generates a new unique label name."""
        self.label_count += 1
        return f"L{self.label_count}"

    def add_instruction(self, op, arg1, arg2, result):
        """Adds a new TAC instruction to the list."""
        self.instructions.append((op, arg1, arg2, result))

    def __str__(self):
        """Returns a human-readable string representation of the TAC."""
        if not self.instructions:
            return "(No TAC generated)"
        
        lines = []
        for op, arg1, arg2, result in self.instructions:
            if op == 'LABEL':
                lines.append(f"{result}:")
            elif op == 'GOTO':
                lines.append(f"    goto {result}")
            elif op in ('IF_FALSE_GOTO', 'IF_TRUE_GOTO'):
                lines.append(f"    if {arg1} {op.split('_')[0].lower()} goto {result}")
            elif op == '=':
                lines.append(f"    {result} = {arg1}")
            elif op == 'PARAM':
                 lines.append(f"    param {result}")
            elif op == 'CALL':
                 lines.append(f"    {result} = call {arg1}, {arg2}") # arg1=func_name, arg2=num_params
            elif op == 'RETURN':
                 lines.append(f"    return {result}")
            else:
                lines.append(f"    {result} = {arg1} {op} {arg2}")
        return "\n".join(lines)


class ICG:
    """
    Intermediate Code Generator.
    Traverses the AST from the semantic analyzer and generates Three-Address Code.
    """
    def __init__(self, semantic_analyzer):
        self.symbol_table = semantic_analyzer.symbol_table
        self.tac = ThreeAddressCode()
        
    def generate(self, ast_root):
        """
        Starts the TAC generation process from the root of the AST.
        """
        self.tac = ThreeAddressCode()
        if not ast_root or not hasattr(ast_root, 'node_type'):
            print("ICG Error: Invalid or empty AST root provided.")
            return self.tac # Return empty TAC
            
        self.visit(ast_root)
        return self.tac

    def visit(self, node, *args, **kwargs):
        """
        Dynamically calls the appropriate visit method based on the node's type.
        e.g., for a 'DECLARATION' node, it calls 'visit_DECLARATION'.
        """
        method_name = f'visit_{node.node_type}'
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node, *args, **kwargs)

    def generic_visit(self, node, *args, **kwargs):
        """A fallback visit method for nodes that don't have a specific one."""
        # This can be a source of silent failures if a node type is missed.
        # print(f"ICG Warning: No visitor method for node type '{node.node_type}'. Skipping.")
        for child in node.children:
            self.visit(child, *args, **kwargs)
        return None

    def visit_PROGRAM(self, node):
        for child in node.children:
            self.visit(child)

    def visit_DECLARATION(self, node):
        # Declarations don't generate code in this simple ICG,
        # as type information is handled by the semantic analyzer.
        # We could add them to a "data segment" if needed.
        pass

    def visit_ASSIGNMENT(self, node):
        var_name = node.children[0].value
        expr_result = self.visit(node.children[1])
        self.tac.add_instruction('=', expr_result, None, var_name)

    def visit_BINARY_EXPR(self, node):
        left_result = self.visit(node.children[0])
        right_result = self.visit(node.children[1])
        op = node.value
        
        # Create a new temporary variable to store the result of the expression
        temp_var = self.tac.new_temp()
        self.tac.add_instruction(op, left_result, right_result, temp_var)
        return temp_var

    def visit_IDENTIFIER(self, node):
        return node.value

    def visit_INTEGER_LITERAL(self, node):
        return node.value
        
    def visit_STRING_LITERAL(self, node):
        # Representing strings in TAC can be complex. Here we just return the literal.
        # A more robust system would add it to a data segment and return a label.
        return f'"{node.value}"'

    def visit_PARENTHESIZED_EXPR(self, node):
        return self.visit(node.children[0])
    
    def visit_EXPRESSION_STATEMENT(self, node):
        self.visit(node.children[0])

    def visit_IF_STATEMENT(self, node):
        condition_node = node.children[0]
        if_block_node = node.children[1]

        # Labels for control flow
        label_after_if = self.tac.new_label()
        # Check if there's an else clause to determine the jump target
        has_else = len(node.children) > 2
        label_else = self.tac.new_label() if has_else else label_after_if

        # Evaluate the condition
        condition_result = self.visit(condition_node)
        
        # If the condition is false, jump to the else block (or after the if)
        self.tac.add_instruction('IF_FALSE_GOTO', condition_result, None, label_else)

        # Generate code for the 'if' block
        self.visit(if_block_node)
        
        # If there is an else block, we need to jump past it at the end of the 'if' block.
        if has_else:
            self.tac.add_instruction('GOTO', None, None, label_after_if)

        # Handle else/otherIf clauses
        if has_else:
            self.tac.add_instruction('LABEL', None, None, label_else)
            else_node = node.children[2]
            self.visit(else_node)

        # Label for the end of the entire if-else structure
        self.tac.add_instruction('LABEL', None, None, label_after_if)

    def visit_ELSE_CLAUSE(self, node):
        self.visit(node.children[0])
    
    def visit_BLOCK(self, node):
        for statement in node.children:
            self.visit(statement)

    def visit_WHILE_LOOP(self, node):
        condition_node = node.children[0]
        loop_body_node = node.children[1]

        label_loop_start = self.tac.new_label()
        label_loop_end = self.tac.new_label()

        # Label for the start of the loop (for re-evaluating the condition)
        self.tac.add_instruction('LABEL', None, None, label_loop_start)

        # Evaluate the condition
        condition_result = self.visit(condition_node)
        self.tac.add_instruction('IF_FALSE_GOTO', condition_result, None, label_loop_end)
        
        # Generate code for the loop body
        self.visit(loop_body_node)
        self.tac.add_instruction('GOTO', None, None, label_loop_start) # Jump back to the start

        # Label for the end of the loop
        self.tac.add_instruction('LABEL', None, None, label_loop_end)

# This main block is for demonstration and testing.
if __name__ == '__main__':
    print("--- AMCoder Intermediate Code Generation Testbed ---")
    print("Enter your AMCoder code below. Type 'ENDCODE' on a new line to compile.")
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
                    print("Exiting testbed.")
                    sys.exit(0)
                code_lines.append(line)
            except EOFError:
                print("\nExiting...")
                sys.exit(0)
        
        source_code = "\n".join(code_lines)
        if not source_code.strip():
            print("No code entered. Please try again.")
            continue

        print("\n--- Compiling Your Code ---")

        # --- Full pipeline to get to Intermediate Code Generation ---

        # 1. Lexical Analysis
        lexer = Lexer()
        tokens = list(lexer.tokenize(source_code))
        
        # 2. Syntax Analysis
        parser = Parser(tokens)
        ast = parser.parse()
        if parser.errors:
            print("\nSyntax Errors Found. Aborting.")
            for error in parser.errors:
                print(f"  - {error}")
            continue
        
        # 3. Semantic Analysis
        semantic_analyzer = SemanticAnalyzer()
        if not semantic_analyzer.analyze(ast):
            print("\nSemantic Analysis Failed. Errors are printed above.")
            continue
            
        # 4. Intermediate Code Generation
        icg = ICG(semantic_analyzer)
        three_address_code = icg.generate(ast)
        
        print("\n--- Generated Three-Address Code (TAC) ---")
        print(three_address_code)
        print("\nReady for new input...")

