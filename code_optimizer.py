import sys
import re
from intermediate_code_generator import ICG, ThreeAddressCode
from syntax_analyzer import Parser, Node, SyntaxError
from Lexical_Analyzer import Lexer, Token
from semantic_analyzer import SemanticAnalyzer, SemanticError

class Optimizer:
    """
    Performs basic optimization techniques on the Three-Address Code.
    """
    def __init__(self, tac_instructions):
        self.instructions = list(tac_instructions) # Work on a copy
        self.optimized_instructions = []

    def optimize(self):
        """
        Runs a sequence of optimization passes until no more changes can be made.
        """
        changed = True
        pass_count = 0
        max_passes = 10 # Safety break for potential infinite loops
        
        while changed and pass_count < max_passes:
            pass_count += 1
            # print(f"\n--- Optimization Pass {pass_count} ---")
            
            instructions_before_pass = list(self.instructions)

            # Perform optimizations
            self.constant_folding()
            # print("After Constant Folding:")
            # self.print_current_state()

            self.dead_code_elimination()
            # print("\nAfter Dead Code Elimination:")
            # self.print_current_state()

            if instructions_before_pass == self.instructions:
                changed = False
        
        self.optimized_instructions = self.instructions
        # print(f"\nOptimization complete after {pass_count} passes.")
        return self.optimized_instructions

    def print_current_state(self):
        """Helper to print the instructions in their current state."""
        temp_tac = ThreeAddressCode()
        temp_tac.instructions = self.instructions
        print(temp_tac)

    def constant_folding(self):
        """
        Evaluates expressions with constant operands at compile time.
        Example: `t1 = 3 + 5` becomes `t1 = 8`.
        """
        new_instructions = []
        for op, arg1, arg2, result in self.instructions:
            try:
                # Check if both arguments can be interpreted as numbers
                is_arg1_num = str(arg1).isdigit() or (str(arg1).startswith('-') and str(arg1)[1:].isdigit())
                is_arg2_num = str(arg2).isdigit() or (str(arg2).startswith('-') and str(arg2)[1:].isdigit())

                if is_arg1_num and is_arg2_num and op in ['+', '-', '*', '/']:
                    val1 = int(arg1)
                    val2 = int(arg2)
                    
                    if op == '+': new_val = val1 + val2
                    elif op == '-': new_val = val1 - val2
                    elif op == '*': new_val = val1 * val2
                    elif op == '/' and val2 != 0: new_val = val1 // val2
                    else:
                        new_instructions.append((op, arg1, arg2, result))
                        continue

                    # Replace the operation with a simple assignment
                    new_instructions.append(('=', str(new_val), None, result))
                else:
                    new_instructions.append((op, arg1, arg2, result))
            except (ValueError, TypeError):
                 new_instructions.append((op, arg1, arg2, result))
        
        self.instructions = new_instructions
    
    def dead_code_elimination(self):
        """
        Removes instructions that assign a value to a variable
        that is never used again.
        """
        used_vars = set()
        # An initial pass to find all variables that are "live" at the end of the program
        # or are used in control flow.
        for op, arg1, arg2, result in self.instructions:
            if op in ('IF_FALSE_GOTO', 'IF_TRUE_GOTO', 'RETURN'):
                if arg1: used_vars.add(arg1)
            # Find all non-temporary variables (assumed to be program's own variables)
            # These are considered "live" because their final value might be important.
            if result and not str(result).startswith('t'):
                 used_vars.add(result)
        
        live_instructions = []
        # Iterate backwards through the instructions
        for op, arg1, arg2, result in reversed(self.instructions):
            is_live = False
            
            # An instruction is live if its result is in the `used_vars` set,
            # or if it has side effects (like GOTO, LABEL, function calls, etc.).
            if result in used_vars or op in ('GOTO', 'LABEL', 'IF_FALSE_GOTO', 'IF_TRUE_GOTO', 'CALL', 'RETURN', 'PARAM'):
                is_live = True
            
            if is_live:
                live_instructions.append((op, arg1, arg2, result))
                # If we keep this instruction, its result is no longer needed by instructions above it.
                if result:
                    used_vars.discard(result)
                # And its arguments are now needed by this instruction, so they become live.
                if arg1:
                    used_vars.add(arg1)
                if arg2:
                    used_vars.add(arg2)

        self.instructions = list(reversed(live_instructions))


# This main block is for demonstration and testing.
if __name__ == '__main__':
    print("--- AMCoder Optimizer Testbed ---")
    print("Enter your AMCoder code below. Type 'ENDCODE' on a new line to compile and optimize.")
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
                    print("Exiting optimizer testbed.")
                    sys.exit(0)
                code_lines.append(line)
            except EOFError:
                print("Exiting...")
                sys.exit(0)
        
        source_code = "\n".join(code_lines)
        if not source_code.strip():
            print("No code entered. Please try again.")
            continue

        print("\n--- Processing Your Code ---")

        # 1. Lexical Analysis
        lexer = Lexer()
        tokens = list(lexer.tokenize(source_code))
        if not tokens:
            print("Lexical analysis failed or produced no tokens. Please check your code.")
            continue
        
        # 2. Syntax Analysis
        parser = Parser(tokens)
        ast = parser.parse()
        if parser.errors:
            print("\nSyntax Errors Found:")
            for error in parser.errors:
                print(error)
            continue
        
        # 3. Semantic Analysis
        semantic_analyzer = SemanticAnalyzer()
        if not semantic_analyzer.analyze(ast):
            print("\nSemantic Analysis Failed. Errors are printed above.")
            continue
            
        # 4. Intermediate Code Generation
        icg = ICG(semantic_analyzer)
        tac = icg.generate(ast)
        
        print("\n--- Original Three-Address Code ---")
        print(tac)

        # 5. Optimize the TAC
        optimizer = Optimizer(tac.instructions)
        optimized_tac_instructions = optimizer.optimize()

        # Display the final optimized code
        final_tac = ThreeAddressCode()
        final_tac.instructions = optimized_tac_instructions
        print("\n--- Final Optimized Three-Address Code ---")
        print(final_tac)
        print("\nReady for new input...")
