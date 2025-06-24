import sys
import re

# Import all the necessary components from your compiler modules
from Lexical_Analyzer import Lexer, Token
from syntax_analyzer import Parser, Node
from semantic_analyzer import SemanticAnalyzer
from intermediate_code_generator import ICG, ThreeAddressCode
from code_optimizer import Optimizer
from target_code_generator import TargetCodeGenerator

class Simulator:
    """
    A simple Virtual Machine to execute the generated assembly code.
    """
    def __init__(self):
        self.registers = {'R0': 0, 'R1': 0, 'R2': 0, 'R3': 0}
        self.memory = {}
        self.labels = {}
        self.instructions = []
        self.pc = 0  # Program Counter
        self.zero_flag = False # For conditional jumps

    def parse_program(self, assembly_code):
        """Parses the assembly code into data, labels, and instructions."""
        self.registers = {'R0': 0, 'R1': 0, 'R2': 0, 'R3': 0}
        self.memory = {}
        self.labels = {}
        self.instructions = []
        self.pc = 0
        self.zero_flag = False

        lines = assembly_code.strip().split('\n')
        
        in_text_section = False
        instruction_index = 0
        
        for line in lines:
            line = line.strip()
            if not line or line.startswith(';'):
                continue
            
            if line == '.data':
                in_text_section = False
                continue
            elif line == '.text':
                in_text_section = True
                continue
            
            if line.endswith(':'):
                label_name = line[:-1]
                if in_text_section:
                    self.labels[label_name] = instruction_index
                continue
            
            if in_text_section:
                self.instructions.append(line)
                instruction_index += 1
            else: # .data section
                parts = line.split(':')
                var_name = parts[0].strip()
                self.memory[var_name] = 0 # Initialize all variables to 0

    def run(self):
        """Executes the loaded program until HALT."""
        if 'start' in self.labels:
            self.pc = self.labels['start']
        
        max_steps = 1000 # Safety break to prevent infinite loops
        steps = 0

        while self.pc < len(self.instructions) and steps < max_steps:
            instruction = self.instructions[self.pc]
            parts = re.split(r'[ ,]+', instruction)
            opcode = parts[0]

            handler_method = getattr(self, f"op_{opcode}", None)
            if handler_method:
                # Store pc before executing, in case it's a jump
                old_pc = self.pc
                handler_method(parts[1:])
                # If pc wasn't changed by a jump, increment it
                if self.pc == old_pc:
                    self.pc += 1
            else:
                print(f"Simulator Error: Unknown instruction '{opcode}'")
                break
            steps += 1
        
        if steps >= max_steps:
            print("Simulator Warning: Exceeded maximum execution steps. Possible infinite loop.")

    def get_value(self, operand):
        """Gets the value of an operand, whether it's a register, memory, or immediate."""
        if operand.startswith('#'):
            return int(operand[1:])
        if operand in self.registers:
            return self.registers[operand]
        if operand in self.memory:
            return self.memory[operand]
        raise ValueError(f"Unknown operand: {operand}")

    def op_MOV(self, args): # MOV #imm, reg OR MOV reg, reg
        source_val = self.get_value(args[0])
        self.registers[args[1]] = source_val

    def op_LOAD(self, args): # LOAD mem, reg
        self.registers[args[1]] = self.memory[args[0]]

    def op_STORE(self, args): # STORE reg, mem
        self.memory[args[1]] = self.registers[args[0]]
        
    def op_ADD(self, args): # ADD reg1, reg2, dest_reg
        val1 = self.registers[args[0]]
        val2 = self.registers[args[1]]
        self.registers[args[2]] = val1 + val2

    def op_SUB(self, args): # SUB reg1, reg2, dest_reg
        val1 = self.registers[args[0]]
        val2 = self.registers[args[1]]
        self.registers[args[2]] = val1 - val2
        
    def op_MUL(self, args): # MUL reg1, reg2, dest_reg
        val1 = self.registers[args[0]]
        val2 = self.registers[args[1]]
        self.registers[args[2]] = val1 * val2
        
    def op_DIV(self, args): # DIV reg1, reg2, dest_reg
        val1 = self.registers[args[0]]
        val2 = self.registers[args[1]]
        self.registers[args[2]] = val1 // val2 if val2 != 0 else 0

    def op_CMP(self, args): # CMP reg, #imm
        val1 = self.get_value(args[0])
        val2 = self.get_value(args[1])
        self.zero_flag = (val1 == val2)

    def op_JMP(self, args): # JMP label
        self.pc = self.labels[args[0]]

    def op_JE(self, args): # JE label (Jump if Equal / Zero Flag is true)
        if self.zero_flag:
            self.pc = self.labels[args[0]]
        else:
            self.pc += 1
            
    def op_HALT(self, args):
        self.pc = len(self.instructions) # End the loop

    def print_final_state(self):
        """Prints the values of variables in memory at the end."""
        print("Final variable states:")
        if not self.memory:
            print("  (No variables in memory)")
            return
        for var, value in sorted(self.memory.items()):
            if not var.startswith('t'): # Don't print temporary variables
                print(f"  {var} = {value}")

def run_compiler_pipeline(source_code, sample_name="User Input"):
    """
    Executes the full compiler pipeline for a given piece of source code.
    """
    print(f"======================================================")
    print(f"  COMPILING: '{sample_name}'")
    print(f"======================================================")

    # --- Original Code ---
    print("\n--- 1. High-Level Source Code ---")
    print(source_code.strip())

    # --- Phase 1: Lexical Analysis ---
    print("\n--- 2. Lexical Analysis (Token Stream) ---")
    lexer = Lexer()
    tokens = list(lexer.tokenize(source_code))
    if not tokens:
        print("Lexical analysis produced no tokens. Halting.")
        return
    for token in tokens:
        print(f"  TOKEN: {token.value:<15} TYPE: {token.type}")

    # --- Phase 2: Syntax Analysis (Parsing) ---
    print("\n--- 3. Syntax Analysis (Abstract Syntax Tree) ---")
    parser = Parser(tokens)
    ast = parser.parse()
    if parser.errors:
        print("\nSyntax errors detected. Halting backend processing.")
        for error in parser.errors: print(f"  ERROR: {error}")
        return
    if not ast or not ast.children:
        print("Parsing produced an empty AST. Halting.")
        return
    ast.print_ast()

    # --- Phase 3: Semantic Analysis ---
    print("\n--- 4. Semantic Analysis ---")
    semantic_analyzer = SemanticAnalyzer()
    analysis_ok = semantic_analyzer.analyze(ast)
    if not analysis_ok:
        print("\nSemantic errors detected. Halting backend processing.")
        return
    
    # --- Phase 4: Intermediate Code Generation ---
    print("\n--- 5. Intermediate Code (Before Optimization) ---")
    icg = ICG(semantic_analyzer)
    tac = icg.generate(ast)
    print(tac)
    
    # --- Phase 5: Code Optimization ---
    print("\n--- 6. Intermediate Code (After Optimization) ---")
    optimizer = Optimizer(tac.instructions)
    optimized_tac_instructions = optimizer.optimize()
    
    optimized_tac_obj = ThreeAddressCode()
    optimized_tac_obj.instructions = optimized_tac_instructions
    print(optimized_tac_obj)

    # --- Phase 6: Target Code Generation ---
    print("\n--- 7. Final Target Code ---")
    target_gen = TargetCodeGenerator(optimized_tac_instructions)
    assembly_code = target_gen.generate()
    print(assembly_code)
    
    print("\nTarget code generation completed successfully.")

    # --- Phase 7: Simulation of Target Code ---
    print("\n--- 8. Program Output (Simulated) ---")
    simulator = Simulator()
    simulator.parse_program(assembly_code)
    simulator.run()
    simulator.print_final_state()


if __name__ == "__main__":
    print("--- AMCoder Interactive Compiler ---")
    print("Enter your AMCoder code below. Type 'ENDCODE' on a new line to compile and run.")
    print("Type 'EXIT' to quit the compiler.")

    while True:
        print("\n" + "="*50)
        print("Ready for new input...")

        code_lines = []
        while True:
            try:
                line = input("> ")
                if line.strip().upper() == 'ENDCODE':
                    break
                if line.strip().upper() == 'EXIT':
                    print("Exiting compiler.")
                    sys.exit(0)
                code_lines.append(line)
            except EOFError:
                print("\nExiting...")
                sys.exit(0)

        source_code = "\n".join(code_lines)
        if not source_code.strip():
            print("No code entered. Please try again.")
            continue

        # Run the full pipeline on the user's code
        run_compiler_pipeline(source_code)
