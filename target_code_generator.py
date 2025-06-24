import sys
import re
from intermediate_code_generator import ICG, ThreeAddressCode
from code_optimizer import Optimizer
from syntax_analyzer import Parser
from Lexical_Analyzer import Lexer
from semantic_analyzer import SemanticAnalyzer

class TargetCodeGenerator:
    """
    Translates optimized Three-Address Code (TAC) into a simple,
    assembly-like target language.
    """
    def __init__(self, tac_instructions):
        self.tac = tac_instructions
        self.assembly_code = []
        # A simple model of our target machine with 4 general-purpose registers
        self.registers = ['R0', 'R1', 'R2', 'R3']
        self.free_registers = list(self.registers)
        # Maps variables to the register that currently holds their value
        self.register_map = {}
        # Maps variables to their memory location (e.g., stack offset or label)
        self.memory_map = {}

    def generate(self):
        """
        Main method to generate the assembly code from the TAC.
        """
        self.assembly_code.append(".data")
        self.generate_data_declarations()

        self.assembly_code.append("\n.text")
        self.assembly_code.append("start:")
        
        # Then, translate each TAC instruction
        for op, arg1, arg2, result in self.tac:
            # Add the original TAC as a comment for readability
            self.assembly_code.append(f"    ; TAC: {result} = {arg1} {op} {arg2 if arg2 else ''}".strip())
            
            if op in ['+', '-', '*', '/']:
                self.generate_arithmetic(op, arg1, arg2, result)
            elif op == '=':
                self.generate_assignment(arg1, result)
            elif op == 'LABEL':
                self.assembly_code.append(f"{result}:")
            elif op == 'GOTO':
                self.assembly_code.append(f"    JMP {result}")
            elif op == 'IF_FALSE_GOTO':
                self.generate_conditional_jump(arg1, result)
            else:
                 self.assembly_code.append(f"    ; Unsupported TAC op: {op}")

        self.assembly_code.append("    HALT ; End the program")
        return "\n".join(self.assembly_code)

    def get_register_for(self, variable):
        """
        Allocates a register for a variable. If the variable is already in a
        register, it returns that register. Otherwise, it finds a free one
        and generates a LOAD instruction.
        """
        if variable in self.register_map:
            return self.register_map[variable]

        if not self.free_registers:
            self.free_up_all_registers()

        reg = self.free_registers.pop(0)
        self.register_map[variable] = reg
        
        if variable in self.memory_map:
            self.assembly_code.append(f"    LOAD {self.memory_map[variable]}, {reg} ; Load '{variable}' into {reg}")
        
        return reg

    def free_up_all_registers(self):
        """
        A simple way to clear all registers.
        """
        self.free_registers = list(self.registers)
        self.register_map = {}

    def generate_data_declarations(self):
        """
        Scans the TAC to find all variables and declares them in the .data segment.
        """
        variables = set()
        for _, arg1, arg2, result in self.tac:
            if arg1 and not str(arg1).isdigit() and not str(arg1).startswith('L'): variables.add(arg1)
            if arg2 and not str(arg2).isdigit() and not str(arg2).startswith('L'): variables.add(arg2)
            if result and not str(result).isdigit() and not str(result).startswith('L'): variables.add(result)
        
        for var in sorted(list(variables)):
            self.memory_map[var] = var
            self.assembly_code.append(f"    {var}: .word 0 ; Allocate space for variable '{var}'")

    def generate_assignment(self, source, dest):
        """Generates code for: dest = source"""
        dest_reg = self.get_register_for(dest)

        if str(source).isdigit():
            self.assembly_code.append(f"    MOV #{source}, {dest_reg}")
        else:
            source_reg = self.get_register_for(source)
            if source_reg != dest_reg:
                self.assembly_code.append(f"    MOV {source_reg}, {dest_reg}")
            
        self.assembly_code.append(f"    STORE {dest_reg}, {self.memory_map[dest]} ; Store result in '{dest}'")
        self.free_up_all_registers()

    def generate_arithmetic(self, op, arg1, arg2, result):
        """Generates code for: result = arg1 op arg2"""
        reg1 = self.get_register_for(arg1)
        if str(arg1).isdigit():
            self.assembly_code.append(f"    MOV #{arg1}, {reg1}")

        reg2 = self.get_register_for(arg2)
        if str(arg2).isdigit():
            self.assembly_code.append(f"    MOV #{arg2}, {reg2}")
        
        result_reg = reg1
        self.register_map[result] = result_reg
        if result in self.free_registers: self.free_registers.remove(result)

        op_map = {'+': 'ADD', '-': 'SUB', '*': 'MUL', '/': 'DIV'}
        self.assembly_code.append(f"    {op_map[op]} {reg1}, {reg2}, {result_reg}")
        
        self.assembly_code.append(f"    STORE {result_reg}, {self.memory_map[result]} ; Store result in '{result}'")
        self.free_up_all_registers()
        
    def generate_conditional_jump(self, condition_var, label):
        """Generates code for: if condition_var is false, goto label"""
        cond_reg = self.get_register_for(condition_var)
        self.assembly_code.append(f"    CMP {cond_reg}, #0")
        self.assembly_code.append(f"    JE {label}")
        self.free_up_all_registers()

# This main block is for demonstration and testing.
if __name__ == '__main__':
    print("--- AMCoder Target Code Generation Testbed ---")
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

        # --- Full pipeline to get to Target Code Generation ---
        
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
        tac = icg.generate(ast)
        
        # 5. Code Optimization
        optimizer = Optimizer(tac.instructions)
        optimized_tac = optimizer.optimize()
        
        print("\n--- Optimized Three-Address Code ---")
        temp_tac = ThreeAddressCode()
        temp_tac.instructions = optimized_tac
        print(temp_tac)

        # 6. Target Code Generation
        target_gen = TargetCodeGenerator(optimized_tac)
        assembly_code = target_gen.generate()
        print("\n--- Final Generated Target Assembly Code ---")
        print(assembly_code)
        print("\nReady for new input...")

