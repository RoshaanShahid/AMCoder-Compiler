AMCoder Compiler
Date: June 25, 2025

Introduction
This project is a complete, multi-phase compiler for a custom, C-like programming language called AMCoder. It takes high-level source code written in AMCoder and compiles it all the way down to a simple, custom assembly language, performing lexical, syntax, and semantic analysis, as well as intermediate code generation and optimization along the way.

The project is built in Python and demonstrates the fundamental stages of compiler construction.

Features
Full Compiler Pipeline: Implements all major stages from frontend to backend.

Custom Language: Defines and processes a unique language, "AMCoder," with its own keywords and syntax.

Detailed Error Reporting: Provides clear, actionable error messages with line and column numbers for syntax and semantic errors.

Scope Management: A robust Symbol Table that correctly manages global and local (block-level) scopes.

Code Optimization: Includes a multi-pass optimizer that performs constant folding and dead code elimination.

Interactive Driver: A main driver (main.py) that allows users to input code and see the output from every stage of the compilation process.

Compiler Architecture
The compiler is designed as a pipeline, where the output of one phase becomes the input for the next. The architecture is as follows:

Lexical Analyzer (Lexer)

Methodology: Regular Expression-Based

Function: Reads the raw AMCoder source code and converts it into a stream of tokens (e.g., IDENTIFIER, KEYWORD, INTEGER_LITERAL).

Syntax Analyzer (Parser)

Methodology: Recursive Descent Parser

Function: Takes the token stream and verifies that it conforms to the language's grammar rules. It builds an Abstract Syntax Tree (AST) to represent the code's hierarchical structure.

Semantic Analyzer

Methodology: Syntax-Directed, Tree-Traversal using the Visitor Pattern

Function: Walks the AST to check for semantic correctness (the "meaning" of the code). It uses a Symbol Table to perform type checking, scope checking, and ensure variables are declared before use.

Intermediate Code Generator (ICG)

Methodology: Generates Three-Address Code (TAC) in the form of Quadruples.

Function: Traverses the AST and converts it into a machine-independent intermediate representation. Each instruction has at most three "addresses" (e.g., t1 = x + 5).

Code Optimizer

Methodology: Multi-Pass, Local Optimizer

Function: Improves the generated Three-Address Code by applying techniques like Constant Folding and Dead Code Elimination.

Target Code Generator

Methodology: Simple TAC-to-Assembly Translator

Function: Converts the optimized TAC into a final, low-level custom assembly language, performing basic register allocation in the process.

How to Run
The entire compiler pipeline can be run from the main driver script.

Open a terminal or command prompt.

Navigate to the root directory of the project.

Run the main.py script:

python main.py

The program will prompt you to enter your AMCoder source code.

After typing your code, type ENDCODE on a new line to start the compilation.

The output from every stage of the compiler will be printed to the console.

AMCoder Language Syntax
AMCoder is a simple imperative language. Here are some examples of its syntax:

Variable Declaration:

AMint my_variable;
AMfloat another_variable;

Assignment:

my_variable = 100;
another_variable = my_variable + 5 * 2;

Control Flow:

startIf (my_variable) {
   // Code to run if my_variable is not 0
} otherwise {
   // Code to run otherwise
}

AMwhile (my_variable) {
   my_variable = my_variable - 1;
}

File Structure
main.py: The main driver for the compiler. Run this file to start the interactive compiler.

Lexical_Analyzer.py: Contains the Lexer class for tokenizing source code.

syntax_analyzer.py: Contains the Parser and Node classes for building the AST.

semantic_analyzer.py: Contains the SemanticAnalyzer and SymbolTable classes.

intermediate_code_generator.py: Contains the ICG class for generating Three-Address Code.

code_optimizer.py: Contains the Optimizer class.

target_code_generator.py: Contains the TargetCodeGenerator class.

README.md: This file.