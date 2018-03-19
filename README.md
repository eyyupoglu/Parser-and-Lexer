https://github.com/eyyupoglu/Parser-and-Lexer

# Parser-and-Lexer
Parser, lexer, Compiler and Interpreter School Project

This folder contains examples of "Simple Guarded Command Language" programs in F#. It evaluates if the input string is obeying the rules of the guarded command language or not. It is called a parser.


## Files

F#/FsLexYacc
* [lexer.fsl](lexer.fsl): The F# lexer for GCL expressions
* [parser.fsp](parser.fsp): The F# parser for GCL expressions
* [AST.fs](AST.fs): F# types for AST(Abstract Syntax Tree) of GCL expressions
* [program.fsx](program.fsx): The F# script for the GCL

## Instructions for F# #/FSLexYacc

This is instructions assume that you have followed the guidelines to [getting started with F# and FSLexYacc](getting-started-fs.md) and that you are using a terminal on a folder contaning the F# files mentioned above.

Invoke the lexer generator with 

```
FsLexYacc.7.0.6\build\fslex.exe lexer.fsl --unicode
```
Invoke the parser generator with
```
FsLexYacc.7.0.6\build\fsyacc.exe parser.fsp --module parser
```
Invoke the program.fsx with

```
fsi.exe program.fsx
```
or if it doesnt work
```
fsharpi.exe program.fsx
```

You should be able to interact with the program and give a possible GCL input and the program will give a feedback depending on the input. If it is valid it will say "ok", if not "ko".

TESTS
```
fsi.exe C:\Users\eyyup\source\repos\ConsoleApplication1\github\myLang\tests.fsx
```
