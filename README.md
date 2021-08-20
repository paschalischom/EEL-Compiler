# EEL-Compiler
Fully fledged compiler created for the EEL language.

## The EEL Language
The <b>E</b>arly <b>E</b>xperimental <b>L</b>anguage was developed by professor George Manis during the spring semester of 2017-2018 for the course of [Compilers - MYY802](http://www.cse.uoi.gr/~manis/). Its grammar, which is of LL(1) type, comprises of 39 rules.

## How to run
The Compiler requires Python 3 in order to run. For it to compile a <b>.eel</b> file just type:
```
py Compiler.py /path/to/test.eel
```
For more help, type:
```
py Compiler.py -h
```
or
```
py Compiler.py -i
```
The compiler generates the following files within a subdirectory of the `results` folder named after the EEL file:
1. A <b>.int</b> file containing all the intermediate code.  
   - The intermediate code consists of <i>quads</i> which are a representation of the syntax of the EEL.
2. A <b>.txt</b> file a representation of the symbol table.  
   - The file visualises the different scopes that exist within the EEL code and prints the total framelength of the program.
3. A <b>.c</b> file containing the final compilable C code.  
   - All variables needed are declared in the beggining of the file. 
4. An <b>.asm</b> file containing the final executable assembly code.
   - The MIPS assembly code is compilable and executable. Download [MARS 4.5](https://courses.missouristate.edu/KenVollmar/mars/download.htm) in order to compile it.

**Currently the only report written on the project is in Greek... soon to follow in English and be more thorough.
