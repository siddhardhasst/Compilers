
This repository contains all the works done by:-


1. Siddhardha SST	(111501028)


2. Amish Ranjan	(111501032)


for the project under compiler course.


This repository contains the complete front end compiler for Tiger language.
(except for one exception -> comma to seperate expressions)


There are three folders 


1. Lexer 


2. Parser


3. Abstract Syntax Tree


To check the complete front end:-


1. Open parser folder, run sml command in that.


2. Do CM.make "sources.cm"


3. Parse.parse "test.tig" would let you check the complete front end. It would produce an abstract syntax tree for the program.


There are some codes that are directly used from Tiger book like error.sml for printing error.


Type checking tried but there are still some bugs.


Code Generation written for single "if expression" and single "infix expresion(plus, minu, times, divide, equal, not equal ,less than....)" . We are generating C code from Tiger language. 


To Check codegeneration part:-


1.Store the output of parser in some variable like $val x = Parse.parse "test.tig"  


2.Run codegeneration file using  $use "code_generation.sml";


3.Run convert_expression(x);

