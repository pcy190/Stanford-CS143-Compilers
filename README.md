Cool Compiler 
===

Coursera Mooc:

https://www.coursera.org/course/compilers


## Program Assignment 1

> score : 63 / 63

> File : PA2/cool.flex

## Program Assignment 2

> score : 70 / 70

> File : PA3/cool.y

In order to finish this PA, we need to read section 6 of `cool-tour` which contains the Abstract Syntax Trees, along with the detailed API usage.

`Figure 1` of `The Cool Reference Manual` also helps me to design the grammar rule according to the syntax tree.

Bison manual also matters.

html manual:
https://www.gnu.org/software/bison/manual/bison.html

pdf manual:
https://www.gnu.org/software/bison/manual/bison.pdf

## Program Assignment 3

> score : 74 / 74

> File : PA4/

FAQ may helps.
https://www.eecis.udel.edu/~pollock/672/s04/faq.html

Page 17-22 of `cool-manual` describe the main idea of week 5 courses.

To annotate the AST, we design some interface to facilitate lookup.  
 - get_parent and get_type/check_type will works on a given node recursively
 - Traversal & LCA of a AST  

Once we have a clear relationship of the AST, we just build method_table and attribute_table
of for its symbol based on the Cool SymbolTable api.

Something Important to Check:
 -   whether these two types are ancestor relationship ( for inheritance/assign/dispatch check)
 -   whether some important Symbols are defined 
 -   whether method defination and its actual params matched
 -   whether the corresponding features of classes are legal
 -   whether the expressions type match its functional type
 -   Some symbols shouldn't be defined in the method formals, left_value, etc. 
