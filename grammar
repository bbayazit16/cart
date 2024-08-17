# Grammar

## Syntax

program        → declaration* EOF ;

### Declarations

declaration    → enumDecl
               | errorDecl
               | funcDecl
               | structDecl
               | extensionDecl
               | statement ;

enumDecl            → "enum" IDENTIFIER "{" enumVariantList? "}" ;
enumVariantList     → enumVariant ( "," enumVariant )* ;
enumVariant         → IDENTIFIER ( "(" enumTypeList ")" )?
                    | IDENTIFIER ( "{" structList "}" ) ;
enumTypeList        → type ( "," type )* ;

errorDecl           → "error" IDENTIFIER genericArgs? "{" errorVariantList* "}" ;
errorVariantList    → errorVariant ( "," errorVariant )* ;
errorVariant        → IDENTIFIER ( "(" structList ")" )? "=" STRING_LITERAL ;

funcDecl            → "func" function ;

structDecl          → "struct" IDENTIFIER genericArgs? "{" structList? "}" ;

extensionDecl       → "extension" IDENTIFIER genericArgs? "{" function* "}" ;

### Statements

statement      → exprStmt
               | letStmt
               | useStmt
               | returnStmt
               | forStmt
               | whileStmt
               | doWhileStmt ;

exprStmt       → expression ";" ;
letStmt        → "let" "mut"? IDENTIFIER ( ":" type )? "=" expression ";" ;
useStmt        → "use" IDENTIFIER ( "::" IDENTIFIER )* ( "::" "*" )? ";" ;
returnStmt     → "return" expression? ";" ;
forStmt        → "for" ( IDENTIFIER | "_" ) "in" expression block ;
whileStmt      → "while" expression block ;
doWhileStmt    → "do" block "while" expression ";" ;

### Expressions

expression     → assignment

assignment     → ( ( ifExpr "." ) | ( matchExpr "." ) | ( call "." ) )?
                 IDENTIFIER "=" expression
               | ifExpr ;

ifExpr         → "if" expression block 
                 ( "elif" expression block )*
                 ( "else" block )? 
               | matchExpr

matchExpr      → "match" expression "{" matchArmList? "}"
               | logicalOr ;
matchArmList   → matchArm ( "," matchArm )*
matchArm       → pattern "=>" expression ;

logicalOr      → logicalAnd ( "||" logicalAnd )* ;
logicalAnd     → equality ( "&&" equality )* ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;

unary          → ( "!" | "-" ) unary | call ;

call           → primary ( "." IDENTIFIER )* "(" arguments? ")"
               | primary ;

primary        → "true" | "false"
               | NUMBER | STRING | IDENTIFIER | "(" expression ")"
               | block | structLiteral | enumValue | errorValue
               | IDENTIFIER ( "." IDENTIFIER )+ ;

block          → "{" declaration* expression? "}" ;

structLiteral  → IDENTIFIER "{" structParamValues? "}" ;
enumValue      → IDENTIFIER ( "::" IDENTIFIER )? ( "(" arguments? ")" )? ;

errorValue     → IDENTIFIER ( "::" IDENTIFIER )? ( "(" structParamValues? ) ")" )? ;

### Types

type           → IDENTIFIER genericArgs?
               | "(" type? ")" ;
[//]: # (               DEPRECATED | type "|" type ;)

genericArgs    → "<" type ( "," type )* ">" ;

### Utility rules

function           → IDENTIFIER genericArgs? "(" parameters? ")" 
                     ( "->" type )? 
                     block ;
parameters         → parameter ( "," parameter )* ;
parameter          → IDENTIFIER ":" type ;
arguments          → expression ( "," expression )* ;
pattern            → IDENTIFIER ( ( "(" structList ")" )? | ( "{" structList "}" ) )
                   | "_" ; 
structParamValues  → structParamValue ( "," structParamValue )* ; 
structParamValue   → IDENTIFIER ":" expression ;
identList          → IDENTIFIER ( "," IDENTIFIER )* ;

[//]: # (Duplicate of parameters and parameter, but for a different context)
structList     → structField ( "," structField )* ;
structField    → IDENTIFIER ":" type ;

## Lexical Grammar

NUMBER         → DIGIT+ ( "." DIGIT+ )?
               | "0x" HEX_DIGIT+
               | "0o" OCT_DIGIT+
               | "0b" BIN_DIGIT+ ;
STRING         → "\"" <any char except "\"">* "\"" ;
IDENTIFIER     → ALPHA ( ALPHA | DIGIT | "_" )* ;
ALPHA          → "a" ... "z" | "A" ... "Z" ;
DIGIT          → "0" ... "9" ;
HEX_DIGIT      → DIGIT | "a" ... "f" | "A" ... "F" ;
OCT_DIGIT      → "0" ... "7" ;
BIN_DIGIT      → "0" | "1" ;

## Keywords

Reserved keywords that can't be used as identifiers:

use, enum, error, func, struct, extension, if, elif, else, match, for, in, while, do, let, mut, None, true, false, return