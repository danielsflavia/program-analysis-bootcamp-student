%{
  open Ast
%}

%token <int> INT
%token <string> IDENT
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS
%left STAR SLASH
%nonassoc UMINUS

%start <Ast.expr> program

%%

program:
  | e = expr EOF { e }
  ;

expr:
  | e1 = expr PLUS  e2 = expr  { BinOp (Add, e1, e2) }
  | e1 = expr MINUS e2 = expr  { BinOp (Sub, e1, e2) }
  | e1 = expr STAR  e2 = expr  { BinOp (Mul, e1, e2) }
  | e1 = expr SLASH e2 = expr  { BinOp (Div, e1, e2) }
  | a = atom                   { a }
  ;

atom:
  | n = INT                        { Num n }
  | id = IDENT                     { Var id }
  | LPAREN e = expr RPAREN         { e }
  | MINUS a = atom %prec UMINUS    { Neg a }
  ;
