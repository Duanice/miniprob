/* parser.mly */

%{
  open Ast
%}

%token SKIP IF THEN ELSE WHILE DO OBSERVE
%token ASSIGN SEMI
%token PLUS MINUS LT EQ
%token LPAREN RPAREN
%token FLIP
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> IDENT
%token EOF


%start <Ast.stmt> program

/* 运算符优先级（非常重要） */
%left PLUS MINUS
%nonassoc LT EQ

%%

program:
  | stmt EOF { $1 }

stmt:
  | SKIP
      { Skip }

  | IDENT ASSIGN expr SEMI
      { Assign ($1, $3) }

  | stmt SEMI stmt
      { Seq ($1, $3) }

  | IF expr THEN stmt ELSE stmt
      { If ($2, $4, $6) }

  | WHILE expr DO stmt
      { While ($2, $4) }

  | OBSERVE expr SEMI
      { Observe $2 }

expr:
  | INT
      { Int $1 }

  | BOOL
      { Bool $1 }

  | IDENT
      { Var $1 }

  | expr PLUS expr
      { Add ($1, $3) }

  | expr MINUS expr
      { Sub ($1, $3) }

  | expr LT expr
      { Lt ($1, $3) }

  | expr EQ expr
      { Eq ($1, $3) }

  | FLIP LPAREN FLOAT RPAREN
      { Flip $3 }

  | LPAREN expr RPAREN
      { $2 }
