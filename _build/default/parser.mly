/* parser.mly */

%{
  open Ast
%}

%token SKIP IF THEN ELSE WHILE DO OBSERVE PRINT PRINT_WITH
%token ASSIGN SEMI
%token PLUS MINUS TIMES LT GT EQ
%token AND OR NOT XOR COMMA
%token LPAREN RPAREN
%token FLIP MAX MIN ABS
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> IDENT
%token EOF


%start <Ast.stmt> program

/* 运算符优先级（非常重要） */
%left PLUS MINUS TIMES  /* 算术运算：乘法优先级高于加减法 */
%left AND OR XOR        /* 布尔运算 */
%nonassoc LT GT EQ      /* 比较运算 */
%right NOT              /* 单目运算符 */
%right UMINUS           /* 一元负号 */

%%

stmts:
  | stmt { $1 }
  | stmts stmt { Seq ($1, $2) }

program:
  | stmts EOF { $1 }

stmt:
  | SKIP SEMI
      { Skip }

  | IDENT ASSIGN expr SEMI
      { Assign ($1, $3) }

  | IF expr THEN stmts ELSE stmts
      { If ($2, $4, $6) }

  | WHILE expr DO stmts
      { While ($2, $4) }

  | OBSERVE expr SEMI
      { Observe $2 }

  | PRINT expr SEMI
      { Print $2 }

  | PRINT_WITH STRING expr SEMI
      { PrintWith ($2, $3) }

expr:
  | INT
      { Int $1 }

  | MINUS expr %prec UMINUS
      { Neg $2 }

  | BOOL
      { Bool $1 }

  | IDENT
      { Var $1 }

  | expr PLUS expr
      { Add ($1, $3) }

  | expr MINUS expr
      { Sub ($1, $3) }

  | expr TIMES expr
      { Mul ($1, $3) }

  | expr LT expr
      { Lt ($1, $3) }

  | expr GT expr
      { Gt ($1, $3) }

  | expr EQ expr
      { Eq ($1, $3) }

  (* 布尔运算 *)
  | expr AND expr
      { And ($1, $3) }

  | expr OR expr
      { Or ($1, $3) }

  | expr XOR expr
      { Xor ($1, $3) }

  | NOT expr
      { Not $2 }

  (* 函数调用 *)
  | FLIP LPAREN FLOAT RPAREN
      { Flip $3 }

  | MAX LPAREN expr COMMA expr RPAREN
      { Max ($3, $5) }

  | MIN LPAREN expr COMMA expr RPAREN
      { Min ($3, $5) }

  | ABS LPAREN expr RPAREN
      { Abs $3 }

  | LPAREN expr RPAREN
      { $2 }
