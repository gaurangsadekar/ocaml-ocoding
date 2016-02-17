{ open Parser }

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '=' { ASSIGN }
| ',' { SEQ }
| ['a'-'z'] as charlit { VARIABLE(int_of_char charlit - 97) }
| ['0'-'9']+ as numlit { LITERAL(int_of_string numlit) }
| eof { EOF }
