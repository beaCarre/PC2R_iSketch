%{
  open Protocol
%}
  
  
/* protocol */
%token CONNECT CONNECTED SPECTATOR WELCOME EXIT EXITED 
%token NEW_ROUND GUESS GUESSED WORD_FOUND WORD_FOUND_TIMEOUT END_ROUND
%token SCORE_ROUND PASS CHEAT 
%token SET_COLOR SET_LINE SET_SIZE LINE
%token TALK LISTEN SET_COURBE COURBE
  
%token <int> NUM
%token <string> IDENT
/* specials chars */
%token SLASH ASLASH NEWLINE
  
%token EOF
  
%start start
%type <Protocol.command> start
%type <Protocol.command> command
%type <string> ident_arg
%type <(string * int) list> liste_score
  
%left SLASH
  
%%
  start:
  | command NEWLINE EOF 
      { $1 }
  | command EOF 
      { $1 }

      command:
  | CONNECT SLASH ident_arg SLASH
      { Connect $3 }
  | CONNECTED SLASH ident_arg SLASH
      { Connected $3 }
  | SPECTATOR SLASH
      { Spectator }
  | WELCOME SLASH ident_arg SLASH
      { Welcome $3 }
  | EXIT SLASH ident_arg SLASH
      { Exit $3 }
  | EXITED SLASH ident_arg SLASH
      { Exited $3 }

  | NEW_ROUND SLASH ident_arg SLASH ident_arg SLASH
      { New_round ($3, $5, None) }
  | NEW_ROUND SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH
      { New_round ($3, $5, Some $7) }

  | GUESS SLASH ident_arg SLASH
      { Guess $3 }
  | GUESSED SLASH ident_arg SLASH ident_arg SLASH
      { Guessed ($3, $5) }
  | WORD_FOUND SLASH ident_arg SLASH
      { Word_found $3 }
  | WORD_FOUND_TIMEOUT SLASH ident_arg SLASH
      { Word_found_timeout (int_of_string $3) }

  | END_ROUND SLASH ident_arg SLASH
      { End_round (None, $3) } 
  | END_ROUND SLASH ident_arg SLASH ident_arg SLASH
      { End_round (Some $3, $5) }

  | SCORE_ROUND SLASH liste_score SLASH 
      { Score_round $3 }

  | PASS SLASH
      { Pass }
  | CHEAT SLASH ident_arg SLASH 
      { Cheat $3 }

  | SET_COLOR SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH
      { Set_color (int_of_string $3, int_of_string $5, int_of_string $7) }

  | SET_SIZE SLASH ident_arg SLASH
      { Set_size (int_of_string $3) }

  | SET_LINE SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH
      { Set_line ((int_of_string $3, int_of_string $5), (int_of_string $7, int_of_string $9))}

  | LINE SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH 
      ident_arg SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH
      { Line (((int_of_string $3,int_of_string $5),
	       (int_of_string $7,int_of_string $9)),
	      (int_of_string $11, int_of_string $13, int_of_string $15), 
	      int_of_string $17) }

  | TALK SLASH ident_arg SLASH 
      { Talk $3 }
  | LISTEN SLASH ident_arg SLASH ident_arg SLASH 
      { Listen ($3,$5)}

  | SET_COURBE SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH 
      ident_arg SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH
      { Set_courbe ((int_of_string $3, int_of_string $5), 
		    (int_of_string $7, int_of_string $9), 
		    (int_of_string $11, int_of_string $13), 
		    (int_of_string $15, int_of_string $17))}
      
  | COURBE SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH 
      ident_arg SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH ident_arg SLASH 
      ident_arg SLASH ident_arg SLASH ident_arg SLASH
      { Courbe (((int_of_string $3,int_of_string $5),
		 (int_of_string $7,int_of_string $9),
		 (int_of_string $11,int_of_string $13),
		 (int_of_string $15,int_of_string $17)),
		(int_of_string $19, int_of_string $21, int_of_string $23), 
		int_of_string $25) }
      
      ident_arg:
  | ident_arg ASLASH SLASH 
      {  $1 ^ "/" }
  | ident_arg ASLASH ASLASH 
      {  $1 ^ "\\" }
  | ASLASH SLASH ident_arg
      { "/" ^ $3 }
  | ASLASH ASLASH ident_arg
      { "\\" ^ $3 }
  | ident_arg ASLASH SLASH ident_arg
      { $1 ^ "/" ^ $4 }
  | ident_arg ASLASH ASLASH ident_arg
      { $1 ^ "\\" ^ $4 }
  | IDENT
      { $1 }

      liste_score:
  | ident_arg SLASH ident_arg SLASH liste_score
      { ($1, int_of_string $3)::$5 }
  | ident_arg SLASH ident_arg
      { [$1, int_of_string $3] }

%%
