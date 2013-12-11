open Printf
open Str

type command = 
  | Connect of string
  | Connected of string
  | Welcome of string
  | Exit of string
  | Exited of string
  | New_round of string * string * string option
  | Guess of string
  | Guessed of string * string
  | Word_found of string
  | Word_found_timeout of int
  | End_round of string option * string 
  | Score_round of (string * int) list
  | Pass
  | Cheat of string
  | Set_color of int * int * int
  | Set_line of (int*int)*(int*int)
  | Set_size of int
  | Line of ((int*int)*(int*int)) * (int * int * int) * int
  | Talk of string
  | Listen of string * string
  | Set_courbe of (int*int)*(int*int)*(int*int)*(int*int)
  | Courbe of ((int*int)*(int*int)*(int*int)*(int*int)) * (int * int * int) * int
  | Malformed

(**
   [secure x] ajoute les sécurités dans la chaine x   
   toto/ => toto\/
   toto/\ => toto\/\\
   toto/\ => toto/\\ => toto\/\\
   toto\/ => toto\\/ => toto\\\/
*)
let reg_aslash = regexp "\\\\"
let reg_slash = regexp "/"

let secure str =   
  let str = global_replace reg_aslash "\\\\\\\\" str in
  global_replace reg_slash "\\/" str 

let string_of_command = function
  | Connect n -> sprintf "CONNECT/%s/\n" (secure n)

  | Connected n -> sprintf "CONNECTED/%s/\n" (secure n)

  | Welcome n -> sprintf "WELCOME/%s/\n" (secure n)

  | Exit n -> sprintf "EXIT/%s/\n" (secure n)

  | Exited n -> sprintf "EXITED/%s/\n" (secure n)

  | New_round (a, b, Some c) ->  sprintf "NEW_ROUND/%s/%s/%s/\n" (secure a) (secure b) (secure c)

  | New_round (a, b, None) -> sprintf "NEW_ROUND/%s/%s/\n" (secure a) (secure b)

  | Guess n -> sprintf "GUESS/%s/\n" (secure n)

  | Guessed (a, b) -> sprintf "GUESSED/%s/%s/\n" (secure a) (secure b)

  | Word_found n -> sprintf "WORD_FOUND/%s/\n" (secure n)

  | Word_found_timeout t -> sprintf "WORD_FOUND_TIMEOUT/%d/\n" t

  | End_round (Some a, b) -> sprintf "END_ROUND/%s/%s/\n" (secure a) (secure b)
  | End_round (None, b) -> sprintf "END_ROUND/%s/\n" (secure b)

  | Score_round l -> 
      "SCORE_ROUND/" ^ 
	List.fold_left 
	(fun acc (a,b) -> acc ^ secure a ^ "/" ^ (string_of_int b) ^ "/") "" l ^ "\n"

  | Pass -> "PASS/\n"

  | Cheat n -> sprintf "CHEAT/%s/\n" (secure n)

  | Set_color (r, g, b) -> sprintf "SET_COLOR/%d/%d/%d/\n" r g b
      
  | Set_line ((x,y), (u,v)) -> sprintf "SET_LINE/%d/%d/%d/%d/\n" x y u v
      
  | Set_size s -> sprintf "SET_SIZE/%d/\n" s 

  | Line (((x,y),(u,v)), (r, g, b), s) -> 
      sprintf "LINE/%d/%d/%d/%d/%d/%d/%d/%d/\n" x y u v r g b s

  | Talk n -> sprintf "TALK/%s/\n" (secure n)

  | Listen (a, b) -> sprintf "LISTEN/%s/%s/\n" (secure a) (secure b)

  | Set_courbe ((x1,y1), (x2,y2), (x3,y3), (x4,y4)) -> 
      sprintf "SET_COURBE/%d/%d/%d/%d/%d/%d/%d/%d/\n" x1 y1 x2 y2 x3 y3 x4 y4

  | Courbe (((x1,y1), (x2,y2), (x3,y3), (x4,y4)), (r, g, b), s) ->
      sprintf "COURBE/%d/%d/%d/%d/%d/%d/%d/%d/%d/%d/%d/%d/\n" 
	x1 y1 x2 y2 x3 y3 x4 y4 r g b s

  | Malformed -> "error"
