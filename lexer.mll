{ 
  open Parser
}

let slash = '/'
let aslash = '\\'

let ident = [^ '/' '\\']+
let num = ['0' - '9']+

let connect = "CONNECT"
let connected = "CONNECTED"
let welcome = "WELCOME"
let _exit = "EXIT"
let _exited = "EXITED"
let new_round = "NEW_ROUND"
let guess = "GUESS"
let guessed = "GUESSED"
let word_found = "WORD_FOUND"
let word_found_timeout = "WORD_FOUND_TIMEOUT"
let end_round = "END_ROUND"
let score_round = "SCORE_ROUND"
let pass  = "PASS"
let cheat = "CHEAT"
let set_color = "SET_COLOR"
let set_line = "SET_LINE"
let set_size = "SET_SIZE"
let line = "LINE"

let nl = ['\n' '\r']+

rule token = parse
  | nl
      {NEWLINE}

(*   | num as n
     { NUM (n >> int_of_string) (* si c'est des floats, traiter le cas. *)
*)
  | connect 
      {CONNECT}
  | connected
      {CONNECTED}
  | welcome
      {WELCOME}
  | _exit 
      {EXIT}
  | _exited
      {EXITED}
  | new_round
      {NEW_ROUND}
  | guess
      {GUESS}
  | guessed
      {GUESSED}
  | word_found
      {WORD_FOUND}
  | word_found_timeout
      {WORD_FOUND_TIMEOUT}
  | end_round
      {END_ROUND}
  | score_round
      {SCORE_ROUND}
  | pass
      {PASS}
  | cheat
      {CHEAT}
  | set_color 
      {SET_COLOR}
  | set_line
      {SET_LINE}
  | set_size 
      {SET_SIZE}
  | line
      {LINE}

  | slash
      {SLASH}
  | aslash
      {ASLASH}

  | ident as str 
      {IDENT(str)}

  | eof {EOF}

{
}
