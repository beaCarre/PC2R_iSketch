open Protocol
open Parser

(** types *)

type role =
  | Undefined
  | Drawer
  | Guesser 
      
type player_state =
  | Waiting
  | Playing

type player = {
  chan : Unix.file_descr;
  thread : Thread.t;
  name : string;
  mutable role : role;
  mutable state : player_state;
  (* T'es sûre qu'on en a besoin et qu'on doit pas juste filer le
     score total du joueur à la fin de chaque tour avec SCORE_ROUND ? *)
  mutable score_round : int;
  mutable score : int;
}

type color = {
  r : int;
  g : int;
  b : int;
} 

type line = {
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
  color : color;
  size : int;
}


type server = {
  mutable clients : player list;
  mutable lines : line list;
}

let server = {clients = []; lines = []}

(* tmps *)
let max = 2
let delay = 60
let timeout = 20

(** Utils *)

let parse_command (str : string) : Protocol.command =
  try
    let lexbuf = Lexing.from_string str in
    Parser.start Lexer.token lexbuf
  with
    | Parsing.Parse_error -> 
      Printf.eprintf "[Warning] Error at parsing : %s\n%!" str;
      Protocol.Malformed
    | Failure "int_of_string" -> 
      Printf.eprintf "[Warning] Error given a float expect an int : %s\n%!" str;
      Protocol.Malformed
    | _ -> 
      Printf.eprintf "[Warning] Unknown error in parsing : %s\n%!" str;
      Protocol.Malformed

let get_opt opt = match opt with Some r -> r | _ -> assert false 

let add_player player = 
  server.clients <- player::server.clients

let remove_player player = 
  server.clients <- List.filter ((!=) player) server.clients

let finalize_name name =
  let rec loop n name = 
    if List.exists (function {name=n; _} -> n = name) server.clients 
    then
      loop (n + 1) (Printf.sprintf "%s(%d)" name n)
    else 
      name
  in
  loop 1 name

let player_input_line fd =
  let s = " " and r = ref ""
  in while (ThreadUnix.read fd s 0 1 > 0) && s.[0] <> '\n' do r := !r ^s done ;
  (* debug *)
  Printf.printf "input_line : %s\n%!" !r;
  !r

let player_output_line fd str =
  ignore (ThreadUnix.write fd str 0 (String.length str))
(* Printf.printf " command send : %s\n%!" str*)

let send_command (fd : Unix.file_descr) (cmd : Protocol.command) : unit =
  player_output_line fd (string_of_command cmd)

let read_and_parse_line (fd : Unix.file_descr) : Protocol.command =
  parse_command (player_input_line fd)

class timer init_delay callback = 
object(self)
  val mutable time = init_delay 
  val mutable running = false

  val mutex_delay = Mutex.create ()
  val mutex_running = Mutex.create ()

  method start_count () = 
    running <- true;
    ignore (Thread.create 
	      (fun () -> 
		while time > 0 && running
		do
		  Thread.delay 1.0;
		  Mutex.lock mutex_delay;
		  time <- time - 1;
		  Mutex.unlock mutex_delay
		done;
		if running then callback ())
	      ())

  method restart_count () =
    self#stop_timer ();
    self#set_delay init_delay;
    self#start_count ()

  method get_current_delay = time
      
  method set_delay new_delay =
    Mutex.lock mutex_delay;
    time <- new_delay;
    Mutex.unlock mutex_delay

  method stop_timer () =
    Mutex.lock mutex_running;
    running <- false;
    Mutex.unlock mutex_running
end

let broadcast cmd =
  List.iter (fun p -> send_command p.chan cmd) server.clients

(** Logic *)

type round = {
  timer : timer;
  mutable drawer : player;
  mutable winner : string option;
  mutable word_to_find : string;
}

let current_round = ref None 
    
let rec play_round () =
  let round = get_opt !current_round in
  let drawer_name = round.drawer.name in

  (* Send NEW_ROUND requests *)
  List.iter 
    (function 
      | {chan=fd; role=Drawer; _} -> 
	send_command fd (New_round (drawer_name, drawer_name, Some round.word_to_find))
      | {chan=fd; name=name; _} -> send_command fd (New_round (drawer_name, name, None))) 
    server.clients;

  round.timer#restart_count ()
  

and next_round () =
  let round = get_opt !current_round in

  broadcast (End_round (round.winner, round.word_to_find));
  broadcast (Score_round (List.map (function {name=n; score=s; _} -> (n,s)) server.clients));

  (* Update drawer / guessers *)
  let rec update_roles l = 
  match server.clients with
    | ({role=Drawer; _} as curr_drawer)::next_drawer::t ->
      curr_drawer.role <- Guesser; next_drawer.role <- Drawer; 
      round.drawer <- next_drawer;
    | h::t -> 
      update_roles t
    | [] -> raise Pervasives.Exit (* Fin du jeu *)
  in
  try 
    update_roles server.clients;

    round.word_to_find <- "BLOU"; (* new_word () *)
    round.winner <- None;

    print_endline "Fin du tour"; (* debug *)
    Thread.delay 5.;
    print_endline "Début du nouveau tour"; (* debug *)
    play_round ();
  with 
    | Pervasives.Exit -> print_endline "End game"

(* Predicates *)

let can_guess = function
  | { role = Guesser; state = Playing; _} -> true
  | _ -> false

(* Handlers *)

let evaluate_word player word = 
  let round = get_opt !current_round in
  if round.word_to_find = word then begin
      match round.winner with
	| Some _ -> 
	  broadcast (Word_found (player.name))

	| None -> 
	  round.winner <- Some player.name;
	  
	  broadcast (Word_found (player.name));
	  broadcast (Word_found_timeout timeout);
	  round.timer#set_delay timeout
  (* todo : mise-à-jour des scores *)
  (* todo : si tout le monde a trouvé, on peut caler le timer à 1 pour
     passer dans next_round*)

  end else 
    broadcast (Guessed (player.name, word))
      

let player_scheduling player =
  let rec loop () =
    let cmd = read_and_parse_line player.chan in
    begin
      match cmd with
	| Connect _ -> 
	  Printf.eprintf "Player : %s asking for a connect. => Retarded client\n%!"
	    player.name
	| Guess word -> if can_guess player then evaluate_word player word

	(* | ... todo *)

	| p -> Printf.printf "Unhandled request %s\n" (string_of_command p)
    end;
    loop ()
  in
  try 
    loop ()
  with 
    (* todo handle proper exceptions. E.g : connection lost *)
    | _ -> Printf.eprintf "Connection lost - removing player : %s\n%!" player.name;
      remove_player player;
      Thread.exit ()

let start_playing () =
  (* Peut-être un mutex à caler dans cette fonction pour les états *)

  (* on dispose les clients par ordre d'arrivée *)
  server.clients <- List.rev server.clients;
  
  (* Tous deviennent joueur *)
  List.iter (fun p -> p.state <- Playing; p.role <- Guesser) server.clients;
  (* Le premier dessine, les autres devinent *)
  (List.hd server.clients).role <- Drawer;
  
  current_round := Some { timer= new timer delay next_round;
			  drawer= List.hd server.clients;
			  winner = None;			  
			  word_to_find= "bla" (* new_word () *) };
  play_round ()
	
(** Connections *)

let await_connect sock_descr = 
  let rec loop () =  
      try
	let cmd = read_and_parse_line sock_descr in
	match cmd with
	  | Protocol.Connect(name) -> 
	    let name = finalize_name name in
	    send_command sock_descr (Connected name);
	    name
	  | _ -> loop () (* si il envoie pas connect, on boucle..? *)
      with
	(* todo exhaustive error patterns - socket failed etc *)
	| _ -> Printf.eprintf "Client connection failed - discarded...\n%!"; raise Pervasives.Exit
    in
    loop ()	  

let start_player player_name sock_descr =
  let player = 
    { chan = sock_descr
    ; thread = Thread.self ()
    ; name = player_name
    ; role = Undefined
    ; state = Waiting
    ; score_round = 0
    ; score = 0
    } in
  add_player player;
  if List.length server.clients = max then 
    ignore (Thread.create start_playing ());
  player_scheduling player

let init_new_player sock_descr = 
  try 
    let name = await_connect sock_descr in
    ignore (Thread.create (start_player name) sock_descr)
  with 
    | Pervasives.Exit -> ()
      
let start_server port =
  let sock = ThreadUnix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.inet_addr_of_string "127.0.0.1" in
  (* (Unix.gethostbyname(Unix.gethostname()).Unix.h_addr_list.(0)) in *)	
  Unix.bind sock (Unix.ADDR_INET(addr,port));
  Unix.listen sock 3;
  while true do
    let sd, _ = ThreadUnix.accept sock in
    begin
      Unix.setsockopt sd Unix.SO_REUSEADDR true;
      ignore (Thread.create init_new_player sd);
      print_endline "th client lancé"
    end
  done


let () = 
  print_endline 
"################################################################
\tSuper server two thousand : online
################################################################";
  start_server 2013
