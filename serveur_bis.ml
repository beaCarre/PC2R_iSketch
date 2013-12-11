open Protocol
open Parser

(** types *)

exception Closed_connection

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
  mutable already_draw : bool;
  mutable has_found : bool;
  mutable state : player_state;
  mutable score_round : int;
  mutable score : int;
}

type color = {
  mutable r : int;
  mutable g : int;
  mutable b : int;
} 

(*
type line = {
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
  color : color;
  size : int;
}*)

type server = {
  mutable players : player list;
  mutable mots_rounds : string list;
  mutable is_game_started : bool;
  mutable commandes : Protocol.command list
}

let server = {players = [];
	      mots_rounds=[]; 
	      is_game_started=false;
	      commandes = []}


(*************   Timer   *************)

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

(*************   types part2   *************)

type round = {
  timer : timer;
  mutable drawer : player option;
  mutable winner : string option;
  mutable word_to_find : string;
  mutable cpt_found : int;
  mutable nb_cheat_report : int;
  mutable color : color;
  mutable size : int;
}

let current_round = ref None 

(*************   Args   *************)

let timeout = ref 20
let max = ref 2
let fdico = ref "dico.txt"
let port = ref 2013
let nbReport = ref 3

let dico = ref []
let delay = 60

(*************   Utils_args   *************)

let charger_dico filename =
  let chan = open_in filename in
    begin
      try 
	while true do
	  let mot = input_line chan in
	    dico := mot::!dico
	done  
      with | End_of_file -> close_in chan
    end
      
let parse_args () =
  let speclist = [("-timeout", Arg.Set_int timeout, "set timeout");
		  ("-max", Arg.Set_int max, "set max");
		  ("-dico", Arg.Set_string fdico, "set fdico");
		  ("-port", Arg.Set_int port, "set port");
		  ("-n", Arg.Set_int nbReport, "set nbReport")]    
  in let usage_msg = "Options available:"
  in Arg.parse speclist print_endline usage_msg;
    charger_dico !fdico


(*************   Utils   *************)

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
  server.players <- player::server.players

let remove_player player = 
  server.players <- List.filter ((!=) player) server.players

let new_word () =
  let mot = ref "" in
  let rec loop () =
    let i = Random.int (List.length !dico) in
      if (List.exists (fun m -> m = (List.nth !dico i)) server.mots_rounds) then
	loop ()
      else
	begin
	  mot := List.nth !dico i;
	  server.mots_rounds <- !mot::server.mots_rounds;
	  !mot
	end
  in loop()

let finalize_name name =
  let rec loop n name =
    if List.exists(function p -> p.name = name) server.players
    then
      loop (n + 1) (Printf.sprintf "%s(%d)" name n)
    else 
      name
  in
    loop 1 name
      
let player_input_line fd =
  let s = " " and r = ref "" in
    while (ThreadUnix.read fd s 0 1 > 0) && s.[0] <> '\n' do r := !r ^s done ;
    if String.length !r = 0 then 
      raise Closed_connection;
    !r
      
let player_output_line fd str =
  ignore (ThreadUnix.write fd str 0 (String.length str));
  Printf.printf " command send : %s\n%!" str

let send_command (fd : Unix.file_descr) (cmd : Protocol.command) : unit =
  player_output_line fd (string_of_command cmd)

let read_and_parse_line (fd : Unix.file_descr) : Protocol.command =
  parse_command (player_input_line fd)

let broadcast cmd =
  List.iter (fun p -> send_command p.chan cmd) server.players


(*************   Gestion partie   *************)
    
let rec play_round () =
  let round = get_opt !current_round in
  let drawer = get_opt round.drawer in
  let drawer_name = drawer.name in
    List.iter 
      (function 
	 | {chan=fd; role=Drawer; } -> 
	     send_command fd (New_round (drawer_name, drawer_name, Some round.word_to_find))
	 | {chan=fd; name=name; } -> send_command fd (New_round (drawer_name, name, None))) 
      server.players;

    round.timer#restart_count ()

and next_round () =
  let round = get_opt !current_round in
    broadcast (End_round (round.winner, round.word_to_find));
    if round.nb_cheat_report < !nbReport then
      begin 
	let liste_score = (List.map (fun {name=n; score_round=s;} -> (n,s)) server.players)in
	  List.iter (fun (n,s) -> print_endline (n^" : "^(string_of_int s))) liste_score;
	  broadcast (Score_round liste_score)
      end
    else (* cheat ! *)
      broadcast (Score_round (List.map (function {name=n; } -> (n,0)) server.players));
	
    (* Update drawer / guessers *)
    let rec update_roles l = 
      match l with
	| ({role=Drawer; } as curr_drawer)::t ->
	    curr_drawer.role <- Guesser; 
	    curr_drawer.already_draw <- true;
	    update_roles t
	| ({already_draw =false;} as next_drawer)::t ->
	    next_drawer.role <- Drawer;
	    (get_opt !current_round).drawer <- Some next_drawer;
	| h::t -> 
	    update_roles t
	| [] -> raise Pervasives.Exit (* Fin du jeu *)
    in
      try 
	List.iter (fun c -> c.has_found <- false) server.players;
	update_roles server.players;
	round.word_to_find <- new_word();
	round.winner <- None;
	round.cpt_found <- 0;
	round.nb_cheat_report <- 0;
	round.color.r<-0;round.color.g<-0; round.color.b<-0;
	round.size<-0;
	print_endline "Fin du tour"; (* debug *)
	Thread.delay 2.;
	print_endline "Debut du nouveau tour"; (* debug *)
	play_round ();
      with 
	| Pervasives.Exit -> print_endline "End game"
	    
(**********   Predicates   **********)

let can_guess = function
  | { role = Guesser; state = Playing; } -> true
  | _ -> false
let all_has_found () = 
  List.for_all (fun c -> c.role = Drawer || c.has_found = true ) server.players

(* Handlers *)

let give_score player round =
  match round.cpt_found with
    | 1 -> player.score_round <- 10;
	begin
	  try 
	    let drawer = get_opt round.drawer in 
	      drawer.score_round <-10 
	  with | _ -> () (* drawer a quitté la partie ou a pass*)
	end
    | n ->  player.score_round <- Pervasives.max (11 - n) 5;
	try 
	  let drawer = get_opt round.drawer in 
	    drawer.score_round <- drawer.score_round + 1;
	with | _ -> () (* drawer a quitté la partie ou a pass*)
	 
	  
	  
let evaluate_word player word = 
  let round = get_opt !current_round in
    if round.word_to_find = word then 
      begin (* mutex round ? *)
	round.cpt_found <- round.cpt_found + 1;
	give_score player round;
	match round.winner with
	  | Some _ ->
	      broadcast (Word_found (player.name));
	      player.has_found <- true;
	      if all_has_found () then
		round.timer#set_delay 1 
	  | None ->
	      round.winner <- Some player.name;
	      player.has_found <- true;
	      broadcast (Word_found (player.name));
	      if all_has_found ()
	      then
		round.timer#set_delay 1 
	      else
		begin
		  round.timer#set_delay !timeout;
		  broadcast (Word_found_timeout !timeout)
		end
      end 
    else 
      broadcast (Guessed (player.name, word))
	
let treat_exit player =
  Printf.printf "treat_exit %s\n%!" player.name;
  Unix.close player.chan;
  remove_player player;
  Thread.exit ();
  broadcast (Exited player.name)
    
let evaluate_pass player =
  (* mutex round ? *)
  let round = get_opt !current_round in
    if player.role = Drawer && round.winner = None  then
      begin
	round.timer#set_delay 1;
	round.drawer <- None;
      end
	
let evaluate_exit player name =
  if name = player.name then
    begin
      evaluate_pass player;
      treat_exit player
    end
	
let evaluate_cheat player name =
  if name = player.name then
    begin 
      let round = get_opt !current_round in
	match round.drawer with
	  | None -> () (* cheat : drawer deja parti*)
	  | Some drawer ->
	      round.nb_cheat_report <- round.nb_cheat_report +1;
	      if round.nb_cheat_report >= !nbReport then
		begin
		  round.timer#set_delay 1;
		  round.drawer <- None;
		  treat_exit drawer;
		end
    end

let evaluate_set_line x1 y1 x2 y2 =
  let round = get_opt !current_round in
  let color = round.color in
    broadcast ( Line (((x1, y1),(x2, y2)), (color.r, color.g, color.b), round.size))


let player_scheduling player =
  let rec loop () =
    let cmd = read_and_parse_line player.chan in
      begin
	match cmd with
	  | Connect name ->
	      Printf.eprintf 
		"Player : %s asking for a connect. => Retarded client\n%!"
		player.name

	  | Exit name -> evaluate_exit player name

	  | Guess word -> 
	      if can_guess player then evaluate_word player word
	      else print_endline "cannot guess"
	  | Set_color (r, g, b) -> 
	      let round = get_opt !current_round in
		round.color.r <- r;
		round.color.g <- g;
		round.color.b <- b
	      	      
	  | Set_line ((x1, y1),(x2, y2)) -> evaluate_set_line x1 y1 x2 y2
	      
	  | Set_size s ->  let round = get_opt !current_round in round.size <- s;
	      
	  | Pass -> evaluate_pass player
	      
	  | Cheat name -> evaluate_cheat player name

	  | Talk message -> broadcast (Listen (player.name, message))

	  | Set_courbe  ((x1, y1),(x2, y2), (x3, y3),(x4, y4)) ->
	      let round = get_opt !current_round in
	      let c = round.color in
		broadcast (Courbe (
			     ((x1, y1),(x2, y2), (x3, y3),(x4, y4)), 
			     (c.r, c.g, c.b), round.size))
		  
	  | p -> Printf.printf "Unhandled request %s\n" (string_of_command p)
      end;
      loop ()
  in
    try 
      loop ()
    with 
	(* todo handle proper exceptions. E.g : connection lost *)
      | Closed_connection -> 
	  Printf.eprintf "[Warning] Connection lost with player : %s - removing him\n%!" player.name;
	  evaluate_exit player player.name

let start_playing () =
  (* Peut-etre un mutex Ã  caler dans cette fonction pour les etats *)

  (* Tous deviennent joueur *)
  List.iter (fun p -> p.state <- Playing; p.role <- Guesser) server.players;
  (* Le premier dessine, les autres devinent *)
  (List.hd server.players).role <- Drawer;
  
  current_round := Some { timer= new timer delay next_round;
			  drawer= Some (List.hd server.players);
			  winner = None;			  
			  word_to_find= new_word ();
			  cpt_found = 0;
			  nb_cheat_report = 0;
			  color = {r=0;g=0;b=0};
			  size = 0};
  play_round ()
    
(** Connections *)

let await_connect sock_descr = 
  let rec loop () =
    let cmd = read_and_parse_line sock_descr in
      match cmd with
	| Connect name when not server.is_game_started -> 
	    let name = finalize_name name in
	      send_command sock_descr (Welcome name);
	      
	      name
		(*  | Spectator name -> *)
	| _ -> loop ()
  in
    loop ()

let start_player player_name sock_descr =
  let player = 
    { chan = sock_descr
    ; thread = Thread.self ()
    ; name = player_name
    ; role = Undefined
    ; already_draw = false
    ; has_found = false
    ; state = Waiting
    ; score_round = 0
    ; score = 0
    } in
    add_player player;
    broadcast (Connected player_name);
    if List.length server.players = !max then begin
      ignore (Thread.create start_playing ());
      server.is_game_started <- true;
    end;
    player_scheduling player
      


let init_new_player sock_descr =
  try 
    let name = await_connect sock_descr in
      start_player name sock_descr
  with 
    | Closed_connection -> Printf.eprintf "[Warning] Aborted connection\n%!"; Unix.close sock_descr
	
let start_server port =
  let sock = ThreadUnix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.inet_addr_any in
    (*Unix.inet_addr_of_string "127.0.0.1" in*)
    (* (Unix.gethostbyname(Unix.gethostname()).Unix.h_addr_list.(0)) in *)	
    Unix.bind sock (Unix.ADDR_INET(addr,port));
    Unix.listen sock 3;
    while true do
      let sd, _ = ThreadUnix.accept sock in
	begin
	  Unix.setsockopt sd Unix.SO_REUSEADDR true;
	  ignore (Thread.create init_new_player sd);
	  print_endline "th client lance"
	end
    done


let () = 
  print_endline 
    "################################################################
\tSuper server two thousand : online
################################################################";
  parse_args ();
  start_server 2013
