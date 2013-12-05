
(* gerer args avec module Arg *)
(* gerer client a la place des sd !!!! *)

(********************** types et var **********************)

(*gestion client*)
type role =
  | DRAWER
  | GUESSER 
      
type etat =
  | WAITING
  | PLAYING
  | EXITED

type client = {
  chan : Unix.file_descr;
  name : string ;
  mutable role : role ;
  mutable etat : etat;
  mutable score_round : int;
  mutable score : int;
}

let clients = ref []

let clients_mutex = Mutex.create()


(*gestion graphique*)
type line = {
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
}
let lines = ref []

type color = {
  mutable r : int;
  mutable g : int;
  mutable b : int;
} 

let color = {r=0; g=0; b=0}

let size = ref 0

(*var round*)
let mots_round = ref []
let mot = ref ""
let cpt_found = ref 0

(* timers *)
let timerWF = ref None 
let timerR = ref None

(*serv_addr*)
let serv_addr = ref (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0)

(********************** gestion options **********************)
  
let charger_dico filename =
  let dico = ref [] in
  let chan = open_in filename in
  begin
    try 
      while true do
	let mot = input_line chan in
	dico := mot::!dico
      done  
    with  
      | End_of_file -> close_in chan
  end;
  !dico
    
let get_option option defaut =
  let res = ref defaut in
  for i=1 to (Array.length Sys.argv)-1 do
    if  (Sys.argv.(i) = option) && (i < (Array.length Sys.argv))
    then
      res := Sys.argv.(i+1)
  done;
  !res

let timeRound = 20

let timeout = int_of_string (get_option "-timeout" "10")
let max = int_of_string (get_option "-max" "2")
let fdico = get_option "-dico" "dico.txt"
let port = int_of_string (get_option "-port" "2013")
let nbReport = int_of_string (get_option "-n" "3")

let dico = charger_dico fdico



(********************** tools **********************)

let client_from_fd sd =
  List.find (fun c -> c.chan = sd ) !clients

let my_input_line fd =
  let s = " " and r = ref ""
  in while (ThreadUnix.read fd s 0 1 > 0) && s.[0] <> '\n' do r := !r ^s done ;
  Printf.printf "input_line : %s\n%!" !r;
  !r
    
let my_output_line fd str =
  ignore (ThreadUnix.write fd str 0 (String.length str))
(* Printf.printf " command send : %s\n%!" str*)

let stop_thread_client sd =
  print_endline "stop ! \n%!";
  Mutex.lock clients_mutex;
  clients := List.filter (fun c -> c.chan <> sd) !clients;
  Mutex.unlock clients_mutex;
  Unix.close sd;
  List.iter 
    (fun c -> my_output_line c.chan (Printf.sprintf "EXITED/%s/\n%!" (client_from_fd sd).name) )
    !clients;
  Thread.exit ()

let rec init_mot () =
  let i = Random.int (List.length dico) in
  if (List.exists (fun m -> m = (List.nth dico i)) !mots_round) then
    init_mot ()
  else
    begin
      mot := List.nth dico i;
      mots_round := !mot::!mots_round
    end

(* broadcast :an Protocol.command -> unit *)
let broadcast cmd =
  List.iter (fun c -> my_output_line c.chan (Protocol.string_of_command cmd)) !clients

(* parse_command : string -> Protocol.command *)
let parse_command str =
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

let end_round () =
  print_endline "end_round !";
  let liste_scores = ref [] in
  List.iter (fun c -> 
    liste_scores := (c.name,c.score_round)::!liste_scores;
    my_output_line c.chan (Protocol.string_of_command (Protocol.Score_round( !liste_scores ))) ;
    (* envoyer end round avant avec nom gagnant *)
    c.etat <- WAITING ;
    c.score <- c.score+(c.score_round);
    c.score_round <- 0
  )
    !clients
(* TOfinish *)
	
(*timers*)

let timer_word_found () =
  print_endline "deb timerWF";
  Unix.sleep timeout;
  match !timerWF with
    | Some th ->
      if th = Thread.self () then
        begin
          Printf.printf "Fin des %d secondes du mot trouv�\n%!" timeout;
	  begin
	    match !timerR with
	      | Some (t) -> Thread.kill t; 
	      | None -> print_endline "TimerR deja None : CHELOU !"
	  end;
	  if !timerR = None then print_endline "kill => None !! chelou";
	  timerR := None;
	  begin
	    match !timerWF with
	      | Some (t) -> Thread.kill t; 
	      | None -> print_endline "TimerR deja None :: CHELOU !"
	  end;
	  if !timerR = None then print_endline "kill => None !! chelou";
	  timerWF := None;
	  end_round ()
        end
      else
        Printf.printf "timer_word_found inutile fini\n"
    | _ -> Printf.printf "Timer_word_found inutile fini\n"
      
      
let timer_round () =
  print_endline "deb timer";
  Unix.sleep timeRound;
  print_endline "fin sleep";
  match !timerR with
    | Some th ->
      if th = Thread.self () then
        begin
          Printf.printf "Fin des %d minutes, la round est fini\n%!" timeRound;
	  begin
	    match !timerR with
	      | Some (t) -> Thread.kill t; 
	      | None -> print_endline "TimerR deja None : CHELOU !"
	  end;
	  if !timerR = None then print_endline "kill => None !! chelou";
	  timerR := None;
	  begin
	    match !timerWF with
	      | Some (t) -> Thread.kill t; 
	      | None -> print_endline "TimerR deja None :: CHELOU !"
	  end;
	  if !timerR = None then print_endline "kill => None !! chelou";
	  timerWF := None;
	  end_round()
        end
      else
        Printf.printf "Timer_round inutile fini\n"
    | _ -> Printf.printf "Timer_round inutile fini\n"



	     (********************** gestion commandes **********************)

let drawer_exit client =
  end_round ();
  stop_thread_client client.chan
(*TODO traitement sortie dessinateur*)

let set_line  x y u v =
  lines := {x1= x; y1= y; x2= u; y2= v}::!lines;
  List.iter (fun c -> my_output_line c.chan 
    (Printf.sprintf "LINE/%d/%d/%d/%d/%d/%d/%d/%d/\n%!" x y u v color.r  color.g  color.b !size )) 
    !clients;
  print_endline (Printf.sprintf "set_line %d/%d/%d/%d\n%!" x y u v)
    
let guess client proposition (*nth_found*) =
  print_endline (Printf.sprintf "%s a propos� le mot %s\n%!" client.name proposition);
  (*TODO*)
  let nb_guessers = List.length !clients -1 in
  if proposition = !mot then
    begin
      Mutex.lock clients_mutex;
      my_output_line client.chan (Protocol.string_of_command (Protocol.Guess proposition));
      incr cpt_found;
      begin
	match !cpt_found with
	  | 1 -> timerWF := Some (Thread.create timer_word_found ());
	    broadcast (Protocol.Word_found_timeout timeout );
	    client.score_round <- 10
	  | nb_guessers -> 
	    broadcast (Protocol.Word_found client.name);
	    client.score_round <- min (10 - nb_guessers-1) 5;
	    end_round ()
	  | n -> broadcast (Protocol.Word_found client.name);
	    client.score_round <- min (10-n-1) 5;
      end;
      Mutex.unlock clients_mutex
    end
      
  (*incr nth_found
    match ordre_found with
    1 -> timerWF := Some (Thread.create timer_word_found () )
    | (List.length !clients - 1) -> end_round ()(*nth_found*)*)
  

(********************** gestion jeu **********************)

let start_round namedrawer =
  timerR := None ;(* util?*)
  init_mot ();
  List.iter (fun c -> c.role <- ( if c.name=namedrawer then DRAWER else GUESSER)) !clients;
  List.iter 
    (fun c -> 
      my_output_line c.chan ( 
	if c.name = namedrawer
	then (Printf.sprintf "NEW_ROUND/%s/%s/\n" namedrawer !mot)
	else (Printf.sprintf "NEW_ROUND/%s/\n" namedrawer ))
    )
    !clients;
  timerR := Some (Thread.create timer_round ())
      

    
let start_game () =
  List.iter (fun c -> c.etat <- PLAYING) !clients;
  List.iter (fun c -> start_round c.name) !clients
			

let th_joueur client =
  while true do
    let commande_recue = my_input_line client.chan in
    let commande = parse_command commande_recue in
    match client.role , commande with
      | GUESSER, Protocol.Exit(user) 
	-> stop_thread_client client.chan
      | DRAWER, Protocol.Exit(user)
	-> drawer_exit client
      | GUESSER, Protocol.Guess(word)
	-> guess client word ;
      | DRAWER, Protocol.Set_color(r,g,b)
	-> color.r <- r; 
	  color.g <- g; 
	  color.b <- b
      | DRAWER, Protocol.Set_line((x1,y1),(x2,y2))
	-> set_line x1 y1 x2 y2
      | DRAWER, Protocol.Set_size(s)
	-> size := s
      | r,cmd ->
	print_endline (if r=GUESSER then "guesser :" else "drawer :");
	print_endline ("commande recue non traitee:"^(Protocol.string_of_command cmd))
  done
    
let th_spectator spectator =
  ()

let treat_connexion user sd =
  Mutex.lock clients_mutex;
  let client = {name=user;chan=sd;role=GUESSER;etat=WAITING;score_round=0;score=0} in
  clients:=client::!clients;
  my_output_line sd (Printf.sprintf "CONNECTED/%s/\n%!" user);
  if List.length (List.filter (fun c -> c.etat = WAITING) !clients) = max
  then
    start_game ()(* clients ? *)
  else print_endline "attente des autres";
  Mutex.unlock clients_mutex;
  client

let rec rename name i=
  if List.exists ( fun c -> c.name = name ) !clients then
    rename (name^"("^string_of_int (i+1)^")") i
  else 
    name

let connexion_client sd sa =
  let recue = my_input_line sd in
  let commande = parse_command recue in
  print_endline "blop";
  match commande with
    | Protocol.Connect(name)
      -> let new_name = rename name 0 in
	 Some (treat_connexion new_name sd)
      (*|"LOGIN"::login::password::_
	-> if check_login login password = 1
	then
	begin 
	treat_connexion login sd;
	1
	end
	else  (* erreur log *) 0
	
	|"REGISTER"::login::password::_ ->
	if check_register login password = 1
	then 
	begin 
	treat_connexion login sd;
	1
	end
	else  (* erreur log *) 0
	|"SPECTATOR"::_ -> 
	let spect = { TODO !
      *)
    | _ -> None
      
let connexion sd sa =
  match connexion_client sd sa with
      None -> print_endline "erreur connect : stop th_client"; stop_thread_client sd
    | Some(client) -> print_endline "th_joueur :"; th_joueur client
    | Some(_) -> (* spectator *) ()
    | _ -> stop_thread_client sd
   
let serv_socket_run port =
  let sock = ThreadUnix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let mon_adresse =
    (* (Unix.gethostbyname(Unix.gethostname()).Unix.h_addr_list.(0)) in *)	
    Unix.inet_addr_of_string "127.0.0.1" in
  Unix.bind sock (Unix.ADDR_INET(mon_adresse,port));
  Unix.listen sock 3;
  while true do
    let sd, sa = ThreadUnix.accept sock in
    begin
      Unix.setsockopt sd Unix.SO_REUSEADDR true;
      ignore (Thread.create 
		(fun x -> (connexion sd sa) ; print_endline "stop_th"; (stop_thread_client sd) )
		() 
      );
      print_endline "th client lanc�"
    end
  done

let () =
  Unix.handle_unix_error serv_socket_run port
    
let test () =
  let l = [
    "CONNECT/\\\\toto/\n"
    ; "CONNECT/\\/toto/\n"
    ; "CONNECT/to\\/to/\n"
    ; "CONNECT/to\\\\to/\n"
    ; "CONNECT/toto\\//\n"
    ; "CONNECT/toto\\\\/\n"
    ; "SCORE_ROUND/toto/1/\n"
    ; "NEW_ROUND/toto/\n"
    ; "NEW_ROUND/toto/1/\n"
    ; "NEW_ROUND/toto/1.0/\n"
    ; "SET_COLOR/1/2/3/\n"
    ; "SET_LINE/1/1/1/1/\n"
    ; "SET_SIZE/3/\n"
      
    ] in
  List.iter (fun e -> print_endline (Protocol.string_of_command (parse_command e))) l; 
  print_endline (Protocol.string_of_command (Protocol.Connect("user2")));
  print_endline (Protocol.string_of_command (Protocol.Connected("user")));
  print_endline (Protocol.string_of_command (Protocol.Exited("user/\2")));
  print_endline (Protocol.string_of_command (Protocol.Welcome("user\/3")))